use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    sync::Arc,
};

use serde::Deserialize;
use tree_sitter_lint::{
    rule, tree_sitter::Node, violation, Fixer, NodeExt, QueryMatchContext, Rule, SourceTextProvider,
};

use crate::kind::{
    GenericType, Identifier, ScopedIdentifier, ScopedUseList, StructItem, UseAsClause,
    UseDeclaration, UseList,
};

#[derive(Deserialize)]
struct Options {
    known_imports: KnownImports,
}

type KnownImports = HashMap<String, KnownImportSpec>;

#[derive(Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum KnownImportSpec {
    GenericType {
        module: String,
        name: Option<String>,
    },
    TypeIdentifier {
        module: String,
        name: Option<String>,
    },
    TraitMethod {
        module: String,
        #[serde(rename = "trait")]
        trait_: String,
    },
}

impl KnownImportSpec {
    pub fn name(&self) -> Option<&str> {
        match self {
            Self::GenericType { name, .. } => name.as_deref(),
            Self::TypeIdentifier { name, .. } => name.as_deref(),
            _ => None,
        }
    }

    pub fn module(&self) -> &str {
        match self {
            Self::GenericType { module, .. } => module,
            Self::TypeIdentifier { module, .. } => module,
            Self::TraitMethod { module, .. } => module,
        }
    }
}

fn is_use_import(node: Node) -> bool {
    let mut current_node = node;
    loop {
        let parent = current_node.parent().unwrap();
        match parent.kind() {
            UseAsClause => return current_node == parent.field("alias"),
            UseList => return true,
            ScopedIdentifier => (),
            UseDeclaration => return true,
            _ => return false,
        }
        current_node = parent;
    }
}

fn get_use_import_path<'a>(
    node: Node,
    source_text_provider: &impl SourceTextProvider<'a>,
) -> Option<String> {
    let mut current_node = node;
    let mut path: Vec<Cow<'_, str>> = vec![current_node.text(source_text_provider)];
    loop {
        let parent = current_node.parent().unwrap();
        match parent.kind() {
            UseAsClause => {
                if current_node == parent.field("alias") {
                    path.pop().unwrap();
                    path.push(parent.field("path").text(source_text_provider));
                }
            }
            UseList => (),
            ScopedUseList => {
                if current_node == parent.field("list") {
                    if let Some(parent_path) = parent.child_by_field_name("path") {
                        path.push(parent_path.text(source_text_provider));
                    }
                } else {
                    return None;
                }
            }
            ScopedIdentifier => {
                if current_node == parent.field("name") {
                    if let Some(parent_path) = parent.child_by_field_name("path") {
                        path.push(parent_path.text(source_text_provider));
                    }
                } else {
                    return None;
                }
            }
            UseDeclaration => return Some(path.into_iter().rev().collect::<Vec<_>>().join("::")),
            _ => return None,
        }
        current_node = parent;
    }
}

fn insert_import(
    fixer: &mut Fixer,
    name: &str,
    known_import_spec: &KnownImportSpec,
    root_node: Node,
    context: &QueryMatchContext,
) {
    let last_existing_top_level_use_declaration = root_node
        .non_comment_named_children(context)
        .filter(|child| child.kind() == UseDeclaration)
        .last();

    let name_to_import = match known_import_spec {
        KnownImportSpec::TraitMethod { trait_, .. } => trait_,
        _ => name,
    };

    let new_use_declaration_text = match known_import_spec.name() {
        Some(real_name) => format!(
            "use {}::{{{} as {}}};",
            known_import_spec.module(),
            real_name,
            name_to_import
        ),
        None => format!("use {}::{};", known_import_spec.module(), name_to_import),
    };
    match last_existing_top_level_use_declaration {
        Some(last_existing_top_level_use_declaration) => {
            fixer.insert_text_after(
                last_existing_top_level_use_declaration,
                format!("\n{new_use_declaration_text}"),
            );
        }
        None => {
            fixer.insert_text_before(
                root_node
                    .non_comment_named_children(context)
                    .next()
                    .unwrap(),
                format!("{new_use_declaration_text}\n\n"),
            );
        }
    }
}

fn is_type_definition(node: Node) -> bool {
    let parent = node.parent().unwrap();
    match parent.kind() {
        StructItem => parent.field("name") == node,
        _ => false,
    }
}

fn is_beginning_of_path(node: Node) -> bool {
    let mut current_node = node;
    while current_node.kind() == Identifier {
        let parent = current_node.parent().unwrap();
        if parent.kind() == ScopedIdentifier
            && parent.child_by_field_name("path") == Some(current_node)
        {
            return true;
        }
        current_node = parent;
    }
    false
}

pub fn no_undef_rule() -> Arc<dyn Rule> {
    type FullTraitPath = String;

    rule! {
        name => "no-undef",
        fixable => true,
        messages => [
            "not_defined" => "'{{name}}' is not defined.",
            "trait_not_in_scope" => "'{{name}}' is not in scope.",
        ],
        languages => [Rust],
        options_type! => Options,
        state => {
            [per-run]
            known_traits: HashMap<String, Vec<FullTraitPath>> = {
                let mut known_traits: HashMap<String, Vec<FullTraitPath>> = Default::default();

                for known_import in options.known_imports.values() {
                    if let KnownImportSpec::TraitMethod { trait_, module, .. } = known_import {
                        known_traits.entry(trait_.clone()).or_default().push(format!("{}::{}", module, trait_));
                    }
                }

                known_traits
            },
            known_imports: KnownImports = options.known_imports,

            [per-file-run]
            defined_known_imports: HashSet<String>,
            imported_known_imports: HashMap<String, Node<'a>>,
            imported_known_traits: HashMap<FullTraitPath, Node<'a>>,
            referenced_known_imports: HashMap<String, Vec<Node<'a>>>,
            referenced_known_traits: HashMap<FullTraitPath, Vec<Node<'a>>>,
        },
        listeners => [
            "
              (type_identifier) @c
            " => |node, context| {
                let name = node.text(context);
                let Some(known_import) = self.known_imports.get(&*name) else {
                    return;
                };

                if is_type_definition(node) {
                    self.defined_known_imports.insert(name.into_owned());
                    return;
                }

                if matches!(
                    known_import,
                    KnownImportSpec::GenericType { .. }
                ) && node.parent().unwrap().kind() != GenericType {
                    return;
                }

                self.referenced_known_imports.entry(name.into_owned())
                    .or_default()
                    .push(node);
            },
            "
              (identifier) @c
            " => |node, context| {
                let name = node.text(context);
                if let Some(known_trait_imports) = self.known_traits.get(&*name) {
                    if let Some(full_imported_path) = get_use_import_path(node, context) {
                        if known_trait_imports.contains(&full_imported_path) {
                            self.imported_known_traits.insert(
                                full_imported_path,
                                node,
                            );
                        }
                    }
                }

                if !self.known_imports.contains_key(&*name) {
                    return;
                };

                if is_use_import(node) {
                    self.imported_known_imports.insert(name.into_owned(), node);
                    return;
                }

                if is_beginning_of_path(node) {
                    self.referenced_known_imports.entry(name.into_owned())
                        .or_default()
                        .push(node);
                }
            },
            "
              (call_expression
                function: (field_expression
                  field: (field_identifier) @c
                )
              )
            " => |node, context| {
                let method_name = node.text(context);

                let Some(KnownImportSpec::TraitMethod { module, trait_ }) = self.known_imports.get(&*method_name) else {
                    return;
                };

                let full_trait_path = format!("{module}::{trait_}");

                self.referenced_known_traits.entry(full_trait_path).or_default().push(node);
            },
            "source_file:exit" => |node, context| {
                self.referenced_known_imports.iter().filter(|(referenced_known_import, _)| {
                    !self.defined_known_imports.contains(*referenced_known_import) &&
                        !self.imported_known_imports.contains_key(*referenced_known_import)
                }).for_each(|(name, references)| {
                    for (index, &reference) in references.iter().enumerate() {
                        context.report(violation! {
                            node => reference,
                            message_id => "not_defined",
                            data => {
                                name => name,
                            },
                            fix => |fixer| {
                                if index != 0 {
                                    return;
                                }

                                insert_import(fixer, name, &self.known_imports[name], node, context);
                            }
                        });
                    }
                });

                self.referenced_known_traits.iter().filter(|(referenced_known_trait, _)| {
                    !self.imported_known_traits.contains_key(*referenced_known_trait)
                }).for_each(|(name, references)| {
                    for (index, &reference) in references.iter().enumerate() {
                        context.report(violation! {
                            node => reference,
                            message_id => "trait_not_in_scope",
                            data => {
                                name => name,
                            },
                            fix => |fixer| {
                                if index != 0 {
                                    return;
                                }

                                insert_import(fixer, name, &self.known_imports[&*reference.text(context)], node, context);
                            }
                        });
                    }
                });
            }
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use serde_json::json;
    use tree_sitter_lint::{
        get_tokens, rule_tests, tree_sitter::Parser, tree_sitter_grep::SupportedLanguage,
        RuleTester,
    };

    #[test]
    fn test_generic_type() {
        let id_options = json!({
            "known_imports": {
                "Id": {
                    "module": "foo",
                    "kind": "generic_type",
                }
            }
        });
        let index_as_id_options = json!({
            "known_imports": {
                "Id": {
                    "module": "foo",
                    "kind": "generic_type",
                    "name": "Index",
                }
            }
        });
        RuleTester::run(
            no_undef_rule(),
            rule_tests! {
                valid => [
                    {
                        code => "
                            use foo::Id;

                            struct Foo {
                                id: Id<usize>,
                            }
                        ",
                        options => id_options,
                    },
                    // imported from somewhere else
                    {
                        code => "
                            use bar::Id;

                            struct Foo {
                                id: Id<usize>,
                            }
                        ",
                        options => id_options,
                    },
                    // alias
                    {
                        code => "
                            use foo::{Index as Id};

                            struct Foo {
                                id: Id<usize>,
                            }
                        ",
                        options => index_as_id_options,
                    },
                ],
                invalid => [
                    {
                        code => "\
struct Foo {
    id: Id<usize>,
}
                        ",
                        output => "\
use foo::Id;

struct Foo {
    id: Id<usize>,
}
                        ",
                        options => id_options,
                        errors => 1,
                    },
                    // existing import
                    {
                        code => "\
use bar::baz;

struct Foo {
    id: Id<usize>,
}
                        ",
                        output => "\
use bar::baz;
use foo::Id;

struct Foo {
    id: Id<usize>,
}
                        ",
                        options => id_options,
                        errors => 1,
                    },
                    // alias
                    {
                        code => "\
struct Foo {
    id: Id<usize>,
}
                        ",
                        output => "\
use foo::{Index as Id};

struct Foo {
    id: Id<usize>,
}
                        ",
                        options => index_as_id_options,
                        errors => 1,
                    },
                ]
            },
        );
    }

    #[test]
    fn test_simple_type() {
        let id_options = json!({
            "known_imports": {
                "Id": {
                    "module": "foo",
                    "kind": "type_identifier",
                }
            }
        });
        let index_as_id_options = json!({
            "known_imports": {
                "Id": {
                    "module": "foo",
                    "kind": "type_identifier",
                    "name": "Index",
                }
            }
        });
        RuleTester::run(
            no_undef_rule(),
            rule_tests! {
                valid => [
                    {
                        code => "
                            use foo::Id;

                            struct Foo {
                                id: Id,
                            }
                        ",
                        options => id_options,
                    },
                    // imported from somewhere else
                    {
                        code => "
                            use bar::Id;

                            struct Foo {
                                id: Id,
                            }
                        ",
                        options => id_options,
                    },
                    // alias
                    {
                        code => "
                            use foo::{Index as Id};

                            struct Foo {
                                id: Id,
                            }
                        ",
                        options => index_as_id_options,
                    },
                ],
                invalid => [
                    {
                        code => "\
struct Foo {
    id: Id,
}
                        ",
                        output => "\
use foo::Id;

struct Foo {
    id: Id,
}
                        ",
                        options => id_options,
                        errors => 1,
                    },
                    // existing import
                    {
                        code => "\
use bar::baz;

struct Foo {
    id: Id,
}
                        ",
                        output => "\
use bar::baz;
use foo::Id;

struct Foo {
    id: Id,
}
                        ",
                        options => id_options,
                        errors => 1,
                    },
                    // alias
                    {
                        code => "\
struct Foo {
    id: Id,
}
                        ",
                        output => "\
use foo::{Index as Id};

struct Foo {
    id: Id,
}
                        ",
                        options => index_as_id_options,
                        errors => 1,
                    },
                    // scoped identifier
                    {
                        code => "\
let x = Id::Something;
                        ",
                        output => "\
use foo::Id;

let x = Id::Something;
                        ",
                        options => id_options,
                        errors => 1,
                    },
                ]
            },
        );
    }

    #[test]
    fn test_trait_method() {
        let trait_method_options = json!({
            "known_imports": {
                "method": {
                    "module": "foo",
                    "kind": "trait_method",
                    "trait": "Trait",
                }
            }
        });
        RuleTester::run(
            no_undef_rule(),
            rule_tests! {
                valid => [
                    {
                        code => "
                            use foo::Trait;

                            fn whee() {
                                x.method();
                            }
                        ",
                        options => trait_method_options,
                    },
                    // alias
                    {
                        code => "
                            use foo::{Trait as _};

                            fn whee() {
                                x.method();
                            }
                        ",
                        options => trait_method_options,
                    },
                ],
                invalid => [
                    {
                        code => "\
fn whee() {
    x.method();
}
                        ",
                        output => "\
use foo::Trait;

fn whee() {
    x.method();
}
                        ",
                        options => trait_method_options,
                        errors => 1,
                    },
                    // same name imported from somewhere else
                    {
                        code => "\
use bar::Trait;

fn whee() {
    x.method();
}
                        ",
                        output => "\
use bar::Trait;
use foo::Trait;

fn whee() {
    x.method();
}
                        ",
                        options => trait_method_options,
                        errors => 1,
                    },
                ]
            },
        );
    }

    fn find_identifier<'a>(root_node: Node<'a>, identifier: &str, text: &[u8]) -> Node<'a> {
        get_tokens(root_node)
            .find(|node| node.text(&text) == identifier)
            .unwrap()
    }

    #[test]
    fn test_get_use_import_path() {
        [
            ("use bar;", "bar"),
            ("use foo::bar;", "foo::bar"),
            ("use foo as bar;", "foo"),
            ("use foo::baz as bar;", "foo::baz"),
            ("use {foo as bar};", "foo"),
            ("use {foo::baz as bar};", "foo::baz"),
            ("use foo::{bar};", "foo::bar"),
            ("use foo::{baz as bar};", "foo::baz"),
            ("use foo::baz::bar;", "foo::baz::bar"),
            ("use foo::{baz::bar};", "foo::baz::bar"),
            ("use foo::{bar as baz};", "foo::bar"),
            ("use foo::bar as baz;", "foo::bar"),
        ]
        .into_iter()
        .for_each(|(code, path)| {
            let mut parser = Parser::new();
            parser
                .set_language(SupportedLanguage::Rust.language())
                .unwrap();
            let tree = parser.parse(code, None).unwrap();
            assert_eq!(
                get_use_import_path(
                    find_identifier(tree.root_node(), "bar", code.as_bytes()),
                    &code.as_bytes()
                )
                .as_deref(),
                Some(path),
                "Expected '{path}' for '{code}'",
            );
        });
    }
}
