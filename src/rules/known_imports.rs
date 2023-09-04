use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    sync::Arc,
};

use itertools::Itertools;
use serde::Deserialize;
use tree_sitter_lint::{
    range_between_starts, rule,
    squalid::{EverythingExt, VecExt},
    tree_sitter::Node,
    tree_sitter_grep::SupportedLanguage,
    violation, Fixer, NodeExt, QueryMatchContext, Rule, SourceTextProvider,
};
use tree_sitter_lint_plugin_rust_scope_analysis::{Reference, ScopeAnalyzer, UsageKind};

use crate::kind::{ScopedIdentifier, ScopedUseList, UseAsClause, UseDeclaration, UseList};

#[derive(Deserialize)]
struct Options {
    known_imports: KnownImports,
}

type KnownImports = HashMap<String, KnownImportSpec>;

#[derive(Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum KnownImportSpec {
    Type {
        module: String,
        name: Option<String>,
    },
    TraitMethod {
        module: String,
        #[serde(rename = "trait")]
        trait_: String,
    },
    Macro {
        module: String,
        name: Option<String>,
    },
    Attribute {
        module: String,
        name: Option<String>,
    },
    Static {
        module: String,
        name: Option<String>,
    },
    Function {
        module: String,
        name: Option<String>,
    },
    Module {
        module: String,
        name: Option<String>,
    },
}

impl KnownImportSpec {
    pub fn name(&self) -> Option<&str> {
        match self {
            Self::Type { name, .. } => name.as_deref(),
            Self::Macro { name, .. } => name.as_deref(),
            Self::Attribute { name, .. } => name.as_deref(),
            Self::Static { name, .. } => name.as_deref(),
            Self::Function { name, .. } => name.as_deref(),
            Self::Module { name, .. } => name.as_deref(),
            _ => None,
        }
    }

    pub fn module(&self) -> &str {
        match self {
            Self::Type { module, .. } => module,
            Self::TraitMethod { module, .. } => module,
            Self::Macro { module, .. } => module,
            Self::Attribute { module, .. } => module,
            Self::Static { module, .. } => module,
            Self::Function { module, .. } => module,
            Self::Module { module, .. } => module,
        }
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

fn is_compatible_usage_kind(reference: &Reference, known_import: &KnownImportSpec) -> bool {
    matches!(
        (reference.usage_kind(), known_import),
        (UsageKind::IdentifierReference, KnownImportSpec::Type { .. })
            | (
                UsageKind::IdentifierReference,
                KnownImportSpec::Static { .. }
            )
            | (
                UsageKind::IdentifierReference,
                KnownImportSpec::Function { .. }
            )
            | (
                UsageKind::IdentifierReference,
                KnownImportSpec::Module { .. }
            )
            | (UsageKind::AttributeName, KnownImportSpec::Attribute { .. })
            | (UsageKind::Macro, KnownImportSpec::Macro { .. })
    )
}

fn remove_from_use_declaration<'a>(
    node: Node<'a>,
    fixer: &mut Fixer,
    context: &QueryMatchContext<'a, '_>,
) {
    let mut prev_ancestor = node;
    if node.ancestors().any(|ancestor| {
        if ancestor.kind() == UseList
            && ancestor.num_non_comment_named_children(SupportedLanguage::Rust) > 1
        {
            return true;
        }
        prev_ancestor = ancestor;
        false
    }) {
        let mut range = prev_ancestor.range();
        if let Some(following_comma) = context
            .get_token_after(prev_ancestor, Option::<fn(Node) -> bool>::None)
            .when(|token| token.kind() == ",")
        {
            let token_after_comma =
                context.get_token_after(following_comma, Option::<fn(Node) -> bool>::None);
            range = range_between_starts(range, token_after_comma.range());
        }
        fixer.remove_range(range);
    } else {
        fixer.remove(node.next_ancestor_of_kind(UseDeclaration));
    }
}

pub fn known_imports_rule() -> Arc<dyn Rule> {
    type FullTraitPath = String;

    rule! {
        name => "known-imports",
        fixable => true,
        messages => [
            "not_defined" => "'{{name}}' is not defined.",
            "trait_not_in_scope" => "'{{name}}' is not in scope.",
            "unused" => "'{{name}}' is not used.",
        ],
        languages => [Rust],
        concatenate_adjacent_insert_fixes => true,
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
            imported_known_traits: HashMap<FullTraitPath, Node<'a>>,
            referenced_known_traits: HashMap<FullTraitPath, Vec<Node<'a>>>,
        },
        listeners => [
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
            },
            "source_file:exit" => |node, context| {
                let scope_analyzer = context.retrieve::<ScopeAnalyzer<'a>>();

                let mut already_added: HashSet<*const KnownImportSpec> = Default::default();
                let root_scope = scope_analyzer.root_scope();
                for reference in root_scope.through() {
                    let reference_node = reference.node();
                    let name = reference_node.text(context);
                    if let Some(known_import) = self.known_imports.get(&*name) {
                        if is_compatible_usage_kind(&reference, known_import) {
                            context.report(violation! {
                                node => reference_node,
                                message_id => "not_defined",
                                data => {
                                    name => name,
                                },
                                fix => |fixer| {
                                    if already_added.contains(&(known_import as *const KnownImportSpec)) {
                                        return;
                                    }

                                    insert_import(fixer, &name, known_import, node, context);
                                }
                            });
                            already_added.insert(known_import);
                        }
                    }
                }

                self.referenced_known_traits.iter().filter(|(referenced_known_trait, _)| {
                    !self.imported_known_traits.contains_key(*referenced_known_trait)
                }).collect_vec().and_sort_by(|(_, references_a), (_, references_b)| {
                    references_a[0].range().start_byte.cmp(&references_b[0].range().start_byte)
                }).into_iter().for_each(|(name, references)| {
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

                for variable in root_scope.variables().filter(|variable| {
                    variable.references().next().is_none() &&
                        variable.definition().node().kind() == UseDeclaration &&
                        self.known_imports.get(variable.name()).filter(|known_import| {
                            !matches!(
                                known_import,
                                KnownImportSpec::TraitMethod { .. }
                            )
                        }).is_some()
                }) {
                    context.report(violation! {
                        node => variable.definition().name(),
                        message_id => "unused",
                        data => {
                            name => variable.name(),
                        },
                        fix => |fixer| {
                            remove_from_use_declaration(variable.definition().name(), fixer, context);
                        }
                    });
                }
            }
        ]
    }
}

#[cfg(test)]
mod tests {
    use crate::get_instance_provider_factory;

    use super::*;

    use serde_json::json;
    use tree_sitter_lint::{
        get_tokens, rule_tests, squalid::run_once, tree_sitter::Parser,
        tree_sitter_grep::SupportedLanguage, RuleTester,
    };

    fn tracing_subscribe() {
        run_once! {
            tracing_subscriber::fmt::init();
        }
    }

    #[test]
    fn test_generic_type() {
        tracing_subscribe();

        let id_options = json!({
            "known_imports": {
                "Id": {
                    "module": "foo",
                    "kind": "type",
                }
            }
        });
        let index_as_id_options = json!({
            "known_imports": {
                "Id": {
                    "module": "foo",
                    "kind": "type",
                    "name": "Index",
                }
            }
        });
        RuleTester::run_with_from_file_run_context_instance_provider(
            known_imports_rule(),
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
            get_instance_provider_factory(),
        );
    }

    #[test]
    fn test_simple_type() {
        tracing_subscribe();

        let id_options = json!({
            "known_imports": {
                "Id": {
                    "module": "foo",
                    "kind": "type",
                }
            }
        });
        let index_as_id_options = json!({
            "known_imports": {
                "Id": {
                    "module": "foo",
                    "kind": "type",
                    "name": "Index",
                }
            }
        });
        RuleTester::run_with_from_file_run_context_instance_provider(
            known_imports_rule(),
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
            get_instance_provider_factory(),
        );
    }

    #[test]
    fn test_trait_method() {
        tracing_subscribe();

        let trait_method_options = json!({
            "known_imports": {
                "method": {
                    "module": "foo",
                    "kind": "trait_method",
                    "trait": "Trait",
                }
            }
        });
        RuleTester::run_with_from_file_run_context_instance_provider(
            known_imports_rule(),
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
            get_instance_provider_factory(),
        );
    }

    fn find_identifier<'a>(root_node: Node<'a>, identifier: &str, text: &[u8]) -> Node<'a> {
        get_tokens(root_node)
            .find(|node| node.text(&text) == identifier)
            .unwrap()
    }

    #[test]
    fn test_get_use_import_path() {
        tracing_subscribe();

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

    #[test]
    fn test_derive() {
        tracing_subscribe();

        let id_options = json!({
            "known_imports": {
                "Id": {
                    "module": "foo",
                    "kind": "type",
                }
            }
        });
        RuleTester::run_with_from_file_run_context_instance_provider(
            known_imports_rule(),
            rule_tests! {
                valid => [
                    {
                        code => "
                            use foo::Id;

                            #[derive(Id)]
                            struct Foo {}
                        ",
                        options => id_options,
                    },
                ],
                invalid => [
                    {
                        code => "\
#[derive(Id)]
struct Foo {}
                        ",
                        output => "\
use foo::Id;

#[derive(Id)]
struct Foo {}
                        ",
                        options => id_options,
                        errors => 1,
                    },
                ]
            },
            get_instance_provider_factory(),
        );
    }

    #[test]
    fn test_macro() {
        tracing_subscribe();

        let id_options = json!({
            "known_imports": {
                "id": {
                    "module": "foo",
                    "kind": "macro",
                }
            }
        });
        RuleTester::run_with_from_file_run_context_instance_provider(
            known_imports_rule(),
            rule_tests! {
                valid => [
                    {
                        code => "
                            use foo::id;

                            fn whee() {
                                id!();
                            }
                        ",
                        options => id_options,
                    },
                ],
                invalid => [
                    {
                        code => "\
fn whee() {
    id!();
}
                        ",
                        output => "\
use foo::id;

fn whee() {
    id!();
}
                        ",
                        options => id_options,
                        errors => 1,
                    },
                ]
            },
            get_instance_provider_factory(),
        );
    }

    #[test]
    fn test_multiple_fixes() {
        tracing_subscribe();

        let id_options = json!({
            "known_imports": {
                "Id": {
                    "module": "foo",
                    "kind": "type",
                },
                "Idz": {
                    "module": "bar",
                    "kind": "type",
                },
            }
        });
        RuleTester::run_with_from_file_run_context_instance_provider(
            known_imports_rule(),
            rule_tests! {
                valid => [
                    {
                        code => "
                            use foo::Id;
                            use bar::Idz;

                            struct Foo {
                                id: Id,
                                idz: Idz,
                            }
                        ",
                        options => id_options,
                    },
                ],
                invalid => [
                    {
                        code => "\
struct Foo {
    id: Id,
    idz: Idz,
}
                        ",
                        output => "\
use foo::Id;

use bar::Idz;

struct Foo {
    id: Id,
    idz: Idz,
}
                        ",
                        options => id_options,
                        errors => 2,
                    },
                ]
            },
            get_instance_provider_factory(),
        );
    }

    #[test]
    fn test_attribute() {
        tracing_subscribe();

        let id_options = json!({
            "known_imports": {
                "id": {
                    "module": "foo",
                    "kind": "attribute",
                }
            }
        });
        RuleTester::run_with_from_file_run_context_instance_provider(
            known_imports_rule(),
            rule_tests! {
                valid => [
                    {
                        code => "
                            use foo::id;

                            #[id]
                            fn whee() {}
                        ",
                        options => id_options,
                    },
                ],
                invalid => [
                    {
                        code => "\
#[id]
fn whee() {}
                        ",
                        output => "\
use foo::id;

#[id]
fn whee() {}
                        ",
                        options => id_options,
                        errors => 1,
                    },
                ]
            },
            get_instance_provider_factory(),
        );
    }

    #[test]
    fn test_const() {
        tracing_subscribe();

        let id_options = json!({
            "known_imports": {
                "Id": {
                    "module": "foo",
                    "kind": "static",
                }
            }
        });
        RuleTester::run_with_from_file_run_context_instance_provider(
            known_imports_rule(),
            rule_tests! {
                valid => [
                    {
                        code => "
                            use foo::Id;

                            fn whee() {
                                let x = Id;
                            }
                        ",
                        options => id_options,
                    },
                ],
                invalid => [
                    {
                        code => "\
fn whee() {
    let x = Id;
}
                        ",
                        output => "\
use foo::Id;

fn whee() {
    let x = Id;
}
                        ",
                        options => id_options,
                        errors => 1,
                    },
                ]
            },
            get_instance_provider_factory(),
        );
    }

    #[test]
    fn test_function() {
        tracing_subscribe();

        let id_options = json!({
            "known_imports": {
                "id": {
                    "module": "foo",
                    "kind": "function",
                }
            }
        });
        RuleTester::run_with_from_file_run_context_instance_provider(
            known_imports_rule(),
            rule_tests! {
                valid => [
                    {
                        code => "
                            use foo::id;

                            fn whee() {
                                id();
                            }
                        ",
                        options => id_options,
                    },
                ],
                invalid => [
                    {
                        code => "\
fn whee() {
    id();
}
                        ",
                        output => "\
use foo::id;

fn whee() {
    id();
}
                        ",
                        options => id_options,
                        errors => 1,
                    },
                ]
            },
            get_instance_provider_factory(),
        );
    }

    #[test]
    fn test_module() {
        tracing_subscribe();

        let id_options = json!({
            "known_imports": {
                "id": {
                    "module": "foo",
                    "kind": "module",
                }
            }
        });
        RuleTester::run_with_from_file_run_context_instance_provider(
            known_imports_rule(),
            rule_tests! {
                valid => [
                    {
                        code => "
                            use foo::id;

                            fn whee() {
                                id::something();
                            }
                        ",
                        options => id_options,
                    },
                ],
                invalid => [
                    {
                        code => "\
fn whee() {
    id::something();
}
                        ",
                        output => "\
use foo::id;

fn whee() {
    id::something();
}
                        ",
                        options => id_options,
                        errors => 1,
                    },
                ]
            },
            get_instance_provider_factory(),
        );
    }

    #[test]
    fn test_basic_removal() {
        tracing_subscribe();

        let id_options = json!({
            "known_imports": {
                "id": {
                    "module": "foo",
                    "kind": "module",
                }
            }
        });
        RuleTester::run_with_from_file_run_context_instance_provider(
            known_imports_rule(),
            rule_tests! {
                valid => [
                    {
                        code => "
                            use foo::id;

                            fn whee() {
                                id::something();
                            }
                        ",
                        options => id_options,
                    },
                ],
                invalid => [
                    {
                        code => "\
use foo::id;
fn whee() {
    something();
}
                        ",
                        output => "\
\nfn whee() {
    something();
}
                        ",
                        options => id_options,
                        errors => 1,
                    },
                ]
            },
            get_instance_provider_factory(),
        );
    }

    #[test]
    fn test_remove_nested() {
        tracing_subscribe();

        let id_options = json!({
            "known_imports": {
                "id": {
                    "module": "foo::bar",
                    "kind": "module",
                }
            }
        });
        RuleTester::run_with_from_file_run_context_instance_provider(
            known_imports_rule(),
            rule_tests! {
                valid => [
                    {
                        code => "
                            use foo::bar::id;

                            fn whee() {
                                id::something();
                            }
                        ",
                        options => id_options,
                    },
                ],
                invalid => [
                    {
                        code => "\
use foo::bar::id;
fn whee() {
    something();
}
                        ",
                        output => "\
\nfn whee() {
    something();
}
                        ",
                        options => id_options,
                        errors => 1,
                    },
                ]
            },
            get_instance_provider_factory(),
        );
    }

    #[test]
    fn test_remove_alias() {
        tracing_subscribe();

        let id_options = json!({
            "known_imports": {
                "id": {
                    "module": "foo",
                    "kind": "module",
                    "name": "bar",
                }
            }
        });
        RuleTester::run_with_from_file_run_context_instance_provider(
            known_imports_rule(),
            rule_tests! {
                valid => [
                    {
                        code => "
                            use foo::bar as id;

                            fn whee() {
                                id::something();
                            }
                        ",
                        options => id_options,
                    },
                ],
                invalid => [
                    {
                        code => "\
use foo::bar as id;
fn whee() {
    something();
}
                        ",
                        output => "\
\nfn whee() {
    something();
}
                        ",
                        options => id_options,
                        errors => 1,
                    },
                ]
            },
            get_instance_provider_factory(),
        );
    }

    #[test]
    fn test_remove_others_simple() {
        tracing_subscribe();

        let id_options = json!({
            "known_imports": {
                "id": {
                    "module": "foo",
                    "kind": "module",
                }
            }
        });
        RuleTester::run_with_from_file_run_context_instance_provider(
            known_imports_rule(),
            rule_tests! {
                valid => [
                    {
                        code => "
                            use foo::{id, baz};

                            fn whee() {
                                id::something();
                            }
                        ",
                        options => id_options,
                    },
                ],
                invalid => [
                    {
                        code => "\
use foo::{id, baz};
fn whee() {
    something();
}
                        ",
                        output => "\
use foo::{baz};
fn whee() {
    something();
}
                        ",
                        options => id_options,
                        errors => 1,
                    },
                ]
            },
            get_instance_provider_factory(),
        );
    }

    #[test]
    fn test_remove_others_alias() {
        tracing_subscribe();

        let id_options = json!({
            "known_imports": {
                "id": {
                    "module": "foo",
                    "kind": "module",
                    "name": "bar",
                }
            }
        });
        RuleTester::run_with_from_file_run_context_instance_provider(
            known_imports_rule(),
            rule_tests! {
                valid => [
                    {
                        code => "
                            use foo::{bar as id, baz};

                            fn whee() {
                                id::something();
                            }
                        ",
                        options => id_options,
                    },
                ],
                invalid => [
                    {
                        code => "\
use foo::{bar as id, baz};
fn whee() {
    something();
}
                        ",
                        output => "\
use foo::{baz};
fn whee() {
    something();
}
                        ",
                        options => id_options,
                        errors => 1,
                    },
                ]
            },
            get_instance_provider_factory(),
        );
    }

    #[test]
    fn test_remove_others_trailing() {
        tracing_subscribe();

        let id_options = json!({
            "known_imports": {
                "id": {
                    "module": "foo",
                    "kind": "module",
                }
            }
        });
        RuleTester::run_with_from_file_run_context_instance_provider(
            known_imports_rule(),
            rule_tests! {
                valid => [
                    {
                        code => "
                            use foo::{id, baz};

                            fn whee() {
                                id::something();
                            }
                        ",
                        options => id_options,
                    },
                ],
                invalid => [
                    {
                        code => "\
use foo::{baz, id};
fn whee() {
    something();
}
                        ",
                        output => "\
use foo::{baz, };
fn whee() {
    something();
}
                        ",
                        options => id_options,
                        errors => 1,
                    },
                ]
            },
            get_instance_provider_factory(),
        );
    }
}
