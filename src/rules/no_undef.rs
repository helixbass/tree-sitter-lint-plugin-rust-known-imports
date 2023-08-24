use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use serde::Deserialize;
use tree_sitter_lint::{
    rule, tree_sitter::Node, violation, Fixer, NodeExt, QueryMatchContext, Rule,
};

use crate::kind::{
    GenericType, Identifier, ScopedIdentifier, StructItem, UseAsClause, UseDeclaration, UseList,
};

#[derive(Deserialize)]
struct Options {
    known_imports: KnownImports,
}

type KnownImports = HashMap<String, KnownImportSpec>;

#[derive(Deserialize)]
struct KnownImportSpec {
    module: String,
    #[allow(dead_code)]
    kind: KnownImportKind,
    name: Option<String>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
enum KnownImportKind {
    GenericType,
    TypeIdentifier,
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

    let new_use_declaration_text = match known_import_spec.name.as_ref() {
        Some(real_name) => format!(
            "use {}::{{{} as {}}};",
            known_import_spec.module, real_name, name
        ),
        None => format!("use {}::{};", known_import_spec.module, name),
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
    rule! {
        name => "no-undef",
        fixable => true,
        messages => [
            "not_defined" => "'{{name}}' is not defined.",
        ],
        languages => [Rust],
        options_type! => Options,
        state => {
            [per-run]
            known_imports: KnownImports = options.known_imports,

            [per-file-run]
            defined_known_imports: HashSet<String>,
            imported_known_imports: HashMap<String, Node<'a>>,
            referenced_known_imports: HashMap<String, Vec<Node<'a>>>,
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

                if known_import.kind == KnownImportKind::GenericType && node.parent().unwrap().kind() != GenericType {
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
            }
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use serde_json::json;
    use tree_sitter_lint::{rule_tests, RuleTester};

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
}
