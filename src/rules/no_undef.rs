use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use serde::Deserialize;
use tree_sitter_lint::{
    rule, tree_sitter::Node, violation, Fixer, NodeExt, QueryMatchContext, Rule,
};

use crate::kind::{GenericType, ScopedIdentifier, UseAsClause, UseDeclaration, UseList};

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
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
enum KnownImportKind {
    GenericType,
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

    let new_use_declaration_text = format!("use {}::{};\n", known_import_spec.module, name);
    match last_existing_top_level_use_declaration {
        Some(last_existing_top_level_use_declaration) => {
            fixer.insert_text_after(
                last_existing_top_level_use_declaration,
                new_use_declaration_text,
            );
        }
        None => {
            fixer.insert_text_before(
                root_node
                    .non_comment_named_children(context)
                    .next()
                    .unwrap(),
                format!("{new_use_declaration_text}\n"),
            );
        }
    }
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
            imported_known_imports: HashSet<String>,
            referenced_known_imports: HashMap<String, Vec<Node<'a>>>,
        },
        listeners => [
            "
              (type_identifier) @c
            " => |node, context| {
                if node.parent().unwrap().kind() != GenericType {
                    return;
                }

                let name = node.text(context);
                if !self.known_imports.contains_key(&*name) {
                    return;
                }

                self.referenced_known_imports.entry(name.into_owned())
                    .or_default()
                    .push(node);
            },
            "
              (identifier) @c
            " => |node, context| {
                if !is_use_import(node) {
                    return;
                }

                let name = node.text(context);
                if !self.known_imports.contains_key(&*name) {
                    return;
                };


                self.imported_known_imports.insert(name.into_owned());
            },
            "source_file:exit" => |node, context| {
                self.referenced_known_imports.iter().filter(|(referenced_known_import, _)| {
                    !self.imported_known_imports.contains(*referenced_known_import)
                }).for_each(|(name, references)| {
                    for (index, &reference) in references.iter().enumerate() {
                        context.report(violation! {
                            node => reference,
                            message_id => "not_defined",
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
                    }
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
                    }
                ]
            },
        );
    }
}
