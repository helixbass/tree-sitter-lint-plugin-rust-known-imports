use std::{collections::HashMap, sync::Arc};

use serde::Deserialize;
use tree_sitter_lint::{rule, Rule};

#[derive(Deserialize)]
struct Options {
    known_imports: KnownImports,
}

type KnownImports = HashMap<String, KnownImportSpec>;

#[derive(Deserialize)]
struct KnownImportSpec {
    module: String,
    kind: KnownImportKind,
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
enum KnownImportKind {
    GenericType,
}

pub fn no_undef_rule() -> Arc<dyn Rule> {
    rule! {
        name => "no-undef",
        fixable => true,
        messages => [
            "undef" => "'{{name}}' is not defined.",
        ],
        languages => [Rust],
        options_type! => Options,
        state => {
            [per-run]
            known_imports: KnownImports = options.known_imports,
        },
        listeners => [
            "(type_identifier) @c" => |node, context| {
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
                        code => "
                            struct Foo {
                                id: Id<usize>,
                            }
                        ",
                        output => "
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
