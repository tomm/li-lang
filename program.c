#include "program.h"
#include <assert.h>

static void collect_symbols(Program *prog) {
    AstNode *root_node = get_node(prog->root);
    assert(root_node->type == AST_MODULE);

    for (NodeIdx node=root_node->module.first_child; node != 0; node=get_node(node)->next_sibling) {
        AstNode *n = get_node(node);
        if (n->type == AST_FN) {
            vec_push(&prog->symbols, &(Symbol) {
                .name = n->fn.name,
                .obj = node
            });
        }
        else if (n->type == AST_DEF_VAR) {
            vec_push(&prog->symbols, &(Symbol) {
                .name = n->var_def.name,
                .obj = node
            });
        }
        else {
            assert(false);
        }
    }
}

Symbol *lookup_program_symbol(Program *prog, Str name) {
    for (int i=0; i<prog->symbols.len; ++i) {
        Symbol *sym = vec_get(&prog->symbols, i);
        if (Str_eq2(sym->name, name)) return sym;
    }
    return NULL;
}

Program new_program(NodeIdx root) {
    Program prog = {
        .root = root,
        .symbols = vec_init(sizeof(Symbol))
    };

    collect_symbols(&prog);

    return prog;
}

void free_program(Program *prog) {
    vec_free(&prog->symbols);
}
