#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include "vec.h"
#include "str.h"
#include "parser.h"
#include "tokenizer.h"
#include "program.h"
#include "output_lr35902.h"
#include "output_llvm.h"

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: ./lic [--noemit] [--llvm] <input.li>\n");
    } else {
        bool noemit = false;
        bool llvm = false;
        int argpos = 1;

        for (; argpos < argc-1; argpos++) {
            if (strcmp("--noemit", argv[argpos]) == 0) noemit = true;
            else if (strcmp("--llvm", argv[argpos]) == 0) llvm = true;
            else break;
        }

        init_parser();

        // produce AST with typed globals and functions,
        // but 'unknown' type expressions in function bodies
        AstNode *ast_root = parse_file(argv[argpos]);
        print_ast(ast_root, 0);

        Program *prog = ast_to_program(ast_root);
        print_ast(prog->root, 0);
        
        if (!noemit) {
            if (llvm) output_llvm(prog);
            else output_lr35902(prog);
        }
    }
}
