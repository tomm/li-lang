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

        Program prog = new_program();
        init_parser();

        // produce AST with typed globals and functions,
        // but 'unknown' type expressions in function bodies
        parse_file(&prog, argv[argpos]);

        //print_ast(prog.root, 0);
        // typecheck function bodies
        typecheck_program(&prog);
        //print_ast(prog.root, 0);
        
        if (!noemit) {
            if (llvm) output_llvm(&prog);
            else output_lr35902(&prog);
        }
    }
}
