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

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: ./lic [--noemit] <input.li>\n");
    } else {
        bool noemit = strcmp("--noemit", argv[1]) == 0;

        Program prog = new_program();
        init_parser();

        // produce AST with typed globals and functions,
        // but 'unknown' type expressions in function bodies
        parse_file(&prog, argv[noemit ? 2 : 1]);

        // typecheck function bodies
        typecheck_program(&prog);

        //print_ast(prog.root, 0);
        
        if (!noemit) {
            output_lr35902(&prog);
        }
    }
}
