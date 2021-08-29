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

Str read_file(FILE *f) {
    fseek(f, 0, SEEK_END);
    const size_t len = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *buf = malloc(len);
    const size_t num_read = fread(buf, 1, len, f);
    assert(num_read == len);

    return (Str) { buf, len };
}

void dump_tokens(Vec *tokens) {
    for (int i=0; i<tokens->len; ++i) {
        printf("%s ", token_type_cstr(((Token*)vec_get(tokens, i))->type));
    }
    printf("\n");
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: ./cc <input.c>\n");
    } else {
        printf("opening %s\n", argv[1]);
        FILE *f = fopen(argv[1], "r");
        if (f == NULL) {
            fprintf(stderr, "%s not found\n", argv[1]);
            exit(-1);
        }
        Str buf = read_file(f);
        fclose(f);

        init_parser();
        Vec token_vec = lex(buf);
        dump_tokens(&token_vec);
        const NodeIdx root = parse_module(&(TokenCursor) { .tokens = token_vec, .next = 0 });
        print_ast(root, 0);

        Program prog = new_program(root);

        output_lr35902(&prog);

        vec_free(&token_vec);

        Str_free(buf);
    }
}
