#include "output_llvm.h"
#include <assert.h>

static Program *program;
static FILE *output;

void emit_boilerplate() {
    fprintf(output, "target triple = \"x86_64-pc-linux-gnu\"\n");
    fprintf(output, "@.str = private unnamed_addr constant [13 x i8] c\"Hello world\\0A\\00\", align 1\n");
    fprintf(output, "define dso_local i32 @main() #0 {\n");
    fprintf(output, "  %%1 = call i32 @puts(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str, i32 0, i32 0))\n");
    fprintf(output, "  ret i32 0\n");
    fprintf(output, "}\n");
    fprintf(output, "declare dso_local i32 @puts(i8*) #1\n");
}

void output_llvm(Program *prog) {
    printf("Hello from llvm backend!\n");
    program = prog;
    output = fopen("out.ll", "w");

    AstNode *root_node = prog->root;
    assert(root_node->type == AST_MODULE);

    emit_boilerplate();

    fclose(output);
}
