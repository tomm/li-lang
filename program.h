#ifndef PROGRAM_H
#define PROGRAM_H

#include "str.h"
#include "vec.h"
#include "parser.h"

typedef struct Symbol {
    Str name;
    AstNode *obj;
    TypeId type;
} Symbol;

typedef struct Program {
    AstNode *root;

    Vec /*<Symbol>*/ symbols;
} Program;

extern Program new_program();
extern void free_program(Program *);
extern void typecheck_program(Program *);
extern Symbol *lookup_program_symbol(Program *, Str name);

#endif /* PROGRAM_H */
