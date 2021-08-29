#ifndef PROGRAM_H
#define PROGRAM_H

#include "str.h"
#include "vec.h"
#include "parser.h"

typedef struct Symbol {
    Str name;
    NodeIdx obj;
} Symbol;

typedef struct Program {
    NodeIdx root;

    Vec /*<Symbol>*/ symbols;
} Program;

extern Program new_program(NodeIdx root);
extern void free_program(Program *);
extern Symbol *lookup_program_symbol(Program *, Str name);

#endif /* PROGRAM_H */
