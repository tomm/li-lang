#ifndef __TOKEN_H
#define __TOKEN_H

#include "str.h"
#include "vec.h"

typedef struct Token {
    enum TokType {
        T_SPACE,
        T_LBRACE,
        T_RBRACE,
        T_LPAREN,
        T_RPAREN,
        T_SEMICOLON,
        T_COLON,
        T_MINUS,
        T_PLUS,
        T_BITAND,
        T_BITOR,
        T_BITXOR,
        T_ASTERISK,
        T_COMMA,
        T_GT,
        T_AS,
        T_RETURN,
        T_FN,
        T_IDENT,
        T_LITERAL_U8,
        T_LITERAL_U16,
        T_RARROW,
        T_EOF
    } type;

    int line;
    int col;

    union {
        Str ident;
        int int_literal;
    };
} Token;

extern const char *token_type_cstr(enum TokType type);
extern Vec lex(Str buf);

#endif /* __TOKEN_H */
