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
        T_LSQBRACKET,
        T_RSQBRACKET,
        T_SEMICOLON,
        T_COLON,
        T_MINUS,
        T_PLUS,
        T_AMPERSAND,
        T_EXCLAMATION,
        T_PIPE,
        T_TILDE,
        T_ACUTE,
        T_ASTERISK,
        T_SLASH,
        T_PERCENT,
        T_DOLLAR,
        T_COMMA,
        T_ASSIGN,
        T_PLUSASSIGN,
        T_MINUSASSIGN,
        T_MULASSIGN,
        T_DIVASSIGN,
        T_MODASSIGN,
        T_LSHIFTASSIGN,
        T_RSHIFTASSIGN,
        T_BITANDASSIGN,
        T_BITORASSIGN,
        T_BITXORASSIGN,
        T_EQ,
        T_NEQ,
        T_GT,
        T_GTE,
        T_LT,
        T_LTE,
        T_SHIFT_LEFT,
        T_SHIFT_RIGHT,
        T_AS,
        T_IF,
        T_ELSE,
        T_RETURN,
        T_FN,
        T_VAR,
        T_CONST,
        T_BREAK,
        T_CONTINUE,
        T_IDENT,
        T_JUMP_LABEL,
        T_LITERAL_STR,
        T_LITERAL_U8,
        T_LITERAL_U16,
        T_LITERAL_ANY_INT,
        T_RARROW,
        T_WHILE,
        T_FOR,
        T_EOF
    } type;

    int line;
    int col;
    const char *filename;

    union {
        Str ident;
        Str label;
        int int_literal;
        Str str_literal;
    };
} Token;

extern const char *token_type_cstr(enum TokType type);
extern Vec lex(Str buf, const char *filename);

#endif /* __TOKEN_H */
