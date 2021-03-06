#include "tokenizer.h"
#include "error.h"
#include <ctype.h>
#include <assert.h>

const char *token_type_cstr(enum TokType type) {
    switch (type) {
        case T_SPACE: return "<space>";
        case T_LBRACE: return "{";
        case T_RBRACE: return "}";
        case T_LPAREN: return "(";
        case T_RPAREN: return ")";
        case T_LSQBRACKET: return "[";
        case T_RSQBRACKET: return "]";
        case T_SEMICOLON: return ";";
        case T_COLON: return ":";
        case T_TILDE: return "~";
        case T_MINUS: return "-";
        case T_PLUS: return "+";
        case T_HASH: return "#";
        case T_ATSIGN: return "@";
        case T_AMPERSAND: return "&";
        case T_PIPE: return "|";
        case T_ACUTE: return "^";
        case T_FN: return "fn";
        case T_VAR: return "var";
        case T_CONST: return "const";
        case T_ASSIGN: return "=";
        case T_PLUSASSIGN: return "+=";
        case T_MINUSASSIGN: return "-=";
        case T_MULASSIGN: return "*=";
        case T_DIVASSIGN: return "/=";
        case T_MODASSIGN: return "%=";
        case T_LSHIFTASSIGN: return "<<=";
        case T_RSHIFTASSIGN: return ">>=";
        case T_BITANDASSIGN: return "&=";
        case T_BITORASSIGN: return "|=";
        case T_BITXORASSIGN: return "^=";
        case T_EXCLAMATION: return "!";
        case T_EQ: return "==";
        case T_NEQ: return "!=";
        case T_GT: return ">";
        case T_LT: return "<";
        case T_GTE: return ">=";
        case T_LTE: return "<=";
        case T_SHIFT_LEFT: return "<<";
        case T_SHIFT_RIGHT: return ">>";
        case T_AS: return "as";
        case T_RETURN: return "return";
        case T_ASTERISK: return "*";
        case T_SLASH: return "/";
        case T_PERCENT: return "%";
        case T_DOLLAR: return "$";
        case T_COMMA: return ",";
        case T_PERIOD: return ".";
        case T_BREAK: return "break";
        case T_CONTINUE: return "continue";
        case T_STRUCT: return "struct";
        case T_IDENT: return "identifier";
        case T_JUMP_LABEL: return "label";
        case T_LITERAL_STR: return "literal str";
        case T_LITERAL_U8: return "literal u8";
        case T_LITERAL_U16: return "literal u16";
        case T_LITERAL_I8: return "literal i8";
        case T_LITERAL_I16: return "literal i16";
        case T_LITERAL_ANY_INT: return "literal any int";
        case T_LITERAL_TRUE: return "literal true";
        case T_LITERAL_FALSE: return "literal false";
        case T_IF: return "if";
        case T_ELSE: return "else";
        case T_RARROW: return "->";
        case T_WHILE: return "while";
        case T_FOR: return "for";
        case T_LOOP: return "loop";
        case T_EOF: return "end-of-file";
        case T_ASM: return "asm";
        default: assert(false);
    }
}

Vec lex(Str buf, const char *filename) {
    //char *tok_start = buf.s;
    Vec tokens = vec_init(sizeof(Token));
    char *pos = buf.s;
    char *end = buf.s + buf.len;
    int line = 1;
    int col = 1;
    // can't have unseparated literals (like 123"hello")
    int last_tok_was_literal = false;

#define NEXT() { if (*pos == '\n') { line++; col = 1; ++pos; } else { ++pos; ++col; } }
#define EMIT(token) { last_tok_was_literal = false; token.filename = filename; vec_push(&tokens, &(token)); }
#define LOOK_AHEAD() (pos < end-1 ? *(pos+1) : 0)
#define LOOK_AHEAD2() (pos < end-2 ? *(pos+2) : 0)

    while (pos < end) {
        if (*pos == '{') { EMIT(((Token) { T_LBRACE, line, col, filename })); NEXT(); }
        else if (*pos == '}') { EMIT(((Token) { T_RBRACE, line, col, filename })); NEXT(); }
        else if (*pos == '(') { EMIT(((Token) { T_LPAREN, line, col, filename })); NEXT(); }
        else if (*pos == ')') { EMIT(((Token) { T_RPAREN, line, col, filename })); NEXT(); }
        else if (*pos == '[') { EMIT(((Token) { T_LSQBRACKET, line, col, filename })); NEXT(); }
        else if (*pos == ']') { EMIT(((Token) { T_RSQBRACKET, line, col, filename })); NEXT(); }
        else if (*pos == ';') { EMIT(((Token) { T_SEMICOLON, line, col, filename })); NEXT(); }
        else if (*pos == ':') { EMIT(((Token) { T_COLON, line, col, filename })); NEXT(); }
        else if (*pos == '~') { EMIT(((Token) { T_TILDE, line, col, filename })); NEXT(); }
        else if ((*pos == '-') && LOOK_AHEAD() == '>') { EMIT(((Token) { T_RARROW, line, col, filename })); NEXT(); NEXT(); }
        else if (*pos == '-' && LOOK_AHEAD() == '=') { EMIT(((Token) { T_MINUSASSIGN, line, col, filename })); NEXT(); NEXT(); }
        else if (*pos == '-') { EMIT(((Token) { T_MINUS, line, col, filename })); NEXT(); }
        else if (*pos == '+' && LOOK_AHEAD() == '=') { EMIT(((Token) { T_PLUSASSIGN, line, col, filename })); NEXT(); NEXT(); }
        else if (*pos == '+') { EMIT(((Token) { T_PLUS, line, col, filename })); NEXT(); }
        else if (*pos == '@') { EMIT(((Token) { T_ATSIGN, line, col, filename })); NEXT(); }
        else if (*pos == '#') { EMIT(((Token) { T_HASH, line, col, filename })); NEXT(); }
        else if (*pos == '&' && LOOK_AHEAD() == '=') { EMIT(((Token) { T_BITANDASSIGN, line, col, filename })); NEXT(); NEXT(); }
        else if (*pos == '&') { EMIT(((Token) { T_AMPERSAND, line, col, filename })); NEXT(); }
        else if (*pos == '|' && LOOK_AHEAD() == '=') { EMIT(((Token) { T_BITORASSIGN, line, col, filename })); NEXT(); NEXT(); }
        else if (*pos == '|') { EMIT(((Token) { T_PIPE, line, col, filename })); NEXT(); }
        else if (*pos == '^' && LOOK_AHEAD() == '=') { EMIT(((Token) { T_BITXORASSIGN, line, col, filename })); NEXT(); NEXT(); }
        else if (*pos == '^') { EMIT(((Token) { T_ACUTE, line, col, filename })); NEXT(); }
        else if (*pos == '%' && LOOK_AHEAD() == '=') { EMIT(((Token) { T_MODASSIGN, line, col, filename })); NEXT(); NEXT(); }
        else if (*pos == '%') { EMIT(((Token) { T_PERCENT, line, col, filename })); NEXT(); }
        else if (*pos == '$') { EMIT(((Token) { T_DOLLAR, line, col, filename })); NEXT(); }
        else if (*pos == '!' && LOOK_AHEAD() == '=') { EMIT(((Token) { T_NEQ, line, col, filename })); NEXT(); NEXT(); }
        else if (*pos == '!') { EMIT(((Token) { T_EXCLAMATION, line, col, filename })); NEXT(); }
        else if (*pos == '=' && LOOK_AHEAD() == '=') { EMIT(((Token) { T_EQ, line, col, filename })); NEXT(); NEXT(); }
        else if (*pos == '=') { EMIT(((Token) { T_ASSIGN, line, col, filename })); NEXT(); }
        else if (*pos == '>' && LOOK_AHEAD() == '>' && LOOK_AHEAD2() == '=') { EMIT(((Token) { T_RSHIFTASSIGN, line, col, filename })); NEXT(); NEXT(); NEXT(); }
        else if (*pos == '>' && LOOK_AHEAD() == '=') { EMIT(((Token) { T_GTE, line, col, filename })); NEXT(); NEXT(); }
        else if (*pos == '>' && LOOK_AHEAD() == '>') { EMIT(((Token) { T_SHIFT_RIGHT, line, col, filename })); NEXT(); NEXT(); }
        else if (*pos == '>') { EMIT(((Token) { T_GT, line, col, filename })); NEXT(); }
        else if (*pos == '<' && LOOK_AHEAD() == '<' && LOOK_AHEAD2() == '=') { EMIT(((Token) { T_LSHIFTASSIGN, line, col, filename })); NEXT(); NEXT(); NEXT(); }
        else if (*pos == '<' && LOOK_AHEAD() == '=') { EMIT(((Token) { T_LTE, line, col, filename })); NEXT(); NEXT(); }
        else if (*pos == '<' && LOOK_AHEAD() == '<') { EMIT(((Token) { T_SHIFT_LEFT, line, col, filename })); NEXT(); NEXT(); }
        else if (*pos == '<') { EMIT(((Token) { T_LT, line, col, filename })); NEXT(); }
        else if (*pos == '*' && LOOK_AHEAD() == '=') { EMIT(((Token) { T_MULASSIGN, line, col, filename })); NEXT(); NEXT(); }
        else if (*pos == '*') { EMIT(((Token) { T_ASTERISK, line, col, filename })); NEXT(); }
        else if (*pos == '\'') {
            if (LOOK_AHEAD2() != '\'' && (isalpha(LOOK_AHEAD()) || LOOK_AHEAD() == '_')) {
                // jump label
                Token t = { T_JUMP_LABEL, line, col, filename };
                t.label.s = ++pos;
                t.label.len = 0;

                while ((isalpha(*pos) || isdigit(*pos) || *pos == '_') && pos < end) {
                    NEXT();
                    ++t.label.len;
                }
                last_tok_was_literal = true;
                EMIT(t);
            } else {
                // u8 char literal
                Token t;
                t.line = line;
                t.col = col;
                t.filename = filename;
                t.type = T_LITERAL_U8;

                NEXT();
                t.int_literal = *pos;
                NEXT();
                if (*pos != '\'') {
                    error(&t, "Malformed character literal");
                }
                // XXX handle escape chars
                NEXT();
                EMIT(t);
            }
        }
        else if (!last_tok_was_literal && *pos == '"') {
            const char delim = *pos;
            Token t = { T_LITERAL_STR, line, col, filename };

            NEXT();

            t.str_literal.len = 0;
            t.str_literal.s = pos;

            while (pos < end && *pos != delim) {
                t.str_literal.len++;
                NEXT();
            }

            // skip terminating delimiter
            NEXT();

            EMIT(t);
            last_tok_was_literal = true;
        }
        else if (*pos == '`') {
            Token t = { T_ASM, line, col, filename };
            NEXT();
            t.asm_.len = 0;
            t.asm_.s = pos;
            while (pos < end && *pos != '`') {
                t.asm_.len++;
                NEXT();
            }
            // skip terminating delimiter
            NEXT();
            EMIT(t);
        }
        else if (*pos == ',') { EMIT(((Token) { T_COMMA, line, col, filename })); NEXT(); }
        else if (*pos == '.') { EMIT(((Token) { T_PERIOD, line, col, filename })); NEXT(); }
        // C++ comment
        else if ((*pos == '/') && LOOK_AHEAD() == '/') {
            NEXT(); NEXT();
            while (pos < end && (*pos != '\n')) NEXT();
        }
        // C comment
        else if ((*pos == '/') && LOOK_AHEAD() == '*') {
            NEXT(); NEXT();
            while (pos < end && !((*pos == '*') && LOOK_AHEAD() == '/')) {
                if (*pos == '\n') {
                    line++;
                    col = 1;
                    ++pos;
                } else {
                    NEXT();
                }
            }
            if (pos < end) NEXT();
            if (pos < end) NEXT();
        }
        else if (*pos == '/' && LOOK_AHEAD() == '=') { EMIT(((Token) { T_DIVASSIGN, line, col, filename })); NEXT(); NEXT(); }
        else if (*pos == '/') { EMIT(((Token) { T_SLASH, line, col, filename })); NEXT(); }
        else if (!last_tok_was_literal && (*pos == '_' || isalpha(*pos))) {
            Token t = { T_IDENT, line, col, filename };
            t.ident.s = pos;
            t.ident.len = 0;

            while ((isalpha(*pos) || isdigit(*pos) || *pos == '_') && pos < end) {
                NEXT();
                ++t.ident.len;
            }

            if (Str_eq(t.ident, "as")) {
                EMIT(((Token) { T_AS, t.line, t.col, filename }));
            } else if (Str_eq(t.ident, "true")) {
                EMIT(((Token) { T_LITERAL_TRUE, t.line, t.col, filename }));
            } else if (Str_eq(t.ident, "false")) {
                EMIT(((Token) { T_LITERAL_FALSE, t.line, t.col, filename }));
            } else if (Str_eq(t.ident, "return")) {
                EMIT(((Token) { T_RETURN, t.line, t.col, filename }));
            } else if (Str_eq(t.ident, "fn")) {
                EMIT(((Token) { T_FN, t.line, t.col, filename }));
            } else if (Str_eq(t.ident, "var")) {
                EMIT(((Token) { T_VAR, t.line, t.col, filename }));
            } else if (Str_eq(t.ident, "const")) {
                EMIT(((Token) { T_CONST, t.line, t.col, filename }));
            } else if (Str_eq(t.ident, "if")) {
                EMIT(((Token) { T_IF, t.line, t.col, filename }));
            } else if (Str_eq(t.ident, "else")) {
                EMIT(((Token) { T_ELSE, t.line, t.col, filename }));
            } else if (Str_eq(t.ident, "while")) {
                EMIT(((Token) { T_WHILE, t.line, t.col, filename }));
            } else if (Str_eq(t.ident, "for")) {
                EMIT(((Token) { T_FOR, t.line, t.col, filename }));
            } else if (Str_eq(t.ident, "loop")) {
                EMIT(((Token) { T_LOOP, t.line, t.col, filename }));
            } else if (Str_eq(t.ident, "break")) {
                EMIT(((Token) { T_BREAK, t.line, t.col, filename }));
            } else if (Str_eq(t.ident, "continue")) {
                EMIT(((Token) { T_CONTINUE, t.line, t.col, filename }));
            } else if (Str_eq(t.ident, "struct")) {
                EMIT(((Token) { T_STRUCT, t.line, t.col, filename }));
            } else {
                EMIT(t);
            }
            last_tok_was_literal = true;
        }
        else if (!last_tok_was_literal && isdigit(*pos) && LOOK_AHEAD() == 'x') {
            Token t;
            t.line = line;
            t.col = col;
            t.filename = filename;
            if (sscanf(pos, "%x", &t.int_literal) != 1) {
                error(&t, "Malformed numeral");
                NEXT();
            } else {
                pos += 2;
                while (isxdigit(*pos)) NEXT();
                while (*pos == '_') NEXT();
                
                if (*pos == 'u' && LOOK_AHEAD() == '8') {
                    t.type = T_LITERAL_U8;
                    pos += 2;
                } else if (*pos == 'i' && LOOK_AHEAD() == '8') {
                    t.type = T_LITERAL_I8;
                    pos += 2;
                } else if (*pos == 'u' && LOOK_AHEAD() == '1' && LOOK_AHEAD2() == '6') {
                    t.type = T_LITERAL_U16;
                    pos += 3;
                } else if (*pos == 'i' && LOOK_AHEAD() == '1' && LOOK_AHEAD2() == '6') {
                    t.type = T_LITERAL_I16;
                    pos += 3;
                } else {
                    t.type = T_LITERAL_ANY_INT;
                }
                EMIT(t);
                last_tok_was_literal = true;
            }
        }
        else if (!last_tok_was_literal && isdigit(*pos)) {
            Token t;
            t.line = line;
            t.col = col;
            t.filename = filename;
            if (sscanf(pos, "%d", &t.int_literal) != 1) {
                error(&t, "Malformed numeral");
                NEXT();
            } else {
                while (isdigit(*pos)) NEXT();
                while (*pos == '_') NEXT();
                
                if (*pos == 'u' && LOOK_AHEAD() == '8') {
                    t.type = T_LITERAL_U8;
                    pos += 2;
                } else if (*pos == 'i' && LOOK_AHEAD() == '8') {
                    t.type = T_LITERAL_I8;
                    pos += 2;
                } else if (*pos == 'u' && LOOK_AHEAD() == '1' && LOOK_AHEAD2() == '6') {
                    t.type = T_LITERAL_U16;
                    pos += 3;
                } else if (*pos == 'i' && LOOK_AHEAD() == '1' && LOOK_AHEAD2() == '6') {
                    t.type = T_LITERAL_I16;
                    pos += 3;
                } else {
                    t.type = T_LITERAL_ANY_INT;
                }
                EMIT(t);
                last_tok_was_literal = true;
            }
        }
        else if (isspace(*pos)) {
            while (isspace(*pos) && pos < end) {
                NEXT();
            }
            EMIT(((Token) { T_SPACE, line, col, filename }));
            last_tok_was_literal = false;
        }
        else {
            Token t;
            t.line = line;
            t.col = col;
            t.filename = filename;
            error(&t, "Unexpected character '%c'", *pos);
            NEXT();
        }
    };

    EMIT(((Token) { T_EOF, line, col, filename }));

    return tokens;
}

