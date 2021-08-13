#include "tokenizer.h"
#include <ctype.h>

const char *token_type_cstr(enum TokType type) {
    switch (type) {
        case T_SPACE: return "<space>";
        case T_LBRACE: return "{";
        case T_RBRACE: return "}";
        case T_LPAREN: return "(";
        case T_RPAREN: return ")";
        case T_SEMICOLON: return ";";
        case T_COLON: return ":";
        case T_MINUS: return "-";
        case T_PLUS: return "+";
        case T_BITAND: return "&";
        case T_BITOR: return "|";
        case T_BITXOR: return "^";
        case T_FN: return "fn";
        case T_GT: return ">";
        case T_AS: return "as";
        case T_RETURN: return "return";
        case T_ASTERISK: return "*";
        case T_COMMA: return ",";
        case T_IDENT: return "identifier";
        case T_LITERAL_U8: return "literal u8";
        case T_LITERAL_U16: return "literal u16";
        case T_RARROW: return "->";
        case T_EOF: return "end-of-file";
        default: abort();
    }
}

Vec lex(Str buf) {
    //char *tok_start = buf.s;
    Vec tokens = vec_init(sizeof(Token));
    char *pos = buf.s;
    char *end = buf.s + buf.len;
    int line = 1;
    int col = 1;

#define NEXT() { ++pos; ++col; }
#define EMIT(token) { vec_push(&tokens, &(token)); }
#define LOOK_AHEAD() (pos < end-1 ? *(pos+1) : 0)
#define LOOK_AHEAD2() (pos < end-2 ? *(pos+2) : 0)

    do {
        if (*pos == '{') { EMIT(((Token) { T_LBRACE, line, col })); NEXT(); }
        else if (*pos == '}') { EMIT(((Token) { T_RBRACE, line, col })); NEXT(); }
        else if (*pos == '(') { EMIT(((Token) { T_LPAREN, line, col })); NEXT(); }
        else if (*pos == ')') { EMIT(((Token) { T_RPAREN, line, col })); NEXT(); }
        else if (*pos == ';') { EMIT(((Token) { T_SEMICOLON, line, col })); NEXT(); }
        else if (*pos == ':') { EMIT(((Token) { T_COLON, line, col })); NEXT(); }
        else if ((*pos == '-') && LOOK_AHEAD() == '>') {
            EMIT(((Token) { T_RARROW, line, col })); NEXT(); NEXT();
        }
        else if (*pos == '-') { EMIT(((Token) { T_MINUS, line, col })); NEXT(); }
        else if (*pos == '+') { EMIT(((Token) { T_PLUS, line, col })); NEXT(); }
        else if (*pos == '&') { EMIT(((Token) { T_BITAND, line, col })); NEXT(); }
        else if (*pos == '|') { EMIT(((Token) { T_BITOR, line, col })); NEXT(); }
        else if (*pos == '^') { EMIT(((Token) { T_BITXOR, line, col })); NEXT(); }
        else if (*pos == '>') { EMIT(((Token) { T_GT, line, col })); NEXT(); }
        else if (*pos == '*') { EMIT(((Token) { T_ASTERISK, line, col })); NEXT(); }
        else if (*pos == ',') { EMIT(((Token) { T_COMMA, line, col })); NEXT(); }
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
        else if (*pos == '_' || isalpha(*pos)) {
            Token t = { T_IDENT };
            t.line = line;
            t.col = col;
            t.ident.s = pos;
            t.ident.len = 0;

            while ((isalpha(*pos) || isdigit(*pos) || *pos == '_') && pos < end) {
                NEXT();
                ++t.ident.len;
            }

            if (Str_eq(t.ident, "as")) {
                EMIT(((Token) { T_AS, t.line, t.col }));
            } else if (Str_eq(t.ident, "return")) {
                EMIT(((Token) { T_RETURN, t.line, t.col }));
            } else if (Str_eq(t.ident, "fn")) {
                EMIT(((Token) { T_FN, t.line, t.col }));
            } else {
                EMIT(t);
            }
        }
        else if (isdigit(*pos) && LOOK_AHEAD() == 'x') {
            Token t;
            t.line = line;
            t.col = col;
            if (sscanf(pos, "%x", &t.int_literal) != 1) {
                fprintf(stderr, "Unknown token at line %d, col %d\n", line, col);
                exit(-1);
            }
            pos += 2;
            while (isxdigit(*pos)) pos++;
            
            if (*pos == 'u' && LOOK_AHEAD() == '8') {
                t.type = T_LITERAL_U8;
                pos += 2;
            } else if (*pos == 'u' && LOOK_AHEAD() == '1' && LOOK_AHEAD2() == '6') {
                t.type = T_LITERAL_U16;
                pos += 3;
            } else {
                t.type = (t.int_literal < -128 || t.int_literal > 255) ? T_LITERAL_U16 : T_LITERAL_U8;
            }
            EMIT(t);
        }
        else if (isdigit(*pos)) {
            Token t;
            t.line = line;
            t.col = col;
            if (sscanf(pos, "%d", &t.int_literal) != 1) {
                fprintf(stderr, "Unknown token at line %d, col %d\n", line, col);
                exit(-1);
            }
            while (isdigit(*pos)) pos++;
            
            if (*pos == 'u' && LOOK_AHEAD() == '8') {
                t.type = T_LITERAL_U8;
                pos += 2;
            } else if (*pos == 'u' && LOOK_AHEAD() == '1' && LOOK_AHEAD2() == '6') {
                t.type = T_LITERAL_U16;
                pos += 3;
            } else {
                t.type = (t.int_literal < -128 || t.int_literal > 255) ? T_LITERAL_U16 : T_LITERAL_U8;
            }
            EMIT(t);
        }
        else if (isspace(*pos)) {
            while (isspace(*pos) && pos < end) {
                if (*pos == '\n') {
                    line++;
                    col = 1;
                    ++pos;
                } else {
                    NEXT();
                }
            }
            EMIT(((Token) { T_SPACE, line, col }));
        }
        else {
            fprintf(stderr, "Unknown token at line %d, col %d\n", line, col);
            exit(-1);
        }
    } while (pos < end);

    EMIT(((Token) { T_EOF, line, col }));

    return tokens;
}

