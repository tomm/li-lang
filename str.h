#ifndef __STR_H
#define __STR_H

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

typedef struct Str {
    char *s;
    size_t len;
} Str;

static inline void Str_free(Str s) { free(s.s); }
static inline void Str_puts(Str s, FILE *f) { for (size_t p=0; p < s.len; ++p) { fputc(s.s[p], f); } }
static inline bool Str_eq(Str s, const char *t) {
    if (s.len != strlen(t)) return false;

    for (size_t p=0; p < s.len; ++p) {
        if (s.s[p] != t[p]) return false;
    }
    return true;
}

#endif /* __STR_H */
