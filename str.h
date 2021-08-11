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
static inline bool Str_eq2(Str a, Str b) {
    if (a.len != b.len) return false;
    return memcmp(a.s, b.s, a.len) == 0;
}
static inline bool Str_eq(Str s, char *t) {
    return Str_eq2(s, (Str) { .s = t, .len = strlen(t) });
}

#endif /* __STR_H */
