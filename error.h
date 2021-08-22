#ifndef ERROR_H
#define ERROR_H

#include "tokenizer.h"

extern void error(const Token *t, const char *format, ...) __attribute__((format(printf, 2, 3)));
extern void fatal_error(const Token *t, const char *format, ...) __attribute__((noreturn, format(printf, 2, 3)));

#endif /* ERROR_H */
