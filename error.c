#include "error.h"
#include <stdio.h>
#include <stdarg.h>

void error(const Token *t, const char *format, ...) {
	char buf[1024];
	va_list ap;
	va_start(ap, format);
	vsnprintf(buf, sizeof(buf), format, ap);
	va_end(ap);
    fprintf(stderr, "%d:%d: %s\n", t->line, t->col, buf);
}

void fatal_error(const Token *t, const char *format, ...) {
	char buf[1024];
	va_list ap;
	va_start(ap, format);
	vsnprintf(buf, sizeof(buf), format, ap);
	va_end(ap);
    fprintf(stderr, "%d:%d: %s\n",
            t->line, t->col, buf);
    exit(-1);
}
