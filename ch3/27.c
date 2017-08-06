#include <setjmp.h>
#include <limits.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define T Table_T

struct T {
    size_t capcity;
    unsigned int *buf;
    bool *flag;
};

typedef struct T *T;

void Table_free(T p) {
    assert(p != NULL);
    free(p->flag);
    free(p->buf);
    free(p);
}

T Table_new(size_t capcity) {
    T p;
    p = malloc(sizeof(*p));
    memset(p, 0, sizeof(*p));

    size_t s = capcity * sizeof(unsigned int);
    p->capcity = capcity;
    p->buf = malloc(s);
    memset(p->buf, 0, s);
    p->flag = malloc(sizeof(bool) * capcity);
    size_t i;
    for (i = 0; i != p->capcity; i++) {
        p->flag[i] = false;
    }
    return p;
}

int Table_insert(T p, unsigned int idx, unsigned int data) {
    assert(idx < p->capcity);
    p->buf[idx] = data;
    p->flag[idx] = true;
    return 0;
}

int Table_lookup(T p, unsigned int idx, unsigned int *data) {
    if (idx >= p->capcity) {
        printf("idx = %d capcity = %lu\n", idx, p->capcity);
        exit(-1);
    }
    assert(data != NULL);
    if (p->flag[idx]) {
        *data = p->buf[idx];
        return 0;
    } else {
        return -1;
    }
}

// I like pythonic output
void Table_show(T p) {
    printf("tbl=%p capcity=%lu\n", p, p->capcity);
    size_t i;
    for (i = 0; i != p->capcity; i++) {
#define LITER_LEN   11
        char liter[LITER_LEN];
        if (p->flag[i]) {
            snprintf(liter, LITER_LEN, "%u", p->buf[i]);
        } else {
            snprintf(liter, LITER_LEN, "%s", "_");
        }
        char fmt[LITER_LEN] = {0};
        const char *leading = NULL, *suffix = "";
        if (i == 0) {
            leading = "[";
        } else {
            leading = ", ";
        }
        if (i == p->capcity - 1) {
            suffix = "]\n";
        }
        strncat(fmt, leading, LITER_LEN);
        strncat(fmt, "%s", LITER_LEN);
        strncat(fmt, suffix, LITER_LEN);
        printf(fmt, liter);
    }
}

#undef T

#define T Ctx_T

struct T {
    unsigned int (*func)(struct T *ctx, unsigned int);
    Table_T tbl;
};

typedef struct T *T;

void Ctx_free(T ctx) {
    assert(ctx != NULL);
    Table_free(ctx->tbl);
    free(ctx);
}

T Ctx_new(unsigned int (*func)(T ctx, unsigned int),
          unsigned capcity) {
    T p = malloc(sizeof(*p));
    p->func = func;
    p->tbl = Table_new(capcity);
    return p;
}

unsigned int Ctx_apply(T ctx, unsigned int n) {
    Table_T tbl = ctx->tbl;
    unsigned int ret;
    if (Table_lookup(tbl, n, &ret) != 0) {
        ret = ctx->func(ctx, n);
        Table_insert(ctx->tbl, n, ret);
    }
    return ret;
}

#undef T

#define ERR_OVERFLOW    2
jmp_buf jmpPoint;

unsigned int add(unsigned int a, unsigned int b) {
    unsigned int v = a + b;
    if (v < a || v < b) {
        // THROW
        printf("%u + %u overflow\n", a, b);
        longjmp(jmpPoint, ERR_OVERFLOW);
    }
    return v;
}

unsigned int fib(Ctx_T ctx, unsigned int n) {
    if (n == 0 || n == 1) {
        return 1;
    }
    return add(Ctx_apply(ctx, n - 2), Ctx_apply(ctx, n - 1));
}

void unit(Ctx_T ctx, int n) {
    unsigned int v = Ctx_apply(ctx, n);
    printf("fib(%d) = %u\n", n, v);
    Table_show(ctx->tbl);
    printf("\n");
}

int main() {
    unsigned int max = 50;
    Ctx_T ctx = Ctx_new(fib, max + 1);

    // TRY
    int jmpret = setjmp(jmpPoint);
    if (jmpret == 0) {
        unit(ctx, 10);
        unit(ctx, 20);
        unit(ctx, max);
    } else { // EXCEPT
        switch (jmpret) {
            case ERR_OVERFLOW:
                printf("catch except=%x overflow\n", jmpret);
                break;
            default:
                printf("catch except=%x unknown\n", jmpret);
                break;
        }
    }

    Ctx_free(ctx);
    return 0;
}
