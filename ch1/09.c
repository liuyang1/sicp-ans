#include <stdlib.h>
#include <stdio.h>

int inc(int a) {
    return a + 1;
}

int dec(int a) {
    if (a == 0) {
        fprintf(stderr, "cannot dec on zero\n");
        exit(-1);
    }
    return a - 1;
}

/** recursion solution
 * ~~C-language cannot optimize this solution~~
 *
 * test result with gcc 6.2.1 version
 * - no opt, fail at 256K
 * - opt -O1, pass at 256K
 */
unsigned int add_recr(unsigned int a, unsigned int b) {
    if (a == 0) {
        return b;
    } else {
        return inc(add_recr(dec(a), b));
    }
}

/** iterate solution
 * ~~C-language don't support tail-recursion.~~
 */
unsigned int add_iter(unsigned int a, unsigned int b) {
    if (a == 0) {
        return b;
    } else {
        return add_iter(dec(a), inc(b));
    }
}

void test_add(unsigned int (*add)(unsigned int, unsigned int),
              unsigned int a, unsigned int b) {
    printf("%d + %d = %d\n", a, b, add(a, b));
}

int main(int argc, char **argv) {
    unsigned int a = 256 * 1024;
    unsigned int b = 0;
    if (argc > 1) {
        a = strtoul(argv[1], NULL, 10);
        printf("a = %d as input=%s\n", a, argv[1]);
    } else {
        printf("a = %d as default\n", a);
    }
    test_add(add_iter, a, b);
    return 0;
}
