#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int func() {
    printf("dead-loop\n");
    return func();
}

int test(int x, int (*f)(void)) {
    return x == 0 ? 0 : f();
}

int testLogic(bool b, int x, int y) {
    return b ? x : y;
}

int main(int argc, char **argv) {
    int x = 0;
    if (argc >= 2) {
        x = atoi(argv[1]);
    }
    printf("%d\n", test(x, func));

    // dead-loop here
    printf("%d\n", testLogic(true, x, func()));
    return 0;
}
