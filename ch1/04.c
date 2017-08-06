#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

int sub(int a, int b) {
    return a - b;
}

int plus_abs(int a, int b) {
    return (b > 0 ? add : sub)(a, b);
}

int main() {
    int a = 5, b = 3;
    printf("plus_abs(%d, %d) = %d\n", a, b, plus_abs(a, b));
    b = -3;
    printf("plus_abs(%d, %d) = %d\n", a, b, plus_abs(a, b));
    return 0;
}
