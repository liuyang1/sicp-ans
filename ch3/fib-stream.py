def fib1(n):
    if n == 0 or n == 1:
        return 1
    return fib1(n - 1) + fib(n - 2)


# python yield style
# fib stream
# this algo complexity is O(phi^n) exponential level
# same as
def fib():
    yield 0
    yield 1
    f1 = fib()
    f2 = fib()
    f1.next()
    while 1:
        yield f1.next() + f2.next()


# test code
f = fib()
for i in range(20):
    print f.next()
