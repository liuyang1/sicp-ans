# python yield style
# fib stream

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
