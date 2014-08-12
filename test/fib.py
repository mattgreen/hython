def fib(n):
    if n == 1:
        return 1
    if n == 2:
        return 1

    #Hi
    return fib(n-1) + fib(n-2)

print(fib(20))
