Q = int(input())

def exponentiate(a,n,p):
    res=1
    while (n>0):
        if (n%2==1):
            res=(res*a)%p
        n=n//2
        a=(a*a)%p
    return res

def inverse(x,p):
    return exponentiate(x,p-2,p)

for _ in range(1000):
    n, k, p = list(map(int, input().split(' ')))

    factorial = {}
    factorial[0] = 1

    for i in range(1,n+1):
        factorial[i] = (factorial[i-1] * i)%p

    result = ((factorial[n] * inverse(factorial[n-k],p))%p * inverse(factorial[k],p))%p
    print(result)
