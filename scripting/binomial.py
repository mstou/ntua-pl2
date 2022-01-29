def modpow(a, n, p): # computes a^n mod p
    result = 1
    while n > 0:
        if n%2==1:
            result = (result * a)%p
        n = n // 2
        a = (a*a)%p
    return result

def inverse(x,p):
    return modpow(x,p-2,p)

def binomial(n,k,p):
    factorial = {}
    factorial[0] = 1

    for i in range(1,n+1):
        factorial[i] = (factorial[i-1] * i)%p

    return ((factorial[n] * inverse(factorial[n-k],p))%p * inverse(factorial[k],p))%p
