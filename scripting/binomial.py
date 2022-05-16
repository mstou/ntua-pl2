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
    fact = 1 # 0!
    n_fact = None
    k_fact = None
    n_k_fact = None

    for i in range(1,n+1):
        fact = (fact * i)%p
        if i == n:
            n_fact  = fact

        if i == k:
            k_fact = fact

        if i == n-k:
            n_k_fact = fact

    return ((n_fact * inverse(n_k_fact,p))%p * inverse(k_fact,p))%p

if __name__ == '__main__':
    binomial(264790719,74272,758260777)
