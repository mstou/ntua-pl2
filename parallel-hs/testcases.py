import pickle
import random

Q = 1000
MAX_NUM = int(10**6)
primes = []

def generate_primes():
    prime = {}
    prime[2] = True


    for n in range(3, MAX_NUM, 2):
        prime[n] = True

    for n in range(3, MAX_NUM, 2):
        if not prime[n]:
            continue

        for i in range(2, MAX_NUM//n + 1):
            if i * n < MAX_NUM:
                prime[i*n] = False

    primes = set()
    for x in prime:
        if prime[x]:
            primes.add(x)

    with open('primes.pickle','wb') as f:
        pickle.dump(primes, f)

def load_primes():
    global primes
    with open('primes.pickle','rb') as f:
        primes = list(pickle.load(f))

def generate_testcase(file, queries):

    with open(file,'w') as f:
        print(f'{queries}', file = f)
        for _ in range(queries):
            P = random.choice(primes)
            N = random.randrange(2,P)
            K = random.randrange(1,N)
            print(f'{N} {K} {P}', file = f)

if __name__ == '__main__':
    load_primes()
    generate_testcase('test10.in', 10)
