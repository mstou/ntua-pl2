import pickle
import random
from time import time
from binomial import binomial
from math import sqrt, ceil
from flask import Flask, render_template, url_for, session, request

app = Flask(__name__, template_folder='pages')
app.secret_key = '73'

MAX_N = 100000
primes = []

try:
    with open('primes.pickle','rb') as f:
        primes = pickle.load(f)
except:
    isPrime = {i:True for i in range(2,MAX_N+1)}

    for p in range(2,ceil(sqrt(MAX_N))+1):
        if not isPrime[p]: continue

        j = p*p
        while j <= MAX_N:
            isPrime[j] = False
            j += p

    primes = [i for i in range(2,MAX_N+1) if isPrime[i]]

    with open('primes.pickle','wb') as f:
        pickle.dump(primes, f)


def newQuestion():
    n = random.randrange(0,MAX_N+1)
    k = random.randrange(0,n)
    p = primes[random.randrange(0, len(primes))]

    return n,k,p

@app.route('/', methods = ['GET','POST'])
def new_question():
    if 'question_num' not in session:
        session['question_num'] = 1
        session['start_time'] = time()
        session['correct'] = 0
    else:
        session['question_num'] += 1

    session['n'], session['k'], session['p'] = newQuestion()

    return render_template('question.html',
            question_num=session['question_num'],
            n=session['n'],
            k=session['k'],
            p=session['p'],
            answer_url=url_for('answer'))

@app.route('/answer', methods=['POST'])
def answer():
    ans = binomial(session['n'], session['k'], session['p'])
    print(session['n'], session['k'], session['p'], ans)
    try:
        answer = request.form['answer']
        correct =  int(answer) == ans
    except:
        correct = False

    msg = 'Correct answer!' if correct else f'Wrong answer :(, the correct one was {ans}'
    class_ = 'right' if correct else 'wrong'

    if correct:
        session['correct'] += 1

    if session['question_num'] == 10:
        del session['question_num']

        return render_template('end.html',
                   question_num=10,
                   n=session['n'],
                   k=session['k'],
                   p=session['p'],
                   answer=class_,
                   answer_msg=msg,
                   time_=f'{time()-session["start_time"]:.4f} seconds',
                   wrong_num_txt = str(10 - session['correct']) if session['correct'] < 10 else 'no',
                   url_new_question = url_for('new_question'))
    else:
        return render_template('answer.html',
                   question_num=session['question_num'],
                   n=session['n'],
                   k=session['k'],
                   p=session['p'],
                   answer=class_,
                   answer_msg=msg,
                   url_new_question = url_for('new_question'))



if __name__ == '__main__':
    app.run(host='0.0.0.0', port=3000)
