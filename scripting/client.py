import sys
import requests
from time import time
from binomial import binomial
from bs4 import BeautifulSoup

if len(sys.argv) < 2:
    print('Please provide the site through the command line arguments')
    exit()

url = sys.argv[1]
q = 1

start_time = time()

with requests.Session() as s:
    while True:
        response = s.get(url)
        if response.status_code != 200:
            print('Error requesting a new question..')
            print(f'Response status {page.status_code}')
            break

        parser = BeautifulSoup(response.content, 'html.parser')

        try:
            n = int(parser.find(id='N').text)
            k = int(parser.find(id='K').text)
            p = int(parser.find(id='P').text)
        except:
            print('Error parsing the page.\n\n')
            print(parser.prettify())
            break

        print(f'Question {q}: calculate C({n},{k}) % {p}')
        ans = binomial(n,k,p)
        print(f'Answering {ans} ...')

        response = s.post(url, data={'answer': ans})
        if response.status_code != 200:
            print('Error answering the question..')
            print(f'Response status {response.status_code}')
            break
        parser = BeautifulSoup(response.content, 'html.parser')
        correct_result = parser.find(class_='right')

        if correct_result:
            print('Correct answer!\n')
        else:
            print('Wrong answer :(\n')
            break

        q += 1

        ended = parser.find(class_='congratulations')
        end_time = time()

        if ended:
            print('========================\n')
            print(f'We answered all questions in {end_time-start_time:.4f} seconds!')
            print(f'See you next time! :)\n')
            break
