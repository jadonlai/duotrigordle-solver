from itertools import combinations
from math import comb

# the words in the file 'wordlist.pl' are of the form
# words('ZYMES').
# words('ZYMIC').



wordlist_file = '../wordlist.pl'

with open(wordlist_file, 'r') as f:
    lines = f.read().splitlines()

words = [line[7:12] for line in lines]

print(words[:10])

num_letter_maximizing_guesses = 2

best_first_guesses = None
best_num_unique_letters = 0

num_combinations = comb(len(words), num_letter_maximizing_guesses)


for i, words_tuple in enumerate(combinations(words, num_letter_maximizing_guesses)):
    print(f"{i / num_combinations * 100: 0.2f}% complete      ")
    unique_letters = set()
    for word in words_tuple:
        for letter in word:
            unique_letters.add(letter)

    if len(unique_letters) > best_num_unique_letters:
        best_first_guesses = words_tuple
        best_num_unique_letters = len(unique_letters)


print(best_first_guesses)