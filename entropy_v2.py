from collections import Counter
import numpy as np

def calculate_entropy(word_list):
    '''
    Calculates the entropy of a list of words

    Args:
        word_list (list): A list of words

    Returns:
        float: The entropy of the word list
    '''

    # Calculate the probabilities of each letter
    letter_counts = Counter(''.join(word_list))
    total_letters = sum(letter_counts.values())
    probabilities = np.array(list(letter_counts.values())) / total_letters

    # Calculate the entropy
    entropy = -np.sum(probabilities * np.log2(probabilities))
    return entropy

def find_greedy_optimal_words(wordlist, k=5):
    '''
    Find the k most optimal words in the wordlist

    Args:
        wordlist (list): A list of words
        k (int): The number of words to return

    Returns:
        list: A list of the k most optimal words
    '''

    # Initialize the selected words and used letters
    selected_words = []
    used_letters = set()
    
    # Loop until we have selected k words
    while len(selected_words) < k:
        # Initialize the maximum entropy and the best word
        max_entropy = 0
        best_word = None
        
        # Loop through the remaining words
        for word in wordlist:
            # Skip the word if it has already been selected
            if word in selected_words:
                continue
            
            # Calculate potential new unique letters and entropy if this word is added
            potential_new_letters = set(word) - used_letters
            potential_word_list = selected_words + [word]
            entropy = calculate_entropy(potential_word_list)
            
            # Evaluate the word based on number of new unique letters and entropy
            if len(potential_new_letters) > 0 and entropy > max_entropy:
                max_entropy = entropy
                best_word = word
        
        # Add the best word to the selected words
        if best_word:
            selected_words.append(best_word)
            used_letters.update(set(best_word))
        else:
            # If no word adds new unique letters, break the loop
            break
    
    return selected_words, calculate_entropy(selected_words)

# Load your wordlist (assuming it is in a text file with one word per line)
with open('./entropy/wordlist.pl') as file:
    wordlist = [line.strip() for line in file]

best_words, max_entropy = find_greedy_optimal_words(wordlist)
print(f'Best set of words: {best_words}')
print(f'Maximum entropy: {max_entropy}')

'''
Best set of words: ABENG, CHIKO, DRUMS, APTLY, WAQFS
'''
