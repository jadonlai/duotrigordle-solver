#include <algorithm>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <bit>
#include <chrono>

#define WORD_LENGTH 5

typedef struct
{
    std::string s;
    uint32_t mask;
} word;


// assuming all chars are upper-case letters
uint32_t convert_word_to_mask(std::string word)
{
    uint32_t mask = 0;
    for(int i = 0; i < word.size(); i++)
    {
        int bitToSet = word[i] - 'A';
        mask |= (1 << bitToSet);
    }
    return mask;
}


std::vector<word> read_words(std::string filename)
{
    std::vector<word> lines;
    std::ifstream infile(filename);

     // Check if the file was opened successfully
    if (!infile.is_open()) 
    {
        std::cerr << "Error: Could not open file '" << filename << "'" << std::endl;
        return lines;
    }

    std::string line;
    while (std::getline(infile, line))
    {
        std::string s = line.substr(7, WORD_LENGTH);
        uint32_t mask = convert_word_to_mask(s);
        lines.push_back({s, mask});
    }
    infile.close();

    return lines; 
}

bool compareByPopcount(const word& a, const word& b)
{
  int countA = std::popcount(a.mask);
  int countB = std::popcount(b.mask);
  // Stable sort for equal counts
  return countA > countB || (countA == countB && a.mask < b.mask);
}

void sortByPopcount(std::vector<word>& words)
{
    std::sort(words.begin(), words.end(), compareByPopcount);
}

std::vector<word> removeSubsetWords(std::vector<word> &words)
{
    sortByPopcount(words);
    std::vector<word> seen_words;

    for(word w : words)
    {
        bool is_subset = false;
        for(word seen_word : seen_words)
        {
            if((w.mask & seen_word.mask) == w.mask)
            {
                // std::cout << w.s << " is a subset of " << seen_word.s << std::endl;
                is_subset = true;
                break;
            }
        }

        if(!is_subset)
            seen_words.push_back(w);
    }

    return seen_words;
}


/*
If there are 'N' more words to add to the list, then
(5 * N) is the theoretical maximum number of additional letters 
you can increase your current score by.
If the current best running maximum is greater than or equal to
current-score + (5 * N), then there is no reason to continue exploring, 
and you should break early from that loop
*/
inline bool shouldBreakEarly(uint32_t runningOr, int wordsLeft, int currentBestScore)
{
    return currentBestScore >= std::popcount(runningOr) + WORD_LENGTH * wordsLeft;
}


int main()
{
    auto start = std::chrono::high_resolution_clock::now();

    std::string filename = "../wordlist.pl";

    std::vector<word> words = read_words(filename); 
    std::cout << "Number of words: " << words.size() << std::endl;

    words = removeSubsetWords(words);
    std::cout << "Number of words after removing subset words: " << words.size() << std::endl;

    // optimization:
    // remove all words whose letters are subsets of other words

    int maxNumUniqueLetters = 0;
    std::vector<word> bestWordSet;
    
    for(int g = 0; g < words.size() - 4; g++)
    {
        // if(g % 100 == 0)
        //    std::cout << "(" << g << "/" << words.size() - 4 << ") complete" << std::endl;

        for(int h = g + 1; h < words.size() - 3; h++)
        {
            if(shouldBreakEarly(words[g].mask | words[h].mask, 3, maxNumUniqueLetters))
                break;

            for(int i = h + 1; i < words.size() - 2; i++)
            {
                if(shouldBreakEarly(words[g].mask | words[h].mask | words[i].mask, 2, maxNumUniqueLetters))
                    break;

                for(int j = i + 1; j < words.size() - 1; j++)
                {
                    if(shouldBreakEarly(words[g].mask | words[h].mask | words[i].mask | words[j].mask, 1, maxNumUniqueLetters))
                        break;

                    for(int k = j + 1; k < words.size(); k++)
                    {
                        uint32_t combined_mask = 
                        words[g].mask | 
                        words[h].mask | 
                        words[i].mask | 
                        words[j].mask | 
                        words[k].mask;

                        int numUniqueLetters = std::popcount(combined_mask);
                        if(numUniqueLetters > maxNumUniqueLetters)
                        {
                            maxNumUniqueLetters = numUniqueLetters;
                            bestWordSet = {words[g], words[h], words[i], words[j], words[k]};
                            std::cout << "New best combo found: " << maxNumUniqueLetters << std::endl;
                            for(word w : bestWordSet) {
                                std::cout << "\t" << w.s << std::endl;
                            }
                        }
                    }
                }
            }
        }
    }

    std::cout << "The best set of words are: " << std::endl;
    for(word w : bestWordSet) {
        std::cout << "\t" << w.s << std::endl;
    }
    std::cout << "Max number of unique letters: " << maxNumUniqueLetters << std::endl;
    


    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
 
    std::cout << "Time taken by function: " << duration.count() << " ms" << std::endl;
}