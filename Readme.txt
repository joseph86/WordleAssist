# WordleAssist
 WordleAssist is an R Script to choose the best guesses in Wordle based on 
 letter frequency. It works exclusively from the official list of possible 
 answers in Wordle, which was extracted from the source code. Sure, maybe 
 this is cheating when it comes to the game, but I'm treating this as an 
 exercise.
 
 This relies on regular expressions to filter the full list of words based 
 on the hints given as feedback.
 
 
 Known bugs:
 If a word has two of the same letter, it won't handle the black, yellow, or 
 green hints correctly.
 
 Future development:
 I need to run some data analysis on word guesses to see whether it's better 
 to go for straight letter frequency, or letter frequency by position in
 the 5-letter words, or perhaps some mixture of the two.
 
 I need to add some flexibility to remove words with a repeated letter during 
 at least the first guess, but let them back in. Maybe I can do this by just
 ranking the words by entropy. Maybe I don't need to.
 
 Add a way to fully simulate the game, by giving appropriate hints, Then 
 run simulations to determine the best strategy for minimizing the average
 number of guesses to find the correct word. This will require at least 
 a funtion that produces hints based on the current answer, and something to
 run through every possible word with some parameters that control the strategy.
 
