# Use the tidyverse library
library(tidyverse)
source("wordsByFreq.R")


# Import list of words from text file
# Use unlist to convert to atomic vector for convenient string manipulation
wordList = unlist(read.csv('~/JoeyFails/Wordle/words.txt',FALSE),use.names = FALSE)

# Print suggested guesses based on total letter frequency
print("Suggested guesses:")
wordsByFreq(wordList[!str_detect(wordList, "(.).*\\1")], FALSE)[1:10]
# Get the first guess from the user
print("Enter your first word guess. Enter q to quit")
input = str_trim(str_to_lower(as.character(readline())))

while (input != "q") {
  guess = input
  print("Enter the 5-letter color code for each letter.")
  print("y=yellow, g=green, b=black")
  print("Example: yybgb")
  input = str_trim(str_to_lower(as.character(readline())))
  if (input == 'q') break
  colorCode = str_trim(str_to_lower(input))
  
  # Filter based on green letters
  # These will will have the largest effect on the length of wordList
  # skip this step if colorCode has no 'g's
  if(str_detect(colorCode, "g")){
    # Build a regular expression to filter out any words that do not have
    # letters in the same position as green letters in guess.
    include = '^.....'
    for(i in 1:5){
      if (str_sub(colorCode,i,i)=='g'){
        str_sub(include,i,i) = str_sub(guess,i,i)
      }
    }
    
    # Filter out all words that don't have the green letters from guess 
    # in the same position as guess
    wordList = wordList[str_detect(wordList,include)]
  }
  
  # Filter based on black letters
  # Skip this step if colorCode has no 'b's
  if(str_detect(colorCode, "b")){
    # Build a regular expression of any letters from guess in the same
    # position as the 'b's in colorCode.
    include = '['
    for(i in 1:5){
      if (str_sub(colorCode,i,i)=='b'){
        include = str_c(include, str_sub(guess,i,i), sep = '')
      }
    }
    include = str_c(include, ']', sep = '')
    
    # Filter out  words in wordList that have letters from guess 
    # associated with 'b' in colorCode
    # the output of str_detect is negated here to exclude
    wordList = wordList[!str_detect(wordList, include)]
  }
  
  # Filter based on yellow letters
  #skip this step if there are no 'y's in colorCode
  if(str_detect(colorCode, "y")){
    # Build a regular expression that excludes any words that have letters in
    # the same position as yellow letters in guess
    include = ''
    for(i in 1:5){
      if (str_sub(colorCode,i,i)=='y'){
        template = '^.....|'
        str_sub(template,i+1,i+1) = str_sub(guess,i,i)
        include = str_c(include, template, sep = '')
        # Include only words that have the letter at location i in guess
        wordList = wordList[str_detect(wordList, str_sub(guess,i,i))]
      }
    }
    # remove the last '|' from the regular expression stored in exclude
    include = str_sub(include,1,-2)
    
    # Filter out all words that have yellow letters in the same position
    # as guess
    # the output of str_detect() is negated to exclude 
    wordList = wordList[!str_detect(wordList, include)]
  }
  # Suggest some guesses based on letter frequency in the remaining words
  # If there are few words remaining in x_char, go for green by ranking
  # according to letter frequency by letter position
  # I chose 100 as few enough, but some further analysis might find 
  # something better
  byLetterPos = ifelse(length(wordList)<50, TRUE, FALSE) 
  print("Good guesses among remaining words:")
  print(wordsByFreq(wordList,byLetterPos))
  print(str_c(as.character(length(wordList)),"possible matches",sep = " "))
  print("Enter your next guess or q to quit")
  input = str_trim(str_to_lower(as.character(readline())))
}

