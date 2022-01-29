#########################################################################
# scoreWords: 
# Receives a character vector of words, and a letter
# frequency matrix defining letter frequency by position and 
# returns a vector of scores for the words according to character
# frequency. If byLetterPos is TRUE, the score is generated letter
# position, otherwise, by total letter frequency
#########################################################################
scoreWords = function(v_word, letterPosFreq, byLetterPos = FALSE){
  # initialize the letter frequency matrix and the frequency column
  # from which to pull letter frequencies
  letterFreq = letterPosFreq
  freqCol = 1:5
  
  if(byLetterPos == FALSE){
    # freqCol will always return index 6
    freqCol = rep(6,5)
    # Augment the letter frequency matrix with a new column to store
    # the total letter frequency for each letter
    letterFreq = cbind(letterFreq,rep(0,26))
    for(l in 1:26){
      letterFreq[l,6] = sum(letterPosFreq[l,])
    }
  }
  
  # Initialize score vector and fill it with the scores for each word
  scores = rep(0,length(v_word))
  for (w in 1:length(v_word)){
    for(p in 1:5){
      # Use row names to index the letter row corresponding to the
      # letters at each position in the word, pull the frequency from 
      # the letter frequency matrix at the column identified by freqCol
      scores[w] = scores[w] + letterFreq[str_sub(v_word[w],p,p),freqCol[p]]
    }
  }
  return(scores)

}

#########################################################################
# wordsByFreq: 
# Takes a character vector of words (x_char), calculates the 
# frequency of each letter by position, and calls scoreWords 
# to give each word a rank according to letter frequency
#########################################################################
wordsByFreq = function(x_char, byLetterPos = FALSE){

  # Put each word into a vector 5 long to create a matrix for easy analysis
  # of character frequency in each position
  letterPos = matrix("", length(x_char), 5)
  
  # Fill the matrix with one letter at each position
  # This makes counting letter frequencies per position very easy
  # The rows are words and the columns are the letters at each position
  for(w in 1:length(x_char)){
    for(p in 1:5){
      letterPos[w,p] = str_sub(x_char[w],p,p)
    }
  }
  
  # Create a matrix to store letter frequencies
  # name the rows by the alphabet
  letterPosFreq = matrix(0,26,5,dimnames = list(letters))
  # Find the frequency of each letter in each position of letterPos
  for (l in 1:26){
    for (p in 1:5){
      letterPosFreq[l,p] = sum(str_count(letterPos[,p],letters[l]))
    }
  }
  
  # Remove words with repeated letters if byLetterPos == FALSE
  # suggesting words without repeated letters in the early game 
  # increases the amount of information we can gain from guesses
  if (byLetterPos == FALSE){
    x_char = x_char[!str_detect(x_char, "(.).*\\1")]
  }
  # Give each word in x_char a score based on letter frequency
  scores = scoreWords(x_char, letterPosFreq, byLetterPos)
  
  # Re-order v_word by descending score. Highest scoring words will be 
  # those with the most frequent letters in each position
  x_char[order(scores, decreasing = TRUE)]
 
}
