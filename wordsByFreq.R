#########################################################################
# scoreWords: 
# Receives a character vector of words, and a letter
# frequency matrix defining letter frequency by position and 
# returns a vector of scores for the words according to character
# frequency. If byLetterPos is TRUE, the score is generated letter
# position, otherwise, by total letter frequency
#########################################################################
scoreWords = function(v_words, byLetterPos = FALSE){
  # Put each word into a vector 5 long to create a matrix for easy analysis
  # of character frequency in each position
  letterPos = matrix("", length(v_words), 5)
  
  # Fill the matrix with one letter at each position
  # This makes counting letter frequencies per position very easy
  # The rows are words and the columns are the letters at each position
  for(w in 1:length(v_words)){
    for(p in 1:5){
      letterPos[w,p] = str_sub(v_words[w],p,p)
    }
  }
  
  # Create a matrix to store letter frequencies. the last column stores
  # the total letter frequency regardless of position. 
  # name the rows by the alphabet
  letterFreq = matrix(0,26,6,dimnames = list(letters))
  # Find the frequency of each letter in each position of letterPos
  for (l in 1:26){
    for (p in 1:5){
      letterFreq[l,p] = sum(str_count(letterPos[,p],letters[l]))
    }
    letterFreq[l,6] = sum(letterFreq[l,])
  }
  
  # initialize the frequency column
  # from which to pull letter frequencies. Pegged to column 6 
  # if we're not going by letter position
  ifelse(byLetterPos,(freqCol = 1:5),(freqCol = rep(6,5)))
  
  # Initialize score vector and fill it with the scores for each word
  scores = rep(0,length(v_words))
  for (w in 1:length(v_words)){
    for(p in 1:5){
      # Use row names to index the letter row corresponding to the
      # letters at each position in the word, pull the frequency from 
      # the letter frequency matrix at the column identified by freqCol
      scores[w] = scores[w] + letterFreq[str_sub(v_words[w],p,p),freqCol[p]]
    }
  }
  return(scores)

}

#########################################################################
# wordsByFreq: 
# Takes a character vector of words (v_words), calculates the 
# frequency of each letter by position, and calls scoreWords 
# to give each word a rank according to letter frequency
#########################################################################
wordsByFreq = function(v_words, byLetterPos = FALSE){

    # Remove words with repeated letters if byLetterPos == FALSE
  # suggesting words without repeated letters in the early game 
  # increases the amount of information we can gain from guesses
  if (byLetterPos == FALSE){
    v_words = v_words[!str_detect(v_words, "(.).*\\1")]
  }
  # Give each word in v_words a score based on letter frequency
  scores = scoreWords(v_words, byLetterPos)
  
  # Re-order v_words by descending score. Highest scoring words will be 
  # those with the most frequent letters in each position
  v_words[order(scores, decreasing = TRUE)]
 
}
