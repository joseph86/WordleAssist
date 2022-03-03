###############################################################################
# WordleAssist.R
# Author: Joseph Parish
# Date: 2022/03/03
# This is the final working version of WordleAssist. It works as a companion
# to the game Wordle. When you make a guess in the game, you enter the same
# guess here and then a sequence of bs, gs, and ys to indicate the color-
# coded hints the game gives you. WordleAssist filters the list of possible
# solutions and suggests guesses based on letter frequency.
###############################################################################

# Import list of words from text file
# Use unlist to convert to atomic vector for convenient string manipulation
wordList = unlist(read.csv('~/JoeyFails/WordleAssist/words.txt',FALSE),use.names = FALSE)

# Print suggested guesses based on total letter frequency
print('Suggested guesses:')
# start with words that have the highest letter frequency without regard
# to letter position, though I'm not yet certain this is the best strategy
# the regular expression '(.).*\\1' finds any words with repeated letters
# because we want information about as many different letters as possible
# at least for our initial guess
noDubs = wordList[!str_detect(wordList, '(.).*\\1')]
scores = scoreWords(noDubs)
print(noDubs[order(scores, decreasing = TRUE)][1:20])
# Get the first guess from the user
print('Enter your first word guess. Enter q to quit')
input = str_trim(str_to_lower(as.character(readline())))

while (input != 'q') {
  guess = input
  print('Enter the 5-letter color code for each letter.')
  print('y=yellow, g=green, b=black')
  print('Example: yybgb')
  input = str_trim(str_to_lower(as.character(readline())))
  if (input == 'q') break
  colorCode = str_trim(str_to_lower(input))
  
  wordList = filterList(wordList, guess, colorCode)
  
  print('Good guesses among remaining words:')
  # Print re-ordered wordList by descending score.
  if(length(wordList)<50){
    print(bestGuesses(wordList)[1:min(20,length(wordList))])
  }
  # If the list is still more than 50 long, exclude words with repeated letters
  else{
    noDubs = wordList[!str_detect(wordList, '(.).*\\1')]
    scores = scoreWords(noDubs)
    print(noDubs[order(scores, decreasing = TRUE)][1:20])
  }
  print(str_c(as.character(length(wordList)),'possible matches',sep = ' '))
  print('Enter your next guess or q to quit')
  input = str_trim(str_to_lower(as.character(readline())))
}

#########################################################################
# filterList(wordList, guess, colorCode): 
# Returns a filtered list of possible words based on color coded hints
#########################################################################
filterList = function(wordList, guess, colorCode){
  # Keep track of the indices of green and yellow letters for special 
  # treatment when filtering out words with black letters
  ignore = rep(' ',5)
  byg = ignore
  
  # Filter based on green letters
  # Skip this step if colorCode has no 'g's
  if(str_detect(colorCode, 'g')){
    # Build a regular expression to require green letters in specific positions.
    include = '^.....'
    for(i in 1:5){
      if (str_sub(colorCode,i,i)=='g'){
        str_sub(include,i+1,i+1) = str_sub(guess,i,i)
        ignore[i] = str_sub(guess,i,i)
        byg[i] = str_sub(colorCode,i,i)
      }
    }
    
    # Include only words with the green letters in the same position as guess
    wordList = wordList[str_detect(wordList,include)]
  } # end check for greens
  
  # Filter based on yellow letters
  # Skip this step if there are no 'y's in colorCode
  if(str_detect(colorCode, 'y')){
    # Build a regular expression that excludes any words that have letters in
    # the same position as yellow letters in guess
    include = ''
    for(i in 1:5){
      if (str_sub(colorCode,i,i)=='y'){
        template = '^.....|'
        str_sub(template,i+1,i+1) = str_sub(guess,i,i)
        include = str_c(include, template, sep = '')
        ignore[i] = str_sub(guess,i,i)
        byg[i] = str_sub(colorCode,i,i)
        
        # At each iteration, include only words that have the yellow letter 
        # somewhere in the word
        wordList = wordList[str_detect(wordList, str_sub(guess,i,i))]
      } # end check color code at this position is 'y'
    } # end for (i in 1:5) loop
    
    # remove the last '|' from the regular expression stored in include
    include = str_sub(include,1,-2)
    
    # Filter words that have yellow letters in the same position as guess
    # the output of str_detect() is negated to exclude 
    wordList = wordList[!str_detect(wordList, include)]
  } # end check for yellow letters
  
  # Filter based on black letters
  # Skip this step if colorCode has no 'b's
  if(str_detect(colorCode, 'b')){
    for(i in 1:5){
      if (str_sub(colorCode,i,i)=='b'){
        ltr = str_sub(guess,i,i)
        # Check if this letter was also yellow or green elsewhere in guess
        if (!any(ignore==ltr)){
          # Not yellow or green
          # Filter all words that have this letter in any position
          wordList = wordList[!str_detect(wordList, ltr)]
        }
        else {
          # This letter was also green or yellow in another position. 
          if (any((ignore==ltr)&(byg=='y'))){
            # This letter WAS YELLOW elsewhere. Filter only from this position
            exclude = '^.....'
            str_sub(exclude,i+1,i+1)=ltr
          }
          else{
            # This letter was NOT yellow at a different position. 
            # Filter from all positions except green
            exclude = ''
            isGreen = (ignore==ltr)&(byg=='g')
            for(j in 1:5){
              if (!isGreen[j]){
                template = '^.....|'
                str_sub(template,j+1,j+1) = ltr
                exclude = str_c(exclude, template, sep='')
              }
            } # end for j in 1:5
            # Cut off the trailing | from the regular expression
            exclude = str_sub(exclude,1,-2)
          } # end code block for green with no yellows of this letter  
          
          # the output of str_detect() is negated to exclude 
          wordList = wordList[!str_detect(wordList, exclude)]
        } # end block for yellow or green of this letter
        
      } # end if colorCode at position i contains 'b'
    } # end for(i in 1:5) loop
  } # end check for black letters
  return(wordList)
} # end filterList()

#########################################################################
# 
# bestGuesses(wordList): 
# This is a wrapper for scoreWords()
# Returns wordList sorted in order of descending score
# 
#########################################################################
bestGuesses = function(wordList){
  scores = scoreWords(wordList)
  wordList[order(scores, decreasing = TRUE)]
}

#########################################################################
# 
# scoreWords(v_words): 
# Receives a character vector of words and returns a vector of scores for 
# the words according to character frequency taking letter position into 
# account. 
#
#########################################################################
scoreWords = function(v_words){
  # Put each word into a vector 5 long to create a matrix for easy analysis
  # of character frequency in each position
  letterPos = matrix("", length(v_words), 5)
  for(w in 1:length(v_words)){
    for(p in 1:5){
      letterPos[w,p] = str_sub(v_words[w],p,p)
    }
  }
  
  # Create a 26x5 matrix to store scores by letter position. 
  letterScores = matrix(0,26,5,dimnames = list(letters))
  # Find the frequency of each letter in each position of letterPos
  for (l in 1:26){
    for (p in 1:5){
      letterScores[l,p] = sum(str_count(letterPos[,p],letters[l])) 
      # Confirm this is the same as sum(letterPos[,p]==letters[l])
    }
    # If a letter is yellow, there are 4 remaining possible locations it could 
    # be in. So getting a letter and position correct (green) is 4 times more 
    # valuable than getting a letter correct but in the wrong position. 
    # This is almost certainly incorrect because I haven't put enough thought
    # into it.
    totalFreq = sum(letterScores[l,])
    letterScores[l,] = 4*letterScores[l,]+totalFreq-letterScores[l,]
  }
  
  # Initialize score vector and fill it with the scores for each word
  scores = rep(0,length(v_words))
  for (w in 1:length(v_words)){
    for(p in 1:5){
      # Use row names to index the letter row corresponding to the
      # letters at each position in the word, pull the frequency from 
      # the letter frequency matrix at the column identified by p for position
      scores[w] = scores[w] + letterScores[letterPos[w,p],p]
    }
  }
  return(scores)
  
} # end scoreWords()

