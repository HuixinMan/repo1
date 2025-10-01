# 2
setwd("/Users/manhuixin/Desktop/UoE课程资料/sem1/statistical programming/project1")
# 3
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8")
cat("Print first 50 words as an example: ") 
head(a, 50)

# Huixin's part
# 4(a)
left_brackets <- grep("\\[",a)                  # Find the positions of all the "[" in a
delete <- c()                                   # A place for storing vectors that need to be deleted
for (i in left_brackets) {
  right_brackets <- grep("\\]", a[i:(i+100)])   # Search for "]" within the maximum of 100 words after the "[" position
  if (length(right_brackets) > 0) {
    delete <- c(delete, i:(i + right_brackets[1] - 1))
  }                                             # If the "]" character is found, record all the positions between "[" and "]", from '[' until it is deleted until it reaches the position of ']'
  else {
    delete <- c(delete, i)                      # If the "]" is not found, then just delete the "["
  }
}
a_clean1 <- a[-unique(delete)]
cat("Print out the first 50 words after removing '[ ]' as an example: ")  
head(a_clean1, 50)

# 4(b)
remove <- function(a_clean1) {
  keep <- rep(TRUE, length(a_clean1))           # Initialize a logical vector to indicate which words should be retained
  for (i in seq_along(a_clean1)) {
    w <- a_clean1[i]
    if (w == toupper(w) && !(w %in% c("I", "A"))) {
      keep[i] <- FALSE                          # If a word is all caps and not "I" or "A", delete it
    }
    if (grepl("^[0-9]+$", w)) {
      keep[i] <- FALSE                          # If a word is only digits, delete it
    }
  }
  return(a_clean1[keep])
}
a_clean2 <- remove(a_clean1)
cat("Print out the first 50 words after removing fully upper word and numbers as examples:")
head(a_clean2, 50)

# 4(c)
a_clean3 <- gsub("_", " ", a_clean2)
a_clean4 <- gsub("-", " ", a_clean3)     #remove “ ” and “-” from words
cat("Print out the first 50 words after removing '-' and '_' as examples:")
head(a_clean4,50)

# 4(d)
split_punct <- function(words, marks) {
  marks_regex <- paste0("([", paste0("\\", marks, collapse=""), "])")   # Escape each punctuation with \ and join them into a regex
  words_spaced <- gsub(marks_regex, " \\1", words, perl = TRUE)         # Replace each punctuation with " punctuation" (add a space before it)
  result <- unlist(strsplit(words_spaced, "\\s+"))                      # Split into individual words/punctuation and output as vectors
  result <- result[result != ""]                                        # Delete empty strings
  return(result)
}

# 4(e)
text <- a_clean4
marks <- c(",", ".", ";", "!", ":", "?")
a_clean5 <- split_punct(text, marks)                        # Use split_punct function to separate the punctuation marks
cat("Print out the first 50 words by using split_punct 
    function to separate the punctuation marks as examples:")
head(a_clean5,50)

# 4(f)
a_clean6 <- tolower(a_clean5)    #Convert all letters to lowercase
cat("Print out the first 50 words by converting all letters to lowercase. as examples:")
head(a_clean6,50)

# 5 Ruoxi's part
b_all <- unique(a_clean6)                                 #find the unique word
token_all <- match(a_clean6,b_all)                        #match each word in the a_clean4 to its unique position in b_all
word_counts <- tabulate(token_all,nbins = length(b_all))  #count the occurrence of each unique word
ranks <- rank(-word_counts,ties.method = "first")         #rank each word by its frequency, higher frequency ranked higher
b <- b_all[ranks <= 1000]                                 #keep the top 1000 ranked word
token <- match(a_clean6,b)                                #switch the words in a_clean6 into positions in b(ranked word list)
cat("b:",b,"\n")           
print(token)

# Leying's part
# 6(a)
mlag <- 4  # to predict the next word considering the previous 4 words

n <- length(token)# to calculate the length of n which needs to be the same as "a"

# 6(b) 
M <- matrix(NA, nrow = n - mlag, ncol = mlag + 1)# to generate a matrix, with (n-malag)rows, (mlag+1)columns, and initial value is NA

#Fill the M matrix
for (j in 1:(mlag + 1)) { # to create a loop，from 1 to (mlag+1),and (mlag+1) is the column of the matrix，because every column represents a unit time，so the number of loops is equal to the number of the columns
  
  startindex <- j # to calculate the start index 
  
  endindex <- j+(n-mlag-1) # to calculate the end index
  
  newdata <- token[startindex:endindex]# to get the words data from start index to end index
  
  M[, j] <- newdata}# to assign the words data to the Jth column of M

# 7 
#construct next.word function
next.word <- function(key, M, M1, w = rep(1, ncol(M)-1)) {
  
  mlag <- ncol(M) - 1 # to calculate the order of Marcov model
  keylength <- length(key)# to get the length of key
  
  if (keylength> mlag) {
    key <- tail(key, mlag)
    keylength <- mlag
  } # to get the last mlag number of key if key is too long
  
  allcandidates <- integer(0)
  allweights <- numeric(0)
  
  #Try all possible memory lengths from higher-order to lower-order
  for (order in min(keylength, mlag):1) {# Take the last order of the key words to be the current context
    
    currentkey <- tail(key, order) # Calculate the start column to be matched in the M-matrix
    
    mc <- mlag - order + 1
    
    # Now construct the matching formula：
    
    Msur <- M[, mc:mlag, drop = FALSE] #to get the surrounding column corresponding to the current order
    
    ii <- colSums(!(t(Msur) == currentkey)) #to calculate the matching degree between the sample and the current context
    
    matchingrows <- which(ii == 0 & is.finite(ii)) #Find the complete matching sample line 
    
    
    if (length(matchingrows) > 0) { # to check if there are any matching samples
      
      nextwords <- M[matchingrows, mlag+1] # take the token value of next word in the matching row
      
      validwords <- nextwords[!is.na(nextwords)] # abandon all the NA 
      
      #Collect candidate words and weights
      if (length(validwords) > 0) { #Check the length 
        
        allcandidates <- c(allcandidates, validwords) #Add the candidate words to the list
        
        allweights <- c(allweights, rep(w[order], length(validwords))) #Assign the weight w to each candidate word
      }
    }
  }
  
  #Have candidate words:randomly select one word according to the weight
  if (length(allcandidates) > 0) {
    return(sample(allcandidates, 1, prob = allweights))
  }
  
  #If don't have candidate words: select randomly one word form text
  validtokens <- M1[!is.na(M1)]
  return(ifelse(length(validtokens) > 0, sample(validtokens, 1), 1))
}

# 8 Ruoxi's part
select <- function(M1,b) {                    #select a single word token randomly
validtokens <- M1[!is.na(M1)]                 #get all valid tokens (remove NA)
punctuation <- c(".", ",", ";", ":", "!", "?")#cannot start a sentence with a punctuation
  
  for (i in 1:20) {                               #try to find a non-punctuation starting word multiple times
    token <- sample(validtokens,1)                #select a tocken randomly
    if (!b[token] %in% punctuation) return(token) #check if the corresponding word is a punctuation
  }
  return(match("the",b))                          #if multiple attempts fail, default to using 'the'
}

generate_sentence <- function(M,M1,b,start_word=NULL) {  #generate a sentence
  if (is.null(start_word)) { 
    token <- select(M1,b)                                #if no starting word is specified, randomly choose one
  } else {
    token <- match(tolower(start_word),b)                #if a starting word is specified, search it in the b
    if (is.na(token)) token <- select(M1,b)              #if the word cannot be found, go back to random selection
  }
  
  tokens <- c(token)                                        #starting from the starting word
  mlag <- ncol(M)-1                                         #get the maximum memory length of the model
  
  repeat {
    context <- tail(tokens, min(length(tokens), mlag))      #get currently available contextual words, but not exceeding the memory capacity of the model
    cat("present key (context):", paste(b[context], collapse = " "), "\n")
    next_token <- next.word(context, M, M1)                 #predict the next word
    tokens <- c(tokens, next_token)                         #add new words to the sentence
    if (b[next_token] == ".") break                         #if it is a period, if so, stop generating
  }
  
  words <- b[tokens]                                 #convert token back to words
  words <- words[!is.na(words)]                      #do not use NA value  
  cat("Final Result: ")
  sentence <- paste(words, collapse = " ")
  sentence <- gsub(" ([,\\.;:!\\?])","\\1", sentence)#remove excess spaces before punctuation
  cat(sentence,"\n")
  
  return(sentence)
}
# 9
cat(" A Shakespeare sentence simulator \n")

# example 1：starting with a random word
cat("Example 1: Random start\n")
generate_sentence(M, token, b)
cat("\n")

# example 1：starting with “romeo”
cat("Example 2: Start from 'romeo'\n")  
generate_sentence(M, token, b, "romeo")
cat("\n")
