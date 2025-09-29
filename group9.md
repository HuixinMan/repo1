# 2
setwd("/Users/manhuixin/Desktop/UoE课程资料/sem1/statistical programming/project1")
# 3
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8")
cat("Print first 50 words as an example: ") 
head(a, 50)

# 4(a)
left_brackets <- grep("\\[",a)                  # Find the positions of all the "[" in a
delete <- c()                                   # A place for storing vectors that need to be deleted
for (i in left_brackets) {
  right_brackets <- grep("\\]", a[i:(i+100)])   # Search for "]" within the maximum of 100 words after the "[" position
  if (length(right_brackets) > 0) {             # If the "]" character is found, record all the positions between "[" and "]"
    delete <- c(delete, i:(i + right_brackets[1] - 1))   #From '[' until it is deleted until it reaches the position of ']'
  } else {
    delete <- c(delete, i)                      # If the "]" is not found, then just delete the "["
  }
}
a_clean1 <- a[-delete]                          # Delete the "[ ]" and the words in the brackets that are recorded in "a"
cat("The original length of a: ", length(a), "\n")
cat("The length after removing '[ ]': ", length(a_clean1), "\n")
cat("Print out the first 50 words after removing '[ ]' as an example: ")  
head(a_clean1, 50)

#4(b)
remove <- function(a_clean1) {                  # defined function
  keep <- rep(TRUE, length(a_clean1))           # Initialize a logical vector to indicate which words should be retained
  for (i in seq_along(a_clean1)) {
    w <- a_clean1[i]
    if (w == toupper(w) && !(w %in% c("I", "A"))) {
      keep[i] <- FALSE                          # If a word is all caps and not "I" or "A", delete it
    } 
    if (grepl("^[0-9]+$", w)) {                 # If a string is only digits, delete it
    }
  }
  return(a_clean1[keep])
}
a_clean2 <- remove(a_clean1)
cat("Print out the first 50 words after removing fully upper word and numbers as examples:")
head(a_clean2, 50)

#4(c)
a_clean3 <- gsub("_", " ", a_clean2)
a_clean4 <- gsub("-", " ", a_clean3)            #remove “ ” and “-” from words
cat("Print out the first 50 words after removing '-' and '_' as examples:")
head(a_clean4, 50)

#4(d)
split_punct <- function(words, marks) {
  marks_regex <- paste0("([", paste0("\\", marks, collapse=""), "])")    # Escape each punctuation with \ and join them into a regex
  for (m in marks) {
    words <- gsub(m, paste0("", m), words, fixed = TRUE)                # Replace each punctuation with " punctuation" (add a space before it)
  }
  words_spaced <- words          
  result <- unlist(strsplit(words_spaced, "\\s+"))                       # Split into individual words/punctuation and output as vectors.
  result <- result[result != ""]                                         # Delete empty strings
  return(result)
}

# 4(e)
puncts <- c(",", ".", ";", "!", ":", "?")
a_clean5 <- split_punct(a_clean4, puncts)                               #Use split_punct function to separate the punctuation marks
cat("Print out the first 50 words by using split_punct 
    function to separate the punctuation marks as examples:")
head(a_clean5,50)

# 4(f)
a_clean6 <- tolower(a_clean5)                                           #Convert all letters to lowercase
cat("Print out the first 50 words by converting all letters to lowercase. as examples:")
head(a_clean6,50)
