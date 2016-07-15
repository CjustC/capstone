# global.R

suppressWarnings(suppressMessages(library(shiny)))
suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(quanteda)))
suppressWarnings(suppressMessages(library(stringi)))

load(file = "ng.1.RData", envir=.GlobalEnv)
load(file = "ng.2.RData", envir=.GlobalEnv)
load(file = "ng.3.RData", envir=.GlobalEnv)
load(file = "ng.4.RData", envir=.GlobalEnv)

# Clean words
cleanData <- function(data){
     # Strip unwanted UTF-8 characters
     data <- iconv(data, "UTF-8", "ASCII", "?")
     # Adjust unicode ' characters to be all consistent
     data <- gsub("\xe2\x80\x99", "'", data, perl = TRUE)
     data <- gsub("\u0091|\u0092|\u0093|\u0094|\u0060|\u0027|\u2019|\u000A", "'", data, perl = TRUE)
     # strip unused characters but leave ' and -
     data <- gsub("[^[:alpha:][:space:]'-]", " ", data)
     # Remove single quotes that are quoted strings and not in the middle of a word leaving contractions
     # Leave - and ' in the middle of words
     data <- gsub("(?<!\\w)[-'](?<!\\w)" , " ", data, perl = TRUE)
     # Consolidate spaces
     data <- gsub("[[:space:]]+", " ", data, perl=TRUE) 
     # Strip leading spaces
     data <- gsub("^[[:space:]]+", "", data, perl = TRUE)
     
     return(data)
}

# Strip_Split words
splitData <- function(data){
   data <- toLower(data)
   data <- gsub("[^\\p{L}\\s]+", "", data, ignore.case=F, perl=T)
   data <- stri_split_fixed(data, " ", simplify = TRUE) # strip and split the data
   
   return(data)
}


# Unigram Function: Input phrase 'wordCount' = 0, bring back top 5 unigrams
ng1Func <- function(blank){
     possibilities <- ng.1[1:5,] # return first 5 rows
     
     # List possiblities for display in Table output
     alternates <<- as.data.frame(possibilities[ , .(w1, uniFreq)])
     alternates <<- alternates[!is.na(alternates$uniFreq), ] # No NAs
     alternates <<- alternates[!duplicated(alternates), ] # No Duplicates
     if(nrow(alternates)==0){
          alternates <<- data.frame(Word = NA, Frequency = NA)
          ngramOut <<- "No ngram selected"
     }else{
          alternates <<- alternates
          colnames(alternates) <<- c("Word", "Frequency")
          rownames(alternates) <<- NULL
          ngramOut <<- "unigram"
     }
     
     guessOut <- possibilities$w1[1]
     if(is.na(guessOut)|is.null(guessOut)){ # No NAs and No NULLs
          #          guessOut <- "Apologies, either a rare word has been entered or a word has been mispelled. Please try again."
          guessOut <- "Apologies, either a rare word has been entered or a word has been mispelled. Please try again."
     }
     
     return(guessOut)
}

# BiGram Function: Input phrase 'wordsCount' = 1, match ng2 data and bring back word 2
ng2Func <- function(phraseIn_a){
     word1 <- phraseIn_a[1] # grab 1st word
     matched <- ng.2[ which(ng.2$w1 == word1) , ] # Match on 'word'
     possibilities <- matched[1:5,] # return first 3 rows that match input phrase
     
     # List possiblities for display in Table output
     alternates <<- as.data.frame(possibilities[ , .(w2, likelihood)])
     alternates <<- alternates[!is.na(alternates$likelihood), ] # No NAs
     alternates <<- alternates[!duplicated(alternates), ] # No Duplicates
     if(nrow(alternates)==0){
          alternates <<- data.frame(Word = NA, Likelihood = NA)
          ngramOut <<- "No ngram selected"
     }else{
          alternates <<- alternates
          colnames(alternates) <<- c("Word", "Likelihood")
          rownames(alternates) <<- NULL
          ngramOut <<- "bigram"
     }
     
     guessOut <- possibilities$w2[1]
     if(is.na(guessOut)|is.null(guessOut)){ # No NAs and No NULLs
#          guessOut <- "Apologies, either a rare word has been entered or a word has been mispelled. Please try again."
          guessOut <- ng1Func(phraseIn_a)
     }
     
     return(guessOut)
     }

# TriGram Function: Input phrase 'wordsCount' = 2, match ng3 data and bring back word 3
ng3Func <- function(phraseIn_b){
     word1 <- phraseIn_b[1] # grab 1st word
     word2 <- phraseIn_b[2] # grab 2nd word
     matched <- ng.3[ which(ng.3$w1 == word1 & ng.3$w2 == word2) , ]
     possibilities <- matched[1:5,] # return first 3 rows that match input phrase
     
     # List possiblities for display in Table output
     alternates <<- as.data.frame(possibilities[ , .(w3, likelihood)])
     alternates <<- alternates[!is.na(alternates$likelihood), ] # No NAs
     alternates <<- alternates[!duplicated(alternates), ] # No Duplicates
     if(nrow(alternates)==0){
          alternates <<- data.frame(Word = NA, Likelihood = NA)
          ngramOut <<- "No ngram selected"
     }else{
          alternates <<- alternates
          colnames(alternates) <<- c("Word", "Likelihood")
          rownames(alternates) <<- NULL
          ngramOut <<- "trigram"
     }
     
     guessOut <- possibilities$w3[1]
     if(is.na(guessOut)|is.null(guessOut)){ # No NAs and No NULLs
          guessOut <- ng2Func(phraseIn_b[2])
     }
     
     return(guessOut)
}

# QuadGram Function: Input phrase 'wordsCount' = 3, match ng4 data and bring back word 4
ng4Func <- function(phraseIn_c){
     word1 <- phraseIn_c[1] # grab 1st word
     word2 <- phraseIn_c[2] # grab 2nd word
     word3 <- phraseIn_c[3] # grab 3rd word
     matched <- ng.4[ which(ng.4$w1 == word1 & ng.4$w2 == word2 & ng.4$w3 == word3) , ]
     possibilities <- matched[1:5,] # return first 3 rows that match input phrase
     
# List possiblities for display in Table output
     alternates <<- as.data.frame(possibilities[ , .(w4, likelihood)])
     alternates <<- alternates[!is.na(alternates$likelihood), ] # No NAs
     alternates <<- alternates[!duplicated(alternates), ] # No Duplicates
     if(nrow(alternates)==0){
          alternates <<- data.frame(Word = NA, Likelihood = NA)
          ngramOut <<- "No ngram selected"
     }else{
          alternates <<- alternates
          colnames(alternates) <<- c("Word", "Likelihood")
          rownames(alternates) <<- NULL
          ngramOut <<- "quadgram"
     }
     
     guessOut <- possibilities$w4[1]
     if(is.na(guessOut)|is.null(guessOut)){ # No NAs and No NULLs
          guessOut <- ng2Func(phraseIn_c[3])
     }
     
     return(guessOut)
}

# Ng+ Function: Input phrase 'wordsCount' > 3, match ng3 data and bring back word 
ngMoreFunc <- function(phraseIn_d){
     revInput <- rev(phraseIn_d)
     guessOut <- ng2Func(revInput[1])
#     ngramOut <<- "ngram rolled back"

     return(guessOut)
}


