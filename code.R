# Capstone Code

rm(list=ls(all=TRUE)) # removes all *objects* from the workspace.
setwd("~/DataScience/capstone") # set working directory

library(quanteda) # Faster cleaning/processing than RWeka or tm packages, can also create n-grams in the same call.  
library(stringi) # Stats on the files and splitting data frame
library(data.table) # Table joins
library(ggplot2) # Visualization
library(wordcloud) # Word Cloud Generator
library(RColorBrewer) # Color Palettes

# Download and unpack zip file
DownURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

if(!file.exists("Coursera-SwiftKey.zip")) {  # If file doesn't exist download the source file
     download.file(url = DownURL, destfile = "Coursera-SwiftKey.zip", method = 'curl')  # download file to local disk
     unzip("Coursera-SwiftKey.zip")  # unzip file
     unlink("Coursera-SwiftKey.zip")  # unlink zip file (delete zip file)
}

# Look at file size
blogSize <- round(file.info("./final/en_US/en_US.blogs.txt")$size / 1024^2)
twitterSize <- round(file.info("./final/en_US/en_US.twitter.txt")$size / 1024^2)
newsSize <- round(file.info("./final/en_US/en_US.news.txt")$size / 1024^2)
fileSize <- c(blogSize, twitterSize, newsSize)
print(fileSize) # Blogs: 200MB, Twitter: 159MB, News: 196MB
#----------------------------------------------------------------------------------------------

# Read in Data - Open Connection, readLines(), close connection for all 3 files
# set warn = FALSE to supresses the warning message that there was 'no newline' after the last line.
# BLOGS: 
blogCon <- file("./final/en_US/en_US.blogs.txt")
blogs <- readLines(blogCon, encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
close(blogCon)

# TWITTER: 
twitterCon <- file("./final/en_US/en_US.twitter.txt", 'rb')
twitter <- readLines(twitterCon, encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
close(twitterCon)

# NEWS: (News needs to open in binary due to special characters in text)
newsCon <- file("./final/en_US/en_US.news.txt", 'rb')
news <- readLines(newsCon, encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
close(newsCon)

# Get Line count. Remove duplicates in Twitter (re-tweets). Other data statistics can be seen in the Milestone Report.
# Twitter text needs to be explicitly utf-8 and NAs removed after converting.
convTweets <- iconv(twitter, to = "utf-8")
# The conversion leaves a vector with NAs. Remove the NAs.
twitter <- (convTweets[!is.na(convTweets)])
rm(convTweets) # tidy up

# Line Count 
blogLines <- length(blogs) 
twitterLines <- length(twitter) # only need twitter lines in order to remove duplicates
newsLines <- length(news) 
print(paste0("Blog Lines:", blogLines))
print(paste0("Twitter Lines:", twitterLines)) 
print(paste0("News Lines:", newsLines))

# Twitter will have duplicate lines (re-tweets) - duplicate lines removed
uniqueTwitter<-unique(twitter)
uniqueTwitterLines<-length(uniqueTwitter)
newTwitterLines<- twitterLines - uniqueTwitterLines
print(paste0("Number of Twitter source lines removed: ", newTwitterLines)) # Number of Twitter source lines removed: 54225
# Use 'uniqueTwitter' from this point
#----------------------------------------------------------------------------------------------

# Preform some pre-cleaning prior to cleaning & processing that will occur in the 'dfm' (part of the quanteda package)
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

# Clean the data
blogs <- cleanData(blogs)
twitter <- cleanData(uniqueTwitter) # clean reduced twitter lines 'uniqueTwitter'
news <- cleanData(news)
#----------------------------------------------------------------------------------------------

# Create a Corpus of each file and convert to lowercase - converting to lowercase at this time to reduce processing speed.
blogCorpus <- toLower(corpus(blogs))
twitterCorpus <- toLower(corpus(uniqueTwitter))
newsCorpus <- toLower(corpus(news))

# tidy up - Remove original datasets
rm(blogs); rm(twitter); rm(news); rm(uniqueTwitter)
rm(blogCon); rm(twitterCon); rm(newsCon)
#----------------------------------------------------------------------------------------------
# Each data source is **sentence** tokenized. Tokenizing to sentences helps to create proper n-grams that take into account sentence start/end *<s> </s>*. Quanteda's 'tokenize' - returns a character vector of sentences that have been segmented

# Blog tokens - Tokenize then write tokens to a file for later processing - Tokenized 898,321 texts
blogTokens <- tokenize(blogCorpus, removeURL = TURE, what = "sentence", simplify = TRUE, verbose = TRUE) 

# Twitter tokens - Tokenize then write tokens to a file for later processing - Tokenized  2,917,219 texts
twitterTokens <- tokenize(twitterCorpus, removeURL = TRUE, what = "sentence", simplify = TRUE, verbose = TRUE) 

# News tokens - Tokenize then write tokens to a file for later processing -  Tokenized 1,009,465 texts
newsTokens <- tokenize(newsCorpus, removeURL = TRUE, what = "sentence", simplify = TRUE, verbose = TRUE)

# tidy up - Remove each corpus that was created. We will be using the tokenized data from this point
rm(blogCorpus); rm(twitterCorpus); rm(newsCorpus) 
#----------------------------------------------------------------------------------------------

# **Samples** - Create a large Corpus from the 3 tokenized Corpra
set.seed(1000)
# Convert samples of tokens to corpus (remember the tokens at this point are sentences)
blogSample <- corpus(sample(blogTokens, length(blogTokens) * 0.05))
twitterSample <- corpus(sample(twitterTokens, length(twitterTokens) * 0.05))
newsSample <- corpus(sample(newsTokens, length(newsTokens) * 0.05))

# Combine the 3 corpora into one large corpus of tokenized data
myCorpus <- c(blogSample, twitterSample, newsSample)

## Save Corpus - testing recovery point
#    save(myCorpus.RData, file = "sample/myCorpus.RData")
#    load("myCorpus.RData")

# Tidy Up - remove each corpus and each sample leaving only the one large combined 'myCorpus'
rm(blogSample); rm(twitterSample); rm(newsSample); rm(blogTokens); rm(twitterTokens); rm(newsTokens)
#----------------------------------------------------------------------------------------------

# Create Document Feature Matrix for each N-gram
set.seed(1000)
# Create uniGram DFM
my_dfm <- dfm(myCorpus, what = "word", verbose = FALSE, # display messages
              removeNumbers = TRUE, # remove numbers
              removePunct = TRUE, # remove punctuation
              removeTwitter = TRUE) # remove Twitter symbols
# Look at dimensions
dim(my_dfm)  # 241249 136390

# Sort my_dfm
my_dfm <- sort(my_dfm)

# Features(words) in the unigram
my_dfmFeats <- features(my_dfm) 
head(my_dfmFeats, 10) 

# Document frequencies of my_dfm
my_dfmDocFreq <- docfreq(my_dfm)
head(my_dfmDocFreq, 10) 

# Get Total Words for unigram
totalWords <- ntoken(my_dfm) # Total features/words
length(totalWords) # 241249
# Get Total unique words
#    numberVocab <- ntype(my_dfm) # Number of unique features/words - Vocab

# Top features for unigram
unigramTopFeats <- topfeatures(my_dfm, 25) # use for visualization
#------------------------------------------------------------------------------
# Create uniGram DFM w/ STOP-words Removed
stop_dfm <- dfm(myCorpus, what = "fastestword", verbose = FALSE, # display messages
                removeNumbers = TRUE, # remove numbers
                removePunct = TRUE, # remove punctuation
                removeTwitter = TRUE, # remove Twitter symbols
                ignoredFeatures = stopwords("english")) # remove stop words

# Look at the dimensions of the stop_dfm
dim(stop_dfm)  # 241249 142667

# Sort stop_dfm
stop_dfm <- sort(stop_dfm)

# Look at the features (words) in the unigram for stop words
stop_dfmFeats <- features(stop_dfm)
head(stop_dfmFeats, 10)

# Look at the document frequencies of my_dfm
stop_dfmDocFreq <- docfreq(stop_dfm)
head(stop_dfmDocFreq, 10)

# Top features for stop_dfm
unigramStopTopFeats <- topfeatures(stop_dfm, 25) # use for visualization
#------------------------------------------------------------------------------
# Create biGram
bigram <- tokenize(myCorpus, what = "fastestword",
                   removeNumbers= TRUE,
                   removePunct = TRUE,
                   removeTwitter = TRUE,
                   removeHyphens = TRUE,
                   ngrams = 2, # bigram
                   concatenator = " ")

# Create biGram DFM
bi_dfm <- dfm(bigram, verbose = FALSE)

# Look at the dimensions of the bi_dfm
dim(bi_dfm)  # 241249 1546702

# Sort bi_dfm
bi_dfm <- sort(bi_dfm)
Look at bi_dfm
head(bi_dfm, 10) # Document-feature matrix of: 241,249 documents, 1,545,063 features

# Trim bi_dfm - features habving less than 10 occurances
bi_dfm <- trim(bi_dfm, minCount = 10) # Removing features occurring fewer than 10 times: 1491207

# Look at the bi_dfm again
head(bi_dfm, 10) # Document-feature matrix of: 241,249 documents, 53,856 features

# Look at the features (words) in the bigram
bi_dfmFeats <- features(bi_dfm)
head(bi_dfmFeats, 10)

# Look at the document frequencies of bi_dfm
bi_dfmDocFreq <- docfreq(bi_dfm)
head(bi_dfmDocFreq, 10)

# Get Token and Type for bigrams
biToken <- ntoken(bi_dfm)
biTokenLen <- length(biToken)
biType <- ntype(bi_dfm) # how many unique bi-grams

# Top features for bi_dfm
bigramTopFeats <- topfeatures(bi_dfm, 25) # for use in visualization
#------------------------------------------------------------------------------
# Create triGram
trigram <- tokenize(myCorpus, what="word", 
                    removeNumbers = TRUE, 
                    removePunct = TRUE,
                    # stem = TRUE, 
                    removeTwitter = TRUE, 
                    removeHyphens = TRUE,
                    ngrams=3, # trigram
                    concatenator = " ")

# Create triGram 'DFM'
tri_dfm<-dfm(trigram, verbose = FALSE)

# Look at dimensions of tri_dfm
dim(tri_dfm)  # 241249 3339503

# Sort tri_dfm
tri_dfm <- sort(tri_dfm)

# Look at tri_dfm
head(tri_dfm, 10) # 241,249 documents, 3,348,931 features

# Trim tri_dfm - features habving less than 10 occurances
tri_dfm <- trim(tri_dfm, minCount = 10) 
# Look at the bi_dfm again
head(tri_dfm, 10) # 241,249 documents, 24,700 features

# Look at the features (words) in the trigram
tri_dfmFeats <- features(tri_dfm)
head(tri_dfmFeats, 10)

# Look at the document frequencies of tri_dfm
tri_dfmDocFreq <- docfreq(tri_dfm)
head(tri_dfmDocFreq, 10)

# Get Token and Type for Trigrams
triToken <- ntoken(tri_dfm)
triTokenLen <- length(triToken)
triType <- ntype(tri_dfm) # how many unique tri-grams

# Top features for tri_dfm
trigramTopFeats <- topfeatures(tri_dfm, 25) # for use in visualization
#------------------------------------------------------------------------------
# Create quadGram
quadgram <- tokenize(myCorpus, what="word", 
                     removeNumbers = TRUE, 
                     removePunct = TRUE,
                     # stem = TRUE, 
                     removeTwitter = TRUE, 
                     removeHyphens = TRUE,
                     ngrams = 4, # quadgram
                     concatenator = " ")

# Create quadGram 'DFM'
quad_dfm<-dfm(quadgram, verbose = FALSE)

# Look at dimensions of the quad_dfm
dim(quad_dfm)  # 241249 4073025

# Sort quad_dfm
quad_dfm <- sort(quad_dfm)
# Look at quad_dfm
head(quad_dfm, 10) # 241,249 documents, 4,073,025 features

# Trim quad_dfm - features habving less than 10 occurances
quad_dfm <- trim(quad_dfm, minCount = 10)  
# Look at the quad_dfm again
head(quad_dfm, 10) # 241,249 documents, 4,186 features

# Look at the features (words) in the quadgram
quad_dfmFeats <- features(quad_dfm)
head(quad_dfmFeats, 10)

# Look at the document frequencies of quad_dfm
quad_dfmDocFreq <- docfreq(quad_dfm)
head(quad_dfmDocFreq, 10)

# Get Token and Type for Quadgrams
quadTokenLen <- length(quad_dfm)
quadToken <- ntoken(quad_dfm)
quadType <- ntype(quad_dfm) # how many unique quad-grams

quadgramTopfeats <- topfeatures(quad_dfm, 25) # for use in visualizations
#------------------------------------------------------------------------------   

# Visualization
# Function for plots
plotTopNWordsHist <- function(txtDFM, topNWords, fillColor, title, xlabel, ylabel) {
     topNWords <- topfeatures(txtDFM, n=topNWords) 
     topNWordsDf <- data.frame(Words=names(topNWords), 
                               Frequency=topNWords) 
     histPlot <- ggplot(topNWordsDf, aes(x=reorder(Words, Frequency), 
                                         y=Frequency)) + 
          geom_bar(stat="Identity", fill=fillColor) + 
          coord_flip() + xlab(xlabel) + ylab(ylabel) + ggtitle(title)
     histPlot
}

# Plot Unigram
uniPlot <- plotTopNWordsHist(my_dfm, 25, "green", "Top 25 Words", "Words", "Frequency")

# Plot Stop Words - Unigram
stopPlot <- plotTopNWordsHist(stop_dfm, 25, "maroon", "Top 25 stop Words", "Words", "Frequency")

# Plot Bigrams
biPlot <- plotTopNWordsHist(bi_dfm, 25, "green", "Top 25 bigrams", "Words", "Frequency")

# Plot Trigrams
triPlot <- plotTopNWordsHist(tri_dfm, 25, "purple", "Top 25 trigrams", "Words", "Frequency")

# Plot Quadgrams
quadPlot <- plotTopNWordsHist(quad_dfm, 25, "purple", "Top 25 quadgrams", "Words", "Frequency")

# WORD CLOUD - Get top 150 words to create a word cloud for unigram
wordcloud(names(unigramTopFeats ), unigramTopFeats , random.order = FALSE, max.words = 100, colors = brewer.pal(8, "Dark2"))

# WORD CLOUD - Get top 150 words to create a word cloud for unigram w/ stop words removed
wordcloud(names(unigramStopTopFeats), unigramStopTopFeats , random.order = FALSE, max.words = 150, colors = brewer.pal(8, "Dark2"))

# WORD CLOUD - Get top 150 words to create a word cloud for bigram
wordcloud(names(bigramTopFeats), bigramTopFeats, random.order = FALSE, max.words = 100, colors = brewer.pal(8, "Accent"))
#------------------------------------------------------------------------------ 
###  Removing stop words will have a large impact for predicting the real 'next' word. Stopwords will remain. 
#------------------------------------------------------------------------------

# Convert to Dataframe to facilitate the prediction of next word from the N-grams files created.
set.seed(1000)
# Convert my_dfm to a data frame  
ngrams1 <- data.frame(Content = features(my_dfm), Frequency = colSums(my_dfm), row.names = NULL, stringsAsFactors = FALSE)
# Look at ngrams1 data
head(ngrams1)
tail(ngrams1)
# Compare size
object.size(ngrams1) # 9371264 bytes
object.size(my_dfm) # 75041672 bytes

vocab <- ngrams1
vocab$Frequency <- NULL # Drop Frequency column
head(vocab)
#mydict <- dictionary(vocab, concatenator = " ", toLower = TRUE) # create dictionary using quanteda - later
# Save vocab file
save(vocab, file = "nextApp/vocab.RData")

# Convert bi_dfm to data frame
ngrams2 <- data.frame(Content = features(bi_dfm), Frequency = colSums(bi_dfm), row.names = NULL, stringsAsFactors = FALSE)
# Look at ngrams2
head(ngrams2)
tail(ngrams2)
# Compare Sizes
object.size(ngrams2) # 3805736 bytes
object.size(bi_dfm) # 49334592 bytes

# Convert tri_dfm to data frame
ngrams3 <- data.frame(Content = features(tri_dfm), Frequency = colSums(tri_dfm), row.names = NULL, stringsAsFactors = FALSE)
# Look at ngrams3
head(ngrams3)
tail(ngrams3)
# Compare Sizes
object.size(ngrams3) # 1813712 bytes
object.size(tri_dfm) # 24628848 bytes

# Convert quad_dfm to data frame
ngrams4 <- data.frame(Content = features(quad_dfm), Frequency = colSums(quad_dfm), row.names = NULL, stringsAsFactors = FALSE)
# Look at ngrams4
head(ngrams4)
tail(ngrams4)
# Compare Sizes
object.size(ngrams4) # 340680 bytes
object.size(quad_dfm) # 16723720 bytes

# Tidy up - remove large datasets and unused items
rm(my_dfm); rm(my_dfmDocFreq); rm(my_dfmFeats)
rm(stop_dfm); rm(stop_dfmDocFreq); rm(stop_dfmFeats)
rm(bi_dfm); rm(bi_dfmDocFreq); rm(bi_dfmFeats); rm(bigramTopFeats)
rm(tri_dfm); rm(tri_dfmDocFreq); rm(tri_dfmFeats); rm(trigramTopFeats)
rm(quad_dfm); rm(quad_dfmDocFreq); rm(quad_dfmFeats)

#------------------------------------------------------------------------------

# Create ngrams directory
dir.create("ngrams", showWarnings = FALSE)

# Save ngrams1 - unigrams
save(ngrams1, file = "ngrams/ngrams1.RData")
# Save ngrams2 - Bigrams
save(ngrams2, file = "ngrams/ngrams2.RData")
#Save ngrams3 - Trigrams
save(ngrams3, file = "ngrams/ngrams3.RData")
# Save ngrams4 - Quadgrams
save(ngrams4, file = "ngrams/ngrams4.RData")

# Recovery point
# load(file = "ngrams/ngrams1.RData")
# load(file = "ngrams/ngrams2.RData")
# load(file = "ngrams/ngrams3.RData")
# load(file = "ngrams/ngrams4.RData")

#------------------------------------------------------------------------------

# Convert dataframes to data.table for speed. Setting the encoding of the entire data table.
ngrams1 <- data.table(ngrams1)
for (name in colnames(ngrams1)){
     Encoding(colnames(ngrams1)) <- "UTF-8"
}

ngrams2 <- data.table(ngrams2)
for (name in colnames(ngrams2)){
     Encoding(colnames(ngrams2)) <- "UTF-8"
}

ngrams3 <- data.table(ngrams3)
for (name in colnames(ngrams3)){
     Encoding(colnames(ngrams3)) <- "UTF-8"
}

ngrams4 <- data.table(ngrams4)
for (name in colnames(ngrams4)){
     Encoding(colnames(ngrams4)) <- "UTF-8"
}

#------------------------------------------------------------------------------

# The frequency of each n-gram up to 4-grams has been determined. The tables consist of 2 columns; Context and Frequency. In order to predict the *next word* we need to split the *Context* column into individual columns for each word. We will also retain the frequency for each n-gram. 

ng1 <- ngrams1
# Set column names for ng1
colnames(ng1) <- c("w1", "uniFreq")

# Split the bigram, trigram, and quadgram data frames - for calculating probability
biSplit <- data.table(stri_split_fixed(ngrams2$Content, " ", simplify = TRUE))
biFreq <- ngrams2$Frequency
ng2 <- cbind(biSplit, biFreq)
colnames(ng2) <- c("w1", "w2", "biFreq")

triSplit <- data.table(stri_split_fixed(ngrams3$Content, " ", simplify = TRUE))
triFreq <- ngrams3$Frequency
ng3 <- cbind(triSplit, triFreq)
colnames(ng3) <- c("w1", "w2", "w3", "triFreq")

quadSplit <- data.table(stri_split_fixed(ngrams4$Content, " ", simplify = TRUE))
quadFreq <- ngrams4$Frequency
ng4 <- cbind(quadSplit, quadFreq)
colnames(ng4) <- c("w1", "w2", "w3", "w4", "quadFreq")

# tidy up
rm(ngrams1); rm(ngrams2); rm(ngrams3); rm(ngrams4)
rm(biSplit); rm(triSplit); rm(quadSplit)
#------------------------------------------------------------------------------

# Create ngrams directory
dir.create("split", showWarnings = FALSE)

save(ng1, file = "split/ng1.RData")
save(ng2, file = "split/ng2.RData")
save(ng3, file = "split/ng3.RData")
save(ng4, file = "split/ng4.RData")

# Recovery point
# load(file = "split/ng1.RData")
# load(file = "split/ng2.RData")
# load(file = "split/ng3.RData")
# load(file = "split/ng4.RData")
#------------------------------------------------------------------------------

#Join the data tables on the column of "w1" (1st column). 
# Save unigrams in descending order
ng.1 <- ng1
save(ng.1, file = "nextApp/ng.1.RData")

# Join tables on common column "w1" - Inner Join.
setkey(ng1, w1) # set key for ng1 dataset
setkey(ng2, w1) # set key for ng2 dataset
setkey(ng3, w1) # set key for ng3 dataset
setkey(ng4, w1) # set key for ng4 dataset

# Join ng1 and ng2 on 'w1'
suppressWarnings(ng.2 <-ng1[ng2, on = "w1", nomatch = 0])
# Reorder columns
setcolorder(ng.2, c("w1", "w2", "biFreq", "uniFreq"))
# Compute maximum likelihood estimates "likelihood" and add it to the data.table as a new column
ng.2[ , likelihood := ng.2$biFreq / ng.2$uniFreq] # MLE for biGram
# order dataset by biFreq in descending order
ng.2 <- ng.2[order(-biFreq),]

# Join ng3 and ng2 on 'w1' and 'w2
suppressWarnings(ng.3 <- ng3[ng2, on = c("w1", "w2"), nomatch = 0])
# Compute maximum likelihood estimates "likelihood" and add it to the data.table as a new column
ng.3[ , likelihood := ng.3$triFreq / ng.3$biFreq] # MLE for triGram
# order dataset by triFreq in descending order
ng.3 <- ng.3[order(-triFreq),]

# Join ng4 and ng1 on 'w1', 'w2', and 'w3'
suppressWarnings(ng.4 <- ng4[ng3, on = c("w1", "w2", "w3"), nomatch = 0])
# Compute maximum likelihood estimates "likelihood" and add it to the data.table as a new column
ng.4[ , likelihood := ng.4$quadFreq / ng.4$triFreq] # MLE for quadGram
# order dataset by triFreq in descending order
ng.4 <- ng.4[order(-quadFreq),]

#  Save Joined ngrams - will be used in the Shiny App
save(ng.2, file = "nextApp/ng.2.RData")
save(ng.3, file = "nextApp/ng.3.RData")
save(ng.4, file = "nextApp/ng.4.RData")
