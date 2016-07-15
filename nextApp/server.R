# server.R

# input value = "userInput"
# output values = "userOut", "guessNext"
# output Table = "viewTable"

shinyServer(
     function(input, output) {
          # Validate there is an entry
#          validate(
#               need(input$userInput != "", "Please enter a few kind words")
#          )
          
          # Next Word that will be returned to the input "guessNext"
          output$userOut <- renderText({
               userText_output <- input$userInput
               return(userText_output)
               })
     
          output$guessNext <- renderText({
               userIn <- input$userInput
               guessNext_output <- "The predicted next word will display here."
                    # Clean words
               clean_lastWords <- cleanData(userIn)
                    # Split words
               inputCleanSplit <- splitData(clean_lastWords)
                    
                    # Get length of last words that have cleaned and split
               inputWordCount <- length(inputCleanSplit)
          
          # Match 'inputCleanSplit' with correct ngram to determine next word
               if(inputWordCount == 0){ # match with unigram
                    guessNext_output <- ng1Func(inputCleanSplit)
               }
               
               if(inputWordCount == 1){ # match with bigram
                    guessNext_output <- ng2Func(inputCleanSplit)
               }
                    
               if(inputWordCount == 2){ # match with trigram
                    guessNext_output <- ng3Func(inputCleanSplit)
               }
                    
               if(inputWordCount == 3){ # match with quadgram
                    guessNext_output <- ng4Func(inputCleanSplit)
                    }
                    
               if(inputWordCount > 3){ # match with bigram
                    guessNext_output <- ngMoreFunc(inputCleanSplit)
               }
               
               return(guessNext_output)

          })
          
          output$viewTable <- renderTable({
               userIn <- input$userInput
               # Clean words
               clean_lastWords <- cleanData(userIn)
               # Split words
               inputCleanSplit <- splitData(clean_lastWords)
               # Get length of last words that have cleaned and split
               inputWordCount <- length(inputCleanSplit)
               
               if(inputWordCount == 0){ # match with unigram
                    guessNext_output <- ng1Func(inputCleanSplit)
               }
          
               if(inputWordCount == 1){ # match with trigram
                    guessNext_output <- ng2Func(inputCleanSplit)
               }
               if(inputWordCount == 2){ # match with trigram
                    guessNext_output <- ng3Func(inputCleanSplit)
               }
               if(inputWordCount == 3){ # match with quadgram
                    guessNext_output <- ng4Func(inputCleanSplit)
               }
               if(inputWordCount > 3){ # match with quadgram
                    guessNext_output <- ngMoreFunc(inputCleanSplit)
               }
               
               if(exists("alternates", where = -1)){
                    alternates
               }else{
                    XNgramsTable <- data.frame(Word=NA, Likelihood=NA)
               }
          })
          
          output$ngram <- renderText({
               userIn <- input$userInput
               # Clean words
               clean_lastWords <- cleanData(userIn)
               # Split words
               inputCleanSplit <- splitData(clean_lastWords)
               # Get length of last words that have cleaned and split
               inputWordCount <- length(inputCleanSplit)
               
               if(inputWordCount == 0){ # match with unigram
                    ngramOut_output <- ngramOut
               }
               
               if(inputWordCount == 1){ # match with trigram
                    ngramOut_output <- ngramOut
               }
               if(inputWordCount == 2){ # match with trigram
                    ngramOut_output <- ngramOut
               }
               if(inputWordCount == 3){ # match with quadgram
                    ngramOut_output <- ngramOut
               }
               if(inputWordCount > 3){ # match with quadgram
                    ngramOut_output <- ngramOut
               }
               return(ngramOut_output)
          })
          
          
})
