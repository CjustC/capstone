# ui.R

suppressWarnings(suppressMessages(library(shiny)))

shinyUI(fluidPage(
      
     titlePanel("Predict Next Word"),
     # Horizontal Line
     tags$hr(),
     h4("Instructions"),
     h5("Enter a few words then click the 'Submit' button. The predicted Next word will display along with a table displaying the top five suggestions for the next word. The table will display the Frequency of the unigram or the Maximum Estimation Likelihood for other ngrams. The 'ngram' being used will display below the table."),
     br(),
     
     sidebarLayout(
          sidebarPanel(
               textInput("userInput", label = "Enter a few words then click the Submit button"),
               
               #helpText("Two words minimum", style = "color:red"),
               
               submitButton("Submit")
               ),
          
          mainPanel(
               h5("You entered the following text:"),
               div(textOutput("userOut"), style = "color:green"),
               br(),
               h3("Suggested Next Word:"),
               div(textOutput("guessNext"), style = "color:blue"),
               br(),
               h4("The top five suggestions"),
               tableOutput("viewTable"),
               h4("Ngram used:"),
               div(textOutput("ngram"), style = "color:red")
               
               )
     )
))

