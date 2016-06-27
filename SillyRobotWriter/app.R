#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringi)
library(stringr)
library(qdap)
library(data.table)


load("data/sent_transition.R")
load("data/hybridvecdt.R")

PredictNext <- function(y, l, b){ 
  qe <- names(sent_transition)[1:b]
  lookupgram <- function(y, l){
    if (nchar(y) == 1){
      lsub <- l[let1 == y]
    }
    else if (nchar(y) == 2){
      lsub <- l[let12 == y]
    } 
    else if (nchar(y) == 3){
      lsub <- l[let123 == y]
    }
    else if (nchar(y) == 4){
      lsub <- l[let1234 == y]
    }
    else if (nchar(y) == 5){
      lsub <- l[let12345 == y]
    }
    else if (nchar(y) == 6){
      lsub <- l[let123456 == y]
    }
    else if (nchar(y) == 7){
      lsub <- l[let1234567 == y]
    }
    else if (nchar(y) == 8){
      lsub <- l[let12345678 == y]
    }
    else if (nchar(y) == 9){
      lsub <- l[let123456789 == y]
    }
    else {
      lsub <- l[let12345678910 == substring(y, 1, 10)]
    }
    lsub <- as.data.frame(lsub)
    z <- lsub[grep(paste("^", y, sep = ""), lsub$string, useBytes = TRUE), ]
    if (nrow(z) > 0){
      z <- z[order(z$freq, decreasing = TRUE),]
      z <- z[1:b, 2]
      z <- z[!is.na(z) & z!= y]
      m <- as.character()
      for (n in z){
        nw <- setdiff(strsplit(n, " ")[[1]], strsplit(y, " ")[[1]])
        if (length(nw) > 0){
          m <- c(m, nw[1])
        }
        else {
          nm <- strsplit(n, paste(y, " ", sep = ""), fixed = TRUE)[[1]]
          m <- c(m, nm[nm != ""])
        }
      }
      m <- unique(m[!is.na(m)])
      if (length(m) > 0){
        unname(m)
      }
      else { 
        if(stri_count_words(y) == 4){
          y <- word(y, -3, -1)
          lookupgram(y, l)
        }
        else if(stri_count_words(y) == 3){
          y <- word(y, -2, -1)
          lookupgram(y, l)
        }
        else if(stri_count_words(y) == 2){
          y <- word(y, -1)
          lookupgram(y, l)
        }
        else{
          "Continue typing!"
        }
      }
    }
    else{
      if(stri_count_words(y) == 4){
        y <- word(y, -3, -1)
        lookupgram(y, l)
      }
      else if(stri_count_words(y) == 3){
        y <- word(y, -2, -1)
        lookupgram(y, l)
      }
      else if(stri_count_words(y) == 2){
        y <- word(y, -1)
        lookupgram(y, l)
      }
      else{
        "Continue typing!"
      }
    }
  }
  if (nchar(y) == 1){
    y <- paste(y, " ", sep = "")
  }
  if ((nchar(y) > 0) & (stri_count_words(y) < 5)){
    lasenmar <- c(".", "?", "!")
    lasen <- strsplit(y, "")[[1]]
    if (lasen[length(lasen)] %in% lasenmar){
      qe
    }
    else if (lasen[length(lasen)-1] %in% lasenmar & lasen[length(lasen)] == " " ){
      qe
    }
    else {
      y <- sub("\\s+$", " ", y)
      y <- tolower(y)
      y <- multigsub(c(":", ",", "-", "!", "_", "(", ")", ".", "?", ";", "~", "=", "+", "*", "&", "^", "%", "$", "#", "@", "{", "}", "[", "]", "|"), c(""), y) 
      y <- qprep(y)
      lookupgram(y, l)
    }
  }
  else if ((nchar(y) > 0) & !(stri_count_words(y) < 5)) {
    lasenmar <- c(".", "?", "!")
    lasen <- strsplit(y, "")[[1]]
    if (lasen[length(lasen)] %in% lasenmar){
      qe
    }
    else if (lasen[length(lasen)-1] %in% lasenmar & lasen[length(lasen)] == " "){
      qe
    }
    else {
      y <- sub("\\s+$", " ", y)
      y <- word(y, -4, -1)
      y <- tolower(y)
      y <- multigsub(c(":", ",", "-", "!", "_", "(", ")", ".", "?", ";", "~", "=", "+", "*", "&", "^", "%", "$", "#", "@", "{", "}", "[", "]", "|"), c(""), y)
      y <- qprep(y)
      lookupgram(y, l)
    }
  }
  else {
    qe
  }
}
# Define UI for application that draws a histogram
ui <- fluidPage(
        # Application title
        titlePanel("SillyRobotWriter"),
        
        sidebarLayout(sidebarPanel(
                textInput("text_input", "SillyRobot wants to help you write something!", "go ahead, try the SillyRobot!", "800px")
                # actionButton("action", "Predict my next word")
        ), 
        mainPanel(
                # h3(verbatimTextOutput("extract_next_word_en_US")),
                # h3(verbatimTextOutput("extract_next_word_BYU")),
                h3(verbatimTextOutput("extract_next_word_en_US"))
        )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        # data1 <- eventReactive(input$action, {(PredictNext((input$text_input), en_US_uni_bi_trigrams, 4))})
        # h3(output$extract_next_word_en_US <- renderPrint({data1()}))
        # data2 <- eventReactive(input$action, {(PredictNext((input$text_input), byu_grams_list, 4))})
        # h3(output$extract_next_word_BYU <- renderPrint({data2()}))
        # data3 <- eventReactive(input$action, {(PredictNext((input$text_input), hybrid[1:3], 4))})
        h3(output$extract_next_word_en_US <- renderPrint({(PredictNext((input$text_input), hybridvecdt, 4))}))
}


# Run the application 
shinyApp(ui = ui, server = server)
