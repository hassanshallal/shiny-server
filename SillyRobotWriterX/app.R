# Loading necessary packages
library(shiny)
library(shinydashboard)
library(stringi)
library(stringr)
library(data.table)

# Loading data
load("data/sent_transition.R")
load("data/hybriddt_clean.R")

# Main prediction function
PredictNext <- function(y, l, b = 3, r = 1){ 
        qe <- names(sent_transition)[1:b]
        sent_detect_mod <- function(y, endmarks = c("?", ".", "!", "|")){
                splitpoint <- paste0("[", paste("\\", endmarks, sep = "", collapse = ""), "]")
                text.var <- as.character(y)
                text.var <- paste(y, collapse = " ")
                splits <- strsplit(y, sprintf("(?<=%s)", splitpoint), perl = TRUE)
                out <- unlist(splits)
                out
        }  # Adapted from qdap sent_detect
        clean_start <- function(y){
                y <- gsub("’", "'", y)
                y <- gsub("[^A-Za-z0-9' ]", "", y)
                y <- gsub("\\s+$", " ", y)
                y <- str_trim(y, side = c("left"))
                y <- tolower(y)
                lookupgram(y, l)
        }
        clean_trim_start <- function(y){
                y <- gsub("’", "'", y)
                y <- gsub("[^A-Za-z0-9' ]", "", y)
                y <- gsub("\\s+$", " ", y)
                y <- str_trim(y, side = c("left"))
                y <- tolower(y)
                y <- word(y, -4, -1) # One line of code difference from clean_start function
                lookupgram(y, l)
        } # One line of code difference from clean_start function
        backoff <- function(y){
                if(stri_count_words(y) > 2){
                        y <- word(y, -(stri_count_words(y) - 1), -1)
                        lookupgram(y, l)
                }
                else if(stri_count_words(y) == 2){
                        y <- word(y, -1)
                        lookupgram(y, l)
                }
                else {
                        noquote("")
                }
        } # Called by lookupgram if no match is found
        lookupgram <- function(y, l){
                if (nchar(y) == 1){
                        lsub <- l[l1 == y]
                }
                else if (nchar(y) == 2){
                        lsub <- l[l2 == y]
                } 
                else if (nchar(y) == 3){
                        lsub <- l[l3 == y]
                }
                else if (nchar(y) == 4){
                        lsub <- l[l4 == y]
                }
                else if (nchar(y) == 5){
                        lsub <- l[l5 == y]
                }
                else if (nchar(y) == 6){
                        lsub <- l[l6 == y]
                }
                else if (nchar(y) == 7){
                        lsub <- l[l7 == y]
                }
                else if (nchar(y) == 8){
                        lsub <- l[l8 == y]
                }
                else if (nchar(y) == 9){
                        lsub <- l[l9 == y]
                }
                else {
                        lsub <- l[l10 == substring(y, 1, 10)]
                }
                lsub <- as.data.frame(lsub)
                z <- lsub[grep(paste("^", y, sep = ""), lsub$string, useBytes = TRUE), ]
                if (nrow(z) > 0){
                        z <- z[order(z$freq, decreasing = TRUE),]
                        z <- z[1:(b+1), 2]
                        z <- z[!is.na(z) & z!= y]
                        m <- as.character()
                        for (n in z){
                                nw <- setdiff(strsplit(n, " ")[[1]], strsplit(y, " ")[[1]])
                                if (length(nw) > r){
                                        nw <- nw[1:r]
                                        if (length(nw) > 1){
                                                ph <- paste(nw[1:r], collapse = " ")
                                                m <- c(m, ph)
                                        }
                                        else {
                                                m <- c(m, nw[1])
                                        }
                                }
                                else if (length(nw) > 0 & !(length(nw) > r)){
                                        if (length(nw) > 1){
                                                ph <- paste(nw[1:length(nw)], collapse = " ")
                                                m <- c(m, ph)
                                        }
                                        else {
                                                m <- c(m, nw[1])
                                        }
                                        
                                }
                                else {
                                        nm <- strsplit(n, paste(y, " ", sep = ""), fixed = TRUE)[[1]]
                                        nm <- word(nm, -1)
                                        m <- c(m, nm[nm != ""])
                                }
                        }
                        m <- unique(m[!is.na(m)])
                        if ((length(m) > 0) & !(length(m) < b)){
                                format(m[1:b], justify = "centre")
                        }
                        else if ((length(m) > 0) & !(length(m) > b)){
                                format(m, justify = "centre")
                        }
                        else {
                                backoff(y)
                        }
                }
                else {
                        backoff(y)
                }
        } # Major lookup function dependent on the binary search of data.tables
        if (nchar(y) == 0){
                noquote("Start a new sentence please")
        }
        else{
                sentbreak <- sent_detect_mod(y)
                y <- sentbreak[length(sentbreak)]
                lasenmar <- c(".", "?", "!")
                lasen <- strsplit(y, "")[[1]]
                if (lasen[length(lasen)] %in% lasenmar){
                        noquote("Start a new sentence please")
                }
                else if (stri_count_words(y) < 5){
                        clean_start(y)
                }
                else if (!(stri_count_words(y) < 5)) {
                        clean_trim_start(y)
                }
                else {
                        qe
                }
        }
}

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = "SillyRobotWriter!"
  ),
  dashboardSidebar(
    sidebarMenu(
      selectInput("NoOfPred", "Maximum number of suggestions",
                  c(1, 2, 3, 4, 5), selected = 3
      ),
      selectInput("NoOfWordsPred", "Maximum number of words per suggestion",
                  c(1, 2, 3, 4), selected = 1
      )
    )
  ),
  dashboardBody(
    tabBox(
      title = "SillyRobot modes",
      id = "modesofoperation", width = "800px",
      tabPanel("Reactive", fluidRow(
        column(width = 12,
               box(
                 status = "info", solidHeader = TRUE,
                 collapsible = FALSE,
                 width = 12,
                 title = "SillyRobot wants to help you write something!", 
                 textInput("text_input1", "Go ahead, try the SillyRobot...", "Start a new sentence please.", "800px")
               ),
               box(
                 background = "purple",
                 collapsible = FALSE,
                 width = 12,
                 title = "SillyRobot suggestions, on the fly", 
                 h3(verbatimTextOutput("hybriddt_clean1")))
        )
      )
      ),
      tabPanel("Action button", fluidRow(
        column(width = 12,
               box(
                 status = "info", solidHeader = TRUE,
                 collapsible = FALSE,
                 width = 12,
                 title = "SillyRobot wants to help you write something!", 
                 textInput("text_input2", "Go ahead, try the SillyRobot...", "Start a new sentence please.", "800px"),
                 actionButton("action", "Predict my next word")
               ),
               box(
                 background = "purple",
                 collapsible = FALSE,
                 width = 12,
                 title = "SillyRobot suggestions", 
                 h3(verbatimTextOutput("hybriddt_clean2")))
               )
        )
        )
      )
    )
  )
                                                  
                                                
server <- function(input, output) {
  b <- eventReactive(input$NoOfPred, {as.numeric(input$NoOfPred)})
  r <- eventReactive(input$NoOfWordsPred, {as.numeric(input$NoOfWordsPred)})
  
  h3(output$hybriddt_clean1 <- renderPrint({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}))
  
  data1 <- eventReactive(input$action, {(PredictNext((input$text_input2), hybriddt_clean, b(), r()))})
  h3(output$hybriddt_clean2 <- renderPrint({data1()}))
}


# Run the application 
shinyApp(ui = ui, server = server)
