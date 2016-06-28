# Full version on Digital Ocean
# Loading necessary packages
library(shiny)
library(shinydashboard)
library(stringi)
library(stringr)
library(qdap)
library(data.table)
library(stringdist)
library(formatR)
# Loading data

load("data/hybriddt_clean.R")

# Main prediction function
PredictNext <- function(y, l, b = 3, r = 1){ 
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
                y <- gsub("\\s+", " ", y)
                y <- str_trim(y, side = c("left"))
                y <- tolower(y)
                lookupgram(y, l)
        }
        clean_trim_start <- function(y){
                y <- gsub("’", "'", y)
                y <- gsub("[^A-Za-z0-9' ]", "", y)
                y <- gsub("\\s+", " ", y)
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
                        noquote("Continue typing!")
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
                                noquote(m[1:b])
                        }
                        else if ((length(m) > 0) & !(length(m) > b)){
                                noquote(m)
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
                noquote("Start a new sentence")
        }
        else{
                sentbreak <- sent_detect_mod(y)
                y <- sentbreak[length(sentbreak)]
                lasenmar <- c(".", "?", "!")
                lasen <- strsplit(y, "")[[1]]
                if (lasen[length(lasen)] %in% lasenmar){
                        noquote("Start a new sentence")
                }
                else if (stri_count_words(y) < 5){
                        clean_start(y)
                }
                else {
                        clean_trim_start(y)
                }
                
        }
}




ui <- dashboardPage(
        
        skin = "black",
        dashboardHeader(
                title = "SillyBot text auto-completion app (version X3.1-Full). By Hassan Shallal", 
                titleWidth = 700
        ),
        dashboardSidebar(
                
                sidebarMenu(
                        selectInput("NoOfPred", label = "Maximum number of suggestions",
                                    c(1, 2, 3, 4, 5), selected = 3
                        ),
                        selectInput("NoOfWordsPred", label = "Maximum number of words per suggestion",
                                    c(1, 2, 3, 4, 5), selected = 1
                        )
                )
        ),
        dashboardBody(
                # tags$script('$(document).on("keydown", function (e) {Shiny.onInputChange("hotkey", e.which);});'), 
                tags$body(tags$style(HTML('.skin-black .main-sidebar {background-color: #FFB6C1;}'))),
                tabBox(
                        title = "SillyBot modes and FAQ",
                        # The id lets us use input$tabset1 on the server to find the current tab
                        id = "modesofoperation", width = "800px",
                        tabPanel("Reactive (On the fly)",
                                 fluidRow(
                                         column(width = 12,
                                                box(
                                                        # verbatimTextOutput("results"),
                                                        
                                                        status = "info", solidHeader = TRUE,
                                                        collapsible = FALSE,
                                                        width = 12, 
                                                        title = "SillyBot wants to help you write something!", 
                                                        tags$textarea(id = "text_input1", rows =  2, cols = 101, ""),
                                                        fluidRow(
                                                                column(width = 12, offset = 3, 
                                                                       conditionalPanel(
                                                                               condition = "output.noofsuggestions == 1",
                                                                               actionButton("addword1", label = textOutput("addword1_label"), style = "background-color: #FFB6C1")
                                                                       ),
                                                                       conditionalPanel(
                                                                               condition = "output.noofsuggestions == 2",
                                                                               actionButton("addword12", label = textOutput("addword12_label"), style = "background-color: #FFB6C1"),
                                                                               actionButton("addword2", label = textOutput("addword2_label"), style = "background-color: #FFB6C1")
                                                                       ),
                                                                       conditionalPanel(
                                                                               condition = "output.noofsuggestions == 3",
                                                                               actionButton("addword13", label = textOutput("addword13_label"), style = "background-color: #FFB6C1"),
                                                                               actionButton("addword23", label = textOutput("addword23_label"), style = "background-color: #FFB6C1"),
                                                                               actionButton("addword3", label = textOutput("addword3_label"), style = "background-color: #FFB6C1")
                                                                       ),
                                                                       conditionalPanel(
                                                                               condition = "output.noofsuggestions == 4",
                                                                               actionButton("addword14", label = textOutput("addword14_label"), style = "background-color: #FFB6C1"),
                                                                               actionButton("addword24", label = textOutput("addword24_label"), style = "background-color: #FFB6C1"),
                                                                               actionButton("addword34", label = textOutput("addword34_label"), style = "background-color: #FFB6C1"),
                                                                               actionButton("addword4", label = textOutput("addword4_label"), style = "background-color: #FFB6C1")
                                                                       ),
                                                                       conditionalPanel(
                                                                               condition = "output.noofsuggestions == 5",
                                                                               actionButton("addword15", label = textOutput("addword15_label"), style = "background-color: #FFB6C1"),
                                                                               actionButton("addword25", label = textOutput("addword25_label"), style = "background-color: #FFB6C1"),
                                                                               actionButton("addword35", label = textOutput("addword35_label"), style = "background-color: #FFB6C1"),
                                                                               actionButton("addword45", label = textOutput("addword45_label"), style = "background-color: #FFB6C1"),
                                                                               actionButton("addword5", label = textOutput("addword5_label"), style = "background-color: #FFB6C1")
                                                                       )
                                                                )
                                                        )
                                                        
                                                )
                                         )
                                 ),
                                 
                                 
                                 
                                 fluidRow(
                                         column(width = 12,
                                                box(
                                                        status = "info", solidHeader = TRUE,
                                                        collapsible = TRUE,
                                                        width = 12, 
                                                        title = "The number of suggestions SillyBot likes to present to you:", 
                                                        h3(verbatimTextOutput("noofsuggestions")))
                                         )
                                 )
                        ),
                        tabPanel("Static (Action button)", 
                                 fluidRow(column(width = 12,
                                                 box(
                                                         status = "info", solidHeader = TRUE,
                                                         collapsible = FALSE,
                                                         width = 12,
                                                         title = "SillyBot wants to help you write something!", 
                                                         tags$textarea(id = "text_input2", rows =  2, cols = 101, ""),
                                                         actionButton("action", "SillyBot, what do you think the current and/or the next word(s)? "),
                                                         fluidRow(
                                                                 column(width = 12, offset = 3, 
                                                                        conditionalPanel(
                                                                                condition = "output.noofsuggestions2 == 1",
                                                                                actionButton("addword11", label = textOutput("addword11_label"), style = "background-color: #FFB6C1")
                                                                        ),
                                                                        conditionalPanel(
                                                                                condition = "output.noofsuggestions2 == 2",
                                                                                actionButton("addword112", label = textOutput("addword112_label"), style = "background-color: #FFB6C1"),
                                                                                actionButton("addword22", label = textOutput("addword22_label"), style = "background-color: #FFB6C1")
                                                                        ),
                                                                        conditionalPanel(
                                                                                condition = "output.noofsuggestions2 == 3",
                                                                                actionButton("addword113", label = textOutput("addword113_label"), style = "background-color: #FFB6C1"),
                                                                                actionButton("addword223", label = textOutput("addword223_label"), style = "background-color: #FFB6C1"),
                                                                                actionButton("addword33", label = textOutput("addword33_label"), style = "background-color: #FFB6C1")
                                                                        ),
                                                                        conditionalPanel(
                                                                                condition = "output.noofsuggestions2 == 4",
                                                                                actionButton("addword114", label = textOutput("addword114_label"), style = "background-color: #FFB6C1"),
                                                                                actionButton("addword224", label = textOutput("addword224_label"), style = "background-color: #FFB6C1"),
                                                                                actionButton("addword334", label = textOutput("addword334_label"), style = "background-color: #FFB6C1"),
                                                                                actionButton("addword44", label = textOutput("addword44_label"), style = "background-color: #FFB6C1")
                                                                        ),
                                                                        conditionalPanel(
                                                                                condition = "output.noofsuggestions2 == 5",
                                                                                actionButton("addword115", label = textOutput("addword115_label"), style = "background-color: #FFB6C1"),
                                                                                actionButton("addword225", label = textOutput("addword225_label"), style = "background-color: #FFB6C1"),
                                                                                actionButton("addword335", label = textOutput("addword335_label"), style = "background-color: #FFB6C1"),
                                                                                actionButton("addword445", label = textOutput("addword445_label"), style = "background-color: #FFB6C1"),
                                                                                actionButton("addword55", label = textOutput("addword55_label"), style = "background-color: #FFB6C1")
                                                                        )
                                                                 )
                                                         )
                                                 )
                                 )
                                 ),
                                 fluidRow(column(width = 12,
                                                 box(
                                                         status = "info", solidHeader = TRUE,
                                                         collapsible = TRUE,
                                                         width = 12, 
                                                         title = "The number of suggestions SillyBot likes to present to you:", 
                                                         h3(verbatimTextOutput("noofsuggestions2"))
                                                 )
                                 )
                                 
                                 )
                        ),
                        tabPanel("Frequently asked questions",
                                 fluidRow(
                                         column(width = 10, offset=1,
                                                h4('How can I get started with the SillyBot ?', style = 'background-color: #FFB6C1'),
                                                h4('I recommend you start with the ', tags$b('reactive'), 'mode by clicking on the relevant tab. Then, you can type some text in the text input area within the box labelled: ', tags$pre('SillyBot wants to help you write something'), 'Once you start typing a few words, the SillyBot will pick it up and will present some suggestions that you may find relevant or interesting.', tags$i('You can select to add any suggestion to your text by clicking on which suggestion you prefer'), ', and Bam, the SilyBot will add the suggestion you selected to your text.'),
                                                hr(),
                                                h4("What are the differences between the Reactive and the Static mode?", style = "background-color: #FFB6C1"),
                                                h4("There are two major differences between the reactive and the static modes:"),
                                                h4(tags$b('First difference'), ': the reactive mode presents and updates suggestions while you are typing on the fly whereas the static mode presents and updates susggestions only when you click on the action button labelled:', tags$pre('SillyBot, what do you think the current and/or the next word(s)?.')),
                                                h4(tags$b('Second difference'), ': the reactive mode allows you to click on any suggestion and it will automatically add this suggestion to your input so that you can save time during typing whereas the static mode does not allow you to add any suggestion to your text.'),
                                                hr(),
                                                h4('What do you mean by', tags$b('Maximum number of suggestions'), '?', style = "background-color: #FFB6C1"),
                                                h4('This is a feature that allows the user to determine the maximum number of suggestions he/she would like the SillyBot to present. It is, however, important to mention that the SillyBot', tags$b('may'),   'present fewer suggestions and', tags$b('may never'), 'present more suggestions than the maximum number chosen by the user. The same scenario applies for the feature of maximum number of words per suggestion.'),
                                                hr(),
                                                h4("Which word the SillyBot suggests; the current word I am typing OR the next word that I haven't typed yet?", style = "background-color: #FFB6C1"),
                                                h4("This is a good question. The SillyBot, in either reactive or static mode, suggests the current word you are typing and/or the next word you haven't typed yet, pretty much similar to the benchmark products available in the market such as SwiftKey and QuickType."),
                                                hr(),
                                                h4('What does the message', tags$b('Continue typing!'), 'mean?', style = "background-color: #FFB6C1"),
                                                h4("SillyBot issues a non-clickable message saying 'Continue typing!' in case it doesn't know what to predict next OR in case there is a spelling typo in the user's input"),
                                                hr(),
                                                h4("Does the SillyBot correct spelling errors?", style = "background-color: #FFB6C1"),
                                                h4("As far as version X3.1 goes, nope! The SillyBot reacts to spelling errors by issuing a non-clickable message saying 'Continue typing!'", style = "background-color: #FFFFFF"),
                                                hr(),
                                                h4("Does the SillyBot correct grammatical errors?", style = "background-color: #FFB6C1"),
                                                h4("As far as version X3.1 goes, nope! The SillyBot neither corrects nor prevents grammatical errors"),
                                                hr(),
                                                h4("What are the differences between the Full and Lite version?", style = "background-color: #FFB6C1"),
                                                h4("There are a few differences between the Full and the Lite versions:"),
                                                h4(tags$b('Location:'), tags$a(href = 'http://192.241.247.166:3838/SillyRobotWriterX/','The full version'), 'is hosted on Digital ocean utilizing a 2GB Ram droplet.', tags$a(href = 'https://hassanshallal.shinyapps.io/SillyBotShiny/', 'The lite version'), 'is hosted on Shinyapp.io'),
                                                h4("The full version relies on a 140.6 MB n-gram language model (n = 1:5). The lite version relies on a 40.8 MB n-gram language model (n = 1:3)."),
                                                h4(tags$b('Features:'), 'The full version can offer a maximum of 5 words per suggestion. The lite version can offer a maximum of 3 words per suggestion.'),
                                                h4(tags$b('Responsiveness:'), 'The full version seems to respond to the user input faster than does the lite version. No quantitative data is available to support or rebut this claim though.'),
                                                hr(),
                                                h4("Where is the code of this app?", style = "background-color: #FFB6C1"),
                                                h4("The code of this app is exclusively written using the R language and is not available for publication in the time being due to several reasons among which the most relevant one is the senstivie nature of this app as a Data Science Specilaization capstone project on Coursera. In case you need more information, please contact the developer at: hshallal@icloud.com"), 
                                                hr(),
                                                h4("Which libraries this app relies on?", style = "background-color: #FFB6C1"),
                                                h4("The development, testing, and deployment of the SillyBot app has been based on utilizing the technologies provided by several packages, mainly R base, stringi, stringr, stringdist, qdap, data.table, TSTr, hash, shiny, shinydashboard, microbenchmark, pryr, and parallel"),
                                                hr(),
                                                h4("Why did you call the app the SillyBot?", style = "background-color: #FFB6C1"),
                                                h4("Once again, this is a nice question. When you use the SillyBot, you will see by yourself that it is indeed far from being silly most of the time. However, the app is sometimes very oblivious to the context or the topic the user is writing about. So, I'd recommend you expect a few silly suggestions every once and a while.")
                                         )
                                 )
                        )
                )
        )
)


server <- function(input, output, session) {
        b <- eventReactive(input$NoOfPred, {as.numeric(input$NoOfPred)})
        r <- eventReactive(input$NoOfWordsPred, {as.numeric(input$NoOfWordsPred)})
        
        data1 <- eventReactive(input$text_input1, length({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}))
        output$noofsuggestions <- renderText(data1())
        
        data12 <- eventReactive(input$text_input1, {(PredictNext((input$text_input1), hybriddt_clean, b(), r()))})
        data13 <- eventReactive(input$text_input1, {(gsub("\\s*\\w*$", "", input$text_input1))})
        data14 <- eventReactive(input$text_input1, {nchar(PredictNext((input$text_input1), hybriddt_clean, b(), r()))})
        h3(output$addword1_label <- renderPrint(cat({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}[1])))
        # output$results = renderPrint({input$hotkey})
        observe({
                if (input$addword1 == 0) return()
                # if (input$hotkey > 112 | input$hotkey < 112) return()
                isolate({
                        if (data12()[1] == "Start a new sentence" | data12()[1] == "Continue typing!") {}
                        else if((stringdist(tail(strsplit(input$text_input1, split = " ")[[1]], 1), data12()[1], method = "osa", useBytes = TRUE)) < data14()[1] & substr(tail(strsplit(input$text_input1, split = " ")[[1]], 1), 1, 1) == substr(data12()[1], 1, 1)){
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(data13()[1], data12()[1], "", sep = " ")))
                                
                        }
                        else {
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(input$text_input1, data12()[1], "", sep = " ")))
                        }
                })
        })
        h3(output$addword12_label <- renderPrint(cat({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}[1])))
        observe({
                if (input$addword12 == 0) return()
                # if (input$hotkey > 112 | input$hotkey < 112) return()
                isolate({
                        if((stringdist(tail(strsplit(input$text_input1, split = " ")[[1]], 1), data12()[1], method = "osa", useBytes = TRUE)) < data14()[1] & substr(tail(strsplit(input$text_input1, split = " ")[[1]], 1), 1, 1) == substr(data12()[1], 1, 1)){
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(data13()[1], data12()[1], "", sep = " ")))
                                
                        }
                        else {
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(input$text_input1, data12()[1], "", sep = " ")))
                        }
                })
        })
        h3(output$addword13_label <- renderPrint(cat({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}[1])))
        observe({
                if (input$addword13 == 0) return()
                # if (input$hotkey > 112 | input$hotkey < 112) return()
                isolate({
                        if((stringdist(tail(strsplit(input$text_input1, split = " ")[[1]], 1), data12()[1], method = "osa", useBytes = TRUE)) < data14()[1] & substr(tail(strsplit(input$text_input1, split = " ")[[1]], 1), 1, 1) == substr(data12()[1], 1, 1)){
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(data13()[1], data12()[1], "", sep = " ")))
                                
                        }
                        else {
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(input$text_input1, data12()[1], "", sep = " ")))
                        }
                })
        })
        h3(output$addword14_label <- renderPrint(cat({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}[1])))
        observe({
                if (input$addword14 == 0) return()
                # if (input$hotkey > 112 | input$hotkey < 112) return()
                isolate({
                        if((stringdist(tail(strsplit(input$text_input1, split = " ")[[1]], 1), data12()[1], method = "osa", useBytes = TRUE)) < data14()[1] & substr(tail(strsplit(input$text_input1, split = " ")[[1]], 1), 1, 1) == substr(data12()[1], 1, 1)){
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(data13()[1], data12()[1], "", sep = " ")))
                                
                        }
                        else {
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(input$text_input1, data12()[1], "", sep = " ")))
                        }
                })
        })
        h3(output$addword15_label <- renderPrint(cat({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}[1])))
        observe({
                if (input$addword15 == 0) return()
                # if (input$hotkey > 112 | input$hotkey < 112) return()
                isolate({
                        if((stringdist(tail(strsplit(input$text_input1, split = " ")[[1]], 1), data12()[1], method = "osa", useBytes = TRUE)) < data14()[1] & substr(tail(strsplit(input$text_input1, split = " ")[[1]], 1), 1, 1) == substr(data12()[1], 1, 1)){
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(data13()[1], data12()[1], "", sep = " ")))
                                
                        }
                        else {
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(input$text_input1, data12()[1], "", sep = " ")))
                        }
                })
        })
        h3(output$addword2_label <- renderPrint(cat({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}[2])))
        observe({
                if (input$addword2 == 0) return()
                # if (input$hotkey > 113 | input$hotkey < 113) return()
                isolate({
                        if((stringdist(tail(strsplit(input$text_input1, split = " ")[[1]], 1), data12()[2], method = "osa", useBytes = TRUE)) < data14()[2] & substr(tail(strsplit(input$text_input1, split = " ")[[1]], 1), 1, 1) == substr(data12()[2], 1, 1)){
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(data13()[1], data12()[2], "", sep = " ")))
                                
                        }
                        else {
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(input$text_input1, data12()[2], "", sep = " ")))   
                        }
                })
        })
        h3(output$addword23_label <- renderPrint(cat({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}[2])))
        observe({
                if (input$addword23 == 0) return()
                # if (input$hotkey > 113 | input$hotkey < 113) return()
                isolate({
                        if((stringdist(tail(strsplit(input$text_input1, split = " ")[[1]], 1), data12()[2], method = "osa", useBytes = TRUE)) < data14()[2] & substr(tail(strsplit(input$text_input1, split = " ")[[1]], 1), 1, 1) == substr(data12()[2], 1, 1)){
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(data13()[1], data12()[2], "", sep = " ")))
                                
                        }
                        else {
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(input$text_input1, data12()[2], "", sep = " ")))     
                        }
                })
        })
        h3(output$addword24_label <- renderPrint(cat({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}[2])))
        observe({
                if (input$addword24 == 0) return()
                # if (input$hotkey > 113 | input$hotkey < 113) return()
                isolate({
                        if((stringdist(tail(strsplit(input$text_input1, split = " ")[[1]], 1), data12()[2], method = "osa", useBytes = TRUE)) < data14()[2] & substr(tail(strsplit(input$text_input1, split = " ")[[1]], 1), 1, 1) == substr(data12()[2], 1, 1)){
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(data13()[1], data12()[2], "", sep = " ")))
                                
                        }
                        else {
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(input$text_input1, data12()[2], "", sep = " "))) 
                        }
                })
        })
        h3(output$addword25_label <- renderPrint(cat({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}[2])))
        observe({
                if (input$addword25 == 0) return()
                # if (input$hotkey > 113 | input$hotkey < 113) return()
                isolate({
                        if((stringdist(tail(strsplit(input$text_input1, split = " ")[[1]], 1), data12()[2], method = "osa", useBytes = TRUE)) < data14()[2] & substr(tail(strsplit(input$text_input1, split = " ")[[1]], 1), 1, 1) == substr(data12()[2], 1, 1)){
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(data13()[1], data12()[2], "", sep = " ")))
                                
                        }
                        else {
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(input$text_input1, data12()[2], "", sep = " ")))        
                        }
                })
        })
        h3(output$addword3_label <- renderPrint(cat({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}[3])))
        observe({
                if (input$addword3 == 0) return()
                # if (input$hotkey > 114 | input$hotkey < 114) return()
                isolate({
                        if((stringdist(tail(strsplit(input$text_input1, split = " ")[[1]], 1), data12()[3], method = "osa", useBytes = TRUE)) < data14()[3] & substr(tail(strsplit(input$text_input1, split = " ")[[1]], 1), 1, 1) == substr(data12()[3], 1, 1)){
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(data13()[1], data12()[3], "", sep = " ")))
                                
                        }
                        else {
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(input$text_input1, data12()[3], "", sep = " ")))
                        }
                })
        })
        h3(output$addword34_label <- renderPrint(cat({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}[3])))
        observe({
                if (input$addword34 == 0) return()
                # if (input$hotkey > 114 | input$hotkey < 114) return()
                isolate({
                        if((stringdist(tail(strsplit(input$text_input1, split = " ")[[1]], 1), data12()[3], method = "osa", useBytes = TRUE)) < data14()[3] & substr(tail(strsplit(input$text_input1, split = " ")[[1]], 1), 1, 1) == substr(data12()[3], 1, 1)){
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(data13()[1], data12()[3], "", sep = " ")))
                                
                        }
                        else {
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(input$text_input1, data12()[3], "", sep = " ")))
                        }
                })
        })
        h3(output$addword35_label <- renderPrint(cat({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}[3])))
        observe({
                if (input$addword35 == 0) return()
                # if (input$hotkey > 114 | input$hotkey < 114) return()
                isolate({
                        if((stringdist(tail(strsplit(input$text_input1, split = " ")[[1]], 1), data12()[3], method = "osa", useBytes = TRUE)) < data14()[3] & substr(tail(strsplit(input$text_input1, split = " ")[[1]], 1), 1, 1) == substr(data12()[3], 1, 1)){
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(data13()[1], data12()[3], "", sep = " ")))
                                
                        }
                        else {
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(input$text_input1, data12()[3], "", sep = " ")))
                        }
                })
        })
        h3(output$addword4_label <- renderPrint(cat({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}[4])))
        observe({
                if (input$addword4 == 0) return()
                # if (input$hotkey > 115 | input$hotkey < 115) return()
                isolate({
                        if((stringdist(tail(strsplit(input$text_input1, split = " ")[[1]], 1), data12()[4], method = "osa", useBytes = TRUE)) < data14()[4] & substr(tail(strsplit(input$text_input1, split = " ")[[1]], 1), 1, 1) == substr(data12()[4], 1, 1)){
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(data13()[1], data12()[4], "", sep = " ")))
                                
                        }
                        else {
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(input$text_input1, data12()[4], "", sep = " ")))
                        }
                })
        })
        h3(output$addword45_label <- renderPrint(cat({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}[4])))
        observe({
                if (input$addword45 == 0) return()
                # if (input$hotkey> 115 | input$hotkey < 115) return()
                isolate({
                        if((stringdist(tail(strsplit(input$text_input1, split = " ")[[1]], 1), data12()[4], method = "osa", useBytes = TRUE)) < data14()[4] & substr(tail(strsplit(input$text_input1, split = " ")[[1]], 1), 1, 1) == substr(data12()[4], 1, 1)){
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(data13()[1], data12()[4], "", sep = " ")))
                                
                        }
                        else {
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(input$text_input1, data12()[4], "", sep = " ")))
                        }
                })
        })
        h3(output$addword5_label <- renderPrint(cat({(PredictNext((input$text_input1), hybriddt_clean, b(), r()))}[5])))
        observe({
                if (input$addword5 == 0) return()
                # if (input$hotkey > 116 | input$hotkey < 116) return()
                isolate({
                        if((stringdist(tail(strsplit(input$text_input1, split = " ")[[1]], 1), data12()[5], method = "osa", useBytes = TRUE)) < data14()[5] & substr(tail(strsplit(input$text_input1, split = " ")[[1]], 1), 1, 1) == substr(data12()[5], 1, 1)){
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(data13()[1], data12()[5], "", sep = " ")))
                                
                        }
                        else {
                                updateTextInput(session, "text_input1",
                                                value = multigsub(c("\\s+", " n't"), c(" ", "n't"), paste(input$text_input1, data12()[5], "", sep = " ")))
                        }
                })
        })
        
        data2 <- eventReactive(input$action, {(PredictNext((input$text_input2), hybriddt_clean, b(), r()))})
        h3(output$addword11_label <- renderPrint(cat(data2()[1])))
        h3(output$addword112_label <- renderPrint(cat(data2()[1])))
        h3(output$addword113_label <- renderPrint(cat(data2()[1])))
        h3(output$addword114_label <- renderPrint(cat(data2()[1])))
        h3(output$addword115_label <- renderPrint(cat(data2()[1])))
        h3(output$addword22_label <- renderPrint(cat(data2()[2])))
        h3(output$addword223_label <- renderPrint(cat(data2()[2])))
        h3(output$addword224_label <- renderPrint(cat(data2()[2])))
        h3(output$addword225_label <- renderPrint(cat(data2()[2])))
        h3(output$addword33_label <- renderPrint(cat(data2()[3])))
        h3(output$addword334_label <- renderPrint(cat(data2()[3])))
        h3(output$addword335_label <- renderPrint(cat(data2()[3])))
        h3(output$addword44_label <- renderPrint(cat(data2()[4])))
        h3(output$addword445_label <- renderPrint(cat(data2()[4])))
        h3(output$addword55_label <- renderPrint(cat(data2()[5])))
        
        data3 <- eventReactive(input$action, length({(PredictNext((input$text_input2), hybriddt_clean, b(), r()))}))
        output$noofsuggestions2 <- renderText(data3())
}


# Run the application 
shinyApp(ui = ui, server = server)
