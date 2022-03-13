# Shiny app for choosing and interacting with chatbots 

#### Initialize ####
library(shiny)
library(dplyr)
library(lubridate)
library(DT)
library(tidyr)
library(shinydashboard)
library(shinythemes)
library(odbc)
library(shinyFeedback)
library(data.table) 

#### Define UI  ####
ui <- shinyUI(fluidPage(theme = shinytheme("spacelab"), 
  
  # App title ----
  titlePanel("Let's talk : Playing around with chatbots"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      selectInput(inputId = "chatbot",
                  label = "Chatbot Selector",
                  choices = c("",
                              "Leafey" = "Leafey"
                  ),
                  selected = "Leafey",
                  multiple = F
      ),
      
      #TODO: adding a "select user image" option
      
      # Adding div tag to the sidebar with git link           
      tags$div(class="header", checked=NA,
               #tags$p("Raw code located on Git"),
               tags$a(href="https://github.com/leesahanders/Chatbot", "Raw code located on Git, check it out!")
      ),
      
      width=3
      
    ),
    
    mainPanel(
      
      # Buttons for starting and clearing the chat
      fluidRow(
        column(3, div( style = "margin-top: 20px;", actionButton(inputId = "Submit", label = "Start chatting"))),
        
        column(3, div( style = "margin-top: 20px;", actionButton(inputId = "Clear", label = "Clear/reset chat"))))
    )
  ),
  
  # Show the image of our chosen chatbot, auto scaled to fit
  tags$head(tags$style(
    type="text/css",
    "#chatbotImg img {max-width: 100%; width: 100%; height: auto}"
  )),
  
  sidebarPanel(
    
    #Display chosen chatbot name and image 
    textOutput("chatbotName"),
    
    imageOutput("chatbotImg"),
    
    width = 2),
  
  mainPanel(
    # Show outputs and gif while "thinking"
    # verbatimTextOutput("chatText"),
    
    # Show chat message inputs a la texting : TODO: only have this show after user has selected a chatbot
    fluidRow(
      column(11, textInput("chatInput", label = " ", width = "100%")),
      
      column(1, div( style = "margin-top: 20px;", actionButton(inputId = "Send", label = "Send")))
    ),
    
    tableOutput("chat"),
    
    tags$head(tags$style(type="text/css", "
                             #loadmessage {
                               position: fixed;
                               top: 0px;
                               left: 0px;
                               width: 100%;
                               padding: 5px 0px 5px 0px;
                               text-align: center;
                               font-weight: bold;
                               font-size: 100%;
                               color: #000000;
                               background-color: #CCFF66;
                               z-index: 105;
                             }
                          ")),
    
    conditionalPanel(
      condition="($('html').hasClass('shiny-busy'))",
      tags$img(src=busy)
    )
    
  )
)
)

#### Define server logic  ####
server <- function(input, output) {
  
  #### TODO: Error checking and provide feedback to user ####
  # chatbot_ready_warning <- reactive({
  #   valid <- input$chatbot %in% c("notleafey") #c("Leafey")
  #   shinyFeedback::feedbackWarning("n", !valid, "Please select a chatbot ready to talk with")
  #   
  #   # even <- input$n %% 2 == 0
  #   # shinyFeedback::feedbackWarning("n", !even, "Please select an even number")
  #   # input$n / 2    
  # })
  
  #output$half <- renderText(half())
  
  #### Initialize with blanks ####
  output$chatText <- renderText({ "Please select a chatbot to start chatting" })
  #output$chatInputText <- renderText({ input$chatInput })
  #output$chatAllText <- renderText({ input$chatInput })
  #output$chatAllText <- renderText({ NULL })
  
  # Display initial chatbot image as the default question mark to prompt users to select a chatbot 
  output$chatbotImg <- renderImage({
    filename <- normalizePath(file.path('./files', paste('default','.PNG', sep='')))
    list(src = filename, alt = paste("Chatbot Profile Image"))
  }, deleteFile = FALSE)

  output$chatbotName <- renderText({ "Select a Chatbot from the list above" })
  
  #### Keep track of the chat conversation ####
  # chat <- reactiveValues(dfnew=data.frame(matrix(ncol = 2, nrow = 0)) ,count=1)
  chat <- reactiveValues(dfnew=data.table(Question = as.character(), Response = as.character()),count=1)
  
  # When user asks a question capture that as a reactive variable
  chat_user <- reactive({
    data.frame(
      Question = input$chatInput,
      Response = ""
    )
  })
  
  # When user hits submit add users question to chat log
  storedvalues <- observeEvent(input$Send, {
    if(nchar(input$chatInput) > 0)  {
      chat$dfnew <- rbind(chat$dfnew, chat_user())
      chat$count = chat$count + 1
    } else {
    }
  })
  
  # When user hits clear/reset clear the log
  # storedvalues <- observeEvent(input$Clear, {
  #   if(nchar(input$chatInput) > 0)  {
  #     chat$dfnew <- rbind(chat$dfnew, chat_user())
  #     chat$count = chat$count + 1
  #   } else {
  #   }
  # })
  
  # When Leafey replies add Leafey's response : TODO
  
  # Pass chat log to output for user to see 
  output$chat <- renderTable({
    chat$dfnew
  })
  
  # #### User submits a message to the chatbot : TODO: Only make this possible after user has selected a chatbot ####
  # observeEvent(input$Send,{
  #   
  #   #output$chatText <- renderText({ input$chatInput })
  #   #output$chatAllText <- renderText({ input$chatInput })
  #   
  # })
  
  #### User selects clear chat ####
  observeEvent(input$Clear,{
    
    # Initialize with blanks 
    output$chatText <- renderText({ "Please select a chatbot to start chatting" })
    
    # Display initial chatbot image as the default question mark to prompt users to select a chatbot 
    output$chatbotImg <- renderImage({
      filename <- normalizePath(file.path('./files', paste('default','.PNG', sep='')))
      list(src = filename, alt = paste("Chatbot Profile Image"))
    }, deleteFile = FALSE)
    
    output$chatbotName <- renderText({ "Select a Chatbot from the list above" })
    
    # Clear the chat log
    chat$dfnew <- data.table(Question = as.character(), Response = as.character())
    
  })
  
  #### User selects a chatbot to talk with. Update the images. Load chat functions for selected chatbot. Display initial "hello". # TODO: hide chat window until this button is clicked ####
  observeEvent(input$Submit,{
    
    # Load chatbot Leafey
    if(input$chatbot == "Leafey") {
      source("chatbot_leafey.R")
      
      output$chatText <- renderText({ "Leafey: Hello!" })
    }
    
    # Display updated chatbot image based on who user selected
    output$chatbotImg <- renderImage({
      filename <- normalizePath(file.path('./files', paste(input$chatbot,'.PNG', sep='')))
      list(src = filename, alt = paste("Chatbot Profile Image"))
    }, deleteFile = FALSE)
    
    # Update flavor text for chosen Chatbot
    output$chatbotName <- renderText({ paste("You have chosen: ", input$chatbot ) })
    
  })
  
}

#### Create Shiny app ####
shinyApp(ui = ui, server = server)

#### References ####
#https://stackoverflow.com/questions/65365805/how-to-align-button-next-to-text-input
#https://stackoverflow.com/questions/56608214/how-can-i-keep-input-track-log-in-shiny-then-print-it-and-save-it

#### Archived code snippets ####

# #Connect server code snippet
# #Check operating system, if linux log in as root user for being able to initialize authentication request to databases.
# #Use the Connect UI to create system variables with username and password for appropriate account
# #Credit to: https://conjugateprior.org/2015/06/identifying-the-os-from-r/
# get_os <- function(){
#   sysinf <- Sys.info()
#   if (!is.null(sysinf)){
#     os <- sysinf['sysname']
#     if (os == 'Darwin')
#       os <- "osx"
#   } else { ## mystery machine
#     os <- .Platform$OS.type
#     if (grepl("^darwin", R.version$os))
#       os <- "osx"
#     if (grepl("linux-gnu", R.version$os))
#       os <- "linux"
#   }
#   tolower(os)
# }
# 
# #For development: set locally your system variables for username and password
# #Sys.setenv(username = "my_username")
# #Sys.setenv(password = "my_password")
# 
# # #Check if system is on linux, if so put in the supplied username and password to sudo the account for successful database authentication
# if(get_os() == "linux") {
#   message("Linux OS, putting in developer credentials")
#   
#   #Get developer info using environmental variables entered using the UI
#   username <- Sys.getenv("username")
#   password <- Sys.getenv("password")
#   inputs = c(password)
#   command = paste0('su - ', username)
#   system(command, input = inputs)
# }


# Holding onto this for later if upgrade to database storage   

# Inside the UI: 
#DTOutput("table"),

#Start out with a blank table as a placeholder
# output$table <- renderDT(
#   NULL,
#   class = "display nowrap compact",
#   filter = "top") 

# } else if(input$chatbot == "Test2") {
#   df <- data.frame("Test" = c("Test2"), "Directory" = c(getwd()))

# } else if(input$chatbot == "DataBaseExample") {
#   query <- paste0("--USE METAWH
#                   USE PLACEHOLDER
#                   SELECT * 
#                   FROM PLACEHOLDER
#                   LIMIT 1000
#                     ")
#   
#   con <- DBI::dbConnect(
#     odbc::odbc(),
#     Driver = "",
#     Server = "",
#     Database = "",
#     Warehouse = "",
#     #dbname = 'dbname',
#     #host = 'host',
#     port = 443,
#     UID = '',
#     #UID          = rstudioapi::askForPassword("Database user"),
#     #PWD          = rstudioapi::askForPassword("Database password"),
#     PWD = "",
#     authenticator = "externalbrowser"
#     #sslmode = 'require'
#     #Trusted_Connection = "yes"
#   )
#   
#   df = dbGetQuery(con, query)
#   dbDisconnect(con)
# 
# } else {
#   df <- data.frame(Par = c("NULL"), Par2 = c("Make a valid selection"))
# }

# # Prep dataframe for display 
# Results <- df
# 
# output$table <- renderDT(
#   Results,
#   class = "display nowrap compact", 
#   filter = "top" 
# )
# 
# df <- reactive(Results)

