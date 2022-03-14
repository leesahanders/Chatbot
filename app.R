# Shiny app for choosing and interacting with chatbots 
# Lisa Anders w/ help from code linked in resources at bottom of app

#TODO: Spread throughout app below as placeholders as well as here 
#TODO: Add prompt actions based on timer (IE Leafey will prompt user every 15 min if they've been rained on or if they need water)

#Accessed from: https://leesahanders.shinyapps.io/Chatbot/ 

#### Initialize ####
library(shiny)
library(dplyr)
library(lubridate)
library(DT)
library(tidyr)
library(shinydashboard)
library(shinythemes)
#library(odbc)
library(shinyFeedback)
library(data.table) 
library(knitr)
library(kableExtra)
library(formattable)

valid_chatbots <- c("Leafey")
chatbot_text <- NULL

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
                  #selected = "", # For Dev, testing error cases
                  selected = "Leafey",
                  multiple = F
      ),
      
      #TODO: adding a "select user image" option
      
      # Adding div tag to the sidebar with git link           
      tags$div(class="header", checked=NA,
               #tags$p("Raw code located on Git"),
               tags$a(href="https://github.com/leesahanders/Chatbot", "Raw code located on Git, check it out by clicking here")
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
  
  fluidRow(
  
  # Show the image of our chosen chatbot, auto scaled to fit
  tags$head(tags$style(
    type="text/css",
    "#chatbotImg img {max-width: 100%; width: 100%; height: auto}"
  )),
  
  sidebarPanel(
    
    #Display chosen chatbot name and image 
    imageOutput("chatbotImg"),
    
    textOutput("chatbotName"),

    position = "left", width = 2),
  
  mainPanel(
    
    # Show chat message inputs a la texting 
    # TODO: only have this show after user has selected a chatbot
    fluidRow(
      column(9, textInput("chatInput", label = " ", width = "100%")),
      
      column(1, div( style = "margin-top: 20px;", actionButton(inputId = "Send", label = "Send"))) 
      #TODO: Map user pressing enter to Send button : https://stackoverflow.com/questions/32335951/using-enter-key-with-action-button-in-r-shiny 
    ),
    
    # Show chat log
    htmlOutput("chatHTML"),
    
    # Show gif while "thinking"
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
      
      tags$img(src="busy_leafey.gif")
      
      # busy = "files/busy_leafey.gif",
      # tags$img(src="files/busy_leafey.gif")
      
      # busy = "busy_leafey.gif",
      # tags$img(src="www/busy_leafey.gif")
      
      # busy = "../www/busy_leafey.gif",
      # busy = "\\www\\busy_leafey.gif",
      # busy = "www/busy_leafey.gif",
      # busy = "busy_leafey.gif",
      # busy = "busy_leafey.gif",
      # tags$img(src=busy)
      # tags$img(src=busy)
      
    )
    
  ),
  
  # Show the image of our chosen user, auto scaled to fit
  tags$head(tags$style(
    type="text/css",
    "#userImg img {max-width: 100%; width: 100%; height: auto}"
  )),
  
  sidebarPanel(
    
    imageOutput("userImg"),
    
    tags$p("User panel placeholder : Upcoming"),
    
    # TODO: Display chosen user name and image 
    #textOutput("userName"),
    
    position = "right", width = 2),
  
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
  
  # Display initial chatbot image as the default question mark to prompt users to select a chatbot 
  output$chatbotImg <- renderImage({
    filename <- normalizePath(file.path('./files', paste('default','.PNG', sep='')))
    list(src = filename, alt = paste("Chatbot Profile Image"))
  }, deleteFile = FALSE)
  output$chatbotName <- renderText({ "Select a Chatbot from the list above" })
  
  # Display initial user image as the default question mark
  output$userImg <- renderImage({
    filename <- normalizePath(file.path('./files', paste('default','.PNG', sep='')))
    list(src = filename, alt = paste("User Profile Image"))
  }, deleteFile = FALSE)
  output$userName <- renderText({ "Select a User Image from the list above" })
  
  # Initialize a timer for time based chatbot prompts 
  timer <- Sys.time()
  
  #### Keep log of the chat conversation ####
  chat <- reactiveValues(dfnew=data.table(User = as.character(), Chatbot = as.character(), Who = as.character(), Message = as.character()),count=1)
  
  # When user asks a question capture that as a reactive variable
  chat_user <- reactive({
    data.frame(
      User = paste("You:", input$chatInput),
      Chatbot = "",
      Who = "User",
      Message = paste("You:", input$chatInput)
    )
  })
  
  # When user hits Send
  storedvalues <- observeEvent(input$Send, {
    
    # Add users question to chat log
    if(nchar(input$chatInput) > 0)  {
      chat$dfnew <- rbind(chat$dfnew, chat_user())
      chat$count = chat$count + 1
    } else {
    }
    
    # Talk to appropriate chatbot for a response, check in valid list of chatbots 
    if(nchar(input$chatInput) > 0 && input$chatbot %in% c(valid_chatbots))  {
      
      message = paste0("Leafey: ", chatbot(input$chatInput))
      
      # Let's wait a little bit so users can see the loading gif. Making it random so it feels addicting!
      
      Sys.sleep(runif(1, 0.1, 2))
      
      chatbot_df <-  data.frame(
        User = "",
        Chatbot = message,
        Who = "Chatbot",
        Message = message
      )

      chat$dfnew <- rbind(chat$dfnew, chatbot_df)
      chat$count = chat$count + 1

    } else {
      message = "Admin: Oops! You need to select a chatbot first. "
      chatbot_df <-  data.frame(
        User = "",
        Chatbot = message,
        Who = "Admin",
        Message = message
      )

      chat$dfnew <- rbind(chat$dfnew, chatbot_df)
      chat$count = chat$count + 1
    }
    
  })
  
  # Output: Pass chat log to output for user to see 
  # TODO: Update to use one column (Message) with left and right align and overlay of "thought bubbles"
  output$chatHTML <- renderText({
    if(nrow(chat$dfnew)>0){
      chat$dfnew %>%
        mutate(User = cell_spec(User, background = ifelse(Who == "User", "lightblue", "white"))) %>%
        mutate(Chatbot = cell_spec(Chatbot, background = ifelse(Who == "Chatbot", col, #TODO set up coloring by Chatbot (Leafey is green)
                                                                ifelse(Who == "User", "white", 
                                                                       "lightpink")))) %>%
        select(Chatbot, User) %>%
        kable(escape = F,
            col.names = NULL, longtable = T,
            booktabs = T, align = c("lr")
            ) %>%
        column_spec(column = 1, width_min = "3in", width_max = "3in") %>%
        column_spec(column = 2, width_min = "3in", width_max = "3in") %>%
        kable_styling(
          font_size = 15,
          bootstrap_options = c("responsive"),
          full_width = T, position = "center"
        )
    } else {
      # Addition for case without a first row causing error. This is essentially just a dummy table so it won't print an error. 
      kable(chat$dfnew %>%
              select(Chatbot, User),
            col.names = NULL, booktabs = T, align = c("lr")) %>%
        kable_styling(
          font_size = 15,
          bootstrap_options = c("responsive"),
          full_width = T, position = "center"
        )
    }
  })
  
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
    chat$dfnew <- data.table(User = as.character(), Chatbot = as.character(), Who = as.character(), Message = as.character())
    
    chatbot_text <- NULL
    
  })
  
  #### User selects a chatbot to talk with. Update the images. Load chat functions for selected chatbot. Display initial "hello". # TODO: hide chat window until this button is clicked ####
  observeEvent(input$Submit,{
    
    # Load chatbot Leafey and initialize chat log
    if(input$chatbot == "Leafey") {
      source("chatbot_leafey.R")
      chat$dfnew <- data.table(User = c(""), Chatbot = c("Leafey: Hello! I'm Leafey. What do you want to talk about? I'm good at telling jokes."), Who = c("Chatbot"), Message = c("Hello! I'm Leafey. What do you want to talk about? I'm good at telling jokes."))
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
#https://shiny.rstudio.com/articles/notifications.html
#https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html
#https://clarewest.github.io/blog/post/making-tables-shiny/
#https://community.rstudio.com/t/shiny-contest-submission-table-editor-shiny-app/23600
#https://bookdown.org/yihui/rmarkdown-cookbook/kable.html
#https://stackoverflow.com/questions/62139431/how-can-i-make-the-first-col-aligned-to-left-and-the-rest-aligned-to-center-with
#https://stackoverflow.com/questions/32335951/using-enter-key-with-action-button-in-r-shiny
#https://debruine.github.io/shinyintro/sharing.html
#https://stackoverflow.com/questions/26004302/how-to-display-a-busy-indicator-in-a-shiny-app

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

