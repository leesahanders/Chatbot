# Shiny app for choosing and interacting with chatbots 

library(shiny)
library(dplyr)
library(lubridate)
library(DT)
library(tidyr)
library(shinydashboard)
library(shinythemes)
library(odbc)

# Load chatbot function(s)
source("chatbot_leafey.R")

# Define UI  
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
      
      # Adding div tag to the sidebar with git link           
      tags$div(class="header", checked=NA,
               #tags$p("Raw code located on Git"),
               tags$a(href="https://github.com/leesahanders/Chatbot", "Raw code located on Git, check it out!")
      ),
      
      width=3
      
    ),
    
    # Main panel for displaying outputs - Custom handling for loading gif when chatbot is "thinking"
    mainPanel(
      
      actionButton(inputId = "Submit",
                   label = "Start chatting"
      ),
      
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
  ),
  
  tags$head(tags$style(
    type="text/css",
    "#chatbotImg img {max-width: 100%; width: 100%; height: auto}"
  )),
  
  sidebarPanel(
    
    imageOutput("chatbotImg"),
    
    width = 2),
  
  mainPanel(

    DTOutput("table")
    
  )
)
)

# Define server logic  
server <- function(input, output) {
  
  #Connect server code snippet 
  #Check operating system, if linux log in as root user for being able to initialize authentication request to databases. 
  #Use the Connect UI to create system variables with username and password for appropriate account
  #Credit to: https://conjugateprior.org/2015/06/identifying-the-os-from-r/ 
  get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
  }
  
  #For development: set locally your system variables for username and password
  #Sys.setenv(username = "my_username")
  #Sys.setenv(password = "my_password")
  
  # #Check if system is on linux, if so put in the supplied username and password to sudo the account for successful database authentication
  if(get_os() == "linux") {
    message("Linux OS, putting in developer credentials")
    
    #Get developer info using environmental variables entered using the UI
    username <- Sys.getenv("username")
    password <- Sys.getenv("password")
    inputs = c(password)
    command = paste0('su - ', username)
    system(command, input = inputs)
  }
  
  #Display chatbot image 
  output$chatbotImg <- renderImage({
    filename <- normalizePath(file.path('./files', paste(input$chatbot,'.PNG', sep='')))
    list(src = filename, alt = paste("Chatbot Profile Image"))
  }, deleteFile = FALSE)
  
  
  #Start out with blank text as a placeholder
  #output$timed <- renderText({ NULL })
  
  #Start out with a blank table as a placeholder
  output$table <- renderDT(
    NULL,
    class = "display nowrap compact",
    filter = "top") 
  
  observeEvent(input$Submit,{
    
    timed_t0 <- Sys.time()
    
    if(input$chatbot == "Leafey") {
      df <- data.frame("Chatbot" = c("Leafey"), "Directory" = c(getwd()))
      
    } else if(input$chatbot == "Test2") {
      df <- data.frame("Test" = c("Test2"), "Directory" = c(getwd()))
    
    # Holding onto this for later if upgrade to database storage   
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
      
    } else {
      df <- data.frame(Par = c("NULL"), Par2 = c("Make a valid selection"))
    }
    
    Results <- df
    
    output$table <- renderDT(
      Results,
      class = "display nowrap compact", 
      filter = "top" 
    )
    
    df <- reactive(Results)
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
