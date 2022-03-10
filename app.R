library(shiny)
library(dplyr)
library(lubridate)
library(DT)
library(tidyr)
library(shinydashboard)
library(shinythemes)
library(odbc)


# Define UI  ----
ui <- shinyUI(fluidPage(theme = shinytheme("spacelab"), 
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput(inputId = "InputSelect",
                  label = "Input Type",
                  choices = c("",
                              "Test1" = "Test1",
                              "Test2" = "Test2"
                  ),
                  selected = "Test1",
                  multiple = F
      ),
      
      actionButton(inputId = "Submit",
                   label = "Submit"
      ),
      
      width=2
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
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
        tags$img(src="busy.gif")
      ),
      
      # Output:  ----
      verbatimTextOutput("timed"),
      
      DTOutput("table")
      
    )
    
  ),
  
  # adding the new div tag to the sidebar            
  tags$div(class="header", checked=NA,
           tags$p("Raw code located on Git"),
           tags$a(href="https://git.illumina.com/landers/NovaSeqMaterialsTrace_Shiny.git", "Click Here!")
  )
)
)

# Define server logic  ----
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
  
  # Server side operations 
  
  #Start out with blank time as a placeholder
  output$timed <- renderText({ NULL })
  
  #Start out with a blank table as a placeholder
  output$table <- renderDT(
    NULL,
    class = "display nowrap compact",
    filter = "top") 
  
  observeEvent(input$Submit,{
    
    timed_t0 <- Sys.time()
    
    if(input$InputSelect == "Test1") {
      df <- data.frame("Test" = c("Test1"), "Directory" = c(getwd()))
      
    } else if(input$InputSelect == "Test2") {
      df <- data.frame("Test" = c("Test2"), "Directory" = c(getwd()))
      
    } else if(input$InputSelect == "DataBaseExample") {
      query <- paste0("--USE METAWH
                      USE PLACEHOLDER
                      SELECT * 
                      FROM PLACEHOLDER
                      LIMIT 1000
                        ")
      
      con <- DBI::dbConnect(
        odbc::odbc(),
        Driver = "",
        Server = "",
        Database = "",
        Warehouse = "",
        #dbname = 'dbname',
        #host = 'host',
        port = 443,
        UID = '',
        #UID          = rstudioapi::askForPassword("Database user"),
        #PWD          = rstudioapi::askForPassword("Database password"),
        PWD = "",
        authenticator = "externalbrowser"
        #sslmode = 'require'
        #Trusted_Connection = "yes"
      )
      
      df = dbGetQuery(con, query)
      dbDisconnect(con)
      
    } else {
      df <- data.frame(Par = c("NULL"), Par2 = c("Make a valid selection"))
    }
    
    timed_tf <- Sys.time()
    
    timed_time = difftime(timed_tf, timed_t0, units = "secs")
    
    Results <- df
    
    output$timed <- renderText({ paste0(timed_time, " seconds") })
    
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
