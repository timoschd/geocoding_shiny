#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("shinydashboard")
library("shinydashboardPlus")
library("dashboardthemes")
library("tidyverse")
library("readxl")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    header= dashboardHeader(title= "Address Geocoding",
                            tags$li(a(href = 'http://shinyapps.company.com',
                                      icon("github"),
                                      title = ""),
                                    class = "dropdown"),
                            tags$li(a(href = 'http://www.company.com',
                                      icon("envelope"),
                                          title = "Contact",
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown")
                            ),
    
    sidebar= dashboardSidebar(disable = TRUE),
    
    body= dashboardBody(
        shinyDashboardThemes(theme = "blue_gradient"),
        fluidPage(
            
            
        # Application title
        titlePanel("Get longitude & latitude for an address list"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
          #  sidebarPanel(
               box(
                   title="Upload your address list as CSV or Excel file and recieve the corresponding geocodes.",
                   "After uploading your file, specify the columns that contain the address.", br(), 
                   "Either ZIP or City is a mandatory column as well as country. If all your addresses are from one country, you can choose a country from the dopdown menu.",
                   br(), br(),
                   fileInput(
                       inputId = "file_upload",
                       label= "Upload file",
                       multiple = FALSE,
                       accept = c(".csv", ".xlsx"),
                       width = '30%',
                       buttonLabel = "Browse...",
                       placeholder = "No file selected"
                   ),
                   # check if file has header
                   checkboxInput("header", "Header", TRUE),
                   
                   # Input: Select separator ----
                   radioButtons("sep", "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t",
                                            Pipe = "|"),
                                selected = ","),
                 ),
               
               mainPanel(
                   
                   tableOutput("preview")
               )
            )
        ) 
    )
)
    
    
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$preview <- renderTable({
       
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file_upload)
        
        if (str_detect(input$file_upload$datapath, "\\.csv$")){
        
            df <- read.csv(input$file_upload$datapath,
                       header = input$header,
                       sep = input$sep
                       )
            return(head(df,10))
        }
        
        else if (str_detect(input$file_upload$datapath, "\\.xlsx$")){
        
            df <- read_excel(input$file_upload$datapath, 
                         col_names = input$header
                         )
            return(head(df, 10))
        }
        
        else {
            return("Error: Wrong file format")
            
        }
            
    }, caption= "Data preview")
}

# Run the application 
shinyApp(ui = ui, server = server)
