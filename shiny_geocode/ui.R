library("shinydashboard")
library("shinydashboardPlus")
library("dashboardthemes")


# Define UI for application 
ui <- dashboardPage(
    header= dashboardHeader(title= "Batch Geocoding",
                            tags$li(a(href = 'mailto:info@timschendzielorz.com?subject="Geocoding App"',
                                      icon("envelope", "fa-2x"),
                                      title = "Contact",
                                      style = "color: #17677C; margin-top: -4px; padding-left: 4px;" ),
                                    class = "dropdown"),
                            tags$li(a(href = 'https://www.github.com/timosch29/geocoding_shiny',
                                      icon("github", "fa-2x"), # 2x size from f
                                      title = "GitHub Repo",
                                      style = "color: #17677C; margin-top: -4px; padding-left: 4px;" ),
                                    class = "dropdown"),
                            # Set height of dashboardHeader
                            tags$li(class = "dropdown",
                                    tags$style(".navbar {max-height: 40px;}")
                            )
                                    
    ),
    
    sidebar= dashboardSidebar(disable = TRUE),
    
    body= dashboardBody(
        shinyalert::useShinyalert(),
        shinyDashboardThemes(theme = "blue_gradient"),
        # app title 
        titlePanel("Get longitude & latitude for an address list"),
        column(width=4,
               fluidRow(
                   # box w/ description and upload inputs
                   box(width = '100%',
                       title="1. Upload your address list as CSV or Excel file",
                       "After uploading specify if the data has a header. If it is a CSV file, specify the separator.", br(),
                       "Note that for Excel files, all columns need to be in character format.", br(),
                       "You can check the data in the preview once uploaded.",
                       
                       br(), br(),
                       fileInput(
                           inputId = "file_upload",
                           label= "Upload file",
                           multiple = FALSE,
                           accept = c(".csv", ".xlsx"),
                           width = '40%',
                           buttonLabel = "Browse...",
                           placeholder = "No file selected"
                       ),
                       
                       # check if file has header
                       checkboxInput("header", "Header", TRUE),
                       
                       # Input: Select separator 
                       radioButtons("sep", "Separator",
                                    choices = c(Comma = ",",
                                                Semicolon = ";",
                                                Tab = "\t",
                                                Pipe = "|"),
                                    selected = ","),
                   )
               ), 
               box(
                   width = '100%',
                   title= "2. Choose the address columns",
                   "Either city or ZIP is required. A country column or a country choosen from the dropdown list is required.", br(), br(),
                   uiOutput("address"),
                   uiOutput("zip"),
                   uiOutput("city"),
                   uiOutput("country"),
                   uiOutput("country_list")
               ),
               box(
                   width = '100%',
                   title= "3. Geocode",
                   uiOutput("start_button"),
                   htmlOutput("df_message")
                   
               ),
               box(
                   width = '100%',
                   title= "4. Download results",
                   uiOutput("download_button")
                   
               )
               
        ),
        
        
        column(width = 8,
               htmlOutput("data_size"),
               tableOutput("preview"),
               tableOutput("preview_results")
              
        )
    )
    
    
    
)
