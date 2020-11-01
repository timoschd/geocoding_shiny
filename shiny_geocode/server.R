
library("shiny")
library("tidyverse")
library("readxl")
library("rworldmap")

# load country list once
data("countryExData")
countries<- countryExData[, 2]


# Define server logic
server <- function(input, output) {
    
    # load if csv or xlsx file uploaded
    data_file <- reactive({
        req(input$file_upload)
        
        if (str_detect(input$file_upload$datapath, "\\.csv$")){
            
            df <- read.csv(input$file_upload$datapath,
                           header = input$header,
                           sep = input$sep
            )
            return(df)
        }
        
        else if (str_detect(input$file_upload$datapath, "\\.xlsx$")){
            
            df <- read_excel(input$file_upload$datapath, 
                             col_names = input$header
            )
            return(df)
        }
        
        else {
            return("Error: Wrong file format")
            
        }
        
    })
    
    output$preview <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(data_file())
        
        return(head(data_file(),10))
        
        
    }, caption= "Data preview")
    
    # make Input filter pop up when data was uploaded
    output$address <- renderUI({
        req(data_file())
        selectInput("address", "Street (+ House Number):", choices= c(" ", names(data_file())), width='40%', selected = " ")
    })
    
    output$zip <- renderUI({
        req(data_file())
        selectInput("zip", "ZIP:", choices= c(" ", names(data_file())), width='40%', selected = " ")
    })
    
    output$city <- renderUI({
        req(data_file())
        selectInput("city", "City:", choices= c(" ", names(data_file())), width='40%', selected = " ")
    })
    
    output$country <- renderUI({
        req(data_file())
        selectInput("country", "Country:", choices= c(" ", names(data_file())), width='40%', selected = " ")
    })
    
    
    output$country_list <- renderUI({
        req(data_file())
        selectInput("country_list", "OR choose one country from list:", choices= c(" ", countries), width='40%',
                    selected = " ", selectize = TRUE, multiple= FALSE)
    })
    
    # render start buttom
    output$start_button <- renderUI({
        req(data_file())
        actionButton("start", "Start geocoding")
    })
    
    # on button click make adress column readable by google geocode api
    adress_df <- eventReactive(input$start, {
        req(data_file())
        data_file() %>% 
              mutate(complete_address = paste0(!!sym(input$address), ", ", !!sym(input$zip), " ", !!sym(input$city), ", ", !!sym(input$country)))
    })
    
    # test output
    output$test<- renderTable({
        req(adress_df())
        return(head(adress_df(),10))
    })
}