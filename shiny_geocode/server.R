
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
    
    # render start button
    output$start_button <- renderUI({
        req(data_file())
        actionButton("start", "Start geocoding", style= "background-color: #00ffd5")
    })
    
    # on button click check consistency
    check_df_message <- eventReactive(eventExpr = input$start, label="button_observer", valueExpr = {
        req(data_file())
        if (input$zip == " " & input$city == " "){
            "<span style=\"color:red\">Error: City or ZIP column required</span>"
        }
        else if (input$country == " " & input$country_list == " "){
            "<span style=\"color:red\">Error: Country column or country from list required</span>"
        }
        else if (input$address != " " & !is.character(data_file()[[input$address]])) {
            "<span style=\"color:red\">Error: Street column not in character format</span>"
        }
        else if (input$zip != " " & !is.character(data_file()[[input$zip]])) {
            "<span style=\"color:red\">Error: ZIP column not in character format</span>"
        }
        else if (input$city != " " & !is.character(data_file()[[input$city]])) {
            "<span style=\"color:red\">Error: City column not in character format</span>"
        }
        else if (input$country != " " & !is.character(data_file()[[input$country]])) {
            "<span style=\"color:red\">Error: Country column not in character format</span>"
        }
        else {
            "<span style=\"color:green\">Geocoding started</span>"
        }
        
    })
    
    # Error message output
    output$df_message<- renderText({
        req(check_df_message())
        return(check_df_message())
    })
    
    # if consistent, make complete adress column and geocode it
    geocoded_data<- eventReactive(input$start, {
        req(check_df_message())
        if (check_df_message() == "<span style=\"color:green\">Geocoding started</span>"){
            data_file() %>%
                #mutate(" "= "") %>% 
                mutate(complete_address = paste0(
                   (if(input$address != " "){
                        paste0(!!sym(input$address), ", " )
                    }),
                   (if(input$zip != " "){
                       paste0(!!sym(input$zip), " ") 
                   }),
                   (if(input$city != " "){
                       !!sym(input$city)
                   }), ",",
                   (if(input$country != " "){
                       paste0(" ", !!sym(input$country)) 
                   }),
                   (if(input$country_list != " "){
                       paste0(" ", input$country_list) 
                   })
                   
                ))
        }
    })
    # test pasted column
    output$test<- renderTable({
        req(geocoded_data())
        return(head(geocoded_data()))
    })
}