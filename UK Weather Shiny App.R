#Assignment 4
#Valerie BLANCH

#importing libraries
library(shiny)
library(shinythemes)
library(maps)
library(readr)
library(dplyr)

#importing the data
sites <- read_csv('Sites.csv')
site_1090 <- read_csv('Site_1090.csv')
site_1135 <- read_csv('Site_1135.csv')
site_1144 <- read_csv('Site_1144.csv')
site_1226 <- read_csv('Site_1226.csv')
site_1302 <- read_csv('Site_1302.csv')
site_1450 <- read_csv('Site_1450.csv')
site_161 <- read_csv('Site_161.csv')
site_235 <- read_csv('Site_235.csv')
site_315 <- read_csv('Site_315.csv')
site_384 <- read_csv('Site_384.csv')
site_4 <- read_csv('Site_4.csv')
site_409 <- read_csv('Site_409.csv')
site_613 <- read_csv('Site_613.csv')
site_643 <- read_csv('Site_643.csv')
site_708 <- read_csv('Site_708.csv')
site_709 <- read_csv('Site_709.csv')
site_79 <- read_csv('Site_79.csv')
site_842 <- read_csv('Site_842.csv')
site_908 <- read_csv('Site_908.csv')
site_971 <- read_csv('Site_971.csv')

#building the UI
ui <- fluidPage(
  
  #to add lines   
  tags$head(
    tags$style(HTML('hr {border-top: 1px solid #7185119;}'))),
    
  #adding a theme
  theme = shinytheme('flatly'),

  #adding a title
  titlePanel(title = p(style='text-align: right;', span(style='color: rgb(71, 85, 119); 
                        font-family: Trebuchet MS, 
                        sans-serif; font-size: 40px;', 'UK WEATHER'), 
                        span(style='color: rgb(243, 121, 52); font-family: Trebuchet MS, 
                        sans-serif; font-size: 40px;', 'REPORT'), span(hr()))),

  sidebarLayout(
      sidebarPanel(
          
        #adding the map
        plotOutput('map'),
            
        hr(),


        #a select box to choose the stations
        #I was supposed to add multiple choices but couldn't plot with them
        #I wanted to use selectize
        selectInput(inputId = 'station',
                           label = 'Weather Station:', 
                           choices = sites$Site_Name),
        
        #a select box for the variables    
        selectInput(inputId = 'variable',
                        label = 'Variable:',
                        choices = c('Wind Speed',
                                    'Air Temperature',
                                    'Relative Humidity',
                                    'Visibility')),
        #a select box for the data    
        selectInput(inputId = 'data',
                        label = 'Data:',
                        choices = c('Raw Hourly Data',
                                    'Daily Averages',
                                    'Monthly Averages',
                                    'Daily Maxima',
                                    'Daily Minima')),
        
        #buttons to change the time format
        radioButtons(inputId = 'time',
                         label = 'Time Format:',
                         choices = c('Calendar Time',
                                     'Weekly',
                                     'Daily')),
        
        #adding a download button for the csv
        #I've run out of time and couldn't prepare the r markdown file
        downloadButton(outputId='downloadTableButton',
                            label = 'Download the Table'),
        ),
  
    mainPanel(
        tabsetPanel(
          
          #the main tab 
          tabPanel('General', plotOutput('plot'), hr(),
                     helpText('Last 7 daily averages recorded in the database - Year 2020'), 
                     tableOutput(outputId = 'table')),
          
          #a separate tab where I wanted to place a table for the hutton criteria but I haven't finished yet
          tabPanel('Hutton Criteria', tableOutput(outputId='hutton'), helpText('Under Construction')),  type='pills'))
        ))


#writing the server script
server <- function(input, output){
  
  #creating a reactive variable for the stations 
  #outside a switch function to switch between datasets
  dataset<- reactive({switch(input$station,
               'Shawbury' = site_643,
               'Leuchars' = site_235,
               'Aldergrove' = site_1450,
               'Heathrow' = site_708,
               'Benson' = site_613,
               'Boulmer' = site_315,
               'Northolt' = site_709,
               'Blackpool' = site_1090,
               'Ringway' = site_1135,
               'Waddington' = site_384,
               'Hawarden Airport' = site_1144,
               'Sumburgh' = site_4,
               'Marham' = site_409,
               'Tain Range' = site_79,
               'Hurn' = site_842,
               'Pembrey Sands' = site_1226,
               'Machrihanish' = site_908,
               'Yeovilton' = site_1302,
               'Dyce' = site_161,
               'Abbotsinch' = site_971)})

  #same for other inputs
  varparam <- reactive({switch(input$variable,'Wind Speed'= dataset()$wind_speed,
                               'Air Temperature'= dataset()$air_temperature,
                               'Relative Humidity'= dataset()$rltv_hum,
                               'Visibility'= dataset()$visibility)})
  
  timeparam <- reactive({switch(input$time, 'Calendar Time'= dataset()$ob_time,
                                'Weekly'= dataset()$day,
                                'Daily'= dataset()$hour)})
  
  
  #writing the script of the map
  output$map <- renderPlot({
    
    maps::map('world','UK')    
    stations <- subset(sites,
                       Site_Name %in% input$station)
    
    points(stations$Longitude,stations$Latitude, pch=16, col='#FF6600')
    
  })
  
  

  #script of the plot
  output$plot <- renderPlot({
      plot(timeparam(), varparam(), type='l', col='#33CC99',
           xlab = input$time, ylab = input$variable, main= c(input$station, input$data))
    
    #since I couldn't finish this part, I've added a message  
    validate(need(input$time == 'Calendar Time', 'Under construction'))
    validate(need(input$data == 'Raw Hourly Data', 'Under construction'))
  
      })
    
  #script of the recap table
  output$table <- renderTable({ 
    tableav <- dataset()
    #filtering the data I need
    tableav <- tableav %>% group_by(month, day) %>% summarise(air_temperature = mean(air_temperature),
                                                       wind_speed = mean(wind_speed),
                                                       rltv_hum =mean(rltv_hum),
                                                       visibility = mean(visibility))
    

    tableav$day <- as.integer(tableav$day)
    tableav$month <- as.integer(tableav$month)
    
    #only keeping the last 7 days
    tableav <- tail(tableav, 7)
    
      
    })
    

    #where I wanted to put the script of the hutton criteria
    output$hutton <- renderTable({ 
  })

    #script of the download button
    output$downloadTableButton <- downloadHandler(
      filename = 'table.csv', 
      content = function(file) {
          tableav <- dataset()
          tableav <- tableav %>% group_by(month, day) %>% summarise(air_temperature = mean(air_temperature),
                                                                    wind_speed = mean(wind_speed),
                                                                    rltv_hum =mean(rltv_hum),
                                                                    visibility = mean(visibility))
          tableav$day <- as.integer(tableav$day)
          tableav$month <- as.integer(tableav$month)
          tableav <- tail(tableav, 7)
          write.csv(tableav, file)})

}

shinyApp(ui = ui, server = server)
