#User_Interface 
library(shiny)


shinyUI(fluidPage(
 h1("American Weather Interactive Visualization", align = "center"),
  fluidRow(
    column(8,
           plotOutput("map", click = "point_airport")           
           
      ),
    column(4,
            
           
     wellPanel( style = "overflow: hidden;",uiOutput("info") )

      )
    ,
     uiOutput("predict")
    
    ),
  
  fluidRow(
    column(8,
           plotOutput("history")
           
           ),
    
    column(4,
           dateRangeInput("dates", "Date Range", 
                          start = Sys.Date()-365, end = Sys.Date(),
                          max = Sys.Date()),
           
           selectInput("var", "Weather Type", 
                       choices = c("Temperature", "Humidity","Visibility"), selected = "Temperature")
           )
    )
 
))
