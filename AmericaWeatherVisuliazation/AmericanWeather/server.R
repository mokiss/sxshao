#Sever
library(shiny)
library(maps)
library(mapproj)
library(RColorBrewer)
library(ggplot2)
library(weatherData)
library(forecast)
#library(astsa)
library(xts)
library(forecast)

load("data/airport.rda")
#airport:ident, name, lat, long, iso_country ,iso_region, municipality ,scheduled_service

load("data/water.rda")
#cleanwater: STATE, COUNTY, use


source("plotmap.R")
source("plothistory.R")
source("prediction.R")


shinyServer(function(input, output) {

  select <- reactiveValues(
    ident = "", name = "", 
    region = "", city = "", check = 2, 
    tmpdata = data.frame()
    )
  output$map <- renderPlot({

    m <- mapplot( airport, cleanwater)

  })
  

  observeEvent(input$point_airport, {
    clicks = nearPoints(airport, input$point_airport, xvar = "long", yvar = "lat", 
                            threshold = 10, maxpoints = 1)
    
   if(nrow(clicks) > 0){
     select$ident = clicks$ident
     select$name = clicks$name
     select$region = substr(clicks$iso_region, 4, 5)
     select$city = clicks$municipality
     select$check = checkSummarizedDataAvailability(select$ident, Sys.Date(),
                                             station_type = "airportCode")
     if(select$check == 1){
       select$tmpdata = getSummarizedWeather(select$ident,Sys.Date(), opt_custom_columns = TRUE, 
                                             custom_columns = c(1,2,3,4,6,9,12,15,18,21,22,23))
     }

     else select$tmpdata = ""
   }
   else {
     select$ident = ""
     select$name = ""
     select$region = ""
     select$city = ""
     select$check = 2
     select$tmpdata = data.frame()
   }

  })
  
  output$history <- renderPlot({
    if( select$ident == ""){
      
    }
    else{
      we.type <- switch( input$var, 
                         "Temperature" = 4, 
                         "Humidity" = 9, 
                         "Visibility" = 15)
      if(select$check == 1)
      plothis( we.type, input$var, select$ident, input$dates[1], input$dates[2])
      
    }


    
  }, height = 150, width = 600)

output$info <-renderUI({
    string = ""
    if( select$ident != ""){
      if( select$check == 1){
        string = paste( h3(select$city),
                        "<strong>Max Temperature</strong>",select$tmpdata$Max_TemperatureF, "F <br>",
                        "<strong>Mean Temperature</strong>", select$tmpdata$Mean_TemperatureF, "F <br>",
                        "<strong>Min Temperature</strong>", select$tmpdata$Min_TemperatureF, "F <br>",
                        "<strong>Mean Dew Point </strong>", select$tmpdata$MeanDew_PointF, "F <br>",
                        "<strong>Humidity</strong>", select$tmpdata$Mean_Humidity, "<br>",
                        "<strong>Sea Level Pressure</strong>", select$tmpdata$Mean_Sea_Level_PressureIn, "In <br>",
                        "<strong>Visibility</strong>", select$tmpdata$Mean_VisibilityMiles, "Miles <br>",              
                        "<strong>Wind Speed</strong>", select$tmpdata$Mean_Wind_SpeedMPH, "MPH <br>",
                        "<strong>Cloud Cover</strong>", select$tmpdata$CloudCover, "<br>",
                        "<strong>Events</strong>", select$tmpdata$Events, "<br>")  
        
      }
      else {
        if(select$check == 0) string = paste( h4(select$city),"<br>", 
                                              "Ooops! The data is not Available now! Check your Internet or try another one",
                                              "<br>")
        else string = ""
      }
      
    }

    string = paste("<h4 style='color:green'>Current Local Weather Report</h4>",string)
    HTML(string)
  })

 output$predict <- renderUI({
   string = ""
   if( select$ident != ""){
     if( select$check == 1){
       histTemper = getSummarizedWeather(select$ident, Sys.Date()-365, Sys.Date(), 
                                         opt_custom_columns = TRUE, custom_columns = 4)
       
       pred = forecastArima(histTemper[,ncol(histTemper)])
       string = paste("<strong style = 'color:green'>Temperature Prediction</strong>",round(pred), "F <br>")       
     }
   }   
   HTML(string)
 })
 
 
})