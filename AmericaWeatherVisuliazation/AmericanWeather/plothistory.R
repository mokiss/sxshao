#plot history data
library(grid)
plothis = function(type, name, id, startdate, enddate){
  #type is the column index of weather data
  #type= 4 Mean_TemperatureF
  #type = 7 Mean_Humidity
  #type = 9 Mean_VisibilityMiles
  if(length(type) > 0){
    tempdat = getSummarizedWeather( id ,startdate, enddate, opt_custom_columns = TRUE, 
                                   custom_columns = type)
    names(tempdat)[2] = "y"
    typecol = switch( as.character(type), "4" = "red", "9" = "blue", "15" = "green")
    ggplot(tempdat, aes(x = Date, y = y)) + geom_line(col = typecol) +
      labs( y = name, title = paste(name, "of one Year")) +
      theme(plot.margin = unit(c(0,0,0,0),"cm"))
  }


  
}