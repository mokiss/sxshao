library(RColorBrewer)
mapplot = function(point, water){
  interval = cut(water$use, 9, labels = FALSE)
  shades = brewer.pal(9,"Blues")
  #shades = colorRampPalette(c("white","orange"))(100)
  colors = shades[interval]
  colors[is.na(colors)] = shades[5]
  map("county", fill = TRUE, col = colors, mar = c(0,0,0,0), myborder = 0)
  map("state", col = "white", fill = FALSE, add = TRUE)
  points(point$long, point$lat, col = rgb(0,0,0, alpha = 0.0))
         #col = rgb(0,0,0,alpha=0.0))
  
}