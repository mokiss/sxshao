#clean water data
library(ggplot2)
library(maps)
setwd("/Users/Shawn/Dropbox/242/Project")
#===============
rm(list = ls())
water = read.csv("water.csv")
water = water[c("STATE","COUNTY" ,"DO.PSPCp")]
counties <- readRDS("counties.rds")

name1 = sapply(1:nrow(counties), function(i) strsplit(counties$name[i], ",")[[1]][1])
name2 = sapply(1:nrow(counties), function(i) strsplit(counties$name[i], ",")[[1]][2])
name2 = sapply(1:length(name2), function(i) strsplit( name2[i], ":")[[1]][1])

county = tolower(water$COUNTY)
county = gsub("city|county|parish", "",county)
county = gsub(" $","", county)
state = tolower(water$STATE)

usastate = readHTMLTable("http://www.50states.com/abbreviations.htm#.VSsXHFxWKfQ", which = 1)
colnames(usastate) = c('state', 'Abb')
usastate$state = tolower(usastate$state)
usastate$Abb = tolower(usastate$Abb)
state.t = sapply(1:length(state), function(i) usastate$state[ match(state[i], usastate$Abb) ])

#state.t and county are fromw water, needed to be matched with the order of counties
wherewater = sapply(1:length(name1), function(i) which( state.t == name1[i] & county == name2[i])[1])
emptywater = is.na(wherewater)
name2[which(emptywater)]
name1[which(emptywater)]


cleanwater = water[wherewater,]
colnames(cleanwater)[3] = "use"
cleanwater$use = as.numeric(cleanwater$use)

interval = cut(cleanwater$use,9, labels = FALSE)
shades = brewer.pal(9,"Blues")
#shades = colorRampPalette(c("white","blue"))(9)
colors = shades[interval]
colors[is.na(colors)] = shades[5]
map("county", fill = TRUE, col = colors, mar = c(0,0,0,0), myborder = 0)
map("state", col = "white", fill = FALSE, add = TRUE)
