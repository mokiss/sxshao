#make a data frame containing all the stuff
library(newBML)
grid2 = lapply(5:9, function(i) createBMLGrid(2^i,2^i,0.2))
grid3 = lapply(5:9, function(i) createBMLGrid(2^i,2^i,0.3))
grid4 = lapply(5:9, function(i) createBMLGrid(2^i,2^i,0.4))
grid5 = lapply(5:9, function(i) createBMLGrid(2^i,2^i,0.5))
grid6 = lapply(5:9, function(i) createBMLGrid(2^i,2^i,0.6))
grid7 = lapply(5:9, function(i) createBMLGrid(2^i,2^i,0.7))
grid8 = lapply(5:9, function(i) createBMLGrid(2^i,2^i,0.8))

b2.run = lapply(grid2, function(i) system.time(runBMLGrid(i, 1000))[1])
b3.run = lapply(grid3, function(i) system.time(runBMLGrid(i, 1000))[1])
b4.run = lapply(grid4, function(i) system.time(runBMLGrid(i, 1000))[1])
b5.run = lapply(grid5, function(i) system.time(runBMLGrid(i, 1000))[1])
b6.run = lapply(grid6, function(i) system.time(runBMLGrid(i, 1000))[1])
b7.run = lapply(grid7, function(i) system.time(runBMLGrid(i, 1000))[1])
b8.run = lapply(grid8, function(i) system.time(runBMLGrid(i, 1000))[1])

b2.crun = lapply(grid2, function(i) system.time(crunBMLGrid(i, 1000))[1])
b3.crun = lapply(grid3, function(i) system.time(crunBMLGrid(i, 1000))[1])
b4.crun = lapply(grid4, function(i) system.time(crunBMLGrid(i, 1000))[1])
b5.crun = lapply(grid5, function(i) system.time(crunBMLGrid(i, 1000))[1])
b6.crun = lapply(grid6, function(i) system.time(crunBMLGrid(i, 1000))[1])
b7.crun = lapply(grid7, function(i) system.time(crunBMLGrid(i, 1000))[1])
b8.crun = lapply(grid8, function(i) system.time(crunBMLGrid(i, 1000))[1])

detach("package:newBML", unload=TRUE)
library(newBML2)
b2.crun2 = lapply(grid2, function(i) system.time(crunBMLGrid(i, 1000))[1])
b3.crun2 = lapply(grid3, function(i) system.time(crunBMLGrid(i, 1000))[1])
b4.crun2 = lapply(grid4, function(i) system.time(crunBMLGrid(i, 1000))[1])
b5.crun2 = lapply(grid5, function(i) system.time(crunBMLGrid(i, 1000))[1])
b6.crun2 = lapply(grid6, function(i) system.time(crunBMLGrid(i, 1000))[1])
b7.crun2 = lapply(grid7, function(i) system.time(crunBMLGrid(i, 1000))[1])
b8.crun2 = lapply(grid8, function(i) system.time(crunBMLGrid(i, 1000))[1])

save.image("/Users/Shawn/Dropbox/242/WorkStudio/Assignment4/analysis/an.rda")

datcrun = data.frame(dim = rep(c(5:9), 7), 
                 density = rep( c(2:8), 5)[order(rep( c(2:8), 5) )] , method = rep("crun", 35), 
                 t = c(unlist(b2.crun) ,unlist(b3.crun), unlist(b4.crun), unlist(b5.crun), 
                       unlist(b6.crun), unlist(b7.crun), unlist(b8.crun)))
datrun = data.frame(dim = rep(c(5:9), 7), 
                     density = rep( c(2:8), 5)[order(rep( c(2:8), 5) )] , method = rep("run", 35), 
                     t = c(unlist(b2.run) ,unlist(b3.run), unlist(b4.run), unlist(b5.run), 
                           unlist(b6.run), unlist(b7.run), unlist(b8.run)))

datcrun2 = data.frame(dim = rep(c(5:9), 7), 
                     density = rep( c(2:8), 5)[order(rep( c(2:8), 5) )] , method = rep("crun2", 35), 
                     t = c(unlist(b2.crun2) ,unlist(b3.crun2), unlist(b4.crun2), unlist(b5.crun2), 
                           unlist(b6.crun2), unlist(b7.crun2), unlist(b8.crun2)))

dat = rbind(datrun, datcrun, datcrun2)
library(ggplot2)
ggplot( data = dat, aes(x = density, y = t, fill = factor(dim))) + 
  geom_bar(stat ='identity') + facet_wrap(~ method, scales = "free_y")
ggplot( data = dat, aes(x = density, y = t, fill = factor(dim))) + 
  geom_bar(stat ='identity') + facet_wrap(~ method) + labs(fill = "Dimension(2^i)", x = "Density x 10")

ggplot( data = dat[which(dat$density == 2),], aes(x = dim, y = t, color = factor(method))) +
  geom_line(cex = 0.5) + labs( color = "Function", x = "Dimention(2^i)")

ggplot( data = dat[which(dat$dim == 9),], aes(x = density, y = t, color = factor(method))) +
  geom_line(cex = 0.5) + labs( color = "Function")
ggplot( data = dat[which(dat$dim == 8),], aes(x = density, y = t, color = factor(method))) +
  geom_line(cex = 0.5) + labs( color = "Function")
ggplot( data = dat[which(dat$dim == 7),], aes(x = density, y = t, color = factor(method))) +
  geom_line(cex = 0.5) + labs( color = "Function")
ggplot( data = dat[which(dat$dim == 6),], aes(x = density, y = t, color = factor(method))) +
  geom_line(cex = 0.5) + labs( color = "Function")
ggplot( data = dat[which(dat$dim == 5),], aes(x = density, y = t, color = factor(method))) +
  geom_line(cex = 0.5) + labs( color = "Function")



