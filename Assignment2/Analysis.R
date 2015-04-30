#analysis

g = createBMLGrid(100, 99, 0.3)
playBMLGrid(g,'0.3density.gif', 1000)

g = createBMLGrid(100, 99, 0.7)
playBMLGrid(g,'0.7density.gif', 1000)

#1for a 100x99 dimenson, density from 0.2 to 0.7
g = list()
g = sapply( 2:7, function(i) createBMLGrid(100, 99, i/10))
su = lapply(g, function(i){
  summary(runBMLGrid(i, 6000))
})
density = c(2:7)/10
par( mfrow= c(2,3))
lapply(g, plot)

sapply(1:length(su), function(i) 
  plot( as.numeric(su[[i]]$v), ylab = 'Velocity', main = paste0("Car Density: ", density[i]), cex = 0.3))
par( mfrow= c(1,1))
plot(as.numeric(su[[7]]$v), main = density[7], cex = 0.3)

#2effect of grid shape
#grid shape might influcen the v of certain color
#2.1square dimension
#low car density
n = (6:14)*10
g = list()
g = sapply( n, function(i) createBMLGrid( i, i, 0.3))
su = lapply(g, function(i){
  summary(runBMLGrid(i, 1000))
})
#su = lapply(su, function(i) as.numeric(i$v))
blue = lapply(su, function(i) i[i$color == 'blue',])
red = lapply(su, function(i) i[i$color == 'red',])
par( mfrow= c(3,3))
sapply(1:length(blue), function(i){
  plot( as.numeric(blue[[i]]$v), col = 'blue', cex = 0.2, 
        main = paste0("Dimension:", n[i], "x", n[i]), ylab = "Velocity")
  points( as.numeric(red[[i]]$v), col = 'red', cex = 0.2)
})

#high car density
n = (6:14)*10
g = list()
g = sapply( n, function(i) createBMLGrid( i, i, 0.7))
su = lapply(g, function(i){
  summary(runBMLGrid(i, 1000))
})

blue = lapply(su, function(i) i[i$color == 'blue',])
red = lapply(su, function(i) i[i$color == 'red',])
par( mfrow= c(3,3))
sapply(1:length(blue), function(i){
  plot( as.numeric(blue[[i]]$v), col = 'blue', cex = 0.2, 
        main = paste0("Dimension:", n[i], "x", n[i]), ylab = "Velocity")
  points( as.numeric(red[[i]]$v), col = 'red', cex = 0.2)
})



#2.2c(columns) is bigger
#low car density
r = sample(6:14)*10
c = sample(30:38)*10

g = list()
g = sapply( 1:length(r), function(i) createBMLGrid( r[i], c[i], 0.3) )
su = lapply(g, function(i){
  summary(runBMLGrid(i, 6000))
})

blue = lapply(su, function(i) i[i$color == 'blue',])
red = lapply(su, function(i) i[i$color == 'red',])
par( mfrow= c(3,3))
sapply(1:length(blue), function(i){
  plot( as.numeric(red[[i]]$v), col = 'red', cex = 0.3, ylim = c(0, max(as.numeric(su[[i]]$v)) ),
        main = paste0("Dimension:", r[i], "x", c[i]), ylab = "Velocity")
  points( as.numeric(blue[[i]]$v), col = 'blue', cex = 0.3)
})

sapply( 1:length(blue), function(i) 
 c( nrow(blue[[i]]), nrow(red[[i]]) )
  )

sapply(1:length(blue), function(i){
  plot( as.numeric(red[[i]]$move), col = 'red', cex = 0.2, ylim = c(0, max(as.numeric(su[[i]]$move)) ),
        main = paste0("Dimension:", r[i], "x", c[i]), ylab = "Move")
  points( as.numeric(blue[[i]]$move), col = 'blue', cex = 0.2)
})


#high density
g = list()
g = sapply( 1:length(r), function(i) createBMLGrid( r[i], c[i], 0.7) )
su = lapply(g, function(i){
  summary(runBMLGrid(i, 6000))
})

blue = lapply(su, function(i) i[i$color == 'blue',])
red = lapply(su, function(i) i[i$color == 'red',])
par( mfrow= c(3,3))
sapply(1:length(blue), function(i){
  plot( as.numeric(red[[i]]$v), col = 'red', cex = 0.2, ylim = c(0, max(as.numeric(su[[i]]$v)) ),
        main = paste0("Dimension:", r[i], "x", c[i]), ylab = "Velocity")
  points( as.numeric(blue[[i]]$v), col = 'blue', cex = 0.2)
})

sapply( 1:length(blue), function(i) 
  c( nrow(blue[[i]]), nrow(red[[i]]) )
)

sapply(1:length(blue), function(i){
  plot( as.numeric(red[[i]]$move), col = 'red', cex = 0.2, ylim = c(0, max(as.numeric(su[[i]]$move)) ),
        main = paste0("Dimension:", r[i], "x", c[i]), ylab = "Move")
  points( as.numeric(blue[[i]]$move), col = 'blue', cex = 0.2)
})


#3.code performance
#size
n = (1:10)*100
g = list()
g = sapply( n, function(i) createBMLGrid( i, i, 0.3))
su = lapply(g, function(i){
  system.time(runBMLGrid(i, 1000))
})

time = unlist(lapply(su, function(i) i[1]))
time = data.frame(n,time)

plot(time, type = 'b', ylab = 'Time(second)', main = 'Running Time vs Dimension')

#density
g = list()
density = c(4:14)/20
g = lapply( density, function(i) createBMLGrid(100, 99, i))
su = lapply(g, function(i){
  system.time(runBMLGrid(i, 1000))[1]
})
time = unlist(su)
time = data.frame(density, time)
plot(time, type = 'b', ylab = 'Time(second)', main = 'Running Time vs Density')


