#Code Refine
#Original one
createBMLGrid = function(r, c, ncars){
  #create a matrix, blank = 0, red = 1, blue = 2
  map = matrix(rep(0, r*c), r , c)
  if(length(ncars) == 1){
    #ncars is the total number of cars
    tmp = sample( seq(1, r*c, 1), ncars, replace = FALSE)
    map[tmp] = sample( rep(c(1,2), ncars))
  }
  else{
    #ncars = c( red = ??, blue = ??)
    tmp = sample( seq(1,c*r,1) , sum(ncars), replace = FALSE )
    map[tmp] = 2
    map[sample(tmp, ncars[1], replace = FALSE)] = 1
  }
  class(map) = c("BML grid", "matrix")
  return(map)
}

mover = function(m){
  tp = m
  no = ncol(m)
  tp[, 2:no] = m[, 1:no-1]
  tp[, 1] = m[, no]
  return(tp)
}
movel = function(m){
  no = ncol(m)
  tp = m
  tp[, 1:no-1] = m[, 2:no]
  tp[, no] = m[, 1]
  return(tp)
}
moveu = function(m){
  ro = nrow(m)
  tp = m
  tp[1:ro-1, ] = m[2:ro, ]
  tp[ro, ] = m[1, ]
  return(tp)
}
moved = function(m){
  ro = nrow(m)
  tp = m
  tp[2:ro, ] = m[1:ro-1, ]
  tp[1, ] = m[ro, ]
  return(tp)
}
movelist = list(right = mover, left = movel , up = moveu , down = moved)

movecar= function(m,t){
  if(t%%2 ==1){
    #move blue cars
    blue = m == 2
    blank = m == 0
    blankt = moved(blank)
    tmp = m
    moveable = blankt & blue
    tmp[moveable] = 0
    tmp[ moveu(moveable) ] = 2
    # class(tmp) = c("BML grid","matrix", paste0('move blue = ', sum( moveable) ) )
  }
  else{
    #move red
    red = m == 1
    blank = m == 0
    blankt = movel(blank)
    tmp = m
    moveable = blankt & red
    tmp[moveable] = 0
    tmp[ mover(moveable) ] = 1
    #class(tmp) = c("BML grid","matrix", paste0('move red = ', sum( moveable) ) )
  }
  return(tmp)
}
runBMLGrid = function(grid, step){
  map = list(start = grid)
  for ( i in seq(1, step, 1)){
    map[[i+1]] = movecar(map[[i]],i)
  }
  return(map)
}
playBMLGrid = function(grid, step){
  library(animation)
  ani.options(interval = 0.7, loop = TRUE )
  if( is.list(grid) ){
    saveGIF({
      lapply(grid, function(i) 
        image(t(i)[,nrow(i):1 ], col = c('white', 'red', 'blue'), main = 'Move Car') )
      ani.pause()
    })  
  }
  else{
    map = grid
    saveGIF({   
      for ( i in seq(1, step, 1)){
        map = movecar(map,i, ncol(map), nrow(map))
        image(t(map)[,nrow(map):1 ], col = c('white', 'red', 'blue'), main = 'Move Car')
      }
      ani.pause()
    }, outdir = "/Users/Shawn/Dropbox/242")  
  }
}

Rprof("/Users/Shawn/Dropbox/242/WorkStudio/Assignment2/move.prof")
g = createBMLGrid(100, 99 ,c(100, 100))
glist = runBMLGrid(g, 20)
playBMLGrid(glist)
b = system.time( runBMLGrid(g,10000) )
Rprof(NULL)
summaryRprof("/Users/Shawn/Dropbox/242/WorkStudio/Assignment2/move.prof")$by.self
self.time self.pct total.time total.pct
"<Anonymous>"      4.30    32.33       4.84     36.39
"movecar"          4.24    31.88      11.58     87.07
"&"                1.64    12.33       1.64     12.33
"runBMLGrid"       1.48    11.13      13.06     98.20
"=="               0.76     5.71       0.76      5.71
"-"                0.30     2.26       0.30      2.26
"gc"               0.24     1.80       0.24      1.80
":"                0.20     1.50       0.20      1.50
"[<-"              0.10     0.75       0.10      0.75
"ncol"             0.04     0.30       0.04      0.30
user  system elapsed 
12.834   0.890  14.094 


#Refine 1
mover = function(m){
  no = ncol(m)
  tp = m[, c(no, 1:(no-1) )]
  return(tp)
}
movel = function(m){
  no = ncol(m)
  tp = m[,c(2:no, 1)]
  return(tp)
}
moveu = function(m){
  ro = nrow(m)
  tp = m[c( 2: ro, 1 ),]
  return(tp)
}
moved = function(m){
  ro = nrow(m)
  tp = m[ c(ro, 1:(ro-1) ), ]
  return(tp)
}

Rprof("/Users/Shawn/Dropbox/242/WorkStudio/Assignment2/move1.prof")
b1 = system.time( runBMLGrid(g,10000) )
Rprof(NULL)
summaryRprof("/Users/Shawn/Dropbox/242/WorkStudio/Assignment2/move1.prof")$by.self
self.time self.pct total.time total.pct
"movecar"         3.46    39.59       7.52     86.04
"&"               2.18    24.94       2.18     24.94
"runBMLGrid"      1.00    11.44       8.52     97.48
"=="              0.88    10.07       0.88     10.07
"mover"           0.26     2.97       0.26      2.97
"movel"           0.22     2.52       0.28      3.20
"gc"              0.22     2.52       0.22      2.52
"moveu"           0.22     2.52       0.22      2.52
"moved"           0.18     2.06       0.18      2.06
"%%"              0.04     0.46       0.04      0.46
"c"               0.04     0.46       0.04      0.46
":"               0.02     0.23       0.02      0.23
"[<-"             0.02     0.23       0.02      0.23
user  system elapsed 
8.777   0.597   9.420 

#2
movecar = function(m,t){
  if(t%%2 ==1){
    #move blue cars
    tmp = m
    moveable = ( moveu(m) == 2 ) & (m == 0)
    tmp[moveable] = 2
    tmp[ moved(moveable) ] = 0
    # class(tmp) = c("BML grid","matrix", paste0('move blue = ', sum( moveable) ) )
  }
  else{
    #move red
    tmp = m
    moveable = ( mover(m) == 1 ) & (m == 0)
    tmp[moveable] = 1
    tmp[ movel(moveable) ] = 0
    #class(tmp) = c("BML grid","matrix", paste0('move red = ', sum( moveable) ) )
  }
  return(tmp)
}

Rprof("/Users/Shawn/Dropbox/242/WorkStudio/Assignment2/move2.prof")
b2 = system.time( runBMLGrid(g,10000) )
Rprof(NULL)
summaryRprof("/Users/Shawn/Dropbox/242/WorkStudio/Assignment2/move2.prof")$by.self
self.time self.pct total.time total.pct
"movecar"         3.80    42.89       7.60     85.78
"&"               1.76    19.86       1.76     19.86
"runBMLGrid"      1.16    13.09       8.76     98.87
"=="              0.70     7.90       0.70      7.90
"mover"           0.54     6.09       0.54      6.09
"moveu"           0.34     3.84       0.36      4.06
"moved"           0.20     2.26       0.20      2.26
"movel"           0.16     1.81       0.20      2.26
"gc"              0.10     1.13       0.10      1.13
"[<-"             0.04     0.45       0.04      0.45
":"               0.02     0.23       0.02      0.23
"["               0.02     0.23       0.02      0.23
"c"               0.02     0.23       0.02      0.23
user  system elapsed 
9.146   0.506   9.668 

#3
movecar= function(m, t , no, ro){
  if( t%% 2 == 1) {
    blue = which( m == 2)
    blank = which( m == 0 )
    boundlog = blue%%ro == 1
    future = blue
    future[ boundlog] = blue[ boundlog] + ro
    future = future -1
    movei = future %in% blank
    tmp = m
    tmp[ blue[ movei ] ] = 0
    tmp[ future[movei] ] = 2
  }
  else {
    m = t(m)[no:1,]
    blue = which( m == 1)
    blank = which( m == 0 )
    boundlog = blue%%no == 1
    future = blue
    future[ boundlog] = blue[ boundlog] + no
    future = future -1
    movei = future %in% blank
    tmp = m
    tmp[ blue[ movei ] ] = 0
    tmp[ future[movei] ] = 1
    tmp = t(tmp[no:1,])
  }
  return(tmp)
}

runBMLGrid = function(grid, step){
  map = list(start = grid)
  n = ncol(grid)
  c = nrow(grid)
  for ( i in seq(1, step, 1)){
    map[[i+1]] = movecar(map[[i]], i, n, c)
  }
  return(map)
}

playBMLGrid = function(grid, step){
  library(animation)
  ani.options(interval = 0.7, loop = TRUE )
  if( is.list(grid) ){
    saveGIF({
      lapply(grid, function(i) 
        image(t(i)[,nrow(i):1 ], col = c('white', 'red', 'blue'), main = 'Move Car') )
      ani.pause()
    })  
  }
  else{
    map = grid
    saveGIF({   
      for ( i in seq(1, step, 1)){
        map = movecar(map,i, ncol(map), nrow(map))
        image(t(map)[,nrow(map):1 ], col = c('white', 'red', 'blue'), main = 'Move Car')
      }
      ani.pause()
    }, outdir = "/Users/Shawn/Dropbox/242")  
  }
}

Rprof("/Users/Shawn/Dropbox/242/WorkStudio/Assignment2/move3.prof")
b3 = system.time( runBMLGrid(g,10000) )
Rprof(NULL)
summaryRprof("/Users/Shawn/Dropbox/242/WorkStudio/Assignment2/move3.prof")$by.self
b3
self.time self.pct total.time total.pct
"match"                4.76    44.49       4.76     44.49
"movecar"              2.24    20.93      10.00     93.46
"which"                1.04     9.72       1.84     17.20
"=="                   0.78     7.29       0.78      7.29
"t.default"            0.66     6.17       0.66      6.17
"runBMLGrid"           0.62     5.79      10.62     99.25
"standardGeneric"      0.42     3.93       2.88     26.92
"gc"                   0.08     0.75       0.08      0.75
"%%"                   0.04     0.37       0.04      0.37
"%in%"                 0.02     0.19       4.78     44.67
"t"                    0.02     0.19       1.06      9.91
"loadMethod"           0.02     0.19       0.04      0.37
> b3
user  system elapsed 
10.698   0.821  11.619 

#4 
movecar= function(m,t, no, ro){
  if(t%%2 ==1){
    #move blue cars
    blue = m == 2
    blank = m == 0
    blankt = blank[ c(ro, 1:(ro-1) ), ]
    tmp = m
    moveable = blankt & blue
    tmp[moveable] = 0
    tmp[ moveable[c( 2: ro, 1 ),] ] = 2
    # class(tmp) = c("BML grid","matrix", paste0('move blue = ', sum( moveable) ) )
  }
  else{
    #move red
    red = m == 1
    blank = m == 0
    blankt = blank[,c(2:no, 1)]
    tmp = m
    moveable = blankt & red
    tmp[moveable] = 0
    tmp[ moveable[, c(no, 1:(no-1) )] ] = 1
    #class(tmp) = c("BML grid","matrix", paste0('move red = ', sum( moveable) ) )
  }
  return(tmp)
}
runBMLGrid = function(grid, step){
  map = list(start = grid)
  n = ncol(grid)
  c = nrow(grid)
  for ( i in seq(1, step, 1)){
    map[[i+1]] = movecar(map[[i]], i, n, c)
  }
  return(map)
}

Rprof("/Users/Shawn/Dropbox/242/WorkStudio/Assignment2/move4.prof")
b4 = system.time( runBMLGrid(g,10000) )
Rprof(NULL)
summaryRprof("/Users/Shawn/Dropbox/242/WorkStudio/Assignment2/move4.prof")$by.self
b4
self.time self.pct total.time total.pct
"movecar"         4.54    51.47       6.94     78.68
"runBMLGrid"      1.70    19.27       8.64     97.96
"&"               1.32    14.97       1.32     14.97
"=="              0.82     9.30       0.82      9.30
"gc"              0.18     2.04       0.18      2.04
":"               0.12     1.36       0.12      1.36
"[<-"             0.10     1.13       0.10      1.13
"c"               0.04     0.45       0.04      0.45
> b4
user  system elapsed 
8.649   0.655   9.366 
#5

#6
movecar= function(m,t, no, ro){
  if(t){
    #move blue cars
    blue = m == 2
    blank = m == 0
    blankt = blank[ c(ro, 1:(ro-1) ), ]
    tmp = m
    moveable = blankt & blue
    tmp[moveable] = 0
    tmp[ moveable[c( 2: ro, 1 ),] ] = 2
    # class(tmp) = c("BML grid","matrix", paste0('move blue = ', sum( moveable) ) )
  }
  else{
    #move red
    red = m == 1
    blank = m == 0
    blankt = blank[,c(2:no, 1)]
    tmp = m
    moveable = blankt & red
    tmp[moveable] = 0
    tmp[ moveable[, c(no, 1:(no-1) )] ] = 1
    #class(tmp) = c("BML grid","matrix", paste0('move red = ', sum( moveable) ) )
  }
  return(tmp)
}
runBMLGrid = function(grid, step){
  map = grid
  n = ncol(grid)
  c = nrow(grid)
  col = rep(c( TRUE, FALSE), step/2 + 1)[1:step]
  for ( i in col){
    map = movecar(map, i, n, c)
  }
  return(map)
}
#little better

#7
movecar= function(blue,red, blank, color , no, ro){
  if(color ){
    #move blue cars
    blankt = blank[ c(ro, 1:(ro-1) ), ]
    #tmp = m
    moveable = blankt & blue
    newfuture = moveable[c( 2: ro, 1 ), ]
    blue[ moveable] = FALSE
    blue[ newfuture] = TRUE
    blank[ newfuture ] = FALSE
    blank[ moveable] = TRUE
    # class(tmp) = c("BML grid","matrix", paste0('move blue = ', sum( moveable) ) )
  }
  else{
    #move red
    blankt = blank[,c(2:no, 1)]
    moveable = blankt & red
    newfuture = moveable[, c( no, 1 : (no-1) )]
    red[ moveable] = FALSE
    red[ newfuture] = TRUE
    blank[ newfuture ] = FALSE
    blank[ moveable] = TRUE
    #class(tmp) = c("BML grid","matrix", paste0('move red = ', sum( moveable) ) )
  }
  return(list( blue,red,blank))
}
runBMLGrid = function(grid, step){
  blue = grid == 2
  blank = grid == 0
  red = grid == 1
  map = list(blue, red, blank)
  n = ncol(grid)
  c = nrow(grid)
  col = rep(c( TRUE, FALSE), step/2 + 1)[1:step]
  for ( i in col){
    map = movecar( map[[1]], map[[2]], map[[3]], i, n, c)
  }
  grid[map[[1]]] = 2
  grid[map[[2]]] = 1
  grid[map[[3]]] = 0
  return(grid)
}
#much slower

