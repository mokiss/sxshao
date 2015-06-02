#Percentile Using R readlines
rm(list = ls())
library(parallel)
breaks = 500
b = c(0, 587.79)
file2 = sapply(1:12, function(i) paste0("/home/data/NYCTaxis/trip_fare_", i, ".csv"))
y = c(10,11)
gethistR = function(f, bound, num, index){
  width = (bound[2]- bound[1])/num
  con = file(f , 'r')
  result = rep(0,num)
  line = readLines(con, n = 1)
  while( length( line <- readLines(con, n = 1)) > 0 ){
    spli = strsplit(line, ",")
    total.toll = as.numeric(spli[[1]][ index[2] ]) - as.numeric(spli[[1]][ index[1]])
    if(total.toll > bound[1] & total.toll < bound[2]){
      j = floor( (total.toll - bound[1])/width )
      result[j+1] = result[j+1]+1
    }
  }
  close(con)
  return(result)
}
#gethistR(f2, b, breaks, y)
#f=f2
#bound = b
#num = breaks
#index = y
#rm(f,bound,num,index)
cl = makeCluster(12, "FORK")
hist <- parLapplyLB(cl, 1:12, function(x) gethistR(file2[x], b, breaks, y) ) 
stopCluster(cl)
summ = hist[[1]]
for (i in c(2:12)) summ = summ + hist[[i]]
total = sum(summ, na.rm = TRUE)
cum = cumsum(summ)
decln = total*c(1:9)/10
decls = sapply(decln, function(i) which( i < cum)[1])
wid = (b[2]-b[1])/breaks
deciles = decls*wid + b[1]

save.image("histRonly.rda")