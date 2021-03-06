#newhist
#check minmax
#test simple regression on parallel
library(parallel)
setwd("/home/sxt999/run")
rm(list = ls())
cname = "hist"
compile = paste0("R CMD SHLIB ", cname,".c")
ccode = paste0(cname,".so")
system(compile)
y = c(10,11)
#x = 9
minmaxdif = function(c, f, index) {
  dyn.load(c)
  re = .C("minmaxdif", as.character(f), as.integer(index), as.numeric(0), as.numeric(0), as.integer(0))
  dyn.unload(c)
  out = c( max = re[[3]], min = re[[4]], n = re[[5]] )
  return( out)
}
#file1 = sapply(1:12, function(i) paste0("/home/data/NYCTaxis/trip_data_", i, ".csv"))
file2 = sapply(1:12, function(i) paste0("/home/data/NYCTaxis/trip_fare_", i, ".csv"))

cl = makeCluster(12, "FORK")
result <- parLapplyLB(cl, 1:12, function(x) minmaxdif(ccode, file2[x], y) ) 
stopCluster(cl)

b = c(median( unlist( lapply(result, "[[", 2))),
      median( unlist( lapply(result, "[[", 1))) ) 
breaks = 500

gethist = function(c ,f, bound, num, yindex){
  dyn.load(c)
  wid = (bound[2] - bound[1])/num
  re = .C("gethist", as.character(f), as.integer(yindex), 
          as.integer( rep(0, num)), as.numeric(bound), as.numeric(wid))[[3]]
  class(re) = c( paste0("Width:", wid), paste0("Breaks:", num))
  dyn.unload(c)
  return(re)
}

cl = makeCluster(12, "FORK")
hist <- parLapplyLB(cl, 1:12, function(x) gethist(ccode, file2[x], b, breaks, y) ) 
stopCluster(cl)
summ = hist[[1]]
for (i in c(2:12)) summ = summ + hist[[i]]
total = sum(summ)
cum = cumsum(summ)
decln = total*c(1:9)/10
decls = sapply(decln, function(i) which( i < cum)[1])
wid = (b[2]-b[1])/breaks
deciles = decls*wid + b[1]
#7.05348  8.22906  9.40464 10.58022 11.75580 14.10696 15.28254 18.80928 27.03834
save.image("hist.rda")


