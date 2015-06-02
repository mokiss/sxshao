#check minmax
library(parallel)
setwd("/home/sxt999/run")
rm(list = ls())
cname = "minmax"
compile = paste0("R CMD SHLIB ", cname,".c")
ccode = paste0(cname,".so")
system(compile)
y = c(10,11)
x1 = 9
x2 = 7
minmax = function(c, f, xindex) {
  dyn.load(c)
  re = .C("minmaxone", as.character(f), as.integer(xindex), as.numeric(0), as.numeric(0), as.integer(0))
  dyn.unload(c)
  out = c( max = re[[3]] , min = re[[4]], n = re[[5]])
  return( out)
}
file1 = sapply(1:12, function(i) paste0("/home/data/NYCTaxis/trip_data_", i, ".csv"))
file2 = sapply(1:12, function(i) paste0("/home/data/NYCTaxis/trip_fare_", i, ".csv"))

cl = makeCluster(12, "FORK")
result1 <- parLapplyLB(cl, 1:12, function(x) minmax(ccode, file1[x], x1) ) 
result2 <- parLapplyLB(cl, 1:12, function(x) minmax(ccode, file2[x], x2))
stopCluster(cl)
save.image("minmax.rda")

