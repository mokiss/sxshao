#test simple regression on parallel
library(parallel)
setwd("/home/sxt999/run")
rm(list = ls())
cname = "regsimp"
compile = paste0("R CMD SHLIB ", cname,".c")
ccode = paste0(cname,".so")
system(compile)
y = c(10,11)
x = 9
regsimp = function(c, f1, f2, yindex, xindex) {
  dyn.load(c)
  result = .C("reg",as.character(f1),as.character(f2),as.integer(yindex), as.integer(xindex),
              as.numeric(rep(0,2)),as.integer(0), as.numeric(0), as.numeric(0))
  out = result[[5]]
  out[3:5] = c(result[[7]], result[[8]],result[[6]])
  names(out) = c("x", "y","xy","x^2","n")
  dyn.unload(c)
  return(out)
}
file1 = sapply(1:12, function(i) paste0("/home/data/NYCTaxis/trip_data_", i, ".csv"))
file2 = sapply(1:12, function(i) paste0("/home/data/NYCTaxis/trip_fare_", i, ".csv"))

cl = makeCluster(12, "FORK")
t = system.time(
  result <- parLapplyLB(cl, 1:12, function(i) regsimp(ccode, file1[i], file2[i], y, x) ) )
stopCluster(cl)
summ = sapply(1:5 , function(i) sum(unlist(lapply(result,"[[",i ))))

beta = (summ[3]- summ[1]*summ[2]/summ[5]) / ( summ[4] - (summ[1])*(summ[1])/summ[5]) 

alpha = summ[2]/summ[5] - beta * summ[1]/summ[5]

save.image("result.rda")
