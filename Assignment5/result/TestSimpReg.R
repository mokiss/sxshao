#test simple regression on parallel
library(parallel)
setwd("/home/sxt999")
ccode = "regsimp.so"
regsimp = function(c, f1, f2) {
  dyn.load(c)
  sum = c(0,0)
  inter = 0
  square = 0
  n = 0
  result = .C("reg",as.character(f1),as.character(f2),as.numeric(sum),as.numeric(inter),
              as.numeric(square), as.integer(n))
  out = c( xsum = result[[3]][1], ysum = result[[3]][2], inter = result[[4]], squarex = result[[5]],
           n = result[[6]])
  dyn.unload(c)
  return(out)
}
file1 = sapply(1:12, function(i) paste0("/home/data/NYCTaxis/trip_data_", i, ".csv"))
file2 = sapply(1:12, function(i) paste0("/home/data/NYCTaxis/trip_fare_", i, ".csv"))

cl = makeCluster(12, "FORK")
t = system.time(result <- parLapplyLB(cl, 1:12, function(x) regsimp(ccode, file1[x], file2[x]) ) )
stopCluster(cl)
summ = sapply(1:5 , function(i) sum(unlist(lapply(result,"[[",i ))))
beta = (summ[3] - summ[1]*summ[2]/summ[5]/summ[5])/( summ[4] - (summ[1]/summ[5])^2   )
#[1] 3.420213
alpha = summ[2]/summ[5] - beta * summ[1]/summ[5]
#[1] 762.2698

save.image("result.rda")
