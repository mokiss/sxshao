#test two regression on parallel
library(parallel)
rm(list = ls())
setwd("/home/sxt999/run")
cname = "regtwo"
compile = paste0("R CMD SHLIB ", cname,".c")
ccode = paste0(cname,".so")
system(compile)
y = c(10,11)
x = c(9,7)
regtwo = function(c, f1, f2, yindex, xindex) {
  dyn.load(c)
  #yindex = c(10,11)
  #xindex = c(9,7)
  result = .C("regtwo",as.character(f1),as.character(f2),as.integer(yindex), as.integer(xindex),
              as.numeric(rep(0,3)), as.numeric(rep(0,2)), as.numeric(0), as.numeric(rep(0,2)), as.integer(0))
  out = result[[5]]
  out[4:5] = result[[6]]
  out[6] = result[[7]]
  out[7:8] = result[[8]]
  out[9] = result[[9]]
  names(out) = c("x1", "x2", "y","x1^2", "x2^2","x1*x2","x1*y","x2*y","n")
  dyn.unload(c)
  return(out)
}
file1 = sapply(1:12, function(i) paste0("/home/data/NYCTaxis/trip_data_", i, ".csv"))
file2 = sapply(1:12, function(i) paste0("/home/data/NYCTaxis/trip_fare_", i, ".csv"))

cl = makeCluster(12, "FORK")
t = system.time(
  result <- parLapplyLB(cl, 1:12, function(i) regtwo(ccode, file1[i], file2[i], y, x) ) )
stopCluster(cl)

summ = sapply(1:9 , function(i) sum(unlist(lapply(result,"[[",i ))))


beta1 = (summ[5]*summ[7] - summ[6]*summ[8])/(summ[4]*summ[5] - summ[6]^2 )
beta2 = (summ[4]*summ[8] - summ[6]*summ[7])/(summ[4]*summ[5] - summ[6]^2 )
alpha = summ[3]/summ[9] - beta1 * summ[1]/summ[9] - beta2 * summ[2]/summ[9]

save.image("resulttwo.rda")

