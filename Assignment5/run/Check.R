#need to check medallion,hack_license,vendor_id
#the first three columns
library(parallel)
setwd("/home/sxt999/run")
rm(list = ls())
cname = "check"
compile = paste0("R CMD SHLIB ", cname,".c")
ccode = paste0(cname,".so")
system(compile)
check = function(c, f1, f2) {
  dyn.load(c)
  col1 = c(0,1,2,5)
  col2 = c(0,1,2,3)
  result = .C("check",as.character(f1),as.character(f2),as.integer(6),
     as.integer(4),as.integer(col1),as.integer(col2), as.integer(0))[[7]]
  dyn.unload(c)
  return(result)
}
file1 = sapply(1:12, function(i) paste0("/home/data/NYCTaxis/trip_data_", i, ".csv"))
file2 = sapply(1:12, function(i) paste0("/home/data/NYCTaxis/trip_fare_", i, ".csv"))

cl = makeCluster(12, "FORK")
t = system.time( result <- parLapplyLB(cl, 1:12, function(x) check(ccode, file1[x], file2[x]) ) )
stopCluster(cl)
save.image("checkresult.rda")

