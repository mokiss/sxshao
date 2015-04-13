#Read fILE

setwd('/Users/Shawn/Dropbox/242/proj/Assignment1/data')
flist = list.files()

#define a function to read file and close the connection, in case of warnings
re = function( f){
  #f is the file name
  con = file(f, open = 'r')
  txt = readLines(con)
  close(con)
  return(txt)
}

#put all 24 files txt in alltxt, so alltxt is a large list which contain 24 elements
alltxt = sapply( 1:length(flist), function(i) re(flist[i]))
#relate the elements(txt) with orignal file names
names(alltxt) = flist

#take a look at how "===" locates
for (i in 1:length(flist)){print(alltxt[[i]][9])}
#not all 8th line is "==="

#find which line begins with "=="
#bounindex is a list of 24 elements, 
#and each element is a int representing the ?th line of "=="
bounindex = sapply( 1:length(flist), function(i) grep('=', alltxt[[i]]))


#alltxt[[15]][1:20]
#flist[15]
#> flist[15]
#[1] "women10Mile_2001"
#the file is different because it doesn't have header
#get the pattern of width of columns, then use read.fwf

#strlocate will give the distance between patterns in the string
strlocate = function(string, pattern){
  sp = gregexpr( pattern, string)
  l = nchar(string)
  #first idenfity if there is pattern in string
  if (length(sp) != 0 && sp[[1]] > 0) {
    #loc contain every locations of pattern in the string
    loc = as.numeric(sp[[1]])
    tmp = loc
    n = length(loc)
    loc[2:n] = tmp[2:n] - tmp[1:n-1]
    #if there are another characters following the last pattern in the string
    #eg "PATTERN dsdf"
    #then the last distance should be increased to cover the following characters
    if (tmp[n] != l) loc[n+1] = l-loc[n]     
  }
  else loc = NA
  #if there is no pattern in the string, return "NA"
  #return value is either "NA" or a vector contain numbers
  return(loc)
}

#wid is a list contain 24 elements , each elements contain the width we need to put in read.fwf
wid = sapply(1:length(alltxt), function(i) strlocate(alltxt[[i]][bounindex[[i]]], " {1,}") )

#wid&bounindex has NA in 15th at this step, because the file doesn't contain header
#find the wid of 15th

getwid15 = function( txt15, start){
  #start is the line which the header should have be 
  pattern15 = "[0-9] {1,}"
  loc15 = gregexpr( pattern15 , txt15[start])
  loc15 = as.numeric(loc15[[1]])
  tmp = loc15
  loc15[4:5] = tmp[3:4]
  loc15[6] = tmp[4]
  loc15[3] = loc15[3] - 3
  loc15[5] = loc15[5] - 7
  loc15[7] = nchar(txt15[start])
  wid15 = loc15
  tmp = loc15
  wid15[2:7] = tmp[2:7] - tmp[1:6]
  return(wid15)
}
bounindex[[15]] = grep("    1", alltxt[[15]])[1] + 1
wid[[15]] = getwid15(alltxt[[15]], bounindex[[15]] - 1)

#8th&20th file, the "===" dosen't split the the varible Hometown and Net tim
#find the wid of 8th
tmp = wid[[8]]
wid[[8]][6] = tmp[6] - 8
wid[[8]][7] = 8
wid[[8]][8:10] = tmp[7:9]
rm(tmp)
#find the wid of 20
tmp = wid[[20]]
wid[[20]][6] = tmp[6] - 8
wid[[20]][7] = 8
wid[[20]][8:10] = tmp[7:9]
rm(tmp)

#dat is a large list of 24 elements, and each element is the data.frame
dat = sapply(1:length(flist), 
             function(i) read.fwf(flist[i], widths = wid[[i]] ,
                                  comment.char = '', skip = bounindex[[i]] - 2, stringsAsFactors = F))

#Warning messages:
#1: In readLines(file, n = thisblock) :
#  incomplete final line found on 'men10Mile_2006'
#2: In readLines(file, n = thisblock) :
#  incomplete final line found on 'men10Mile_2008'
#1: In readLines(file, n = thisblock) :
#  incomplete final line found on 'women10Mile_2004'
#2: In readLines(file, n = thisblock) :
#  incomplete final line found on 'women10Mile_2008'
names(dat) = flist

#tableHeader is trying to give the header of a table
tableHeader = function(table, p, t){
  #p is the row number of header, if there is no header then p=0
  #t are the row numbers needed to be dropped 
  if( p != 0){
    name = as.character(unlist(table[p,]))
    #delete the blanket
    name = sapply(1:length(name), function(i) gsub(' ', '' , name[i]) )
    #transfer all the names into lower case
    name = sapply(1:length(name), function(i) tolower(name[i]))
    colnames(table) = name 
  }
  #delete certain rows
  if( t !=0) table = table[-t, ]
  return(table)
}

#since no header in 15th file, so p of 15th should be 0
pv = rep(1,length(dat))
pv[15] = 0
#so, there is no need to delte row in 15th file, so the t of 15th should be 0
tv = lapply(1:length(dat), function(i) c(1,2))
tv[[15]] = 0

#dat is refined with all data.frame has their header
dat = sapply(1:length(dat), function(i) tableHeader(dat[[i]], pv[i], tv[[i]] ))
save.image("~/Dropbox/242/proj/Assignment1/data/AS1.RData")

#===========
test1 = lapply(1:length(dat), function(i) colnames(dat[[i]]))
#however 20th ,11th still have blank in colnames, it may be caused by the txt type
test2 = dat[[11]]
colnames(test2) = sapply(1:ncol(test2), function(i) gsub(' ', '' , colnames(test2)[i]) )
colnames(test2)
#===========

#define the colname by myself LOL
colnames(dat[[11]]) = c('place', 'div/tot', 'num', 'name', 'age', 'hometown', 'guntim', 'nettim', 'pace')
colnames(dat[[15]]) = c('place', 'tot', 'name', 'age', 'hometown', 'guntim', 'nettim')

#need to split div/tot into two columns
#val is a list of 24, each element contains the colnames of that dataframe
val = sapply(1:length(dat), function(i) colnames(dat[[i]]))
#divtot is a list of 24, each element contains the index of div/tot column
#if there is no div/tot, then 0
divtot = sapply( 1:length(val), function(i) which(val[[i]] == 'div/tot'))

#define a function to divide column
divCol = function(table, col, seg, name1, name2){
  #col is the index of column needed to be partitioned
  #seg is the segment sign in that column
  #name1 and name2 are the colnames of new columns
  if (length(col) != 0){
    n = nrow(table)
    div1 = sapply(1:n, function(i) strsplit(as.character(table[i,col]), seg)[[1]][1] )
    div2 = sapply(1:n, function(i) strsplit(as.character(table[i,col]), seg)[[1]][2] )
    table_t = cbind(table[,-col], div1, div2)
    cn = ncol(table_t)
    colnames(table_t)[c(cn-1 , cn)] = c(name1, name2)
  }
  else table_t = table
  
  return(table_t)
}

#then parition it in every dataframe in dat
dat = sapply(1:length(val), function(i) divCol(dat[[i]], divtot[[i]], '/', 'div', 'tot'))
save.image("~/Dropbox/242/proj/Assignment1/data/AS1.RData")

#checkheader return overall levels of colnames in the list which contain table frames
checkheader = function(list){
  #list: a list contains data.frames
  t = sapply(1:length(list), function(i) colnames(list[[i]]))
  t = unlist(t)
  n = levels(factor(t))
  return(n)
}

checkheader(dat)
#some headers have the same meaning but different value

transf = function(data, old, newname){
  #if the data has some colnames which is contain in old,
  #then change them into corresponding ones in newname

  #old, newname  are vectors contain characters
  #tmp : index of colnames of data which is also in old one
  #tmp2: index of old 
  tmp = colnames(data) %in% old
  tmp2 = old %in% colnames(data)
  if (sum(tmp) != 0) colnames(data)[tmp] = newname[tmp2]
  return(data)
}


dat = sapply(1:length(dat), function(i) 
  transf(dat[[i]], 
         c('ag',    '5mi',   'gun',   'net',     'guntim', 'nettim', 'time'),
         c('age', '5mile','guntime','nettime'  ,'guntime','nettime' , 'guntime') )  )
checkheader(dat)
#the colnames are cleaned


#deal with the # * in some time variables
#target contain the potential variables which may have # and *
target = c("10km","5mile", "guntime", "nettime", "pace", "pace.1", "pace.2", "split" )

mark = function(table, range, signal){
  #range is the colunman index we are looking at 
  #signal is a special remark we want to specifiy: #,*
  coln = colnames(table)
  tmp = coln %in% range
  #then the colnames we need to deal with are coln[tmp] 
  #colv is a dataframe which only contain coln[tmp] colnums
  colv = table [coln[tmp] ]
  #sig contains the logical value which identify whether there is signal in that coln
  sig = apply(colv, 2, grepl , pattern = signal) 
  #sigindex is the index of colnum which have signal
  sigindex = which(apply(sig, 2, sum) != 0 )
  if(length(sigindex) !=0){
    #newcolnum is a colnum which indicate if this obs has signal
    newcolnum = sig[, sigindex]
    table_t = cbind(table, newcolnum)
    colnames(table_t)[ncol(table_t)] =paste0('signal',signal)
    #table_t has a new logical coln which indicate the signal appearance
    #then delete the signal in the coln[tmp]
    unchangev = table_t[coln[tmp][sigindex]]
    change = sapply(unchangev, function(i) gsub( signal, '', i))
    table_t[coln[tmp][sigindex]] = change
  }
  else table_t = table
  return(table_t)
}

dat1 = lapply(dat, function(i) mark(i, target, "#"))
dat1 = lapply(dat1, function(i) mark(i, target, "[*]"))

#deal with the time stuff: conver them into seconds
#->0km,5mile,guntim, nettime,pace,pace.1,pace.2,

toSeconds = function(time){
  time = as.character(time)
  time = gsub("[^0-9:]", '', time)
  tmp = strsplit(time, ":")
  #check how many : in time, then we can know if there is hour, minute, second
  tmp = as.numeric(tmp[[1]])
  n = length(tmp)
  if (n == 2) sum(tmp*c(60 , 1))->time
  if (n == 3) sum(tmp*c(3600 , 60 , 1))->time
  return(time)
}

toSecondsv = function(timev){
  #timev is a vector containing times needed to be formatted
  timev = sapply(timev, function(i) toSeconds(i))
  return(timev)
}

toSecondstable = function(table, colname){
  #colname is a vector that contain potential time col names in the table
  coln = colnames(table)
  tmp = coln %in% colname
  #coln[tmp] are the colnames
  #sec are the dataframe that contains those transfered colnums
  sec = apply(table[coln[tmp]] , 2, toSecondsv)
  table[coln[tmp]] = sec
  return(table)
}

dat2 = sapply(dat1, function(i) toSecondstable(i, target))
names(dat2) = flist
save.image("~/Dropbox/242/proj/Assignment1/AS1.RData")




