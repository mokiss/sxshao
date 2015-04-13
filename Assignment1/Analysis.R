#1.1Data Profile
va = checkheader(dat2)
va = matrix(va, nrow = 3, ncol = 6, byrow = T)
#number of runner
runnern = sapply(dat2, function(i) nrow(i))
runntable = cbind(runnern[1:12], runnern[13:24])
colnames(runntable) = c("Men", 'Women')
rownames(runntable) = c(1999:2010)
sumn = apply(runntable, 1, sum)
runntable = cbind(runntable, total = sumn)


for (i in 1:length(dat2)){
  dat2[[i]]$guntime = as.numeric(dat2[[i]]$guntime)
  dat2[[i]]$age = as.numeric(dat2[[i]]$age)
}
#median guntime in a  year
guntime = sapply(dat2, function(i) median(i$guntime, na.rm = T))
#max, min age in a year
agemax = sapply(dat2, function(i) max(i$age, na.rm = T))
agemin = sapply(dat2, function(i) min(i$age, na.rm = T))
guntimetable = cbind(guntime = guntime, year = c(1999:2010,1999:2010), sex = c(rep(1,12),rep(0,12) ),
                     maxa = agemax, mina = agemin)
guntimetable = as.data.frame(guntimetable)
guntimetable$sex = factor(guntimetable$sex)
levels(guntimetable$sex) = c('Male', 'Femal')

library(ggplot2)
library(RColorBrewer)
#plot median guntime over year
ggplot(guntimetable, aes(x = year, y = guntime, group = sex, color = sex))+ 
  geom_line(size = 1) +
  labs(title = "Median Guntime in each Year", x = 'Year', color = 'Sex')

#plot agemin, max over year
ggplot() + geom_line( aes(x = year, y = maxa ), guntimetable, colour = "red", size = 1) +
  geom_line( aes(x = year, y = mina), colour = "lightblue" ,size = 2, guntimetable)+ 
  facet_wrap(~ sex) + labs(title = "Ages of Runners in each Year", x = 'Year', y = "Age")

#2Hometown
placev = lapply(dat2, function(i) i$place)
gunv = lapply(dat2, function(i) i$guntime)
placevv = unlist(placev)
gunvv = unlist(gunv)
hometown = lapply(dat2, function(i) i$hometown)
homeinfo = data.frame(place = as.numeric(placevv),gun =as.numeric(gunvv), hometown = unlist(hometown))
#homeinfo is a dataframe contain all the runners guntime, place, and theri hometown

#clean hometown variable ( delete "blanks")
homeinfo$hometown = sapply(homeinfo$hometown, function(i) gsub("^[^a-zA-Z]|[^a-zA-Z]{2,}", "", i))
#levels(factor(homeinfo$hometown))


#then start to get state from homeinfo

getPattern = function(string, pattern) {
  #getPattern return the substring of those pattern in the string
  t = regmatches(string, regexpr(pattern, string))
  t = gsub(" ", "", t)
  #if there is no pattern in the string, then return "Unknown"
  if(length(t) == 0) t= 'Unknown'
  return(t)
}

getState = function(table){
  #get state name from hometown variable
  sapply(1:nrow(table), function(i) getPattern(table$hometown[i], " [A-Z][A-Z]"))
  #return a chr [1:34234] 
}

statev = getState(homeinfo)
homeinfo = transform(homeinfo, state = statev)

#get all the abbreviation of usa states from Internet
library(XML)
usastate = readHTMLTable("http://www.50states.com/abbreviations.htm#.VSsXHFxWKfQ", which = 1)
colnames(usastate) = c('state', 'Abb')

#levels(factor(statev)) 
#checkstate is named logical vector, 
#to identify whether those abbrevations we pull out are real state abbrevations
checkstate = sapply(levels(factor(statev)) , function(i) i%in%usastate$Abb)

fakestate = names(which(checkstate == FALSE))

replaceindata = function(ttable, coln, old, newval){
  #replaceindata replace the old value in certain coln of ttable, with newval
  #coln is the index of target colunm 
  #old is the old value, newval is the new value
  ttable[which( tolower(ttable[,coln]) == tolower(old)), coln] = newval
  return(ttable)
}

for (i in 1:length(fakestate)){
  homeinfo = replaceindata(homeinfo, 4, fakestate[i], "Unknown")
}
homeinfo$state = droplevels(homeinfo$state)
levels(homeinfo$state) 


#total number of runners from each state
##first delete those obs which doesn't have state
state.na = which(homeinfo$state == "Unknown")
statecount = summary(droplevels(homeinfo$state[-state.na] ))
statecounttable = data.frame( count = as.numeric(statecount), state = names(statecount))
#find top 5 numers of runners of states 
topindex = order(statecounttable$count, decreasing = T)[1:5]
top = rep( "Other",nrow(statecounttable))
top[topindex] = c(1:5)
statecounttable = cbind(statecounttable, top = top )
#statecounttable[topindex,]
#count state top
#49 40212    VA   1
#23 27014    MD   2
#10 20198    DC   3
#41  3315    PA   4
#37  3083    NY   5

#plot it 
ggplot(statecounttable , aes( x = state, y = count, fill = factor(top), order = desc(top)))  + 
  geom_bar(stat = 'identity') + coord_flip() + scale_fill_brewer(palette = "Spectral") +
  labs( x = 'State', fill = 'Top State')

#get the top10 runners
top10 = homeinfo[which( homeinfo$place >=1 & homeinfo$place <= 10 ), ]
#total number of top 10 runners from each state
statecount_10 = summary(top10$state)
statecount_10 = statecount_10[which (names(statecount_10) != "Unknown")]
#the rate of top10 runners from each state
state_10_rate = statecount_10/statecount
round(state_10_rate[which(names(state_10_rate) %in% c('VA','MD','DC','PA','NY'))] , digit = 5)


#plot the detail top10 runners of each state
ggplot(stateplace_10 , aes(factor(state), fill = factor(place), order = desc(place) )) + 
  geom_bar() + 
  scale_fill_brewer() + 
  labs(title = "Top 10 Runners from Different States", x = 'State', fill = 'Rank')


#analysis based on country
#delete the data in 2006, so all the US cities have their state
homeinfo2 = homeinfo[! grepl("2006", rownames(homeinfo) )  ,]
state.na2 = homeinfo2$state == "Unknown"
homeinfo2.nonusa = homeinfo2[state.na2, ]
summary( factor(homeinfo2.nonusa$hometown ))

#clean the country value
oldv = c("Eth",      "Fra",     "Ken",  "Rus", "Tornoto",     "Berlin", "Bri",
         "Mex",    "Rom",        "Us" ,         "Usa",  "London"  ,  "Naruto" ,
         "Scarboroontario", "Toronto", "Uhwiesen" , "Vienna A", "Wellington",
         "AUS", "Baldock Hertfor UK ","Berlin GE", "CAN", "GER", "JAP","Rep Of S.africa",
         "COL","Toronto ON")
newv = c("Ethiopia" , "France", "Kenya", "Russia", "Canada", "Germany", "United Kingdom",
         "Mexico", "Romania", "United States", "United States", "United Kindom", "Japan",
         "Canada",         "Canada", "Switzerland" , "Austria", "New Zealand",
         "Australia", "United Kingdom", "Germany", "Canada", "Germany", "Japan", "RSA",
         "Colombia", "Canada")

#Personally, i think if a person is from Naruto... we can locate Japan for him/her.

for (i in 1:length(oldv)){
  homeinfo2.nonusa = replaceindata( homeinfo2.nonusa, 3, oldv[i], newv[i])
}

levels(factor(homeinfo2.nonusa$hometown ))

tmp = homeinfo2.nonusa$hometown %in%
  c("Australia" , "Austria" , "Brazil", "Canada", "Colombia", "Denmark" , "Ethiopia",
    "France", "Germany", "Holland", "Japan", "Jordan", "Kenya", "Koengen", "Mexico",
    "Morocco", "New Zealand", "Romania", "Russia", "Switzerland", "Tanzania", "Ukraine",
    "United Kingdom", "Uruguay" )

homeinfo2.nonusa  = homeinfo2.nonusa[tmp, ]
test = data.frame(summary( factor(homeinfo2.nonusa$hometown)))
top10foreign = homeinfo2.nonusa[which(homeinfo2.nonusa$place %in% c(1:10)), ]
summary(factor(top10foreign$hometown))

#plot the detail top10 runners of each foreign country
ggplot(top10foreign , aes(factor(hometown), fill = factor(place), order = desc(place) )) + geom_bar() + 
  scale_fill_brewer( palette = "Spectral") + coord_flip() +
  labs(title = "Top 10 Runners from Different Foreign Countries", x = 'Country', fill = 'Place')

#analysis Kenya
#Ken, KEN, Kenya
ken = homeinfo[homeinfo$hometown %in% c("Ken", "KEN", "Kenya") , 1:3 ]
year_ken = getPattern(rownames(ken), "[0-9]{4}")
#1 for men, 0 for woman
sex_ken = as.numeric( !grepl("women",rownames(ken)) )
ken = transform(ken, year = as.numeric(year_ken), sex = sex_ken )
ken$sex = factor(ken$sex)
levels(ken$sex) = c("Female", "Male")
ggplot(ken, aes(x = year, y = gun, group = sex, color = sex))+ 
  geom_line(size = 1) +
  labs(title = "Guntime of Kenyan Runners in each Year", x = 'Year', color = 'Sex')

countKen = with(ken, table( year, sex ))
countKen = data.frame(countKen)

ggplot(countKen, aes(x = year, y = Freq , group = sex, color = sex))+ 
  geom_line(size = 1) +
  labs(title = "Number of Kenyan Runners in each Year", x = 'Year', color = 'Sex')
