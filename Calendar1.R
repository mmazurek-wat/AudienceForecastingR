
library(timeDate)
library(chron)
library(data.table)

##
## Skrypt generuje obiekty kalendarza : minutowego oraz dziennego 
## ctim:  kalendarz minutowy  
## cal:   kalendarz dzienny 
##

#generacja kalendarza minutowego 

t1 <- chron("2008-01-01", "00:00:00", format=c(dates="Y-m-d", times="h:m:s"))
t2 <- chron("2013-12-31", "23:59:59", format=c(dates="Y-m-d", times="h:m:s"))
ctim<-data.table(datetime=seq(t1, t2, by = times("00:01:00")))

ctim[,date:=as.Date(datetime)]
ctim[,hour:= hours(datetime)]
ctim[,minute:= minutes(datetime)]
ctim[,second:= seconds(datetime)]
ctim[,startSecond:= hour*3600+minute*60+second]

ctim


##kalendarz dzienny 
Sys.setlocale("LC_TIME","English")  #nazwy dni i miesiecy angielskie 

cal<-data.table(date=seq(as.Date("2008-01-01"), as.Date("2013-12-31"), by = "days"))
#cal[,year:=years(date)]  #factors 
cal[,year:=as.numeric(format(date, "%Y"))]
cal[,month:=as.numeric(format(date, "%m"))]
cal[,monthName:= months(date)]
cal[,weekno:=as.numeric(format(date, "%W")) ]
cal[,dayOfYear:=dayOfYear(as.timeDate(date))]
cal[,dayOfWeek:=format(date,"%w")]
cal[,weekDayShort:=dayOfWeek(as.timeDate(date))]
cal[,weekDay:=weekdays(date)]
cal[,dayOfMonth:=(days(date))] #ordered factor
cal[,dayOfMonth:=as.numeric(format(date,"%d"))] #ordered factor






##oznaczenie swiat 

#lista swiat zmiennych oraz stalych 
holiday_var<-c("AshWednesday", "GoodFriday", "EasterSunday",  "EasterMonday", "CorpusChristi" )
holiday_fix<-c("NewYearsDay", "Epiphany","LaborDay","AssumptionOfMary", "AllSaints", "FRArmisticeDay", "ChristmasEve","ChristmasDay", "BoxingDay" ,"DENewYearsEve")

hvar<-data.table(holidayName=holiday_var, fixed=0)
hfix<-data.table(holidayName=holiday_fix, fixed=1)
hlist<-rbind(hvar,hfix)
years<-data.table(year=c(2008:2013))

h<-data.table(k=1,hlist,key="k")[ data.table(k=1,years,key="k"), allow.cartesian=TRUE][,k:=NULL]

for (i in 1:nrow(h)){
h[i,date:=as.Date(holiday(h[i,year], h[i,holidayName]))]  
}



##zlaczenie listy swiat z kalendarzem dziennym

setkey(cal, date)
cal<-h[,list(date, holidayName, fixed), key="date"][cal][,date.1:=NULL]
cal[,holiday:=as.numeric(!is.na(holidayName))]

cal

save(cal, ctim, file="calendar1.dat")



