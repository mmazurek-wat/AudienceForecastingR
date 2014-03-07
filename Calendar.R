library(timeDate)
library(chron)
library(data.table)



#generacja kalendarza 
t1 <- chron("2008-01-01", "00:00:00", format=c(dates="Y-m-d", times="h:m:s"))
t2 <- chron("2013-12-31", "23:59:59", format=c(dates="Y-m-d", times="h:m:s"))
calendar<-data.frame(datetime=seq(t1, t2, by = times("00:01:00")))
head(calendar)


calendar$hour = hours(calendar$datetime)
calendar$minute = minutes(calendar$datetime)
calendar$second = seconds(calendar$datetime)
calendar$startSecond =(calendar$hour) * 3600 +  calendar$minute * 60 +  calendar$second 


calendar$date=as.Date(calendar$datetime)
calendar$timeDateTrunc=as.timeDate(calendar$date)
calendar$timeDate = calendar$timeDateTrunc + calendar$startSecond


calendar$dayOfYear =  dayOfYear(calendar$timeDate)
calendar$dayOfWeek =  dayOfWeek(calendar$timeDate)

calendar$year = years(calendar$datetime)
calendar$month = months(calendar$datetime)
calendar$day = days(calendar$datetime)


calendar$weekno <- strftime(as.POSIXlt(calendar$date) ,format="%W")  
calendar$year <- strftime(as.POSIXlt(calendar$date) ,format="%Y")  

head(calendar)

tail(calendar)


calendar.DT<-data.table(calendar[,c("date", "startSecond",  "dayOfYear", "dayOfWeek", "month", "day","hour", "minute", "weekno", "year") ])
setkeyv(calendar.DT, c("date", "startSecond"))
calendar.keys.DT<-data.table(calendar[,c("date", "startSecond")])
setkeyv(calendar.keys.DT, c("date", "startSecond"))



#wylaczamy z zestawienia wszystkie swieta 


holiday_var<-c("AshWednesday", "GoodFriday", "EasterSunday",  "EasterMonday", "CorpusChristi" )
holiday_const<-c("NewYearsDay", "Epiphany","LaborDay","AssumptionOfMary", "AllSaints", "FRArmisticeDay", "ChristmasEve","ChristmasDay", "BoxingDay" ,"DENewYearsEve")

holiday_all<-append(holiday_var, holiday_const)

#wektor swiat (daty w posczegolnych latach)



for (h in 1:length(holiday_all)){
 if (h==1){
   ht<-data.frame(date=holiday(c(2008:2013),holiday_all[1]), holiday.name=holiday_all[1])
   names(ht)[1] <-"date"
 } else {
    tmp<-data.frame(date=holiday(c(2008:2013),holiday_all[h]), holiday.name=holiday_all[h])
    names(tmp)[1]<-"date"
    ht<-rbind(ht, tmp) 
 } 
}

ht[,"date"]<- as.Date(ht[,"date"])

holidays.DT<-data.table(ht)

setkey(holidays.DT, "date")
setkey(calendar.DT, "date")

head(holidays.DT[,date])
head(calendar.DT[,date])

#zlaczanie tabel wg kolumny daty 
holidays.DT[calendar.DT]

head(holidays.DT)
head(calendar.DT)
calendar.DT<-holidays.DT[calendar.DT]
calendar.DT[,IsRegular:=as.numeric(is.na(calendar.DT$holiday.name))]
head(calendar.DT)

holiday(2008, "AshWednesday")


calendar.DT.regular<-subset(calendar.DT, is.na(calendar.DT$holiday.name))
head(calendar.DT.regular)

save(calendar.DT, holidays.DT, calendar.DT.regular,  file="calendar.DT.dat")


#check 
s<-calendar.DT[, .N, by=list(weekno, year)]
s1<-calendar.DT[,x:=.N,by=date]
#select distinct 
s2<-unique(s1[,list(date,month,dayOfWeek, year, weekno)])
s2[1:20]

s22<-s2[, .N, by=list(weekno, year)]


