library(timeDate)
library(chron)
library(data.table)

#calendard table 
load("calendar.DT.dat")


calendar.DT.regular<-subset(calendar.DT, is.na(calendar.DT$holiday.name))

#audience table 

load("audience.DT.dat")
head(audience.DT)


setkeyv(calendar.DT, c("date", "startSecond"))
setkeyv(audience.DT, c("date", "startSecond"))


#basetable  (calendar)

audience.basetable<-audience.DT[calendar.DT,roll=TRUE]
audience.basetable[startSecond>endSecond,"GRP"]<-0
audience.basetable[is.na(GRP), "GRP"]<-0


audience.regular<- subset(audience.basetable, is.na(holiday.name))

audience.test<-audience.basetable[date>='2013-01-01' & date<'2014-01-01' & dayOfWeek!=0]
#hourly average  for regular weekdays  in months 
audience.training<-audience.basetable[date<'2013-01-01']
audience.training.regular<-audience.regular[date<'2013-01-01']
audience.test.daily<-audience.test[,list(GRP.Actual=mean(GRP)),by="date"]

save(audience.regular, audience.basetable, 
     audience.test, audience.training, 
     audience.training.regular, 
     file="audience.dat")



