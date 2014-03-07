library(reshape)
library(data.table)
library(ggplot2)

#Wgranie estymacji 
reference.source<-read.csv2("D:/Pentacomp/ATMedia/ForecastR/SourceData/Forecast_Chan5_Tagr2_2013.csv", header=TRUE, sep=";", dec=",")
names(reference.source)<-c("date", "startSecond", "endSecond", "GRP.Forecast")
                   
reference.source$date<- as.Date(as.character(reference.source$date),"%Y%m%d")
reference.DT<-data.table(reference.source)


calendar.DT.2013<-subset(calendar.DT, date>='2013-01-01' & date<'2014-01-01')
s<-calendar.DT.2013[, .N, by=list(date)]

calendar.DT.2013<-calendar.DT.2013[,c("date", "startSecond"), with=FALSE]

#audience table 

setkeyv(calendar.DT.2013 , c("date", "startSecond"))
setkeyv(reference.DT, c("date", "startSecond"))


#basetable  (calendar)

reference.DT<-reference.DT[calendar.DT.2013,roll=TRUE]
reference.DT[startSecond>endSecond,"GRP.Forecast"]<-0
reference.DT[is.na(GRP.Forecast), "GRP.Forecast"]<-0
reference.DT[GRP.Forecast==0.000001, "GRP.Forecast"]<-0


reference.daily<- reference.DT[,list(GRP.Forecast=mean(GRP.Forecast)),by="date"]
save(reference.DT, reference.daily, file=("reference.dat"))





