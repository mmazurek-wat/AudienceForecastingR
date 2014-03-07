library(data.table)
library(ggplot2)
library(Metrics)
library(forecast)




load ("audience.dat")
audience.regular<-audience.regular[dayOfWeek != 0 ]
audience.mean<-audience.regular[, mean(GRP), by=c("dayOfWeek,hour,month")]
setnames(audience.mean,"V1", "meanGRP")

head(audience.mean)
audience.regular<-audience.regular[dayOfWeek != 0 ]
audience.week.mean<-audience.regular[, mean(GRP), by=c("weekno", "year")]
setnames(audience.week.mean,"V1", "meanGRP")


#rozne kolory 
ggplot(audience.week.mean, aes(x=weekno, y=meanGRP, colour=year)) +  geom_point(aes(group=1))

ggplot(audience.week.mean, aes(x=weekno, y=meanGRP)) +  geom_line(aes(group=1)) + facet_wrap(~ year)

#wykres sumy tygodniowej 


# Wykres macierzowy ogladalnosci 
#dwa warianty
ggplot(audience.mean, aes(x=hour, y=meanGRP)) + geom_bar(stat="identity") +
  facet_grid( month  ~dayOfWeek)

ggplot(audience.mean, aes(x=hour, y=meanGRP)) + stat_summary(fun.y = "mean", fill = "grey50", geom = "bar") +
  facet_grid(month  ~dayOfWeek)

#tylko dla dni 
ggplot(audience.mean, aes(x=hour, y=meanGRP)) + stat_summary(fun.y = "mean", fill = "grey50", geom = "bar") +
  facet_wrap(~ dayOfWeek)


library(Metrics)
#prognozowanie 

head(audience.regular)

audience.forecast<-audience.regular[,c("date", "startSecond", "GRP"), with=FALSE ]
tables()
head(audience.forecast)
head(audience.regular)


#training data set 

forecast.average1<- audience.training[, mean(GRP), by=list(dayOfWeek,hour,month)]
setnames(forecast.average1,"V1", "GRP.forecast")
head(forecast.average1)


setkeyv(audience.test, c( "month", "dayOfWeek", "hour"))
setkeyv(forecast.average1, c( "month", "dayOfWeek", "hour"))

ff<-audience.test[forecast.average1]
head(ff)
ff<-ff[,c("date",  "GRP", "GRP.forecast"), with=FALSE ]
rmse(ff[,GRP], ff[,GRP.forecast])

#0.1249 


forecast.average2<- audience.training[, mean(GRP), by=list(dayOfWeek,hour)]
setnames(forecast.average2,"V1", "GRP.forecast")



setkeyv(audience.test, c(  "dayOfWeek", "hour" ))
setkeyv(forecast.average2, c(  "dayOfWeek", "hour"))

ff<-audience.test[forecast.average2]
head(ff)
ff<-ff[,c("date",  "GRP", "GRP.forecast"), with=FALSE ]
rmse(ff[,GRP], ff[,GRP.forecast])

#0.1223

forecast.average3<- audience.training[, mean(GRP), by=list(hour)]
setnames(forecast.average3,"V1", "GRP.forecast")



setkeyv(audience.test, c(  "hour"))
setkeyv(forecast.average3, c(  "hour"))

ff<-audience.test[forecast.average3]
head(ff)
ff<-ff[,c("date",  "GRP", "GRP.forecast"), with=FALSE ]
rmse(ff[,GRP], ff[,GRP.forecast])



#0.1327

forecast.average4<- audience.training[, mean(GRP), by=list(month,dayOfWeek,startSecond)]
setnames(forecast.average4,"V1", "GRP.forecast")


setkeyv(audience.test, c(  "month", "dayOfWeek","startSecond"))
summary(audience.test)
setkeyv(forecast.average4, c(  "month", "dayOfWeek", "startSecond"))


ff<-audience.test[forecast.average4] 
head(ff)


ff[is.na(GRP.forecast)]

ff<-ff[,c("date",  "GRP", "GRP.forecast"), with=FALSE ]
rmse(ff[,GRP], ff[,GRP.forecast])

#0.1268  - dokladnosc co do minuty 


forecast.average5<- audience.training[, mean(GRP), by=list(weekno,dayOfWeek,hour)]
setnames(forecast.average5,"V1", "GRP.forecast")


setkeyv(audience.test, c( "weekno", "dayOfWeek","hour"))
setkeyv(forecast.average5, c(  "weekno", "dayOfWeek", "hour"))


ff<-audience.test[forecast.average5,,nomatch=0 ] 
head(ff)


forecast.average5[is.na(GRP.forecast)]

ff<-ff[,c("date",  "GRP", "GRP.forecast"), with=FALSE ]
rmse()

accuracy(ff[,GRP.forecast], ff[,GRP])

#sprawdzenie unikalnosci 
s2<-unique(forecast.average5[,list(dayOfWeek,weekno)])
s3<-unique(audience.test[,list(dayOfWeek,weekno)])
 
#dayOfWeek, weekno , hour -> 0.131713


