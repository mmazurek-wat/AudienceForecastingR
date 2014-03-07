library(data.table)
library(ggplot2)
library(Metrics)
library(forecast)
library(xts)
library(lubridate)

library(zoo)


#Predykcja dla dni "powszednich" z zastosowaniem modelu regresji 

#zaladowanie calej historii 
#audience.DT
load ("audience.dat")
load("reference.dat")
load("weather.dat")
load("calendar.DT.dat")


holidays.DT[,c("A", "B"):=list(WeekForward=date+weeks(-1),WeekBackward=date+weeks(+1)) ]
holidays.DT.copyA<-copy(holidays.DT)
holidays.DT.copyB<-copy(holidays.DT)


  
#swieta wypadakace tydzien przed 
setkey(holidays.DT.copyA, A)
setkey(holidays.DT, date)



#swieta wypadakace tydzien przed 
setkey(holidays.DT.copyB, B)
setkey(holidays.DT, date)


#do zbioru A dolaczane wszystkie swieta 
holidays.DT<-holidays.DT[holidays.DT.copyB]
holidays.DT[,list(date.1, holiday.name.1, holiday.name, date, A.1)]
holidays.DT[is.na(holiday.name)==FALSE, date:=A.1]
holidays.DT<-holidays.DT[,list(date=date.1, holiday.name=holiday.name.1, subst=date)]



holidays.DT


#blad referencyjny dla danych regularnych 
audience.test.daily<-audience.test[,list(GRP=mean(GRP)),by="date"]
accuracy(reference.daily$GRP.Forecast, audience.test.daily$GRP)
setkey(holidays.DT, date)

reference.daily.regular<-holidays.DT[reference.daily]
reference.daily.regular<-reference.daily.regular[is.na(holiday.name)]
audience.test.daily.regular<-holidays.DT[audience.test.daily]
audience.test.daily.regular<-audience.test.daily.regular[is.na(holiday.name)]
accuracy(reference.daily.regular$GRP.Forecast, audience.test.daily.regular$GRP)

#koniec testu 




#agregacja dzienna 
audience.training.daily<-audience.training[,list(GRP = mean(GRP)),by="date"]

#dolaczenie danych z 2013 roku (testowe) - bedzie to potrzebne do wyznaczenia srednich 
audience.training.daily<-rbind(audience.training.daily, audience.test.daily)



#zastapienie GRP z dni swiatecznych najblizszymi danymi  nieswiatecznymi  odpowiadajacymi  dniom tygodnia 

setkey(audience.training.daily, date)
audience.training.daily<-holidays.DT[audience.training.daily]
audience.training.daily1<-copy(audience.training.daily[,list(date,GRP)])
setkey(audience.training.daily1, date)

setkey(audience.training.daily, subst)

audience.training.daily<-audience.training.daily1[audience.training.daily,]
audience.training.daily[,GRP.adj:=ifelse(is.na(holiday.name), GRP.1, GRP)]
audience.training.daily<-audience.training.daily[,list(date.1, GRP.adj)]
setkey(audience.training.daily, date.1)
setnames(audience.training.daily, "date.1", "date")

#dodajemy srednia kroczaca 

audience.training.daily$GRP.avg<-rollapply(audience.training.daily$GRP, 7, mean, partial=TRUE)
#TODO: pierwsze dwa rekordy maja za krotka srednia 




#srednia kroczoca sprzed roku 
audience.training.daily[,dummy:=1]
audience.training.daily[,s0:=date+weeks(-52)]
setkey(audience.training.daily, dummy)

audience.training.daily1<-copy(audience.training.daily)
setkey(audience.training.daily1, dummy)


audience.training.daily1 <-merge(audience.training.daily1,audience.training.daily[,list(date,GRP.adj, GRP.avg, dummy),], allow.cartesian=TRUE)[s0==date.y]
setnames(audience.training.daily1,"date.x", "date")
setnames(audience.training.daily1, "GRP.adj.x", "GRP.adj")
setnames(audience.training.daily1, "GRP.avg.x", "GRP.avg")
setnames(audience.training.daily1, "GRP.avg.y", "GRP.avg.ly")
audience.training.daily1[,date.y:=NULL]
audience.training.daily1[,GRP.adj.y:=NULL]
audience.training.daily1[,s0:=NULL]
audience.training.daily1[,dummy:=NULL]

audience.training.daily1<-audience.training.daily1[date<"2013-01-01",]
audience.training.daily1<-audience.training.daily1[date>="2009-01-01",]



setnames(audience.training.daily1,"GRP.y",paste("GRP.adj.ly",sx[ci],sep="."))
setnames(audience.training.daily1,"date.x","date")
setnames(audience.training.daily1,"GRP.adj.x","GRP.adj")
audience.training.daily1[,date.y:=NULL]




#dodajemy 



  
#daty sprzed roku 
audience.training.daily1[,s:=date+weeks(-4)]
audience.training.daily1[,sly:=s+weeks(-52)]

audience.training.daily1[,s0:=date+weeks(-12)]
audience.training.daily1[,s0ly:=s0+weeks(-52)]


# srednia kroczaca 



#przesuniecie dat ze wzgledu na swieta 
sx<-c("s_3", "s_2", "s_1", "s0", "s1", "s2","s3")

for  (ci in 1:length(sx)) {
  q=quote(sx[ci])
  setkeyv(audience.training.daily, sx[ci] )
  audience.training.daily<-holidays.DT[audience.training.daily]
  
  audience.training.daily[,eval(q):=as.Date(ifelse(is.na(holiday.name),date,subst))]
  audience.training.daily<-audience.training.daily[,list(date.1, GRP, s_3, s_2, s_1, s0, s1, s2,s3)]
  setnames(audience.training.daily,"date.1","date")
    
}

audience.training.daily.2009 <- audience.training.daily[date>"2008-12-31",]

audience.training.daily.2009[1:10]
audience.training.daily[,dummy:=1]
setkey(audience.training.daily, dummy)

save(audience.training.daily, file="d://Rtmp.dat")


load("d://Rtmp.dat")
audience.training.daily1<-copy(audience.training.daily)
#audience.training.daily1<-audience.training.daily1[date=="2009-01-05", ]
#audience.training.daily[date=="2008-01-10", ]

for  (ci in 1:length(sx)) {

#for  (ci in 1:2) {
  px<-parse(text=paste0(sx[ci],"==date.y"))
  audience.training.daily1<-merge(audience.training.daily1,audience.training.daily[,list(date,GRP,dummy),], allow.cartesian=TRUE)  [eval(px),]
  setnames(audience.training.daily1,"GRP.y",paste("GRP",sx[ci],sep="."))
  setnames(audience.training.daily1,"date.x","date")
  setnames(audience.training.daily1,"GRP.x","GRP")
  audience.training.daily1[,date.y:=NULL]
}

audience.training.daily1[, GRP.avg:=(GRP.s_3 +  GRP.s_2 + GRP.s_1 +GRP.s0 +  GRP.s1 + GRP.s2 +  GRP.s3)/7 ]

audience.training.daily1<-audience.training.daily1[,list(date, GRP, GRP.avg)]

#usuwamy dni swiateczne 

#dodajemy pogode 


# model regresji dla dni roboczych w oparciu o dane zeszloroczne 





#wzor
load("d://Rtmp.dat")
audience.training.daily2<-merge(audience.training.daily1,audience.training.daily[,list(date,GRP,dummy),], allow.cartesian=TRUE)  [s2==date.y]

setnames(audience.training.daily2,"GRP.y","GRP.s2")
setnames(audience.training.daily2,"date.x","date")
setnames(audience.training.daily2,"GRP.x","GRP")
audience.training.daily2[,date.y:=NULL]




q=quote(sx[1])  
eval(q)

px<-parse(text=paste0(sx[1],"==date.y"))

x<-"a"
y<-"b"
x<-
x


DT <- data.table(n=c("a", "a", "a", "a", "a", "a", "b", "b", "b"),
                 t=c(10, 20, 33, 40, 50, 22, 25, 34, 11),
                 v=c(20, 15, 16, 17, 11, 12, 20, 22, 10)
)

DT[,dummy:=1]
setkey(DT, dummy)

merge(DT,DT, allow.cartesian=TRUE, all.x=TRUE,) 


#inner join 


DT[DT,,allow.cartesian=TRUE]

#outer join 

setkey(audience.training.daily, date)
audience.training.daily[date=audience.training.daily$GRP]

audience.training.daily



#dane za zeszly rok mamy od 2009. 

audience.training.daily[date>="2009-01-01", ]









# dla kazdej daty uzupelniamy o 7 dni z ostatniego roku  z wylaczeniem swiat 

X = data.table(grp=c("a","a","b","b","b","c","c"), foo=1:7)

X[grp=='a',foo:=ifelse(is.na(foo),2,foo)]

str(audience.training.daily)
data.table
library(TTR)



audience.training.daily[,date1:=date+year(-52)+days(-1)]

audience.training.daily[date,GRP]
copy1<-copy(audience.training.daily)
copy1[,date1:=date+weeks(-52)]
setkey(copy1, date1)

audience.training.daily[copy1]

audience.training.daily[,s2:=audience.training.daily[date+weeks(-52)]$GRP]




audience.training.daily[,s3:=date+weeks(-156)]
audience.training.daily[,s3:=date+weeks(-208)]

decomp<-decompose(daily.ts, type="multiplicative")
plot(decomp$trend)
#seasonal component 
decomp$seasonal[1:7]
decomp$seasonal[366:385]
Arima(daily.ts)






#fit<-stl(daily.ts,s.window="periodic")
fit<-tbats(daily.ts,seasonal.periods=c(7,365.25), use.trend=TRUE,use.parallel=TRUE)
fc<-forecast(fit,h=365)
plot(fit)
plot(fc)
length(fc$mean)



audiece.test.daily.forecast<-audience.test.daily[reference.daily]
audiece.test.daily.forecast$GRP.TBATS<-as.vector(fc$mean)


audiece.test.daily.forecast$weekno <- strftime(as.POSIXlt(audiece.test.daily.forecast$date) ,format="%W") 

audiece.test.daily.forecast.week<-audiece.test.daily.forecast[,list(GRP.Forecast=sum(GRP.Forecast),GRP.Actual=sum(GRP.Actual), GRP.TBATS=sum(GRP.TBATS)), by="weekno"]
m<-melt(audiece.test.daily.forecast.week, "weekno")


ggplot(m,aes(weekno,value,color=variable))+geom_line(aes(colour=variable, group=variable))+geom_point()




length(audience.test.daily$GRP.Actual)
accuracy(as.vector(fc$mean),  audience.test.daily$GRP.Actual)
#         ME        RMSE    MAE  
#Reference: Test set 0.01575308 0.03815065 0.02739637 8.368439 22.62631
#slt                -0.02791112 0.05681413 0.04677303 -40.50989 50.04092
#HoltWinters         -0.0525491 0.07271848 0.062604 -





#training.day.average<-audience.training[,j=mean(GRP),by=c("date", "year", "month", "dayOfYear", "weekno", "dayOfWeek") ]
training.day.average<-audience.training[,j=mean(GRP),by="date" ]
setnames(training.day.average,"V1", "GRP.Actual")
head(training.day.average)
#Przepisanie poprzedniego roku 
#Przepisanie srednich dziennych

xts1<-xts(training.day.average$GRP.Actual, order.by=training.day.average$date)
daily.ts<-ts(training.day.average$GRP.Actual,frequency=365, start=c(2008,1))
daily.ts
plot.ts(daily.ts)
tsdisplay(daily.ts)
library(TTR)
plot.ts(SMA(daily.ts, n=10))
plot(decompose(daily.ts))
decomp<-decompose(daily.ts)
decomp$seasonal[1:20]
decomp$seasonal[366:385]
Arima(daily.ts)


#fit<-stl(daily.ts,s.window="periodic")

#                           ME        RMSE       MAE        MPE
#Reference: Test set  0.01575308  0.03815065 0.02739637    8.368439 22.62631
#slt                 -0.02791112  0.05681413 0.04677303   -40.50989 50.04092
#HoltWinters         -0.0525491   0.07271848 0.062604     -64.53329 69.02079
#TBATS               -0.008857698 0.03839071 0.02887339   -16.30545 28.06681
#BATS                -0.01053064  0.04914884 0.03990372   -19.89316 39.09791
ME       RMSE        MAE       MPE     MAPE
Test set 

#podwója sezonowość 


