library(data.table)
library(ggplot2)
library(Metrics)
library(forecast)
library(xts)
library(lubridate)
library(reshape)


##cal, ctim, 
load("calendar1.dat")
load("calendarw.dat")

## aud, ref, atim, rtim 

load(file="audience.dat")


##calendard with weather



##tables()

# Data partition ----------------------------------------------------------


##dane uczace 
aud.trn<-subset(aud,  date<'2013-01-01')  #ciag uczacy 
aud.tst<-subset(aud,  date>='2013-01-01') #ciag testowy 



# weekly charts -----------------------------------------------------------

actuals.weekly<-aud.tst[cal][year==2013,list(Actuals=mean(GRP)),by="weekno"]
ref.weekly<-ref[cal][year==2013,list(Forecast=mean(GRP.Forecast)),by="weekno"]






# exploratory analysis ----------------------------------------------------

#daily.ts<-ts(aud.trn$GRP,frequency=7, start=c(2008,1))
#w pakiecie ts podajemy czestotliwosc danych w odniesieniu do roku 
daily.ts<-ts(aud.trn$GRP, frequency=365.25 ,start=c(2008,1))
tsdisplay(daily.ts)  ##wykres dla ACF i PACF 


acf.output<-acf(aud.trn$GRP,  plot=TRUE)
pacf.output<-pacf(aud.trn$GRP,  plot=TRUE,lag.max=400)

spectrum(aud.trn$GRP, method=c("ar"))
spectrum(aud.trn$GRP, method=c("pgram"),plot=TRUE, detrend=TRUE, fast=TRUE)

decompose(aud.trn$GRP, type=c("additive"))

spec.pgram(aud.trn$GRP,spans=NULL, plot=TRUE, detrend=TRUE, fast=TRUE)

fit<-tbats(daily.ts,seasonal.periods=c(7,365.25), use.trend=TRUE,use.parallel=TRUE)
plot(fit)
fit$season1[1:7]


daily.ts<-ts(aud.trn$GRP,frequency=c(7,365.25), start=c(2008,1))

decomp<-decompose(daily.ts, type="multiplicative")
plot(decomp$trend)
#seasonal component 
decomp$seasonal[1:7]
decomp$seasonal[366:385]
Arima(daily.ts)


# reference forecast ------------------------------------------------------


##ocena dane referencyjne 
accuracy(ref$GRP.Forecast, aud.tst$GRP)
fref<-accuracy(ref$GRP.Forecast, aud.tst$GRP)[1,c("ME", "RMSE", "MAE", "MPE", "MAPE")]


##                 ME       RMSE        MAE      MPE     MAPE
##Test set 0.01575308 0.03815065 0.02739637 8.368439 22.62631



#time series 






# Average level -----------------------------------------------------------


forecast<-subset(aud.trn,   year(date)==2011, "GRP")[,mean(GRP)]
avg<-accuracy(forecast, aud.tst$GRP)[1,c("ME", "RMSE", "MAE", "MPE", "MAPE")]




# tslm --------------------------------------------------------------------

##daily forecast with period =365 (odniesienie do zeszlego roku)
daily.ts<-ts(aud.trn$GRP,frequency=365, start=c(2008,1))
fit<-tslm(daily.ts~trend+season)
accuracy(forecast(fit,365), aud.tst$GRP)
plot(forecast(fit,h=365))


#wykres dla zagregowanych danych tygodniowych 
f<-data.table(tslm=x<-as.numeric(forecast(fit,h=365)$mean), date=seq( as.Date("2013-01-01"), length.out=365, by=1), key="date")
f<-f[cal][year==2013, list(tslm=mean(tslm)), by=c("weekno")]

fw<-cbind(actuals.weekly, ref.weekly, f)
fwm<-melt(fw,id="weekno")
p<-ggplot(fwm, aes(x=weekno, y=value, col=variable)) + geom_line()




tslm365<-accuracy(forecast(fit,365), aud.tst$GRP)[2,c("ME", "RMSE", "MAE", "MPE", "MAPE")]


ds<-(rbind(ds, ref.weekly[,model:="Forecast"]))



daily.ts<-ts(aud.trn$GRP,frequency=7, start=c(2008,1))
fit<-tslm(daily.ts~trend+season)
plot(forecast(fit,h=365))
tslm7<-accuracy(forecast(fit,365), aud.tst$GRP)[2,c("ME", "RMSE", "MAE", "MPE", "MAPE")]







#tylko trend 
daily.ts<-ts(aud.trn$GRP, start=c(2008,1))
fit<-tslm(daily.ts~trend)
accuracy(forecast(fit,7), aud.tst$GRP)
plot(forecast(fit,h=365))
tslm<-accuracy(forecast(fit,365), aud.tst$GRP)[2,c("ME", "RMSE", "MAE", "MPE", "MAPE")]


# tbats -------------------------------------------------------------------



#fit<-stl(daily.ts,s.window="periodic")

daily.ts<-ts(aud.trn$GRP, start=c(2008,1))
fit<-tbats(daily.ts,seasonal.periods=c(7,365.25), use.trend=TRUE,use.parallel=TRUE)
plot(fit)
fit$parameters  #do wyjasnienia ? 

accuracy(forecast(fit,365), aud.tst$GRP)
plot(forecast(fit,h=365))
tbats<-accuracy(forecast(fit,365), aud.tst$GRP)[2,c("ME", "RMSE", "MAE", "MPE", "MAPE")]



f<-data.table(tbatslm=x<-as.numeric(forecast(fit,h=365)$mean), date=seq( as.Date("2013-01-01"), length.out=365, by=1), key="date")
f<-f[cal][year==2013, list(tbats=mean(tbats)), by=c("weekno")]

fw<-cbind(actuals.weekly, ref.weekly, f)
fwm<-melt(fw,id="weekno")
p<-ggplot(fwm, aes(x=weekno, y=value, col=variable)) + geom_line()
p<-p+scale_x_continuous(breaks = seq(1,52,by=1)) 
p<-p+geom_point()
p
# dshw  -------------------------------------------------------------------


#dshw
#Returns forecasts using Taylor's (2003) Double-Seasonal Holt-Winters method.
#poniewaz period 1 oraz period 2 musza byc zagniezdzone rok jest zaokroglony ? 
fit<-dshw(ts(aud.trn$GRP, start=c(2008,1)), period1=7, period2=364, h=365)
dshw<-accuracy(forecast(fit,365), aud.tst$GRP)[2,c("ME", "RMSE", "MAE", "MPE", "MAPE")]

# glm ---------------------------------------------------------------------



aud.trn<-aud.trn[subset(calw, date<'2013-01-01')]
aud.tst<-aud.tst[subset(cal, date>='2013-01-01')]



x<-glm(GRP~weekDayShort+monthName+holiday+clearWeather,gaussian,aud.trn, )  #przyklad glm 
summary(x)

lm<-lm(GRP~holiday+clearWeather,data=aud.trn, )  #rownowazne sformulowanie dla modelu regresji 
summary(lm)

glm<-accuracy(predict(x, aud.tst), aud.tst$GRP)[1,c("ME", "RMSE", "MAE", "MPE", "MAPE")]
summary(x)


predict(x, aud.tst)



f<-data.table(tbatslm=x<-as.numeric(forecast(fit,h=365)$mean), date=seq( as.Date("2013-01-01"), length.out=365, by=1), key="date")
f<-f[cal][year==2013, list(tbats=mean(tbats)), by=c("weekno")]

fw<-cbind(actuals.weekly, ref.weekly, f)
fwm<-melt(fw,id="weekno")
p<-ggplot(fwm, aes(x=weekno, y=value, col=variable)) + geom_line()
p<-p+scale_x_continuous(breaks = seq(1,52,by=1)) 
p<-p+geom_point()
p



# models comparison -------------------------------------------------------




score<-rbind(tslm365,tslm7, tslm,  avg, fref,tbats, glm, dshw)
sct<-data.table(score, model = row.names(score))

sct<-data.table(melt(data.table(sct), "model", variable_name="vars"))
sct<-sct[vars %in% c("MAE","RMSE"),]

bar_chart <- ggplot(sct, aes(x=model, value, fill=vars)) +
  geom_bar(stat="identity", position="dodge") +
  xlab("") +
  ylab("Mean Absolute Error")   +
  theme(axis.text.x=element_text(angle=90, hjust=1))

bar_chart


sct










audience.training.daily[,s1:=date+weeks(-52)]
audience.training.daily[,s2:=date+weeks(-104)]
audience.training.daily[,s3:=date+weeks(-156)]
audience.training.daily[,s3:=date+weeks(-208)]











audiece.test.daily.forecast<-audience.test.daily[reference.daily]
audiece.test.daily.forecast$GRP.TBATS<-as.vector(fc$mean)


audiece.test.daily.forecast$weekno <- strftime(as.POSIXlt(audiece.test.daily.forecast$date) ,format="%W") 

audiece.test.daily.forecast.week<-audiece.test.daily.forecast[,list(GRP.Forecast=sum(GRP.Forecast),GRP.Actual=sum(GRP.Actual), GRP.TBATS=sum(GRP.TBATS)), by="weekno"]
m<-melt(audiece.test.daily.forecast.week, "weekno")


ggplot(m,aes(weekno,value,color=variable))+geom_line(aes(colour=variable, group=variable))+geom_point()

  


length(audience.test.daily$GRP.Actual)
accuracy(as.vector(fc$mean),  audience.test.daily$GRP.Actual)
#                          ME        RMSE    MAE  
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

