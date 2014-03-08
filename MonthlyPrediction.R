

library(data.table)
library(ggplot2)
library(Metrics)
library(forecast)
library(xts)
library(lubridate)
library(reshape)



# weekly charts -----------------------------------------------------------


#dane uczace - detekcja  trendu roczna 

#tymczasowo zamienimy numer tygodnia 

#dane testowe 
actuals.monthly<-aud.tst[cal][year==2013,list(Actuals=mean(GRP)),by="month"]
ref.monthly<-ref[cal][year==2013,list(Forecast=mean(GRP.Forecast)),by="month"]



#dane referencyjne 
ref.month <- ref[cal][year==2013,list(GRP=mean(GRP.Forecast)),by=c("year", "month")]
ref.month<-ref.month[,year:=paste("Y",as.character(year), sep="")]
ref.month[,variable:="forecast"]


#na wykresie przedstawiamy zmiany roczne wzgledem sredniej 
aud.month <- aud[cal][,list(GRP=mean(GRP)),by=c("year", "month")]
aud.month<-aud.month[,year:=paste("Y",as.character(year), sep="")]
#aud.weekly<-aud.weekly[,GRP.avg.year:=mean(GRP), by=c("year")]
x<-cast(aud.month, month~year, value="GRP")

x.df<-data.frame(x)


# zakomentowany kod sluzy do standaryzacji zmiennych 

#x.df$avg<-rowMeans(subset(x.df, select=Y2008:Y2013))
#x.df<-cbind(x.df[,"weekno"], apply(x.df[,2:7], 2, scale))
#colnames(x.df)[1]<-"weekno"
#x<-data.table(x.df)




aud.month<-melt(x, id="month")
setnames(aud.month, "variable", "year")
setnames(aud.month, "value", "GRP")

aud.month<-data.table(aud.month)
aud.month[,avg:=mean(GRP), by="month" ]

aud.month<-melt(aud.month, id=c("month", "year"))
setnames(aud.month ,"value", "GRP")

#wybor lat
#aud.weekly<-data.table(aud.weekly)[year %in% c("Y2008", "Y2013"), ]



aud.month<-rbind(aud.month,ref.month)

p<-ggplot(aud.month, aes(x=month, y=GRP,   col=variable)) + geom_point() + geom_line()+facet_grid(year ~ . ) 
p<-p+scale_x_continuous(breaks = seq(1,12,by=1))
p



#blad forecastu dla granulacji miesiecznej 
accuracy(ref.month$GRP, actuals.monthly$Actuals  )
##
##                 ME      RMSE        MAE      MPE     MAPE
##Test set 0.01571411 0.01672454 0.01571411 13.10191 13.10191


#blad dla sredniej miesiecznej ... gorszy nie  referencyjna prognoza 
aud.trn.m<-aud.trn[cal] [year %in% c(2009, 2010, 2011,2012),list(GRP=mean(GRP)), by=c("month")]
accuracy(aud.trn.m$GRP, actuals.monthly$Actuals  )



#uwzglednienie sezonowosci 

aud.trn.m <-aud.trn[cal] [year<2013, list(GRP1=mean(GRP)) , by=c("year", "month")]
aud.trm.ds<-ts(aud.trn.m$GRP, frequency=12, start=c(2008,1))
plot(aud.trm.ds)


fit<-tbats(aud.trm.ds,seasonal.periods=c(12), use.trend=TRUE,use.parallel=TRUE)
plot(fit)
accuracy(forecast(fit,12),actuals.monthly$Actuals )
plot(forecast(fit,h=365))



#lepszy rezultat niż dla danych referencyjnych 
fit<-tslm(aud.trm.ds~trend+season)
accuracy(forecast(fit,12),actuals.monthly$Actuals)
plot(forecast(fit,h=12))
accuracy(forecast(fit,12), actuals.monthly$Actuals)

##                        ME       RMSE        MAE       MPE      MAPE      MASE      ACF1
##Training set -4.634911e-19 0.02263067 0.01906269 -2.932183 14.555071 0.6630523 0.8114066
##Test set     -1.409542e-02 0.01409542 0.01409542 -8.803547  8.803547        NA        NA




