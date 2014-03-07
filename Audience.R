library(timeDate)
library(chron)
library(data.table)


#calendard table   
load("calendar1.dat")  #ctim, cal


##
## Przygotowanie danych dotyczacych ogladalnosci na podstawie danych zrodlowych 
## Skrypt przygotowuje zbiory danych: minutowe (atim) oraz dzienne  (aud)
## zbiory referencyjne  rtim oraz ref 



# Audience ----------------------------------------------------------------


aud.src<-read.csv2("D:/Pentacomp/ATMedia/ForecastR/SourceData/Actuals_CHAN5_TAGR2.csv", header=TRUE, sep=";", dec=",")
names(aud.src)[1]<-"AUDI_Start_Date"       

a<-data.table(aud.src)


a[,date:=as.Date(as.character(AUDI_Start_Date),"%Y%m%d")]
a[,AUDI_Start_Date:=NULL]
setnames(a,"AUDI_Start_Second", "startSecond")
setnames(a,"AUDI_End_Second", "endSecond")
setnames(a,"AUDI_GRP_Actual", "GRP")


#audience table 
setkeyv(ctim, c("date", "startSecond"))
setkeyv(a, c("date", "startSecond"))

#basetable  audience (#join with time calendar)

atim<-a[ctim,roll=TRUE]
atim[startSecond>endSecond,"GRP"]<-0
atim[is.na(GRP), "GRP"]<-0


aud<-atim[,list(GRP=mean(GRP)),by="date"]



# Reference  ----------------------------------------------------------------

ref.src<-read.csv2("D:/Pentacomp/ATMedia/ForecastR/SourceData/Forecast_Chan5_Tagr2_2013.csv", header=TRUE, sep=";", dec=",")
names(ref.src)<-c("date", "startSecond", "endSecond", "GRP.Forecast")


rtim<-data.table(ref.src)
rtim[,date:=as.Date(as.character(date),"%Y%m%d")]

ctim2013<-subset(ctim, date>='2013-01-01' & date<'2014-01-01')  #selekcja 
ctim2013<-ctim2013[,c("date", "startSecond"), with=FALSE]       #projekcja 

setkeyv(ctim2013 , c("date", "startSecond"))
setkeyv(rtim, c("date", "startSecond"))


rtim<-rtim[ctim2013,roll=TRUE]
rtim[startSecond>endSecond,"GRP.Forecast"]<-0
rtim[is.na(GRP.Forecast), "GRP.Forecast"]<-0
rtim[GRP.Forecast==0.000001, "GRP.Forecast"]<-0


ref<- rtim[,list(GRP.Forecast=mean(GRP.Forecast)),by="date"]
save(reference.DT, reference.daily, file=("reference.dat"))





save(aud, ref, atim, rtim ,  file="audience.dat")


# Reference forecast error ------------------------------------------------

aud2013<-subset(aud, date>='2013-01-01' & date<'2014-01-01') 
accuracy(ref$GRP.Forecast, aud2013$GRP)

##                 ME       RMSE        MAE      MPE     MAPE
##Test set 0.01575308 0.03815065 0.02739637 8.368439 22.62631



