library(data.table)
library (plyr)
library(ggplot2)


##cal 
load("calendar1.dat")


##wczytanie danych szczegolowych 
weather.src<-read.csv2("D:/Pentacomp/ATMedia/ForecastR/SourceData/weather.csv", header=TRUE, sep=",")
wt<-data.table(weather.src)

wcount<-wt[, .N, by=year]
wcount<-wt[, .N, by=weather]
wcount$weather<- reorder(wcount$weather, -wcount$N)

#histogram: czestosc wystepowania warunkow pogodowych 

bar_chart <- ggplot(wcount, aes(x=weather, y=N)) +
  geom_bar(stat="identity") +
  xlab("") +
  ylab("Frequencies")   +
  theme(axis.text.x=element_text(angle=90, hjust=1))

bar_chart



## agregujemy wszystkie pozostale stany pogody z wylaczeniem Clear, ograniczamy do warunkow pogodowych 
## w ciagu dnia 



wt[weather!="Clear", weather:="Cloudy"]
wt.day<-wt[hour %between% c(6,22 )]
wt.day[,nc:=as.numeric((weather=="Clear"))]



w<-wt.day[,list(AvgClear=mean(nc),clearWeather=round(mean(nc))), by=c("year", "month", "day")]
w[,date:=as.Date(ISOdate(year, month, day))]
w<-w[,c("date","clearWeather"), with=FALSE]



##aktualizacja kalendarza dziennego 

setkey(w,date)
calw<-cal[w]

save(calw, file="calendarw.dat")

