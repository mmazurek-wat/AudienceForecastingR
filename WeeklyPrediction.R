

# weekly charts -----------------------------------------------------------


#dane uczace - detekcja  trendu roczna 

#tymczasowo zamienimy numer tygodnia 
cal[,weekno:=dayOfYear%/%7]


#na wykresie przedstawiamy zmiany roczne wzgledem sredniej 
aud.weekly <- aud[cal][,list(GRP=mean(GRP)),by=c("year", "weekno")]
aud.weekly<-aud.weekly[,year:=paste("Y",as.character(year), sep="")]
#aud.weekly<-aud.weekly[,GRP.avg.year:=mean(GRP), by=c("year")]
x<-cast(aud.weekly, weekno~year, value="GRP")




x.df<-data.frame(x)


# zakomentowany kod sluzy do standaryzacji zmiennych 

#x.df$avg<-rowMeans(subset(x.df, select=Y2008:Y2013))
#x.df<-cbind(x.df[,"weekno"], apply(x.df[,2:7], 2, scale))
#colnames(x.df)[1]<-"weekno"
#x<-data.table(x.df)




aud.weekly<-melt(x, id="weekno")
setnames(aud.weekly, "variable", "year")
setnames(aud.weekly, "value", "GRP")

aud.weekly<-data.table(aud.weekly)
aud.weekly[,avg:=mean(GRP), by="weekno" ]

aud.weekly<-melt(aud.weekly, id=c("weekno", "year"))
setnames(aud.weekly, "value", "GRP")

#wybor lat
#aud.weekly<-data.table(aud.weekly)[year %in% c("Y2008", "Y2013"), ]




p<-ggplot(aud.weekly, aes(x=weekno, y=GRP,   col=variable)) + geom_point() + geom_line()+facet_grid(year ~ . ) 
p<-p+scale_x_continuous(breaks = seq(1,54,by=1))
p
