# load 

load("reference.DT.dat")
laod("audience.dat")

rmse(audience.test[,GRP], reference.DT[,GRP.Forecast])
accuracy(reference.DT[,GRP.Forecast], audience.test[,GRP])




#blad bezwzgledny 
ae(audience.test[,GRP], reference.DT[,GRP.Forecast])[1:20]

audience.test[,GRP] - reference.DT[,GRP.Forecast]
setkeyv(audience.test,  c("date", "startSecond"))
benchmark<-audience.test[reference.DT]
benchmark[,ae:=ae(GRP,GRP.Forecast)]

#transpozycja 
b<-melt(benchmark,measure.var=c("GRP", "GRP.Forecast"),  id.var=c("date", "weekno", "month", "hour", "startSecond"), variable_name="measure")
head(b)
# Wykres macierzowy ogladalnosci 
#dwa warianty
ggplot(benchmark, aes(x=hour, y=ae)) + stat_summary(fun.y = "sum", fill = "grey50", geom = "bar") + stat_summary(fun.y = "sum", fill = "red", geom = "bar") +
  facet_grid(month  ~dayOfWeek)


#dwa warianty
ggplot(benchmark, aes(x=hour, y=value)) + stat_summary(fun.y = "sum", fill = "grey50", geom = "bar") + stat_summary(fun.y = "sum", fill = "red", geom = "bar") +
  facet_grid(month  ~dayOfWeek)

ggplot(b, aes(x=hour, y=value, fill=measure)) + geom_bar(, position="dodge", stat="identity") +   facet_grid(month  ~dayOfWeek)



#average prediction by week 
weekno.average<-benchmark[,j=list(mean(GRP), mean(GRP.Forecast)),by=c("weekno") ]
setnames(weekno.average,"V1", "GRP.Actuals")
setnames(weekno.average,"V2", "GRP.Forecast.Reference")

weekno.average<-melt(weekno.average, measure.var=c("GRP.Actuals", "GRP.Forecast.Reference"),  id.var=c("weekno"), variable_name="measure")
ggplot(weekno.average, aes(x=weekno, y=value, group=measure, color=measure)) + geom_line() + geom_point()+ggtitle("GRP Forecast in 2013") + theme_bw()




b<-melt(benchmark,measure.var=c("GRP", "GRP.Forecast"),  id.var=c("date", "weekno", "month", "hour", "startSecond"), variable_name="measure")

#rozne kolory 
ggplot(b, aes(x=weekno, y=value, colour=measure)) +  geom_line(aes(group=1),stat="identity")


#average prediction by day 
day.average<-benchmark[,j=list(mean(GRP), mean(GRP.Forecast)),by=c("date") ]



setnames(day.average,"V1", "GRP.Actuals")
setnames(day.average,"V2", "GRP.Forecast.Reference")

accuracy(day.average[,GRP.Forecast.Reference], day.average[,GRP.Actuals])
#         ME        RMSE    MAE  
#Test set 0.01575308 0.03815065 0.02739637 8.368439 22.62631

mean(day.average$GRP.Actuals) #0.1186462
mean(day.average$GRP.Forecast.Reference) #0.1186462


day.average<-melt(day.average, measure.var=c("GRP.Actuals", "GRP.Forecast.Reference"),  id.var=c("date"), variable_name="measure")
ggplot(day.average, aes(x=date, y=value, group=measure, color=measure)) + geom_line() + geom_point()+ggtitle("GRP Forecast in 2013") + theme_bw()





