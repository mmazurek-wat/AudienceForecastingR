require(quantmod)
require(ggplot2)
require(reshape2)
require(plyr)
require(scales)

# Download audience data 
#audience.basetable
#audience.regular 

load(file="audience.dat")
audience.daily<-audience.basetable[, mean(GRP), by=c("year,month,date,dayOfWeek")]
setnames(audience.daily,"V1", "GRP.actual")

#turn months into ordered facors to control the appearance/ordering in the presentation
audience.daily[, month:=factor(audience.daily$month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)]
audience.daily[, dayOfWeek:=factor(audience.daily$dayOfWeek,levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)]

# the monthweek part is a bit trickier 
# first a factor which cuts the data into month chunks
audience.daily$yearmonth<-as.yearmon(audience.daily$date)
audience.daily$yearmonthf<-factor(audience.daily$yearmonth)
# then find the "week of year" for each day
audience.daily$week <- as.numeric(format(audience.daily$date,"%W"))
# and now for each monthblock we normalize the week to start at 1 
audience.daily<-ddply(audience.daily,.(yearmonthf),transform,monthweek=1+week-min(week))

# Now for the plot
P<- ggplot(audience.daily, aes(monthweek, dayOfWeek, fill = GRP.actual)) + 
  geom_tile(colour = "white") + facet_grid(year~month) + scale_fill_gradient(low="yellow", high="red") +
  labs(title = "Daily Audience") +  xlab("Week of Month") + ylab("")
P