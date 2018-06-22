include(ggplot2)

avcdData_csv <- read.csv("avocado.csv")
avcdData_csv$year <- factor(avcdData_csv$year)
yearlyAverage <- tapply(avcdData_csv$AveragePrice, avcdData_csv$year, FUN=mean)


#yearly data
yearlyAverage_df <- data.frame(year=names(yearlyAverage), average=yearlyAverage)
row.names(yearlyAverage_df) <- NULL
barplot(yearlyAverage, xlab="year", ylab="Price in USD")

ggplot(data=yearlyAverage_df, aes(x=year, y=average)) +
  geom_col()
  

#tri-weekly data
avcdData_csv$Date <- factor(avcdData_csv$Date)
triweeklyAverage <- tapply(avcdData_csv$AveragePrice, avcdData_csv$Date, FUN=mean)
triweeklyAverage_df <- data.frame(Date=names(triweeklyAverage), average=triweeklyAverage)
row.names(triweeklyAverage_df) <- NULL

ggplot(data=triweeklyAverage_df, aes(x=Date, y=average))+
  geom_point()+
  ggtitle("5-year average avocado price by day of the year")+
  xlab("Jan to Dec")+
  ylab("Price USD")+
  theme_classic()+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  ) 