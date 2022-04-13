require(GGally)
library(ggplot2)    
library(ggfortify)  
library(forecast)   
library(xts)       
library(ggcorrplot) 
library(openxlsx)                      
library(zoo)
library(dplyr)
library(lubridate)



adjust<-function(data_path,column_name){
    df_name<-read.xlsx(data_path)
    df_name$Date <-as.Date(as.yearmon(x = df_name$Date))
    colnames(df_name)[2] <- column_name
    return(df_name)
    }

house_prices=adjust("EVDS_housepr.xlsx","house_price")
exchange_rate=adjust("EVDS_exchange.xlsx","exchange_rate")
price_index=adjust("EVDS_cpi.xlsx","price_index")



all_data = merge(house_prices,merge(exchange_rate,price_index,by="Date"),by="Date")
str(all_data)

data_ts <- xts(x = all_data[,-1], order.by = all_data$Date)
glimpse(data_ts)

ggplot(exchange_rate,aes(x=Date,y=exchange_rate)) +  geom_line()+
  xlab("Time") + ylab("USD Buying Price ($/TL)")+ ggtitle("USD Buying Price from 01-2015 to 12-2021")+
  scale_x_date(date_breaks = '3 month', date_labels = '%Y %b', date_minor_breaks = '3 month' ) +
  theme(axis.text.x=element_text(angle=90,hjust=1, vjust = 1.4))

ggplot(exchange_rate,aes(x=month(Date),y=exchange_rate)) + geom_bar(stat='identity') +geom_smooth(color='red',linetype='solid',fill=NA)+ facet_wrap(~year(Date)) + 
xlab('Years') + ylab('USD Buying Price ($/TL)') + ggtitle('USD Buying Price from 01-2015 to 12-2021')+theme(axis.text.x=element_text(angle=60, hjust=1.8, vjust = 1.2))

ggplot(exchange_rate,aes(x=factor(year(Date)),y=exchange_rate))+ geom_boxplot(aes(fill=factor(year(Date))))+ 
  xlab("Years") + ylab("USD Buying Price($/TL)")+ ggtitle(" Box Plot of USD Buying Price from 01-2015 to 12-2021")+
  theme(legend.position = "none")

ggplot(price_index,aes(x=Date,y=price_index)) + geom_line(color = 'black') + 
  
    labs(title = 'Consumer Price Index(General) from 01-2015 to 12-2021', x = 'Time', y = 'Consumer Price Index') +
    scale_x_date(date_breaks = '3 month', date_labels = '%Y %b', date_minor_breaks = '3 month' ) +
    theme(axis.text.x=element_text(angle=90,hjust=1, vjust = 1.4))

    


ggplot(price_index,aes(x=month(Date),y=price_index)) + geom_bar(stat='identity') +geom_smooth(color='red',linetype='solid',fill=NA)+ facet_wrap(~year(Date)) + 
xlab('Years') + ylab('Consumer Price Index') + ggtitle('Consumer Price Index(General) from 01-2015 to 12-2021')

ggplot(price_index,aes(x=factor(year(Date)),y=price_index))+ geom_boxplot(aes(fill=factor(year(Date))))+ 
  xlab("Years") + ylab('Consumer Price Index')+ ggtitle('Box Plot for Consumer Price Index(General) from 01-2015 to 12-2021')
 

ggplot(house_prices,aes(x=Date,y=house_price)) + geom_line(color = 'black') + 
  
    labs(title = 'Housing Unit Prices for Turkey from 01-2015 to 12-2021', x = 'Time', y = 'Housing Prices for Turkey - TL/sq m') +
    scale_x_date(date_breaks = '3 month', date_labels = '%Y %b', date_minor_breaks = '3 month' ) +
    theme(axis.text.x=element_text(angle=90,hjust=1, vjust = 1.4))

ggplot(house_prices,aes(x=month(Date),y=house_price)) + geom_bar(stat='identity') +geom_smooth(color='red',linetype='solid',fill=NA)+ facet_wrap(~year(Date)) + 
xlab('Years') + ylab('Housing Prices for Turkey - TL/sq m') + ggtitle('Housing Unit Prices for Turkey from 01-2015 to 12-2021')

ggplot(house_prices,aes(x=factor(year(Date)),y=house_price))+ geom_boxplot(aes(fill=factor(year(Date))))+ 
  xlab("Years") + ylab('Housing Prices for Turkey - TL/sq m')+ ggtitle('Box PLot for Housing Unit Prices for Turkey from 01-2015 to 12-2021')

trend_df<-read.csv("gold_trends.csv")
trend_df$Ay <-as.Date(as.yearmon(x = trend_df$Ay))
colnames(trend_df) <-c("Date",'search_index')


ggplot(trend_df,aes(x=Date,y=search_index)) + geom_line(color = 'black') + 
  
    labs(title = 'Search frequency for "gold"  covering 01-2015 to 12-2021', x = 'Time', y = 'Serach Frequency for "gold"') +
    scale_x_date(date_breaks = '3 month', date_labels = '%Y %b', date_minor_breaks = '3 month' ) +
    theme(axis.text.x=element_text(angle=90,hjust=1, vjust = 1.4))

ggplot(trend_df,aes(x=month(Date),y=search_index)) + geom_bar(stat='identity') +geom_smooth(color='red',linetype='solid',fill=NA)+ facet_wrap(~year(Date)) + 
xlab('Years') + ylab('Serach Frequency for "gold" ') + ggtitle('Search frequency for "gold" covering 01-2015 to 12-2021')

ggplot(trend_df,aes(x=factor(year(Date)),y=search_index))+ geom_boxplot(aes(fill=factor(year(Date))))+ 
  xlab("Years") + ylab('Serach Frequency for "gold"')+ ggtitle('Search frequency for "gold" covering 01-2015 to 12-2021')

par(mfrow=c(2,2))
plot(x = all_data$exchange_rate,y = all_data$price_index,
     main = "USD price vs.CPI",
     xlab = "USD price ($/TL)", ylab = "CPI", col="darkred")
plot(x = all_data$exchange_rate,y = all_data$house_price,
     main = "USD price vs. Unit Housing price",
     xlab = "USD price ($/TL)", ylab = "Unit housing price (tl/m^2)", col="darkgreen")
plot(x = all_data$price_index,y = all_data$house_price,
     main = "CPI vs. Unit Housing price ",
     xlab = "CPI", ylab = "Unit housing price (tl/m^2)",col = "navyblue")
plot(x = all_data$exchange_rate,y = trend_df$search_index,
     main = "USD price vs.Search Frequency",
     xlab = "USD price ($/TL)", ylab = "Search Frequency", col="darkred")
plot(x = trend_df$search_index,y = all_data$house_price,
     main = "Search Frequency vs. Unit Housing price",
     xlab = "Search Frequency", ylab = "Unit housing price (tl/m^2)", col="darkgreen")
plot(x = all_data$price_index,y = trend_df$search_index,
     main = "CPI vs. Search Frequency ",
     xlab = "CPI", ylab = "Search Frequency",col = "navyblue")


normalize<-function(data_frame){
    col<-colnames(data_frame)
    dummy<-data_frame
    for (i in 1:3){
        dummy[,col[i]]=(dummy[,col[i]]-min(dummy[,col[i]]))/(max(dummy[,col[i]])-min(dummy[,col[i]]))

    }
    return (dummy)
}


colors = c("darkgoldenrod","blue","deeppink4")
plot(x = normalize(data_ts),ylab = "Rate ", main = "CPI, USD Buying Price & Unit Housing Price vs.Time",
     col = colors, lwd = c(2,2,3),legend.loc = "topleft", minor.ticks = "months",
     grid.ticks.on = "quarter", grid.ticks.lty = "dotted")


merged=merge(all_data,trend_df,by="Date")
ggpairs(merged[,c(2:5)])


ggcorrplot(corr = cor(merged[,c(2:5)]),


           type = "upper", lab = TRUE,
           title = "Correlation Matrix",
           colors = c("skyblue4","snow","violet"),
           legend.title = "Correlation",)
