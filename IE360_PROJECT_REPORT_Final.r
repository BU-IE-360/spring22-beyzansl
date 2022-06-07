require(data.table)
require(lubridate)
require(zoo)
require(forecast)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyverse)
library(party)
library(rattle)
library(class)
require(GGally)
library(ggfortify)  
library(xts)       
library(ggcorrplot) 
library(openxlsx)                      
library(zoo)
library(dplyr)
library(stringr)
library(skimr)

todays_date = as.Date("2022-05-26")
weather_file='2022-05-26_weather.csv'
production_file='2022-05-26_production.csv'

weather <- fread(weather_file)
prod <- fread(production_file)

# linear regression for each hour

str(weather)
str(prod)


weather <- dcast(weather, date + hour ~ variable + lat + lon, value.var = "value")

weather$date <- as.Date(weather$date, format = "%m/%d/%Y")
prod$date <- as.Date(prod$date, format = "%Y-%m-%d")

merged <- merge(weather, prod, by = c("date","hour"), all = T)
merged=merged %>% filter( as.Date(date )<=as.Date(todays_date+1))
show_na <-merged[rowSums(is.na(merged)) > 0, ]

merged[,month:=months(as.Date(date))]
for (i in c(0:4,21:23)){
  merged[merged$hour==i]$production=0
}
merged$month=as.factor(merged$month)
merged$hour=as.factor(merged$hour)
days<-unique((as.Date(show_na$date)))




# plot the whole data
ggplot(merged,aes(x=date,y=production)) +  geom_line()+
  xlab("Date") + ylab("solar power production (MWh)")+ ggtitle("Daily Solar Power Production ")+
  scale_x_date(date_breaks = '1 month', date_labels = '%Y %b', date_minor_breaks = '1 month') +
  geom_smooth(color='red',linetype='solid',fill=NA)+theme(axis.text.x=element_text(angle=90,hjust=1, vjust = 0.5))




# plot each hour
for (i in 0:23){
  print(ggplot(merged[merged$hour==i,],aes(x=date,y=production)) +  geom_line()+
          xlab("date") + ylab(i)+ ggtitle("Hourly Solar Power Production")+
          scale_x_date(date_breaks = '1 month', date_labels = '%Y %b', date_minor_breaks = '1 month') +
          geom_smooth(color='red',linetype='solid',fill=NA)+theme(axis.text.x=element_text(angle=90,hjust=1, vjust = 0.5)))}

# missing data imputation
a='production ~ +CLOUD_LOW_LAYER_36.25_33+CLOUD_LOW_LAYER_36.25_33.25+CLOUD_LOW_LAYER_36.25_33.5+CLOUD_LOW_LAYER_36.5_33+CLOUD_LOW_LAYER_36.5_33.25+CLOUD_LOW_LAYER_36.5_33.5+CLOUD_LOW_LAYER_36.75_33+CLOUD_LOW_LAYER_36.75_33.25+CLOUD_LOW_LAYER_36.75_33.5+DSWRF_36.25_33+DSWRF_36.25_33.25+DSWRF_36.25_33.5+DSWRF_36.5_33+DSWRF_36.5_33.25+DSWRF_36.5_33.5+DSWRF_36.75_33+DSWRF_36.75_33.25+DSWRF_36.75_33.5+REL_HUMIDITY_36.25_33+REL_HUMIDITY_36.25_33.25+REL_HUMIDITY_36.25_33.5+REL_HUMIDITY_36.5_33+REL_HUMIDITY_36.5_33.25+REL_HUMIDITY_36.5_33.5+REL_HUMIDITY_36.75_33+REL_HUMIDITY_36.75_33.25+REL_HUMIDITY_36.75_33.5+TEMP_36.25_33+TEMP_36.25_33.25+TEMP_36.25_33.5+TEMP_36.5_33+TEMP_36.5_33.25+TEMP_36.5_33.5+TEMP_36.75_33+TEMP_36.75_33.25+TEMP_36.75_33.5+month'
for (i in 5:20){
  for (j in days[c(-1,-length(days))]){
    df=merged %>% filter(hour == i , as.Date(date )< as.Date(j))
    fit_<- lm(as.formula(a),df)
    merged[(merged$date==as.Date(j))&(merged$hour==i)]$production=predict(fit_,newdata=merged[(merged$date==as.Date(j))&(merged$hour==i)])
  }
}

# add columns
merged[,month:=months(as.Date(date))]
merged$month=as.factor(merged$month)
merged$hour=as.factor(merged$hour)
merged[,trend:=ceiling((1:.N)/24)]
merged[,avg_temp:=rowMeans(merged[,c(30:38)])]
merged[,avg_hum:=rowMeans(merged[,c(21:29)])]
merged[,avg_cloud:=rowMeans(merged[,c(3:11)])]
merged[,avg_dw:=rowMeans(merged[,c(12:20)])]

# separation of data into hours
for (i in 0:23){
  value <- merged[hour==i]
  x <- paste("merged",i, sep="")
  eval(call("<-", as.name(x), value))
}

addWindow <- function(col, type, m){
  if (type=="moving_average"){
    arr<-rollmean(col,m,align='right') 
    arr=c(rep(NA,m-1),arr) 
  }
  else if(type=="max_capacity"){
    arr<-rollmax(col,m,align='right') 
    arr=c(rep(NA,m-1),arr) 
  }
  arr[length(arr)] <- arr[length(arr)-1]
  return (arr)
}

#adding moving average&lag 
merged5$ma_y = addWindow(merged5$production, type="moving_average", m=3)
merged5$max_capacity = addWindow(merged5$production, type="max_capacity", m=7)
merged6$ma_y = addWindow(merged6$production, type="moving_average", m=3)
merged6$max_capacity = addWindow(merged6$production, type="max_capacity", m=7)
merged7$ma_y = addWindow(merged7$production, type="moving_average", m=3)
merged7$max_capacity = addWindow(merged7$production, type="max_capacity", m=7)
merged8$ma_y = addWindow(merged8$production, type="moving_average", m=3)
merged8$max_capacity = addWindow(merged8$production, type="max_capacity", m=7)
merged9$ma_y = addWindow(merged9$production, type="moving_average", m=3)
merged9$max_capacity = addWindow(merged9$production, type="max_capacity", m=7)
merged10$ma_y = addWindow(merged10$production, type="moving_average", m=3)
merged10$max_capacity = addWindow(merged10$production, type="max_capacity", m=7)
merged11$ma_y = addWindow(merged11$production, type="moving_average", m=3)
merged11$max_capacity = addWindow(merged11$production, type="max_capacity", m=7)
merged12$ma_y = addWindow(merged12$production, type="moving_average", m=3)
merged12$max_capacity = addWindow(merged12$production, type="max_capacity", m=7)
merged13$ma_y = addWindow(merged13$production, type="moving_average", m=3)
merged13$max_capacity = addWindow(merged13$production, type="max_capacity", m=7)
merged14$ma_y = addWindow(merged14$production, type="moving_average", m=3)
merged14$max_capacity = addWindow(merged14$production, type="max_capacity", m=7)
merged15$ma_y = addWindow(merged15$production, type="moving_average", m=3)
merged15$max_capacity = addWindow(merged15$production, type="max_capacity", m=7)
merged16$ma_y = addWindow(merged16$production, type="moving_average", m=3)
merged16$max_capacity = addWindow(merged16$production, type="max_capacity", m=7)
merged17$ma_y = addWindow(merged17$production, type="moving_average", m=3)
merged17$max_capacity = addWindow(merged17$production, type="max_capacity", m=7)
merged18$ma_y = addWindow(merged18$production, type="moving_average", m=3)
merged18$max_capacity = addWindow(merged18$production, type="max_capacity", m=7)
merged19$ma_y = addWindow(merged19$production, type="moving_average", m=3)
merged19$max_capacity = addWindow(merged19$production, type="max_capacity", m=7)


merged5[,lag1_y:=shift(production,type="lag",n=1)]
merged5[,lag14_y:=shift(production,type="lag",n=14)]
merged6[,lag1_y:=shift(production,type="lag",n=1)]
merged7[,lag1_y:=shift(production,type="lag",n=1)]
merged8[,lag1_y:=shift(production,type="lag",n=1)]
merged9[,lag1_y:=shift(production,type="lag",n=1)]
merged10[,lag1_y:=shift(production,type="lag",n=1)]
merged11[,lag1_y:=shift(production,type="lag",n=1)]
merged12[,lag1_y:=shift(production,type="lag",n=1)]
merged13[,lag1_y:=shift(production,type="lag",n=1)]
merged14[,lag1_y:=shift(production,type="lag",n=1)]
merged15[,lag1_y:=shift(production,type="lag",n=1)]
merged16[,lag1_y:=shift(production,type="lag",n=1)]
merged17[,lag1_y:=shift(production,type="lag",n=1)]
merged18[,lag1_y:=shift(production,type="lag",n=1)]
merged19[,lag1_y:=shift(production,type="lag",n=1)]

options(warn=-1)
# model building and predictions
forecast_lm <- c(0, 0, 0, 0, 0)

# hour=5
forfit5=merged5 %>% filter( as.Date(date )<todays_date&as.Date(date )>=as.Date("2021-05-01"))
##ggpairs(forfit5[,c(49,48,47,46,45,44,43,42,39)])
formula5='production ~ +max_capacity*trend+avg_cloud+avg_temp+avg_hum+month+lag1_y+lag14_y'
fit5<- lm(as.formula(formula5),forfit5)
#summary(fit5)
#checkresiduals(fit5)
# merged5[nrow(merged5)]$production=predict(fit5,merged5[nrow(merged5)])
forecast_lm <- append(forecast_lm, predict(fit5,merged5[nrow(merged5)]))

# hour=6
forfit6=merged6 %>% filter( as.Date(date )<todays_date&as.Date(date )>=as.Date("2021-05-01"))
##ggpairs(forfit6[,c(48,47,46,45,44,43,42,39)])
formula6='production ~ +max_capacity*trend+avg_cloud+avg_temp+avg_hum+month+ma_y'
fit6<- lm(as.formula(formula6),forfit6)
#summary(fit6)
#checkresiduals(fit6)
# merged6[nrow(merged6)]$production=merged6[nrow(merged6)-1]$production+predict(fit6,merged6[nrow(merged6)])
forecast_lm <- append(forecast_lm, predict(fit6,merged6[nrow(merged6)]))

#hour=7
#ggpairs(merged7[,c(48,47,46,45,44,43,42,39)])
formula7="production~+I(avg_cloud^2)+I(avg_temp^2)+I(avg_hum^2)+max_capacity*trend+ma_y+month"
fit7<- lm(as.formula(formula7),merged7)
#summary(fit7)
#checkresiduals(fit7)

# merged7[nrow(merged7)]$production=predict(fit7,merged7[nrow(merged7)])
forecast_lm <- append(forecast_lm, predict(fit7,merged7[nrow(merged7)]))

#hour=8
#ggpairs(merged8[,c(48,47,46,45,44,43,42,39)])
formula8='production ~ +avg_cloud+avg_dw+avg_hum+avg_temp+month+max_capacity*trend+ma_y+lag1_y'
fit8<- lm(as.formula(formula8),merged8)
#summary(fit8)
#checkresiduals(fit8)
# merged8[nrow(merged8)]$production=predict(fit8,merged8[nrow(merged8)])
forecast_lm <- append(forecast_lm, predict(fit8,merged8[nrow(merged8)]))

#hour=9
forfit9=merged9 %>% filter( as.Date(date )<todays_date&as.Date(date )>=as.Date("2021-04-01"))
forfit9[,log_prod:=log(forfit9$production)]
forfit9[forfit9$production==0]$log_prod=NA
#ggpairs(forfit9[,c(49,48,47,46,45,44,43,42,39)])
formula9='log_prod ~+trend +avg_temp+avg_hum+month+ma_y+max_capacity*trend+avg_hum+lag1_y'
fit9<- lm(as.formula(formula9),forfit9)
#summary(fit9)
#checkresiduals(fit9)
# merged9[nrow(merged9)]$production=exp(predict(fit9,merged9[nrow(merged9)]))
forecast_lm <- append(forecast_lm, exp(predict(fit9,merged9[nrow(merged9)])))

#hour=10
forfit10=merged10 %>% filter( as.Date(date )<todays_date&as.Date(date )>=as.Date("2022-04-01"))
forfit10[,diff_prod:=c(NA,diff(forfit10$production))]
ggpairs(forfit10[,c(49,48,47,46,45,44,43,42,39)])
formula10='diff_prod ~+trend+month+avg_hum+avg_dw+avg_cloud+avg_temp+ma_y'
fit10<- lm(as.formula(formula10),forfit10)
summary(fit10)
checkresiduals(fit10)
# merged10[nrow(merged10)]$production=merged10$production[nrow(merged10)-3]+predict(fit10,merged10[nrow(merged10)])
forecast_lm <- append(forecast_lm, merged10$production[nrow(merged10)-3] + predict(fit10,merged10[nrow(merged10)]))

#hour=11
forfit11=merged10 %>% filter( as.Date(date )<todays_date&as.Date(date )>=as.Date("2022-04-01"))
forfit11[,diff_prod:=c(NA,diff(forfit11$production))]
#ggpairs(forfit11[,c(49,48,47,46,45,44,43,42,39)])
formula11='production ~ma_y+month+avg_temp+avg_hum+avg_cloud+max_capacity+lag1_y'
fit11<- lm(as.formula(formula11),forfit11)
#summary(fit11)
#checkresiduals(fit11)
# merged11[nrow(merged11)]$production=predict(fit11,merged11[nrow(merged11)])
forecast_lm <- append(forecast_lm, predict(fit11,merged11[nrow(merged11)]))

#hour=12
formula12='production ~trend+month+avg_temp+avg_hum+avg_cloud+max_capacity+lag1_y'
fit12<- lm(as.formula(formula12),merged12)
#summary(fit12)
#checkresiduals(fit12)
# merged12[nrow(merged12)]$production=predict(fit12,merged12[nrow(merged12)])
forecast_lm <- append(forecast_lm, predict(fit12,merged12[nrow(merged12)]))

#hour=13
#ggpairs(merged13[,c(48,47,46,45,44,43,42,39)])
formula13='production ~trend+month+avg_temp+avg_hum+lag1_y+ma_y' 
fit13 <- lm(as.formula(formula13),merged13)
#summary(fit13)
#checkresiduals(fit13)
# merged13[nrow(merged13)]$production=predict(fit13,merged13[nrow(merged13)])
forecast_lm <- append(forecast_lm, predict(fit13,merged13[nrow(merged13)]))

#hour=14
#ggpairs(merged14[,c(48,47,46,45,44,43,42,39)])
formula14='production ~month+avg_temp+avg_hum+ma_y+avg_temp+avg_dw+lag1_y' 
fit14 <- lm(as.formula(formula14),merged14)
#summary(fit14)
#checkresiduals(fit14)
# merged14[nrow(merged14)]$production=predict(fit14,merged14[nrow(merged14)])
forecast_lm <- append(forecast_lm, predict(fit14,merged14[nrow(merged14)]))

#hour=15
#ggpairs(merged15[,c(48,47,46,45,44,43,42,39)])
formula15 = 'diff_prod ~ month+avg_hum+ma_y+avg_dw+lag1_y+max_capacity'
merged15[,diff_prod:=c(NA,diff(merged15$production))]
fit15<- lm(as.formula(formula15),merged15)
#summary(fit15)
#checkresiduals(fit15)
# merged15[nrow(merged15)]$production=merged15$production[nrow(merged15)-3]+predict(fit15,merged15[nrow(merged15)])
forecast_lm <- append(forecast_lm, merged15$production[nrow(merged15)-3] + predict(fit15,merged15[nrow(merged15)]))

#hour=16
#ggpairs(merged16[,c(48,47,46,45,44,43,42,39)])
formula16='production ~trend+month+avg_temp+lag1_y+ma_y+I(avg_hum^2)+avg_dw+max_capacity*avg_hum' 
fit16<- lm(as.formula(formula16),merged16)
#summary(fit16)
#checkresiduals(fit16)
# merged16[nrow(merged16)]$production=predict(fit16,merged16[nrow(merged16)])
forecast_lm <- append(forecast_lm, predict(fit16,merged16[nrow(merged16)]))

#hour=17
#ggpairs(merged17[,c(48,47,46,45,44,43,42,39)])
formula17='production ~trend+month+avg_temp+lag1_y+ma_y+I(avg_hum^2)+avg_dw'
fit17<- lm(as.formula(formula17),merged17)
#summary(fit17)
#checkresiduals(fit17)
# merged17[nrow(merged17)]$production=predict(fit17,merged17[nrow(merged17)])
forecast_lm <- append(forecast_lm, predict(fit17,merged17[nrow(merged17)]))

#hour=18
#ggpairs(merged18[,c(48,47,46,45,44,43,42,39)])
formula18='production ~month+avg_temp+lag1_y+ma_y+avg_dw+I(avg_dw^2)'
fit18<- lm(as.formula(formula18),merged18)
#summary(fit18)
#checkresiduals(fit18)
# merged18[nrow(merged18)]$production=predict(fit18,merged18[nrow(merged18)])
forecast_lm <- append(forecast_lm, predict(fit18,merged18[nrow(merged18)]))

#hour=19
forfit19=merged19 %>% filter( as.Date(date )<todays_date&as.Date(date )>=as.Date("2021-04-01"))
#ggpairs(forfit19[,c(48,47,46,45,44,43,42,39)])
formula19='production ~month+lag1_y+ma_y'
fit19<- lm(as.formula(formula19),forfit19)
#summary(fit19)
#checkresiduals(fit19)
# merged19[nrow(merged19)]$production=predict(fit19,merged19[nrow(merged19)])
forecast_lm <- append(forecast_lm, predict(fit19,merged19[nrow(merged19)]))


forecast_lm <- append(forecast_lm, c(0, 0, 0, 0))
forecast_lm


todays_date = as.Date("2022-05-26")
weather_file='2022-05-26_weather.csv'
production_file='2022-05-26_production.csv'
weather <- fread(weather_file)
prod <- fread(production_file)

str(weather)
str(prod)
weather <- dcast(weather, date + hour ~ variable + lat + lon, value.var = "value")
weather$date <- as.Date(weather$date, format = "%m/%d/%Y")
prod$date <- as.Date(prod$date, format = "%Y-%m-%d")
merged <- merge(weather, prod, by = c("date","hour"), all = T)
merged=merged %>% filter( as.Date(date )<=as.Date(todays_date+1))
show_na <-merged[rowSums(is.na(merged)) > 0, ]
# show_na








forecast_date=todays_date+1

# create a template for forecast date
forecast_table=data.table(date=forecast_date,hour=0:23,production=NA)
forecast_table[,arima:=NA]
forecast_table$arima=as.numeric(forecast_table$arima)
forecast_table[,actual:=NA]
forecast_table$actual=as.numeric(forecast_table$actual)
forecast_table[,residual:=NA]
forecast_table$residual=as.numeric(forecast_table$residual)


forecast_date=todays_date+1

# create a template for forecast date
forecast_table=data.table(date=forecast_date,hour=0:23,production=NA)
forecast_table[,arima:=NA]
forecast_table$arima=as.numeric(forecast_table$arima)
forecast_table[,actual:=NA]
forecast_table$actual=as.numeric(forecast_table$actual)
forecast_table[,residual:=NA]
forecast_table$residual=as.numeric(forecast_table$residual)


for (i in c(1:5)) {
  forecast_table$arima[i] =0 
}



#merged5
production_hour5_ts<-ts(merged5[!is.na(production)]$production,frequency=1)
ts.plot(production_hour5_ts, xlab = "Day", ylab = "Production",main="Hour=5 Production")
#data is not stationary and as can be seen from the plot variance is changing so box cox box transformation is required
require(urca)
unt_test=ur.kpss(production_hour5_ts) 
summary(unt_test)

#Box-Cox transformation is done
lambda <- BoxCox.lambda(production_hour5_ts, method ="guerrero", lower = -1, upper = 2)
box_prod<-BoxCox(production_hour5_ts,lambda)

unt_test=ur.kpss(box_prod) 
summary(unt_test)

#differencing is done
diff_series<-diff(box_prod,1)
unt_test=ur.kpss(diff_series) 
summary(unt_test)

ts.plot(diff_series, xlab = "Day", ylab = "Production",main="Hour=5 Production with differencing and Box Cox")
arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
arima_model
checkresiduals(arima_model)
Acf(arima_model$residuals)


forecast_ahead=3
arima_forecast=forecast(arima_model,h=forecast_ahead)
arima_forecast=InvBoxCox(as.numeric(format(arima_forecast$mean[3],scientific = FALSE)),as.numeric(lambda)) 
forecast_table$arima[6]=(arima_forecast)
forecast_table

#merged6
production_hour6_ts<-ts(merged6[!is.na(production)]$production,frequency=1)
ts.plot(production_hour6_ts, xlab = "Day", ylab = "Production",main="Hour=6 Production")
#data is not stationary and as can be seen from the plot variance is changing so box cox box transformation is required
require(urca)
unt_test=ur.kpss(production_hour6_ts) 
summary(unt_test)

#Box-Cox transformation is done
lambda <- BoxCox.lambda(production_hour6_ts, method ="guerrero", lower = -1, upper = 2)
box_prod<-BoxCox(production_hour6_ts,lambda)

unt_test=ur.kpss(box_prod) 
summary(unt_test)

#differencing is done
diff_series<-diff(production_hour6_ts,1)
unt_test=ur.kpss(diff_series) 
summary(unt_test)

ts.plot(diff_series, xlab = "Day", ylab = "Production",main="Hour=6 Production with differencing and Box Cox")
arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
arima_model
checkresiduals(arima_model)
Acf(arima_model$residuals)


forecast_ahead=3
arima_forecast=forecast(arima_model,h=forecast_ahead)
forecast_table$arima[7]=sum(arima_forecast$mean)+merged6[nrow(merged6)-3,]$production



#merged7

production_hour7_ts<-ts(merged7[!is.na(production)]$production,frequency=1)
ts.plot(production_hour7_ts, xlab = "Day", ylab = "Production",main="Hour=7 Production")
#data is not stationary and as can be seen from the plot variance is changing so box cox box transformation is required
require(urca)
unt_test=ur.kpss(production_hour7_ts) 
summary(unt_test)

#Box-Cox transformation is done
lambda <- BoxCox.lambda(production_hour7_ts, method ="guerrero", lower = -1, upper = 2)
box_prod<-BoxCox(production_hour7_ts,lambda)

unt_test=ur.kpss(box_prod) 
summary(unt_test)

#differencing is done
diff_series<-diff(production_hour7_ts,1)
unt_test=ur.kpss(diff_series) 
summary(unt_test)

ts.plot(diff_series, xlab = "Day", ylab = "Production",main="Hour=6 Production with differencing and Box Cox")
arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
arima_model
checkresiduals(arima_model)
Acf(arima_model$residuals)


forecast_ahead=3
arima_forecast=forecast(arima_model,h=forecast_ahead)
forecast_table$arima[8]=sum(arima_forecast$mean)+merged7[nrow(merged7)-3,]$production

#merged8

production_hour8_ts<-ts(merged8[!is.na(production)]$production,frequency=1)
ts.plot(production_hour8_ts, xlab = "Day", ylab = "Production",main="Hour=8 Production")
#data is not stationary and as can be seen from the plot variance is changing so box cox box transformation is required
require(urca)
unt_test=ur.kpss(production_hour8_ts) 
summary(unt_test)

#Box-Cox transformation is done
lambda <- BoxCox.lambda(production_hour8_ts, method ="guerrero", lower = -1, upper = 2)
box_prod<-BoxCox(production_hour8_ts,lambda)

unt_test=ur.kpss(box_prod) 
summary(unt_test)

#differencing is done
diff_series<-diff(production_hour8_ts,1)
unt_test=ur.kpss(diff_series) 
summary(unt_test)

ts.plot(diff_series, xlab = "Day", ylab = "Production",main="Hour=8 Production with differencing and Box Cox")
arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
arima_model
checkresiduals(arima_model)
Acf(arima_model$residuals)

forecast_ahead=3
arima_forecast=forecast(arima_model,h=forecast_ahead)
forecast_table$arima[9]=sum(arima_forecast$mean)+merged8[nrow(merged8)-3,]$production



#merged9

production_hour9_ts<-ts(merged9[!is.na(production)]$production,frequency=1)
ts.plot(production_hour9_ts, xlab = "Day", ylab = "Production",main="Hour=9 Production")
#data is not stationary and as can be seen from the plot variance is changing so box cox box transformation is required
require(urca)
unt_test=ur.kpss(production_hour9_ts) 
summary(unt_test)

#Box-Cox transformation is done
lambda <- BoxCox.lambda(production_hour9_ts, method ="guerrero", lower = -1, upper = 2)
box_prod<-BoxCox(production_hour9_ts,lambda)

unt_test=ur.kpss(box_prod) 
summary(unt_test)

#differencing is done
diff_series<-diff(production_hour9_ts,1)
unt_test=ur.kpss(diff_series) 
summary(unt_test)

ts.plot(diff_series, xlab = "Day", ylab = "Production",main="Hour=6 Production with differencing and Box Cox")
arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
arima_model
checkresiduals(arima_model)
Acf(arima_model$residuals)


forecast_ahead=3
arima_forecast=forecast(arima_model,h=forecast_ahead)
forecast_table$arima[10]=sum(arima_forecast$mean)+merged9[nrow(merged9)-3,]$production



#merged10
production_hour10_ts<-ts(merged10[!is.na(production)]$production,frequency=1)
ts.plot(production_hour10_ts, xlab = "Day", ylab = "Production",main="Hour=7 Production")
#data is not stationary and as can be seen from the plot variance is changing so box cox box transformation is required
require(urca)
unt_test=ur.kpss(production_hour10_ts) 
summary(unt_test)

#Box-Cox transformation is done
lambda <- BoxCox.lambda(production_hour10_ts, method ="guerrero", lower = -1, upper = 2)
box_prod<-BoxCox(production_hour10_ts,lambda)

unt_test=ur.kpss(box_prod) 
summary(unt_test)

#differencing is done
diff_series<-diff(production_hour10_ts,1)
unt_test=ur.kpss(diff_series) 
summary(unt_test)

ts.plot(diff_series, xlab = "Day", ylab = "Production",main="Hour=10 Production with differencing and Box Cox")
arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
arima_model
checkresiduals(arima_model)
Acf(arima_model$residuals)


forecast_ahead=3
arima_forecast=forecast(arima_model,h=forecast_ahead)
forecast_table$arima[11]=sum(arima_forecast$mean)+merged10[nrow(merged10)-3,]$production


#merged11

production_hour11_ts<-ts(merged11[!is.na(production)]$production,frequency=1)
ts.plot(production_hour11_ts, xlab = "Day", ylab = "Production",main="Hour=11 Production")
#data is not stationary and as can be seen from the plot variance is changing so box cox box transformation is required
require(urca)
unt_test=ur.kpss(production_hour11_ts) 
summary(unt_test)

#Box-Cox transformation is done
lambda <- BoxCox.lambda(production_hour11_ts, method ="guerrero", lower = -1, upper = 2)
box_prod<-BoxCox(production_hour11_ts,lambda)

unt_test=ur.kpss(box_prod) 
summary(unt_test)

#differencing is done
diff_series<-diff(production_hour11_ts,1)
unt_test=ur.kpss(diff_series) 
summary(unt_test)

ts.plot(diff_series, xlab = "Day", ylab = "Production",main="Hour=6 Production with differencing and Box Cox")
arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
arima_model
checkresiduals(arima_model)
Acf(arima_model$residuals)


forecast_ahead=3
arima_forecast=forecast(arima_model,h=forecast_ahead)
forecast_table$arima[12]=sum(arima_forecast$mean)+merged11[nrow(merged11)-3,]$production

#merged12 

production_hour12_ts<-ts(merged12[!is.na(production)]$production,frequency=1)
ts.plot(production_hour12_ts, xlab = "Day", ylab = "Production",main="Hour=12 Production")
#data is not stationary and as can be seen from the plot variance is changing so box cox box transformation is required
require(urca)
unt_test=ur.kpss(production_hour12_ts) 
summary(unt_test)

#Box-Cox transformation is done
lambda <- BoxCox.lambda(production_hour12_ts, method ="guerrero", lower = -1, upper = 2)
box_prod<-BoxCox(production_hour12_ts,lambda)

unt_test=ur.kpss(box_prod) 
summary(unt_test)

#differencing is done
diff_series<-diff(production_hour12_ts,1)
unt_test=ur.kpss(diff_series) 
summary(unt_test)

ts.plot(diff_series, xlab = "Day", ylab = "Production",main="Hour=6 Production with differencing and Box Cox")
arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
arima_model
checkresiduals(arima_model)
Acf(arima_model$residuals)


forecast_ahead=3
arima_forecast=forecast(arima_model,h=forecast_ahead)
forecast_table$arima[13]=sum(arima_forecast$mean)+merged12[nrow(merged12)-3,]$production


#merged13
production_hour13_ts<-ts(merged13[!is.na(production)]$production,frequency=1)
ts.plot(production_hour13_ts, xlab = "Day", ylab = "Production",main="Hour=7 Production")
#data is not stationary and as can be seen from the plot variance is changing so box cox box transformation is required
require(urca)
unt_test=ur.kpss(production_hour13_ts) 
summary(unt_test)

#Box-Cox transformation is done
lambda <- BoxCox.lambda(production_hour13_ts, method ="guerrero", lower = -1, upper = 2)
box_prod<-BoxCox(production_hour13_ts,lambda)

unt_test=ur.kpss(box_prod) 
summary(unt_test)

#differencing is done
diff_series<-diff(production_hour13_ts,1)
unt_test=ur.kpss(diff_series) 
summary(unt_test)

ts.plot(diff_series, xlab = "Day", ylab = "Production",main="Hour=6 Production with differencing and Box Cox")
arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
arima_model
checkresiduals(arima_model)
Acf(arima_model$residuals)


forecast_ahead=3
arima_forecast=forecast(arima_model,h=forecast_ahead)
forecast_table$arima[14]=sum(arima_forecast$mean)+merged13[nrow(merged13)-3,]$production


#merged14
production_hour14_ts<-ts(merged14[!is.na(production)]$production,frequency=1)
ts.plot(production_hour14_ts, xlab = "Day", ylab = "Production",main="Hour=14 Production")
#data is not stationary and as can be seen from the plot variance is changing so box cox box transformation is required
require(urca)
unt_test=ur.kpss(production_hour14_ts) 
summary(unt_test)

#Box-Cox transformation is done
lambda <- BoxCox.lambda(production_hour14_ts, method ="guerrero", lower = -1, upper = 2)
box_prod<-BoxCox(production_hour14_ts,lambda)

unt_test=ur.kpss(box_prod) 
summary(unt_test)

#differencing is done
diff_series<-diff(production_hour14_ts,1)
unt_test=ur.kpss(diff_series) 
summary(unt_test)

ts.plot(diff_series, xlab = "Day", ylab = "Production",main="Hour=14 Production with differencing and Box Cox")
arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
arima_model
checkresiduals(arima_model)
Acf(arima_model$residuals)


forecast_ahead=3
arima_forecast=forecast(arima_model,h=forecast_ahead)
forecast_table$arima[15]=sum(arima_forecast$mean)+merged14[nrow(merged14)-3,]$production



#merged15
production_hour15_ts<-ts(merged15[!is.na(production)]$production,frequency=1)
ts.plot(production_hour15_ts, xlab = "Day", ylab = "Production",main="Hour=15 Production")
#data is not stationary and as can be seen from the plot variance is changing so box cox box transformation is required
require(urca)
unt_test=ur.kpss(production_hour15_ts) 
summary(unt_test)

#Box-Cox transformation is done
lambda <- BoxCox.lambda(production_hour15_ts, method ="guerrero", lower = -1, upper = 2)
box_prod<-BoxCox(production_hour15_ts,lambda)

unt_test=ur.kpss(box_prod) 
summary(unt_test)

#differencing is done
diff_series<-diff(production_hour15_ts,1)
unt_test=ur.kpss(diff_series) 
summary(unt_test)

ts.plot(diff_series, xlab = "Day", ylab = "Production",main="Hour=15 Production with differencing and Box Cox")
arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
arima_model
checkresiduals(arima_model)
Acf(arima_model$residuals)


forecast_ahead=3
arima_forecast=forecast(arima_model,h=forecast_ahead)
forecast_table$arima[16]=sum(arima_forecast$mean)+merged15[nrow(merged15)-3,]$production


#merged16

production_hour16_ts<-ts(merged16[!is.na(production)]$production,frequency=1)
ts.plot(production_hour16_ts, xlab = "Day", ylab = "Production",main="Hour=16 Production")
#data is not stationary and as can be seen from the plot variance is changing so box cox box transformation is required
require(urca)
unt_test=ur.kpss(production_hour16_ts) 
summary(unt_test)

#Box-Cox transformation is done
lambda <- BoxCox.lambda(production_hour16_ts, method ="guerrero", lower = -1, upper = 2)
box_prod<-BoxCox(production_hour16_ts,lambda)

unt_test=ur.kpss(box_prod) 
summary(unt_test)

#differencing is done
diff_series<-diff(production_hour16_ts,1)
unt_test=ur.kpss(diff_series) 
summary(unt_test)

ts.plot(diff_series, xlab = "Day", ylab = "Production",main="Hour=16 Production with differencing and Box Cox")
arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
arima_model
checkresiduals(arima_model)
Acf(arima_model$residuals)


forecast_ahead=3
arima_forecast=forecast(arima_model,h=forecast_ahead)
forecast_table$arima[17]=sum(arima_forecast$mean)+merged16[nrow(merged16)-3,]$production



#merged17
production_hour17_ts<-ts(merged17[!is.na(production)]$production,frequency=1)
ts.plot(production_hour17_ts, xlab = "Day", ylab = "Production",main="Hour=17 Production")
#data is not stationary and as can be seen from the plot variance is changing so box cox box transformation is required
require(urca)
unt_test=ur.kpss(production_hour17_ts) 
summary(unt_test)

#Box-Cox transformation is done
lambda <- BoxCox.lambda(production_hour17_ts, method ="guerrero", lower = -1, upper = 2)
box_prod<-BoxCox(production_hour17_ts,lambda)

unt_test=ur.kpss(box_prod) 
summary(unt_test)

#differencing is done
diff_series<-diff(production_hour17_ts,1)
unt_test=ur.kpss(diff_series) 
summary(unt_test)

ts.plot(diff_series, xlab = "Day", ylab = "Production",main="Hour=17 Production with differencing and Box Cox")
arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
arima_model
checkresiduals(arima_model)
Acf(arima_model$residuals)


forecast_ahead=3
arima_forecast=forecast(arima_model,h=forecast_ahead)
forecast_table$arima[18]=sum(arima_forecast$mean)+merged17[nrow(merged17)-3,]$production


#merged18
production_hour18_ts<-ts(merged18[!is.na(production)]$production,frequency=1)
ts.plot(production_hour18_ts, xlab = "Day", ylab = "Production",main="Hour=18 Production")
#data is not stationary and as can be seen from the plot variance is changing so box cox box transformation is required
require(urca)
unt_test=ur.kpss(production_hour18_ts) 
summary(unt_test)

#Box-Cox transformation is done
lambda <- BoxCox.lambda(production_hour18_ts, method ="guerrero", lower = -1, upper = 2)
box_prod<-BoxCox(production_hour18_ts,lambda)

unt_test=ur.kpss(box_prod) 
summary(unt_test)

#differencing is done
diff_series<-diff(production_hour18_ts,1)
unt_test=ur.kpss(diff_series) 
summary(unt_test)

ts.plot(diff_series, xlab = "Day", ylab = "Production",main="Hour=18 Production with differencing and Box Cox")
arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
arima_model
checkresiduals(arima_model)
Acf(arima_model$residuals)


forecast_ahead=3
arima_forecast=forecast(arima_model,h=forecast_ahead)
forecast_table$arima[19]=sum(arima_forecast$mean)+merged18[nrow(merged18)-3,]$production

#merged19
production_hour19_ts<-ts(merged19[!is.na(production)]$production,frequency=1)
ts.plot(production_hour19_ts, xlab = "Day", ylab = "Production",main="Hour=19 Production")
#data is not stationary and as can be seen from the plot variance is changing so box cox box transformation is required
require(urca)
unt_test=ur.kpss(production_hour19_ts) 
summary(unt_test)

#Box-Cox transformation is done
lambda <- BoxCox.lambda(production_hour19_ts, method ="guerrero", lower = -1, upper = 2)
box_prod<-BoxCox(production_hour19_ts,lambda)

unt_test=ur.kpss(box_prod) 
summary(unt_test)

#differencing is done
diff_series<-diff(production_hour19_ts,1)
unt_test=ur.kpss(diff_series) 
summary(unt_test)

ts.plot(diff_series, xlab = "Day", ylab = "Production",main="Hour=6 Production with differencing and Box Cox")
arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
arima_model
checkresiduals(arima_model)
Acf(arima_model$residuals)


forecast_ahead=3
arima_forecast=forecast(arima_model,h=forecast_ahead)
forecast_table$arima[20]=sum(arima_forecast$mean)+merged19[nrow(merged19)-3,]$production

forecast_table

#merged20
production_hour20_ts<-ts(merged20[!is.na(production)]$production,frequency=1)
ts.plot(production_hour20_ts, xlab = "Day", ylab = "Production",main="Hour=20 Production")
#data is not stationary and as can be seen from the plot variance is changing so box cox box transformation is required
require(urca)
unt_test=ur.kpss(production_hour20_ts) 
summary(unt_test)

#Box-Cox transformation is done
lambda <- BoxCox.lambda(production_hour20_ts, method ="guerrero", lower = -1, upper = 2)
box_prod<-BoxCox(production_hour20_ts,lambda)

unt_test=ur.kpss(box_prod) 
summary(unt_test)

#differencing is done
diff_series<-diff(production_hour20_ts,1)
unt_test=ur.kpss(diff_series) 
summary(unt_test)

ts.plot(diff_series, xlab = "Day", ylab = "Production",main="Hour=20 Production with differencing and Box Cox")
arima_model=auto.arima(diff_series,seasonal=F,stepwise=F,approximation=F,trace=T)
arima_model
checkresiduals(arima_model)
Acf(arima_model$residuals)


forecast_ahead=3
arima_forecast=forecast(arima_model,h=forecast_ahead)
forecast_table$arima[21]=sum(arima_forecast$mean)+merged20[nrow(merged20)-3,]$production

for (i in c(22:24)) {
  forecast_table$arima[i]=0
}

forecast_table


forecast_date=todays_date+1

production=fread(production_file)

latest_available_prod_date=as.Date(max(production$date))
n_days=as.numeric(forecast_date-latest_available_prod_date)

forecasted_production=tail(production,n_days*24)
forecasted_production[,date:=date+n_days]
forecasted_production[,production:=NA]

production_with_forecast=rbind(production,forecasted_production)

# create a template for forecast date
# forecast_table=data.table(date=forecast_date,hour=0:23,production=NA)
forecast_sarima_sep <- c()

for (i in 0:23){
  value <- production_with_forecast[hour==i]
  x <- paste("prod",i, sep="")
  eval(call("<-", as.name(x), value))
}

forecast_table[,sarima:=NA]
forecast_table$sarima=as.numeric(forecast_table$sarima)

for (i in 0:23){
  x <- paste("prod",i, sep="")
  value <- ts(eval(parse( text=paste("prod", i, sep = "")))[!is.na(production)]$production,frequency=30)
  ser<-paste("series_",i, sep="")
  eval(call("<-", as.name(ser), value))
  sarima_model=auto.arima(eval(parse( text=paste("series_", i, sep = ""))),seasonal=T,stepwise=T,approximation=T,trace=T)
  forecast_ahead=nrow(forecast_table)/24
  sarima_forecast=forecast(sarima_model,h=forecast_ahead)
  #forecast_table[hour==i,"sarima"]=as.numeric(format(sarima_forecast$mean,scientific = FALSE))
  forecast_sarima_sep <- append(forecast_sarima_sep, as.numeric(format(sarima_forecast$mean,scientific = FALSE)))
}

forecast_sarima_sep

# SARIMA unseparated 
production=fread(production_file)
weather=fread(weather_file)
forecast_date=todays_date+1

latest_available_prod_date=as.Date(max(production$date))
n_days=as.numeric(forecast_date-latest_available_prod_date)

forecasted_production=tail(production,n_days*24)
forecasted_production[,date:=date+n_days]
forecasted_production[,production:=NA]

require(urca)
unt_test=ur.kpss(ts(production$production,frequency=24)) 
summary(unt_test)

production_with_forecast=rbind(production,forecasted_production)
# forecast_table=data.table(date=forecast_date,hour=0:23,production=NA)

production_with_forecast=production_with_forecast[order(date,hour)]
production_series=ts(production_with_forecast[!is.na(production)]$production,frequency=24)

sarima_model=auto.arima(production_series,seasonal=T,stepwise=T,approximation=T,trace=T)

forecast_ahead=nrow(forecast_table)
sarima_forecast=forecast(sarima_model,h=forecast_ahead)

forecast_sarima_unsep = as.numeric(format(tail(sarima_forecast$mean,24),scientific = FALSE))

# decomposition
weather <- fread(weather_file)
prod <- fread(production_file)

str(weather)
str(prod)

weather <- dcast(weather, date + hour ~ variable + lat + lon, value.var = "value")

weather$date <- as.Date(weather$date, format = "%m/%d/%Y")
prod$date <- as.Date(prod$date, format = "%Y-%m-%d")

merged <- merge(weather, prod, by = c("date","hour"), all = T)
merged=merged %>% filter( as.Date(date )<=as.Date(todays_date+1))
show_na <-merged[rowSums(is.na(merged)) > 0, ]

nrow(show_na)/24
unique(show_na$date)
nrow(merged)/24

merged[,month:=months(as.Date(date))]
for (i in c(0:4,21:23)){
  merged[merged$hour==i]$production=0
}
merged$month=as.factor(merged$month)
merged$hour=as.factor(merged$hour)
days<-unique((as.Date(show_na$date)))

# merged_initial <- copy(merged)
a='production ~ +CLOUD_LOW_LAYER_36.25_33+CLOUD_LOW_LAYER_36.25_33.25+CLOUD_LOW_LAYER_36.25_33.5+CLOUD_LOW_LAYER_36.5_33+CLOUD_LOW_LAYER_36.5_33.25+CLOUD_LOW_LAYER_36.5_33.5+CLOUD_LOW_LAYER_36.75_33+CLOUD_LOW_LAYER_36.75_33.25+CLOUD_LOW_LAYER_36.75_33.5+DSWRF_36.25_33+DSWRF_36.25_33.25+DSWRF_36.25_33.5+DSWRF_36.5_33+DSWRF_36.5_33.25+DSWRF_36.5_33.5+DSWRF_36.75_33+DSWRF_36.75_33.25+DSWRF_36.75_33.5+REL_HUMIDITY_36.25_33+REL_HUMIDITY_36.25_33.25+REL_HUMIDITY_36.25_33.5+REL_HUMIDITY_36.5_33+REL_HUMIDITY_36.5_33.25+REL_HUMIDITY_36.5_33.5+REL_HUMIDITY_36.75_33+REL_HUMIDITY_36.75_33.25+REL_HUMIDITY_36.75_33.5+TEMP_36.25_33+TEMP_36.25_33.25+TEMP_36.25_33.5+TEMP_36.5_33+TEMP_36.5_33.25+TEMP_36.5_33.5+TEMP_36.75_33+TEMP_36.75_33.25+TEMP_36.75_33.5+month'
for (i in 5:20){
  for (j in days[c(-1,-length(days))]){
    df=merged %>% filter(hour == i , as.Date(date )< as.Date(j))
    fit_<- lm(as.formula(a),df)
    merged[(merged$date==as.Date(j))&(merged$hour==i)]$production=predict(fit_,newdata=merged[(merged$date==as.Date(j))&(merged$hour==i)])
  }
}

merged <- merged[date != '2021-02-20']

merged[,month:=months(as.Date(date))]
merged$month=as.factor(merged$month)
merged$hour=as.factor(merged$hour)
merged[,trend:=ceiling((1:.N)/24)]
merged[,avg_temp:=rowMeans(merged[,c(30:38)])]
merged[,avg_hum:=rowMeans(merged[,c(21:29)])]
merged[,avg_cloud:=rowMeans(merged[,c(3:11)])]
merged[,avg_dw:=rowMeans(merged[,c(12:20)])]

library(glue)
for (i in 0:23){
  command_str = glue('merged{i} = merged[merged$hour=={i}]')
  eval(parse(text=command_str))
}
#head(merged13)
show_na13 <-merged13[rowSums(is.na(merged13)) > 0, ]


nthroot<-function(x, n){
  sign(x)*abs(x)^(1/n)
}

predictDecomposed <- function(remainder_pred, hour, decomposed){
  trend_component <- NA
  i <- length(decomposed$trend)
  while(1){
    if (!as.logical(is.na(decomposed$trend[i]))){
      trend_component <- decomposed$trend[i]
      break
    }
    i <- i-1
  }
  seasonal_component <- unique(decomposed$seasonal)[hour+1]
  overall_prediction <- trend_component + seasonal_component + remainder_pred
  return (overall_prediction)
}


# decomposition of all data (before the split wrt hours)
l <- nrow(merged) -24
production_ts <- ts(merged[1:l]$production)
acf(production_ts, lag.max=80)
#merged_ts<-ts(merged[1:l]$production, freq=24)
length(merged_ts)/24
merged_decomposed_additive = decompose(merged_ts,type="additive")
#merged_decomposed_additive
plot(merged_decomposed_additive)

remainders <- merged_decomposed_additive$random
unt_test=ur.kpss(remainders) 
summary(unt_test)
unt_test=ur.kpss(nthroot(remainders,3)) 
summary(unt_test)
remainders <- nthroot(remainders, 3)

# separate the remainders into hours
for (h in 0:23){
  command_str = glue("remainder{h} <- c()")
  eval(parse(text=command_str))
}

for (i in 0:length(remainders)){
  h = i%%24
  command_str = glue("remainder{h} <- append(remainder{h}, remainders[i])")
  eval(parse(text=command_str))
}
for (h in 0:23){
  command_str = glue("remainder{h} <- append(remainder{h}, NA)")
  eval(parse(text=command_str))
}

# add remainders as columns to the dataframes separated by hours
for (h in 0:23){
  command_str = glue("merged{h}[,remainder:=remainder{h}]")
  eval(parse(text=command_str))
}

# models and predictions

forecast_decomp_lm <- c(0, 0, 0, 0, 0)
forecast_decomp_arima <- c(0, 0, 0, 0, 0)

# hour = 5
#regression
#ggplot(merged5, aes(x=date, y=remainder))+geom_line()
#ggpairs(data=merged5, columns=c(42:46))


remainder5_ts <- ts(remainder5[c(1:length(remainder5)-1)])
l <- length(remainder5) - 1
#acf(remainder5_ts[c(2:l)])
#pacf(remainder5_ts[c(2:l)])
merged5[,lag1_remainder:=shift(remainder,type="lag",n=1)]

formula5 = 'remainder ~ month + avg_temp + avg_cloud + lag1_remainder'

fit5<- lm(as.formula(formula5),merged5[-nrow(merged5)])
#summary(fit5)
#checkresiduals(fit5)

r_regression_5 <- predict(fit5,merged5[nrow(merged5)])^3
pred <- predictDecomposed(r_regression_5, 5, merged_decomposed_additive)
forecast_decomp_lm <- append(forecast_decomp_lm, pred)

# arima
auto_arima_5 <- auto.arima(merged5$remainder)
#summary(auto_arima_5)
r_arima_5=forecast(auto_arima_5,h=1)
r_arima_5
pred <- predictDecomposed((r_arima_5$mean)^3, 5, merged_decomposed_additive)
forecast_decomp_arima <- append(forecast_decomp_arima, pred)

# hour = 6
#regression
#ggplot(merged6, aes(x=date, y=remainder))+geom_line()
#ggpairs(data=merged6, columns=c(42:46))


remainder6_ts <- ts(remainder6[c(1:length(remainder6)-1)])
l <- length(remainder6) - 1
#acf(remainder6_ts[c(2:l)])
#pacf(remainder6_ts[c(2:l)])
merged6[,lag1_remainder:=shift(remainder,type="lag",n=1)]
merged6[,lag2_remainder:=shift(remainder,type="lag",n=2)]
merged6[,lag3_remainder:=shift(remainder,type="lag",n=3)]

formula6 = 'remainder ~ month + avg_temp + avg_cloud + avg_hum + lag1_remainder'

fit6<- lm(as.formula(formula6),merged6[-nrow(merged6)])
#summary(fit6)
#checkresiduals(fit6)

r_regression_6 <- predict(fit6,merged6[nrow(merged6)])^3
pred <- predictDecomposed(r_regression_6, 6, merged_decomposed_additive)
forecast_decomp_lm <- append(forecast_decomp_lm, pred)

# arima
auto_arima_6 <- auto.arima(merged6$remainder)
#summary(auto_arima_6)
r_arima_6=forecast(auto_arima_6,h=1)
r_arima_6
pred <- predictDecomposed((r_arima_6$mean)^3, 6, merged_decomposed_additive)
forecast_decomp_arima <- append(forecast_decomp_arima, pred)

# hour = 7
#regression
#ggplot(merged7, aes(x=date, y=remainder))+geom_line()
#ggpairs(data=merged7, columns=c(42:46))


remainder7_ts <- ts(remainder7[c(1:length(remainder7)-1)])
l <- length(remainder7) - 1
#acf(remainder7_ts[c(2:l)])
#pacf(remainder7_ts[c(2:l)])
merged7[,lag1_remainder:=shift(remainder,type="lag",n=1)]
merged7[,lag2_remainder:=shift(remainder,type="lag",n=2)]
merged7[,lag3_remainder:=shift(remainder,type="lag",n=3)]

formula7 = 'remainder ~ month + avg_temp + avg_cloud + avg_dw + lag1_remainder'

fit7<- lm(as.formula(formula7),merged7[-nrow(merged7)])
#summary(fit7)
#checkresiduals(fit7)

r_regression_7 <- predict(fit7,merged7[nrow(merged7)])^3
pred <- predictDecomposed(r_regression_7, 7, merged_decomposed_additive)
forecast_decomp_lm <- append(forecast_decomp_lm, pred)

# arima
auto_arima_7 <- auto.arima(merged7$remainder)
#summary(auto_arima_7)
r_arima_7=forecast(auto_arima_7,h=1)
r_arima_7
pred <- predictDecomposed((r_arima_7$mean)^3, 7, merged_decomposed_additive)
forecast_decomp_arima <- append(forecast_decomp_arima, pred)

# hour = 8
#regression
ggplot(merged8, aes(x=date, y=remainder))+geom_line()
ggpairs(data=merged8, columns=c(42:46))


remainder8_ts <- ts(remainder8[c(1:length(remainder8)-1)])
l <- length(remainder8) - 1
acf(remainder8_ts[c(2:l)])
pacf(remainder8_ts[c(2:l)])
merged8[,lag1_remainder:=shift(remainder,type="lag",n=1)]
merged8[,lag2_remainder:=shift(remainder,type="lag",n=2)]
merged8[,lag3_remainder:=shift(remainder,type="lag",n=3)]

formula8 = 'remainder ~ month + avg_temp + avg_cloud + avg_hum + avg_dw + lag1_remainder'

fit8<- lm(as.formula(formula8),merged8[-nrow(merged8)])
summary(fit8)
checkresiduals(fit8)

r_regression_8 <- predict(fit8,merged8[nrow(merged8)])^3
pred <- predictDecomposed(r_regression_8, 8, merged_decomposed_additive)
forecast_decomp_lm <- append(forecast_decomp_lm, pred)

# arima
auto_arima_8 <- auto.arima(merged8$remainder)
summary(auto_arima_8)
r_arima_8=forecast(auto_arima_8,h=1)
r_arima_8
pred <- predictDecomposed((r_arima_8$mean)^3, 8, merged_decomposed_additive)
forecast_decomp_arima <- append(forecast_decomp_arima, pred)

# hour = 9
#regression
#ggplot(merged9, aes(x=date, y=remainder))+geom_line()
#ggpairs(data=merged9, columns=c(42:46))



remainder9_ts <- ts(remainder9[c(1:length(remainder9)-1)])
l <- length(remainder9) - 1
#acf(remainder9_ts[c(2:l)])
#pacf(remainder9_ts[c(2:l)])
merged9[,lag1_remainder:=shift(remainder,type="lag",n=1)]
merged9[,lag2_remainder:=shift(remainder,type="lag",n=2)]
merged9[,lag3_remainder:=shift(remainder,type="lag",n=3)]

formula9 = 'remainder ~ month + avg_temp + avg_dw + lag1_remainder'

fit9<- lm(as.formula(formula9),merged9[-nrow(merged9)])
#summary(fit9)
#checkresiduals(fit9)

r_regression_9 <- predict(fit9,merged9[nrow(merged9)])^3
pred <- predictDecomposed(r_regression_9, 9, merged_decomposed_additive)
forecast_decomp_lm <- append(forecast_decomp_lm, pred)

# arima
auto_arima_9 <- auto.arima(merged9$remainder)
#summary(auto_arima_9)
r_arima_9=forecast(auto_arima_9,h=1)
r_arima_9
pred <- predictDecomposed((r_arima_9$mean)^3, 9, merged_decomposed_additive)
forecast_decomp_arima <- append(forecast_decomp_arima, pred)

# hour = 10
#regression
#ggplot(merged10, aes(x=date, y=remainder))+geom_line()
#ggpairs(data=merged10, columns=c(42:46))



remainder10_ts <- ts(remainder10[c(1:length(remainder10)-1)])
l <- length(remainder10) - 1
#acf(remainder10_ts[c(2:l)])
#pacf(remainder10_ts[c(2:l)])
merged10[,lag1_remainder:=shift(remainder,type="lag",n=1)]
merged10[,lag2_remainder:=shift(remainder,type="lag",n=2)]
merged10[,lag3_remainder:=shift(remainder,type="lag",n=3)]

formula10 = 'remainder ~ month + avg_temp + avg_dw + lag1_remainder'

fit10<- lm(as.formula(formula10),merged10[-nrow(merged10)])
#summary(fit10)
#checkresiduals(fit10)

r_regression_10 <- predict(fit10,merged10[nrow(merged10)])^3
pred <- predictDecomposed(r_regression_10, 10, merged_decomposed_additive)
forecast_decomp_lm <- append(forecast_decomp_lm, pred)

# arima
auto_arima_10 <- auto.arima(merged10$remainder)
#summary(auto_arima_10)
r_arima_10=forecast(auto_arima_10,h=1)
r_arima_10
pred <- predictDecomposed((r_arima_10$mean)^3, 10, merged_decomposed_additive)
forecast_decomp_arima <- append(forecast_decomp_arima, pred)

# hour = 11
#regression
#ggplot(merged11, aes(x=date, y=remainder))+geom_line()
#ggpairs(data=merged11, columns=c(42:46))



remainder11_ts <- ts(remainder11[c(1:length(remainder11)-1)])
l <- length(remainder11) - 1
#acf(remainder11_ts[c(2:l)])
#pacf(remainder11_ts[c(2:l)])
merged11[,lag1_remainder:=shift(remainder,type="lag",n=1)]
merged11[,lag2_remainder:=shift(remainder,type="lag",n=2)]
merged11[,lag3_remainder:=shift(remainder,type="lag",n=3)]

formula11 = 'remainder ~ month + avg_temp + avg_cloud + avg_dw + lag1_remainder + lag2_remainder + lag3_remainder'

fit11<- lm(as.formula(formula11),merged11[-nrow(merged11)])
#summary(fit11)
#checkresiduals(fit11)

r_regression_11 <- predict(fit11,merged11[nrow(merged11)])^3
pred <- predictDecomposed(r_regression_11, 11, merged_decomposed_additive)
forecast_decomp_lm <- append(forecast_decomp_lm, pred)

# arima
auto_arima_11 <- auto.arima(merged11$remainder)
#summary(auto_arima_11)
r_arima_11=forecast(auto_arima_11,h=1)
r_arima_11
pred <- predictDecomposed((r_arima_11$mean)^3, 11, merged_decomposed_additive)
forecast_decomp_arima <- append(forecast_decomp_arima, pred)

# hour = 12
#regression
#ggplot(merged12, aes(x=date, y=remainder))+geom_line()
#ggpairs(data=merged12, columns=c(42:46))



remainder12_ts <- ts(remainder12[c(1:length(remainder12)-1)])
l <- length(remainder12) - 1
#acf(remainder12_ts[c(2:l)])
#pacf(remainder12_ts[c(2:l)])
merged12[,lag1_remainder:=shift(remainder,type="lag",n=1)]
merged12[,lag2_remainder:=shift(remainder,type="lag",n=2)]
merged12[,lag3_remainder:=shift(remainder,type="lag",n=3)]

formula12 = 'remainder ~ month + avg_temp + avg_cloud + lag1_remainder + lag2_remainder + lag3_remainder'

fit12<- lm(as.formula(formula12),merged12[-nrow(merged12)])
#summary(fit12)
#checkresiduals(fit12)

r_regression_12 <- predict(fit12,merged12[nrow(merged12)])^3
pred <- predictDecomposed(r_regression_12, 12, merged_decomposed_additive)
forecast_decomp_lm <- append(forecast_decomp_lm, pred)

# arima
auto_arima_12 <- auto.arima(merged12$remainder)
#summary(auto_arima_12)
r_arima_12=forecast(auto_arima_12,h=1)
r_arima_12
pred <- predictDecomposed((r_arima_12$mean)^3, 12, merged_decomposed_additive)
forecast_decomp_arima <- append(forecast_decomp_arima, pred)

# hour = 13
#regression
#ggplot(merged13, aes(x=date, y=remainder))+geom_line()
#ggpairs(data=merged13, columns=c(42:46))

formula13 = 'remainder ~ month + avg_temp + avg_cloud + avg_hum + avg_dw'
formula13 = 'remainder ~ month + avg_temp + avg_cloud + avg_dw'

remainder13_ts <- ts(remainder13[c(1:length(remainder13)-1)])
l <- length(remainder13) - 2
#acf(remainder13_ts[c(2:l)])
#pacf(remainder13_ts[c(2:l)])

merged13[,lag1_remainder:=shift(remainder,type="lag",n=1)]
merged13[,lag2_remainder:=shift(remainder,type="lag",n=2)]
merged13[,lag3_remainder:=shift(remainder,type="lag",n=3)]
merged13[,lag4_remainder:=shift(remainder,type="lag",n=4)]
merged13[,lag5_remainder:=shift(remainder,type="lag",n=5)]


formula13 = 'remainder ~ month + avg_temp + avg_cloud + avg_dw'

fit13<- lm(as.formula(formula13),merged13[-nrow(merged13)])
#summary(fit13)
#checkresiduals(fit13)

r_regression_13 <- predict(fit13,merged13[nrow(merged13)])^3
pred <- predictDecomposed(r_regression_13, 13, merged_decomposed_additive)
forecast_decomp_lm <- append(forecast_decomp_lm, pred)

# arima
auto_arima_13 <- auto.arima(merged13$remainder)
#summary(auto_arima_13)
r_arima_13=forecast(auto_arima_13,h=1)
r_arima_13
pred <- predictDecomposed((r_arima_13$mean)^3, 13, merged_decomposed_additive)
forecast_decomp_arima <- append(forecast_decomp_arima, pred)


# hour = 14
#regression
#ggplot(merged14, aes(x=date, y=remainder))+geom_line()
#ggpairs(data=merged14, columns=c(42:46))



remainder14_ts <- ts(remainder14[c(1:length(remainder14)-1)])
l <- length(remainder14) - 2
#acf(remainder14_ts[c(2:l)])
#pacf(remainder14_ts[c(2:l)])
merged14[,lag1_remainder:=shift(remainder,type="lag",n=1)]
merged14[,lag2_remainder:=shift(remainder,type="lag",n=2)]
merged14[,lag3_remainder:=shift(remainder,type="lag",n=3)]


formula14 = 'remainder ~ month + avg_temp + avg_hum + avg_dw'

fit14<- lm(as.formula(formula14),merged14[-nrow(merged14)])
#summary(fit14)
#checkresiduals(fit14)

r_regression_14 <- predict(fit14,merged14[nrow(merged14)])^3
pred <- predictDecomposed(r_regression_14, 14, merged_decomposed_additive)
forecast_decomp_lm <- append(forecast_decomp_lm, pred)

# arima
auto_arima_14 <- auto.arima(merged14$remainder)
#summary(auto_arima_14)
r_arima_14=forecast(auto_arima_14,h=1)
r_arima_14
pred <- predictDecomposed((r_arima_14$mean)^3, 14, merged_decomposed_additive)
forecast_decomp_arima <- append(forecast_decomp_arima, pred)

# hour = 15
#regression
#ggplot(merged15, aes(x=date, y=remainder))+geom_line()
#ggpairs(data=merged15, columns=c(42:46))


remainder15_ts <- ts(remainder15[c(1:length(remainder15)-1)])
l <- length(remainder15) - 2
#acf(remainder15_ts[c(2:l)])
#pacf(remainder15_ts[c(2:l)])
merged15[,lag1_remainder:=shift(remainder,type="lag",n=1)]
merged15[,lag2_remainder:=shift(remainder,type="lag",n=2)]
merged15[,lag3_remainder:=shift(remainder,type="lag",n=3)]


formula15 = 'remainder ~ month + avg_dw + avg_temp + avg_hum + avg_dw*avg_temp'

fit15<- lm(as.formula(formula15),merged15[-nrow(merged15)])
#summary(fit15)
#checkresiduals(fit15)

r_regression_15 <- predict(fit15,merged15[nrow(merged15)])^3
pred <- predictDecomposed(r_regression_15, 15, merged_decomposed_additive)
forecast_decomp_lm <- append(forecast_decomp_lm, pred)

# arima
auto_arima_15 <- auto.arima(merged15$remainder)
#summary(auto_arima_15)
r_arima_15=forecast(auto_arima_15,h=1)
r_arima_15
pred <- predictDecomposed((r_arima_15$mean)^3, 15, merged_decomposed_additive)
forecast_decomp_arima <- append(forecast_decomp_arima, pred)

# hour = 16
#regression
#ggplot(merged16, aes(x=date, y=remainder))+geom_line()
#ggpairs(data=merged16, columns=c(42:46))


remainder16_ts <- ts(remainder16[c(1:length(remainder16)-1)])
l <- length(remainder16) - 2
#acf(remainder16_ts[c(2:l)])
#pacf(remainder16_ts[c(2:l)])
merged16[,lag1_remainder:=shift(remainder,type="lag",n=1)]
merged16[,lag2_remainder:=shift(remainder,type="lag",n=2)]
merged16[,lag3_remainder:=shift(remainder,type="lag",n=3)]


formula16 = 'remainder ~ month + avg_dw + avg_hum*avg_temp'

fit16<- lm(as.formula(formula16),merged16[-nrow(merged16)])
#summary(fit16)
#checkresiduals(fit16)

r_regression_16 <- predict(fit16,merged16[nrow(merged16)])^3
pred <- predictDecomposed(r_regression_16, 16, merged_decomposed_additive)
forecast_decomp_lm <- append(forecast_decomp_lm, pred)

# arima
auto_arima_16 <- auto.arima(merged16$remainder)
#summary(auto_arima_16)
r_arima_16=forecast(auto_arima_16,h=1)
r_arima_16
pred <- predictDecomposed((r_arima_16$mean)^3, 16, merged_decomposed_additive)
forecast_decomp_arima <- append(forecast_decomp_arima, pred)

# hour = 17
#regression
#ggplot(merged17, aes(x=date, y=remainder))+geom_line()
#ggpairs(data=merged17, columns=c(42:46))


remainder17_ts <- ts(remainder17[c(1:length(remainder17)-1)])
l <- length(remainder17) - 2
#acf(remainder17_ts[c(2:l)])
#pacf(remainder17_ts[c(2:l)])
merged17[,lag1_remainder:=shift(remainder,type="lag",n=1)]
merged17[,lag2_remainder:=shift(remainder,type="lag",n=2)]
merged17[,lag3_remainder:=shift(remainder,type="lag",n=3)]

formula17 = 'remainder ~ month + poly(avg_dw,2)'

fit17<- lm(as.formula(formula17),merged17[-nrow(merged17)])
#summary(fit17)
#checkresiduals(fit17)

r_regression_17 <- predict(fit17,merged17[nrow(merged17)])^3
pred <- predictDecomposed(r_regression_17, 17, merged_decomposed_additive)
forecast_decomp_lm <- append(forecast_decomp_lm, pred)

# arima
auto_arima_17 <- auto.arima(merged17$remainder)
#summary(auto_arima_17)
r_arima_17=forecast(auto_arima_17,h=1)
r_arima_17
pred <- predictDecomposed((r_arima_17$mean)^3, 17, merged_decomposed_additive)
forecast_decomp_arima <- append(forecast_decomp_arima, pred)

# hour = 18
#regression
#ggplot(merged18, aes(x=date, y=remainder))+geom_line()
#ggpairs(data=merged18, columns=c(42:46))



remainder18_ts <- ts(remainder18[c(1:length(remainder18)-1)])
l <- length(remainder18) - 2
#acf(remainder18_ts[c(2:l)])
#pacf(remainder18_ts[c(2:l)])
merged18[,lag1_remainder:=shift(remainder,type="lag",n=1)]
merged18[,lag2_remainder:=shift(remainder,type="lag",n=2)]
merged18[,lag3_remainder:=shift(remainder,type="lag",n=3)]

formula18 = 'remainder ~ month + avg_temp + avg_hum + avg_temp*avg_hum + poly(avg_dw,2)'

fit18<- lm(as.formula(formula18),merged18[-nrow(merged18)])
#summary(fit18)
#checkresiduals(fit18)

r_regression_18 <- predict(fit18,merged18[nrow(merged18)])^3
pred <- predictDecomposed(r_regression_18, 18, merged_decomposed_additive)
forecast_decomp_lm <- append(forecast_decomp_lm, pred)

# arima
auto_arima_18 <- auto.arima(merged18$remainder)
#summary(auto_arima_18)
r_arima_18=forecast(auto_arima_18,h=1)
r_arima_18
pred <- predictDecomposed((r_arima_18$mean)^3, 18, merged_decomposed_additive)
forecast_decomp_arima <- append(forecast_decomp_arima, pred)

# hour = 19
#regression
#ggplot(merged19, aes(x=date, y=remainder))+geom_line()
#ggpairs(data=merged19, columns=c(42:46))



remainder19_ts <- ts(remainder19[c(1:length(remainder19)-1)])
l <- length(remainder19) - 2
#acf(remainder19_ts[c(2:l)])
#pacf(remainder19_ts[c(2:l)])
merged19[,lag1_remainder:=shift(remainder,type="lag",n=1)]
merged19[,lag2_remainder:=shift(remainder,type="lag",n=2)]
merged19[,lag3_remainder:=shift(remainder,type="lag",n=3)]

formula19 = 'remainder ~ month + avg_temp + avg_cloud + poly(avg_hum,2) + avg_dw*avg_cloud'

fit19<- lm(as.formula(formula19),merged19[-nrow(merged19)])
#summary(fit19)
#checkresiduals(fit19)

r_regression_19 <- predict(fit19,merged19[nrow(merged19)])^3
pred <- predictDecomposed(r_regression_19, 19, merged_decomposed_additive)
forecast_decomp_lm <- append(forecast_decomp_lm, pred)

# arima
auto_arima_19 <- auto.arima(merged19$remainder)
#summary(auto_arima_19)
r_arima_19=forecast(auto_arima_19,h=1)
r_arima_19
pred <- predictDecomposed((r_arima_19$mean)^3, 19, merged_decomposed_additive)
forecast_decomp_arima <- append(forecast_decomp_arima, pred)

forecast_decomp_lm <- append(forecast_decomp_lm, c(0, 0, 0, 0))
forecast_decomp_arima <- append(forecast_decomp_arima, c(0, 0, 0, 0))

forecast_decomp_lm
forecast_decomp_arima



todays_date = as.Date("2022-05-26")
weather_file='2022-05-26_weather.csv'
production_file='2022-05-26_production.csv'

# linear regression for separate hours
LinearRegression_Separated <- function(todays_date, weather_file, production_file){
  
  weather <- fread(weather_file)
  prod <- fread(production_file)
  
  # linear regression for each hour
  weather <- dcast(weather, date + hour ~ variable + lat + lon, value.var = "value")
  
  weather$date <- as.Date(weather$date, format = "%m/%d/%Y")
  prod$date <- as.Date(prod$date, format = "%Y-%m-%d")
  
  merged <- merge(weather, prod, by = c("date","hour"), all = T)
  merged=merged %>% filter( as.Date(date )<=as.Date(todays_date+1))
  show_na <-merged[rowSums(is.na(merged)) > 0, ]
  
  merged[,month:=months(as.Date(date))]
  for (i in c(0:4,21:23)){
    merged[merged$hour==i]$production=0
  }
  merged$month=as.factor(merged$month)
  merged$hour=as.factor(merged$hour)
  days<-unique((as.Date(show_na$date)))
  
  # missing data imputation
  a='production ~ +CLOUD_LOW_LAYER_36.25_33+CLOUD_LOW_LAYER_36.25_33.25+CLOUD_LOW_LAYER_36.25_33.5+CLOUD_LOW_LAYER_36.5_33+CLOUD_LOW_LAYER_36.5_33.25+CLOUD_LOW_LAYER_36.5_33.5+CLOUD_LOW_LAYER_36.75_33+CLOUD_LOW_LAYER_36.75_33.25+CLOUD_LOW_LAYER_36.75_33.5+DSWRF_36.25_33+DSWRF_36.25_33.25+DSWRF_36.25_33.5+DSWRF_36.5_33+DSWRF_36.5_33.25+DSWRF_36.5_33.5+DSWRF_36.75_33+DSWRF_36.75_33.25+DSWRF_36.75_33.5+REL_HUMIDITY_36.25_33+REL_HUMIDITY_36.25_33.25+REL_HUMIDITY_36.25_33.5+REL_HUMIDITY_36.5_33+REL_HUMIDITY_36.5_33.25+REL_HUMIDITY_36.5_33.5+REL_HUMIDITY_36.75_33+REL_HUMIDITY_36.75_33.25+REL_HUMIDITY_36.75_33.5+TEMP_36.25_33+TEMP_36.25_33.25+TEMP_36.25_33.5+TEMP_36.5_33+TEMP_36.5_33.25+TEMP_36.5_33.5+TEMP_36.75_33+TEMP_36.75_33.25+TEMP_36.75_33.5+month'
  for (i in 5:20){
    for (j in days[c(-1,-length(days))]){
      df=merged %>% filter(hour == i , as.Date(date )< as.Date(j))
      fit_<- lm(as.formula(a),df)
      merged[(merged$date==as.Date(j))&(merged$hour==i)]$production=predict(fit_,newdata=merged[(merged$date==as.Date(j))&(merged$hour==i)])
    }
  }
  
  # add columns
  merged[,month:=months(as.Date(date))]
  merged$month=as.factor(merged$month)
  merged$hour=as.factor(merged$hour)
  merged[,trend:=ceiling((1:.N)/24)]
  merged[,avg_temp:=rowMeans(merged[,c(30:38)])]
  merged[,avg_hum:=rowMeans(merged[,c(21:29)])]
  merged[,avg_cloud:=rowMeans(merged[,c(3:11)])]
  merged[,avg_dw:=rowMeans(merged[,c(12:20)])]
  
  # separation of data into hours
  for (i in 0:23){
    value <- merged[hour==i]
    x <- paste("merged",i, sep="")
    eval(call("<-", as.name(x), value))
  }
  
  addWindow <- function(col, type, m){
    if (type=="moving_average"){
      arr<-rollmean(col,m,align='right') 
      arr=c(rep(NA,m-1),arr) 
    }
    else if(type=="max_capacity"){
      arr<-rollmax(col,m,align='right') 
      arr=c(rep(NA,m-1),arr) 
    }
    arr[length(arr)] <- arr[length(arr)-1]
    return (arr)
  }
  merged5$ma_y = addWindow(merged5$production, type="moving_average", m=3)
  merged5$max_capacity = addWindow(merged5$production, type="max_capacity", m=7)
  merged6$ma_y = addWindow(merged6$production, type="moving_average", m=3)
  merged6$max_capacity = addWindow(merged6$production, type="max_capacity", m=7)
  merged7$ma_y = addWindow(merged7$production, type="moving_average", m=3)
  merged7$max_capacity = addWindow(merged7$production, type="max_capacity", m=7)
  merged8$ma_y = addWindow(merged8$production, type="moving_average", m=3)
  merged8$max_capacity = addWindow(merged8$production, type="max_capacity", m=7)
  merged9$ma_y = addWindow(merged9$production, type="moving_average", m=3)
  merged9$max_capacity = addWindow(merged9$production, type="max_capacity", m=7)
  merged10$ma_y = addWindow(merged10$production, type="moving_average", m=3)
  merged10$max_capacity = addWindow(merged10$production, type="max_capacity", m=7)
  merged11$ma_y = addWindow(merged11$production, type="moving_average", m=3)
  merged11$max_capacity = addWindow(merged11$production, type="max_capacity", m=7)
  merged12$ma_y = addWindow(merged12$production, type="moving_average", m=3)
  merged12$max_capacity = addWindow(merged12$production, type="max_capacity", m=7)
  merged13$ma_y = addWindow(merged13$production, type="moving_average", m=3)
  merged13$max_capacity = addWindow(merged13$production, type="max_capacity", m=7)
  merged14$ma_y = addWindow(merged14$production, type="moving_average", m=3)
  merged14$max_capacity = addWindow(merged14$production, type="max_capacity", m=7)
  merged15$ma_y = addWindow(merged15$production, type="moving_average", m=3)
  merged15$max_capacity = addWindow(merged15$production, type="max_capacity", m=7)
  merged16$ma_y = addWindow(merged16$production, type="moving_average", m=3)
  merged16$max_capacity = addWindow(merged16$production, type="max_capacity", m=7)
  merged17$ma_y = addWindow(merged17$production, type="moving_average", m=3)
  merged17$max_capacity = addWindow(merged17$production, type="max_capacity", m=7)
  merged18$ma_y = addWindow(merged18$production, type="moving_average", m=3)
  merged18$max_capacity = addWindow(merged18$production, type="max_capacity", m=7)
  merged19$ma_y = addWindow(merged19$production, type="moving_average", m=3)
  merged19$max_capacity = addWindow(merged19$production, type="max_capacity", m=7)
  
  
  merged5[,lag1_y:=shift(production,type="lag",n=1)]
  merged5[,lag14_y:=shift(production,type="lag",n=14)]
  merged6[,lag1_y:=shift(production,type="lag",n=1)]
  merged7[,lag1_y:=shift(production,type="lag",n=1)]
  merged8[,lag1_y:=shift(production,type="lag",n=1)]
  merged9[,lag1_y:=shift(production,type="lag",n=1)]
  merged10[,lag1_y:=shift(production,type="lag",n=1)]
  merged11[,lag1_y:=shift(production,type="lag",n=1)]
  merged12[,lag1_y:=shift(production,type="lag",n=1)]
  merged13[,lag1_y:=shift(production,type="lag",n=1)]
  merged14[,lag1_y:=shift(production,type="lag",n=1)]
  merged15[,lag1_y:=shift(production,type="lag",n=1)]
  merged16[,lag1_y:=shift(production,type="lag",n=1)]
  merged17[,lag1_y:=shift(production,type="lag",n=1)]
  merged18[,lag1_y:=shift(production,type="lag",n=1)]
  merged19[,lag1_y:=shift(production,type="lag",n=1)]
  
  # model building and predictions
  forecast_lm <- c(0, 0, 0, 0, 0)
  
  # hour=5
  forfit5=merged5 %>% filter( as.Date(date )<todays_date&as.Date(date )>=as.Date("2021-05-01"))
  formula5='production ~ +max_capacity*trend+avg_cloud+avg_temp+avg_hum+month+lag1_y+lag14_y'
  fit5<- lm(as.formula(formula5),forfit5)
  forecast_lm <- append(forecast_lm, predict(fit5,merged5[nrow(merged5)]))
  
  # hour=6
  forfit6=merged6 %>% filter( as.Date(date )<todays_date&as.Date(date )>=as.Date("2021-05-01"))
  formula6='production ~ +max_capacity*trend+avg_cloud+avg_temp+avg_hum+month+ma_y'
  fit6<- lm(as.formula(formula6),forfit6)
  forecast_lm <- append(forecast_lm, predict(fit6,merged6[nrow(merged6)]))
  
  #hour=7
  formula7="production~+I(avg_cloud^2)+I(avg_temp^2)+I(avg_hum^2)+max_capacity*trend+ma_y+month"
  fit7<- lm(as.formula(formula7),merged7)
  forecast_lm <- append(forecast_lm, predict(fit7,merged7[nrow(merged7)]))
  
  #hour=8
  formula8='production ~ +avg_cloud+avg_dw+avg_hum+avg_temp+month+max_capacity*trend+ma_y+lag1_y'
  fit8<- lm(as.formula(formula8),merged8)
  forecast_lm <- append(forecast_lm, predict(fit8,merged8[nrow(merged8)]))
  
  #hour=9
  forfit9=merged9 %>% filter( as.Date(date )<todays_date&as.Date(date )>=as.Date("2021-04-01"))
  forfit9[,log_prod:=log(forfit9$production)]
  forfit9[forfit9$production==0]$log_prod=NA
  formula9='log_prod ~+trend +avg_temp+avg_hum+month+ma_y+max_capacity*trend+avg_hum+lag1_y'
  fit9<- lm(as.formula(formula9),forfit9)
  forecast_lm <- append(forecast_lm, exp(predict(fit9,merged9[nrow(merged9)])))
  
  #hour=10
  forfit10=merged10 %>% filter( as.Date(date )<todays_date&as.Date(date )>=as.Date("2022-04-01"))
  forfit10[,diff_prod:=c(NA,diff(forfit10$production))]
  formula10='diff_prod ~+trend+month+avg_hum+avg_dw+avg_cloud+avg_temp+ma_y'
  fit10<- lm(as.formula(formula10),forfit10)
  forecast_lm <- append(forecast_lm, merged10$production[nrow(merged10)-3] + predict(fit10,merged10[nrow(merged10)]))
  
  #hour=11
  forfit11=merged10 %>% filter( as.Date(date )<todays_date&as.Date(date )>=as.Date("2022-04-01"))
  forfit11[,diff_prod:=c(NA,diff(forfit11$production))]
  formula11='production ~ma_y+month+avg_temp+avg_hum+avg_cloud+max_capacity+lag1_y'
  fit11<- lm(as.formula(formula11),forfit11)
  forecast_lm <- append(forecast_lm, predict(fit11,merged11[nrow(merged11)]))
  
  #hour=12
  formula12='production ~trend+month+avg_temp+avg_hum+avg_cloud+max_capacity+lag1_y'
  fit12<- lm(as.formula(formula12),merged12)
  forecast_lm <- append(forecast_lm, predict(fit12,merged12[nrow(merged12)]))
  
  #hour=13
  formula13='production ~trend+month+avg_temp+avg_hum+lag1_y+ma_y' 
  fit13 <- lm(as.formula(formula13),merged13)
  forecast_lm <- append(forecast_lm, predict(fit13,merged13[nrow(merged13)]))
  
  #hour=14
  formula14='production ~month+avg_temp+avg_hum+ma_y+avg_temp+avg_dw+lag1_y' 
  fit14 <- lm(as.formula(formula14),merged14)
  forecast_lm <- append(forecast_lm, predict(fit14,merged14[nrow(merged14)]))
  
  #hour=15
  formula15 = 'diff_prod ~ month+avg_hum+ma_y+avg_dw+lag1_y+max_capacity'
  merged15[,diff_prod:=c(NA,diff(merged15$production))]
  fit15<- lm(as.formula(formula15),merged15)
  forecast_lm <- append(forecast_lm, merged15$production[nrow(merged15)-3] + predict(fit15,merged15[nrow(merged15)]))
  
  #hour=16
  formula16='production ~trend+month+avg_temp+lag1_y+ma_y+I(avg_hum^2)+avg_dw+max_capacity*avg_hum' 
  fit16<- lm(as.formula(formula16),merged16)
  forecast_lm <- append(forecast_lm, predict(fit16,merged16[nrow(merged16)]))
  
  #hour=17
  formula17='production ~trend+month+avg_temp+lag1_y+ma_y+I(avg_hum^2)+avg_dw'
  fit17<- lm(as.formula(formula17),merged17)
  forecast_lm <- append(forecast_lm, predict(fit17,merged17[nrow(merged17)]))
  
  #hour=18
  formula18='production ~month+avg_temp+lag1_y+ma_y+avg_dw+I(avg_dw^2)'
  fit18<- lm(as.formula(formula18),merged18)
  forecast_lm <- append(forecast_lm, predict(fit18,merged18[nrow(merged18)]))
  
  #hour=19
  forfit19=merged19 %>% filter( as.Date(date )<todays_date&as.Date(date )>=as.Date("2021-04-01"))
  formula19='production ~month+lag1_y+ma_y'
  fit19<- lm(as.formula(formula19),forfit19)
  forecast_lm <- append(forecast_lm, predict(fit19,merged19[nrow(merged19)]))
  
  forecast_lm <- append(forecast_lm, c(0, 0, 0, 0))
  return(forecast_lm) 
}



# SARIMA for separate hours
SARIMA_Separated <- function(todays_date, production_file){
  
  # SARIMA for each hour
  
  forecast_date=todays_date+1
  
  production=fread(production_file)
  
  latest_available_prod_date=as.Date(max(production$date))
  n_days=as.numeric(forecast_date-latest_available_prod_date)
  
  forecasted_production=tail(production,n_days*24)
  forecasted_production[,date:=date+n_days]
  forecasted_production[,production:=NA]
  
  production_with_forecast=rbind(production,forecasted_production)
  
  # create a template for forecast date
  # forecast_table=data.table(date=forecast_date,hour=0:23,production=NA)
  forecast_sarima_sep <- c()
  
  for (i in 0:23){
    value <- production_with_forecast[hour==i]
    x <- paste("prod",i, sep="")
    eval(call("<-", as.name(x), value))
  }
  
  forecast_table[,sarima:=NA]
  forecast_table$sarima=as.numeric(forecast_table$sarima)
  
  for (i in 0:23){
    x <- paste("prod",i, sep="")
    value <- ts(eval(parse( text=paste("prod", i, sep = "")))[!is.na(production)]$production,frequency=30)
    ser<-paste("series_",i, sep="")
    eval(call("<-", as.name(ser), value))
    sarima_model=auto.arima(eval(parse( text=paste("series_", i, sep = ""))),seasonal=T,stepwise=T,approximation=T,trace=T)
    forecast_ahead=nrow(forecast_table)/24
    sarima_forecast=forecast(sarima_model,h=forecast_ahead)
    #forecast_table[hour==i,"sarima"]=as.numeric(format(sarima_forecast$mean,scientific = FALSE))
    forecast_sarima_sep <- append(forecast_sarima_sep, as.numeric(format(sarima_forecast$mean,scientific = FALSE)))
  }
  
  return(forecast_sarima_sep)
}


# SARIMA unseparated 
SARIMA_Unseparated <- function(todays_date, production_file){
  
  production=fread(production_file)
  weather=fread(weather_file)
  forecast_date=todays_date+1
  
  latest_available_prod_date=as.Date(max(production$date))
  n_days=as.numeric(forecast_date-latest_available_prod_date)
  
  forecasted_production=tail(production,n_days*24)
  forecasted_production[,date:=date+n_days]
  forecasted_production[,production:=NA]
  
  production_with_forecast=rbind(production,forecasted_production)
  # forecast_table=data.table(date=forecast_date,hour=0:23,production=NA)
  
  production_with_forecast=production_with_forecast[order(date,hour)]
  production_series=ts(production_with_forecast[!is.na(production)]$production,frequency=24)
  
  sarima_model=auto.arima(production_series,seasonal=T,stepwise=T,approximation=T,trace=T)
  
  forecast_ahead=nrow(forecast_table)
  sarima_forecast=forecast(sarima_model,h=forecast_ahead)
  
  forecast_sarima_unsep = as.numeric(format(tail(sarima_forecast$mean,24),scientific = FALSE))
  
  return (forecast_sarima_unsep)
}


Decomposition <- function(todays_date, weather_file, production_file){
  
  # decomposition
  weather <- fread(weather_file)
  prod <- fread(production_file)
  
  weather <- dcast(weather, date + hour ~ variable + lat + lon, value.var = "value")
  
  weather$date <- as.Date(weather$date, format = "%m/%d/%Y")
  prod$date <- as.Date(prod$date, format = "%Y-%m-%d")
  
  merged <- merge(weather, prod, by = c("date","hour"), all = T)
  merged=merged %>% filter( as.Date(date )<=as.Date(todays_date+1))
  show_na <-merged[rowSums(is.na(merged)) > 0, ]
  
  merged[,month:=months(as.Date(date))]
  for (i in c(0:4,21:23)){
    merged[merged$hour==i]$production=0
  }
  merged$month=as.factor(merged$month)
  merged$hour=as.factor(merged$hour)
  days<-unique((as.Date(show_na$date)))
  
  a='production ~ +CLOUD_LOW_LAYER_36.25_33+CLOUD_LOW_LAYER_36.25_33.25+CLOUD_LOW_LAYER_36.25_33.5+CLOUD_LOW_LAYER_36.5_33+CLOUD_LOW_LAYER_36.5_33.25+CLOUD_LOW_LAYER_36.5_33.5+CLOUD_LOW_LAYER_36.75_33+CLOUD_LOW_LAYER_36.75_33.25+CLOUD_LOW_LAYER_36.75_33.5+DSWRF_36.25_33+DSWRF_36.25_33.25+DSWRF_36.25_33.5+DSWRF_36.5_33+DSWRF_36.5_33.25+DSWRF_36.5_33.5+DSWRF_36.75_33+DSWRF_36.75_33.25+DSWRF_36.75_33.5+REL_HUMIDITY_36.25_33+REL_HUMIDITY_36.25_33.25+REL_HUMIDITY_36.25_33.5+REL_HUMIDITY_36.5_33+REL_HUMIDITY_36.5_33.25+REL_HUMIDITY_36.5_33.5+REL_HUMIDITY_36.75_33+REL_HUMIDITY_36.75_33.25+REL_HUMIDITY_36.75_33.5+TEMP_36.25_33+TEMP_36.25_33.25+TEMP_36.25_33.5+TEMP_36.5_33+TEMP_36.5_33.25+TEMP_36.5_33.5+TEMP_36.75_33+TEMP_36.75_33.25+TEMP_36.75_33.5+month'
  for (i in 5:20){
    for (j in days[c(-1,-length(days))]){
      df=merged %>% filter(hour == i , as.Date(date )< as.Date(j))
      fit_<- lm(as.formula(a),df)
      merged[(merged$date==as.Date(j))&(merged$hour==i)]$production=predict(fit_,newdata=merged[(merged$date==as.Date(j))&(merged$hour==i)])
    }
  }
  
  merged <- merged[date != '2021-02-20']
  
  merged[,month:=months(as.Date(date))]
  merged$month=as.factor(merged$month)
  merged$hour=as.factor(merged$hour)
  merged[,trend:=ceiling((1:.N)/24)]
  merged[,avg_temp:=rowMeans(merged[,c(30:38)])]
  merged[,avg_hum:=rowMeans(merged[,c(21:29)])]
  merged[,avg_cloud:=rowMeans(merged[,c(3:11)])]
  merged[,avg_dw:=rowMeans(merged[,c(12:20)])]
  
  library(glue)
  for (i in 0:23){
    command_str = glue('merged{i} = merged[merged$hour=={i}]')
    eval(parse(text=command_str))
  }
  head(merged13)
  show_na13 <-merged13[rowSums(is.na(merged13)) > 0, ]
  
  nthroot<-function(x, n){
    sign(x)*abs(x)^(1/n)
  }
  
  predictDecomposed <- function(remainder_pred, hour, decomposed){
    trend_component <- NA
    i <- length(decomposed$trend)
    while(1){
      if (!as.logical(is.na(decomposed$trend[i]))){
        trend_component <- decomposed$trend[i]
        break
      }
      i <- i-1
    }
    seasonal_component <- unique(decomposed$seasonal)[hour+1]
    overall_prediction <- trend_component + seasonal_component + remainder_pred
    return (overall_prediction)
  }
  
  # decomposition of all data (before the split wrt hours)
  l <- nrow(merged) -24
  merged_ts<-ts(merged[1:l]$production, freq=24)
  merged_decomposed_additive = decompose(merged_ts,type="additive")
  
  remainders <- merged_decomposed_additive$random
  remainders <- nthroot(remainders, 3)
  
  # separate the remainders into hours
  for (h in 0:23){
    command_str = glue("remainder{h} <- c()")
    eval(parse(text=command_str))
  }
  for (i in 0:length(remainders)){
    h = i%%24
    command_str = glue("remainder{h} <- append(remainder{h}, remainders[i])")
    eval(parse(text=command_str))
  }
  for (h in 0:23){
    command_str = glue("remainder{h} <- append(remainder{h}, NA)")
    eval(parse(text=command_str))
  }
  # add remainders as columns to the dataframes separated by hours
  for (h in 0:23){
    command_str = glue("merged{h}[,remainder:=remainder{h}]")
    eval(parse(text=command_str))
  }
  
  # models and predictions
  forecast_decomp_lm <- c(0, 0, 0, 0, 0)
  forecast_decomp_arima <- c(0, 0, 0, 0, 0)
  
  # hour = 5
  #regression
  merged5[,lag1_remainder:=shift(remainder,type="lag",n=1)]
  formula5 = 'remainder ~ month + avg_temp + avg_cloud + lag1_remainder'
  fit5<- lm(as.formula(formula5),merged5[-nrow(merged5)])
  r_regression_5 <- predict(fit5,merged5[nrow(merged5)])^3
  pred <- predictDecomposed(r_regression_5, 5, merged_decomposed_additive)
  forecast_decomp_lm <- append(forecast_decomp_lm, pred)
  # arima
  auto_arima_5 <- auto.arima(merged5$remainder)
  r_arima_5=forecast(auto_arima_5,h=1)
  pred <- predictDecomposed((r_arima_5$mean)^3, 5, merged_decomposed_additive)
  forecast_decomp_arima <- append(forecast_decomp_arima, pred)
  
  # hour = 6
  #regression
  merged6[,lag1_remainder:=shift(remainder,type="lag",n=1)]
  formula6 = 'remainder ~ month + avg_temp + avg_cloud + avg_hum + lag1_remainder'
  fit6<- lm(as.formula(formula6),merged6[-nrow(merged6)])
  r_regression_6 <- predict(fit6,merged6[nrow(merged6)])^3
  pred <- predictDecomposed(r_regression_6, 6, merged_decomposed_additive)
  forecast_decomp_lm <- append(forecast_decomp_lm, pred)
  # arima
  auto_arima_6 <- auto.arima(merged6$remainder)
  r_arima_6=forecast(auto_arima_6,h=1)
  pred <- predictDecomposed((r_arima_6$mean)^3, 6, merged_decomposed_additive)
  forecast_decomp_arima <- append(forecast_decomp_arima, pred)
  
  # hour = 7
  #regression
  merged7[,lag1_remainder:=shift(remainder,type="lag",n=1)]
  formula7 = 'remainder ~ month + avg_temp + avg_cloud + avg_dw + lag1_remainder'
  fit7<- lm(as.formula(formula7),merged7[-nrow(merged7)])
  r_regression_7 <- predict(fit7,merged7[nrow(merged7)])^3
  pred <- predictDecomposed(r_regression_7, 7, merged_decomposed_additive)
  forecast_decomp_lm <- append(forecast_decomp_lm, pred)
  # arima
  auto_arima_7 <- auto.arima(merged7$remainder)
  r_arima_7=forecast(auto_arima_7,h=1)
  pred <- predictDecomposed((r_arima_7$mean)^3, 7, merged_decomposed_additive)
  forecast_decomp_arima <- append(forecast_decomp_arima, pred)
  
  # hour = 8
  #regression
  merged8[,lag1_remainder:=shift(remainder,type="lag",n=1)]
  formula8 = 'remainder ~ month + avg_temp + avg_cloud + avg_hum + avg_dw + lag1_remainder'
  fit8<- lm(as.formula(formula8),merged8[-nrow(merged8)])
  r_regression_8 <- predict(fit8,merged8[nrow(merged8)])^3
  pred <- predictDecomposed(r_regression_8, 8, merged_decomposed_additive)
  forecast_decomp_lm <- append(forecast_decomp_lm, pred)
  # arima
  auto_arima_8 <- auto.arima(merged8$remainder)
  r_arima_8=forecast(auto_arima_8,h=1)
  pred <- predictDecomposed((r_arima_8$mean)^3, 8, merged_decomposed_additive)
  forecast_decomp_arima <- append(forecast_decomp_arima, pred)
  
  # hour = 9
  #regression
  merged9[,lag1_remainder:=shift(remainder,type="lag",n=1)]
  formula9 = 'remainder ~ month + avg_temp + avg_dw + lag1_remainder'
  fit9<- lm(as.formula(formula9),merged9[-nrow(merged9)])
  r_regression_9 <- predict(fit9,merged9[nrow(merged9)])^3
  pred <- predictDecomposed(r_regression_9, 9, merged_decomposed_additive)
  forecast_decomp_lm <- append(forecast_decomp_lm, pred)
  # arima
  auto_arima_9 <- auto.arima(merged9$remainder)
  r_arima_9=forecast(auto_arima_9,h=1)
  pred <- predictDecomposed((r_arima_9$mean)^3, 9, merged_decomposed_additive)
  forecast_decomp_arima <- append(forecast_decomp_arima, pred)
  
  # hour = 10
  #regression
  merged10[,lag1_remainder:=shift(remainder,type="lag",n=1)]
  formula10 = 'remainder ~ month + avg_temp + avg_dw + lag1_remainder'
  fit10<- lm(as.formula(formula10),merged10[-nrow(merged10)])
  r_regression_10 <- predict(fit10,merged10[nrow(merged10)])^3
  pred <- predictDecomposed(r_regression_10, 10, merged_decomposed_additive)
  forecast_decomp_lm <- append(forecast_decomp_lm, pred)
  # arima
  auto_arima_10 <- auto.arima(merged10$remainder)
  r_arima_10=forecast(auto_arima_10,h=1)
  pred <- predictDecomposed((r_arima_10$mean)^3, 10, merged_decomposed_additive)
  forecast_decomp_arima <- append(forecast_decomp_arima, pred)
  
  # hour = 11
  #regression
  merged11[,lag1_remainder:=shift(remainder,type="lag",n=1)]
  merged11[,lag2_remainder:=shift(remainder,type="lag",n=2)]
  merged11[,lag3_remainder:=shift(remainder,type="lag",n=3)]
  formula11 = 'remainder ~ month + avg_temp + avg_cloud + avg_dw + lag1_remainder + lag2_remainder + lag3_remainder'
  fit11<- lm(as.formula(formula11),merged11[-nrow(merged11)])
  r_regression_11 <- predict(fit11,merged11[nrow(merged11)])^3
  pred <- predictDecomposed(r_regression_11, 11, merged_decomposed_additive)
  forecast_decomp_lm <- append(forecast_decomp_lm, pred)
  # arima
  auto_arima_11 <- auto.arima(merged11$remainder)
  r_arima_11=forecast(auto_arima_11,h=1)
  pred <- predictDecomposed((r_arima_11$mean)^3, 11, merged_decomposed_additive)
  forecast_decomp_arima <- append(forecast_decomp_arima, pred)
  
  # hour = 12
  #regression
  merged12[,lag1_remainder:=shift(remainder,type="lag",n=1)]
  merged12[,lag2_remainder:=shift(remainder,type="lag",n=2)]
  merged12[,lag3_remainder:=shift(remainder,type="lag",n=3)]
  formula12 = 'remainder ~ month + avg_temp + avg_cloud + lag1_remainder + lag2_remainder + lag3_remainder'
  fit12<- lm(as.formula(formula12),merged12[-nrow(merged12)])
  r_regression_12 <- predict(fit12,merged12[nrow(merged12)])^3
  pred <- predictDecomposed(r_regression_12, 12, merged_decomposed_additive)
  forecast_decomp_lm <- append(forecast_decomp_lm, pred)
  # arima
  auto_arima_12 <- auto.arima(merged12$remainder)
  r_arima_12=forecast(auto_arima_12,h=1)
  pred <- predictDecomposed((r_arima_12$mean)^3, 12, merged_decomposed_additive)
  forecast_decomp_arima <- append(forecast_decomp_arima, pred)
  
  # hour = 13
  #regression
  formula13 = 'remainder ~ month + avg_temp + avg_cloud + avg_dw'
  fit13<- lm(as.formula(formula13),merged13[-nrow(merged13)])
  r_regression_13 <- predict(fit13,merged13[nrow(merged13)])^3
  pred <- predictDecomposed(r_regression_13, 13, merged_decomposed_additive)
  forecast_decomp_lm <- append(forecast_decomp_lm, pred)
  # arima
  auto_arima_13 <- auto.arima(merged13$remainder)
  r_arima_13=forecast(auto_arima_13,h=1)
  pred <- predictDecomposed((r_arima_13$mean)^3, 13, merged_decomposed_additive)
  forecast_decomp_arima <- append(forecast_decomp_arima, pred)
  
  # hour = 14
  #regression
  formula14 = 'remainder ~ month + avg_temp + avg_hum + avg_dw'
  fit14<- lm(as.formula(formula14),merged14[-nrow(merged14)])
  r_regression_14 <- predict(fit14,merged14[nrow(merged14)])^3
  pred <- predictDecomposed(r_regression_14, 14, merged_decomposed_additive)
  forecast_decomp_lm <- append(forecast_decomp_lm, pred)
  # arima
  auto_arima_14 <- auto.arima(merged14$remainder)
  r_arima_14=forecast(auto_arima_14,h=1)
  pred <- predictDecomposed((r_arima_14$mean)^3, 14, merged_decomposed_additive)
  forecast_decomp_arima <- append(forecast_decomp_arima, pred)
  
  # hour = 15
  #regression
  formula15 = 'remainder ~ month + avg_dw + avg_temp + avg_hum + avg_dw*avg_temp'
  fit15<- lm(as.formula(formula15),merged15[-nrow(merged15)])
  r_regression_15 <- predict(fit15,merged15[nrow(merged15)])^3
  pred <- predictDecomposed(r_regression_15, 15, merged_decomposed_additive)
  forecast_decomp_lm <- append(forecast_decomp_lm, pred)
  # arima
  auto_arima_15 <- auto.arima(merged15$remainder)
  r_arima_15=forecast(auto_arima_15,h=1)
  pred <- predictDecomposed((r_arima_15$mean)^3, 15, merged_decomposed_additive)
  forecast_decomp_arima <- append(forecast_decomp_arima, pred)
  
  # hour = 16
  #regression
  formula16 = 'remainder ~ month + avg_dw + avg_hum*avg_temp'
  fit16<- lm(as.formula(formula16),merged16[-nrow(merged16)])
  r_regression_16 <- predict(fit16,merged16[nrow(merged16)])^3
  pred <- predictDecomposed(r_regression_16, 16, merged_decomposed_additive)
  forecast_decomp_lm <- append(forecast_decomp_lm, pred)
  # arima
  auto_arima_16 <- auto.arima(merged16$remainder)
  r_arima_16=forecast(auto_arima_16,h=1)
  pred <- predictDecomposed((r_arima_16$mean)^3, 16, merged_decomposed_additive)
  forecast_decomp_arima <- append(forecast_decomp_arima, pred)
  
  # hour = 17
  #regression
  formula17 = 'remainder ~ month + poly(avg_dw,2)'
  fit17<- lm(as.formula(formula17),merged17[-nrow(merged17)])
  r_regression_17 <- predict(fit17,merged17[nrow(merged17)])^3
  pred <- predictDecomposed(r_regression_17, 17, merged_decomposed_additive)
  forecast_decomp_lm <- append(forecast_decomp_lm, pred)
  # arima
  auto_arima_17 <- auto.arima(merged17$remainder)
  r_arima_17=forecast(auto_arima_17,h=1)
  pred <- predictDecomposed((r_arima_17$mean)^3, 17, merged_decomposed_additive)
  forecast_decomp_arima <- append(forecast_decomp_arima, pred)
  
  # hour = 18
  #regression
  formula18 = 'remainder ~ month + avg_temp + avg_hum + avg_temp*avg_hum + poly(avg_dw,2)'
  fit18<- lm(as.formula(formula18),merged18[-nrow(merged18)])
  r_regression_18 <- predict(fit18,merged18[nrow(merged18)])^3
  pred <- predictDecomposed(r_regression_18, 18, merged_decomposed_additive)
  forecast_decomp_lm <- append(forecast_decomp_lm, pred)
  # arima
  auto_arima_18 <- auto.arima(merged18$remainder)
  r_arima_18=forecast(auto_arima_18,h=1)
  pred <- predictDecomposed((r_arima_18$mean)^3, 18, merged_decomposed_additive)
  forecast_decomp_arima <- append(forecast_decomp_arima, pred)
  
  # hour = 19
  #regression
  formula19 = 'remainder ~ month + avg_temp + avg_cloud + poly(avg_hum,2) + avg_dw*avg_cloud'
  fit19<- lm(as.formula(formula19),merged19[-nrow(merged19)])
  r_regression_19 <- predict(fit19,merged19[nrow(merged19)])^3
  pred <- predictDecomposed(r_regression_19, 19, merged_decomposed_additive)
  forecast_decomp_lm <- append(forecast_decomp_lm, pred)
  # arima
  auto_arima_19 <- auto.arima(merged19$remainder)
  r_arima_19=forecast(auto_arima_19,h=1)
  pred <- predictDecomposed((r_arima_19$mean)^3, 19, merged_decomposed_additive)
  forecast_decomp_arima <- append(forecast_decomp_arima, pred)
  
  forecast_decomp_lm <- append(forecast_decomp_lm, c(0, 0, 0, 0))
  forecast_decomp_arima <- append(forecast_decomp_arima, c(0, 0, 0, 0))
  
  results = data.table(linear_regression=forecast_decomp_lm, ARIMA=forecast_decomp_arima)
  return (results)
}


ARIMA_Separated <- function(todays_date, weather_file, production_file){
  
  weather <- fread(weather_file)
  prod <- fread(production_file)
  
  weather <- dcast(weather, date + hour ~ variable + lat + lon, value.var = "value")
  weather$date <- as.Date(weather$date, format = "%m/%d/%Y")
  prod$date <- as.Date(prod$date, format = "%Y-%m-%d")
  merged <- merge(weather, prod, by = c("date","hour"), all = T)
  merged=merged %>% filter( as.Date(date )<=as.Date(todays_date+1))
  show_na <-merged[rowSums(is.na(merged)) > 0, ]
  
  merged[,month:=months(as.Date(date))]
  for (i in c(0:4,21:23)){
    merged[merged$hour==i]$production=0
  }
  merged$month=as.factor(merged$month)
  merged$hour=as.factor(merged$hour)
  days<-unique((as.Date(show_na$date)))
  
  merged[,month:=months(as.Date(date))]
  merged$month=as.factor(merged$month)
  merged$hour=as.factor(merged$hour)
  
  merged0 = (merged[merged$hour==0]) 
  merged1 = (merged[merged$hour==1]) 
  merged2 = (merged[merged$hour==2]) 
  merged3 = (merged[merged$hour==3]) 
  merged4 = (merged[merged$hour==4]) 
  merged5 = (merged[merged$hour==5]) 
  merged6 = (merged[merged$hour==6]) 
  merged7 = (merged[merged$hour==7]) 
  merged8 = (merged[merged$hour==8]) 
  merged9 = (merged[merged$hour==9]) 
  merged10 = (merged[merged$hour==10]) 
  merged11 = (merged[merged$hour==11]) 
  merged12 = (merged[merged$hour==12]) 
  merged13 = (merged[merged$hour==13]) 
  merged14 = (merged[merged$hour==14]) 
  merged15 = (merged[merged$hour==15]) 
  merged16 = (merged[merged$hour==16]) 
  merged17 = (merged[merged$hour==17]) 
  merged18 = (merged[merged$hour==18]) 
  merged19 = (merged[merged$hour==19]) 
  merged20 = (merged[merged$hour==20]) 
  merged21 = (merged[merged$hour==21]) 
  merged22 = (merged[merged$hour==22]) 
  merged23 = (merged[merged$hour==23]) 
  
  forecast_date=todays_date+1
  
  # create a template for forecast date
  forecast_table=data.table(date=forecast_date,hour=0:23,production=NA)
  forecast_table[,arima:=NA]
  forecast_table$arima=as.numeric(forecast_table$arima)
  forecast_table[,actual:=NA]
  forecast_table$actual=as.numeric(forecast_table$actual)
  forecast_table[,residual:=NA]
  forecast_table$residual=as.numeric(forecast_table$residual)
  
  
  for (i in c(1:5)) {
    forecast_table$arima[i] =0 
  }
  
  #merged5
  production_hour5_ts<-ts(merged5[!is.na(production)]$production,frequency=1)
  lambda <- BoxCox.lambda(production_hour5_ts, method ="guerrero", lower = -1, upper = 2)
  box_prod<-BoxCox(production_hour5_ts,lambda)
  diff_series<-diff(box_prod,1)
  arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
  forecast_ahead=3
  arima_forecast=forecast(arima_model,h=forecast_ahead)
  arima_forecast=InvBoxCox(as.numeric(format(arima_forecast$mean[3],scientific = FALSE)),as.numeric(lambda)) 
  forecast_table$arima[6]=(arima_forecast)
  
  #merged6
  production_hour6_ts<-ts(merged6[!is.na(production)]$production,frequency=1)
  lambda <- BoxCox.lambda(production_hour6_ts, method ="guerrero", lower = -1, upper = 2)
  box_prod<-BoxCox(production_hour6_ts,lambda)
  unt_test=ur.kpss(box_prod) 
  diff_series<-diff(production_hour6_ts,1)
  unt_test=ur.kpss(diff_series) 
  arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
  forecast_ahead=3
  arima_forecast=forecast(arima_model,h=forecast_ahead)
  forecast_table$arima[7]=sum(arima_forecast$mean)+merged6[nrow(merged6)-3,]$production
  
  #merged7
  production_hour7_ts<-ts(merged7[!is.na(production)]$production,frequency=1)
  lambda <- BoxCox.lambda(production_hour7_ts, method ="guerrero", lower = -1, upper = 2)
  box_prod<-BoxCox(production_hour7_ts,lambda)
  diff_series<-diff(production_hour7_ts,1)
  arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
  forecast_ahead=3
  arima_forecast=forecast(arima_model,h=forecast_ahead)
  forecast_table$arima[8]=sum(arima_forecast$mean)+merged7[nrow(merged7)-3,]$production
  
  #merged8
  production_hour8_ts<-ts(merged8[!is.na(production)]$production,frequency=1)
  lambda <- BoxCox.lambda(production_hour8_ts, method ="guerrero", lower = -1, upper = 2)
  box_prod<-BoxCox(production_hour8_ts,lambda)
  diff_series<-diff(production_hour8_ts,1)
  arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
  forecast_ahead=3
  arima_forecast=forecast(arima_model,h=forecast_ahead)
  forecast_table$arima[9]=sum(arima_forecast$mean)+merged8[nrow(merged8)-3,]$production
  
  #merged9
  production_hour9_ts<-ts(merged9[!is.na(production)]$production,frequency=1)
  lambda <- BoxCox.lambda(production_hour9_ts, method ="guerrero", lower = -1, upper = 2)
  box_prod<-BoxCox(production_hour9_ts,lambda)
  diff_series<-diff(production_hour9_ts,1)
  arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
  forecast_ahead=3
  arima_forecast=forecast(arima_model,h=forecast_ahead)
  forecast_table$arima[10]=sum(arima_forecast$mean)+merged9[nrow(merged9)-3,]$production
  
  #merged10
  production_hour10_ts<-ts(merged10[!is.na(production)]$production,frequency=1)
  lambda <- BoxCox.lambda(production_hour10_ts, method ="guerrero", lower = -1, upper = 2)
  box_prod<-BoxCox(production_hour10_ts,lambda)
  diff_series<-diff(production_hour10_ts,1)
  arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
  forecast_ahead=3
  arima_forecast=forecast(arima_model,h=forecast_ahead)
  forecast_table$arima[11]=sum(arima_forecast$mean)+merged10[nrow(merged10)-3,]$production
  
  #merged11
  production_hour11_ts<-ts(merged11[!is.na(production)]$production,frequency=1)
  lambda <- BoxCox.lambda(production_hour11_ts, method ="guerrero", lower = -1, upper = 2)
  box_prod<-BoxCox(production_hour11_ts,lambda)
  diff_series<-diff(production_hour11_ts,1)
  arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
  forecast_ahead=3
  arima_forecast=forecast(arima_model,h=forecast_ahead)
  forecast_table$arima[12]=sum(arima_forecast$mean)+merged11[nrow(merged11)-3,]$production
  
  #merged12 
  production_hour12_ts<-ts(merged12[!is.na(production)]$production,frequency=1)
  lambda <- BoxCox.lambda(production_hour12_ts, method ="guerrero", lower = -1, upper = 2)
  box_prod<-BoxCox(production_hour12_ts,lambda)
  diff_series<-diff(production_hour12_ts,1)
  arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
  forecast_ahead=3
  arima_forecast=forecast(arima_model,h=forecast_ahead)
  forecast_table$arima[13]=sum(arima_forecast$mean)+merged12[nrow(merged12)-3,]$production
  
  #merged13
  production_hour13_ts<-ts(merged13[!is.na(production)]$production,frequency=1)
  lambda <- BoxCox.lambda(production_hour13_ts, method ="guerrero", lower = -1, upper = 2)
  box_prod<-BoxCox(production_hour13_ts,lambda)
  diff_series<-diff(production_hour13_ts,1)
  arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
  forecast_ahead=3
  arima_forecast=forecast(arima_model,h=forecast_ahead)
  forecast_table$arima[14]=sum(arima_forecast$mean)+merged13[nrow(merged13)-3,]$production
  
  #merged14
  production_hour14_ts<-ts(merged14[!is.na(production)]$production,frequency=1)
  lambda <- BoxCox.lambda(production_hour14_ts, method ="guerrero", lower = -1, upper = 2)
  box_prod<-BoxCox(production_hour14_ts,lambda)
  diff_series<-diff(production_hour14_ts,1)
  arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
  forecast_ahead=3
  arima_forecast=forecast(arima_model,h=forecast_ahead)
  forecast_table$arima[15]=sum(arima_forecast$mean)+merged14[nrow(merged14)-3,]$production
  
  #merged15
  production_hour15_ts<-ts(merged15[!is.na(production)]$production,frequency=1)
  lambda <- BoxCox.lambda(production_hour15_ts, method ="guerrero", lower = -1, upper = 2)
  box_prod<-BoxCox(production_hour15_ts,lambda)
  diff_series<-diff(production_hour15_ts,1)
  unt_test=ur.kpss(diff_series) 
  arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
  forecast_ahead=3
  arima_forecast=forecast(arima_model,h=forecast_ahead)
  forecast_table$arima[16]=sum(arima_forecast$mean)+merged15[nrow(merged15)-3,]$production
  
  #merged16
  production_hour16_ts<-ts(merged16[!is.na(production)]$production,frequency=1)
  lambda <- BoxCox.lambda(production_hour16_ts, method ="guerrero", lower = -1, upper = 2)
  box_prod<-BoxCox(production_hour16_ts,lambda)
  diff_series<-diff(production_hour16_ts,1)
  arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
  forecast_ahead=3
  arima_forecast=forecast(arima_model,h=forecast_ahead)
  forecast_table$arima[17]=sum(arima_forecast$mean)+merged16[nrow(merged16)-3,]$production
  
  #merged17
  production_hour17_ts<-ts(merged17[!is.na(production)]$production,frequency=1)
  lambda <- BoxCox.lambda(production_hour17_ts, method ="guerrero", lower = -1, upper = 2)
  box_prod<-BoxCox(production_hour17_ts,lambda)
  diff_series<-diff(production_hour17_ts,1)
  arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
  forecast_ahead=3
  arima_forecast=forecast(arima_model,h=forecast_ahead)
  forecast_table$arima[18]=sum(arima_forecast$mean)+merged17[nrow(merged17)-3,]$production
  
  #merged18
  production_hour18_ts<-ts(merged18[!is.na(production)]$production,frequency=1)
  lambda <- BoxCox.lambda(production_hour18_ts, method ="guerrero", lower = -1, upper = 2)
  box_prod<-BoxCox(production_hour18_ts,lambda)
  diff_series<-diff(production_hour18_ts,1)
  arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
  forecast_ahead=3
  arima_forecast=forecast(arima_model,h=forecast_ahead)
  forecast_table$arima[19]=sum(arima_forecast$mean)+merged18[nrow(merged18)-3,]$production
  
  #merged19
  production_hour19_ts<-ts(merged19[!is.na(production)]$production,frequency=1)
  lambda <- BoxCox.lambda(production_hour19_ts, method ="guerrero", lower = -1, upper = 2)
  box_prod<-BoxCox(production_hour19_ts,lambda)
  diff_series<-diff(production_hour19_ts,1)
  arima_model=auto.arima(diff_series,seasonal=F,stepwise=T,approximation=T,trace=T)
  forecast_ahead=3
  arima_forecast=forecast(arima_model,h=forecast_ahead)
  forecast_table$arima[20]=sum(arima_forecast$mean)+merged19[nrow(merged19)-3,]$production
  
  #merged20
  production_hour20_ts<-ts(merged20[!is.na(production)]$production,frequency=1)
  lambda <- BoxCox.lambda(production_hour20_ts, method ="guerrero", lower = -1, upper = 2)
  box_prod<-BoxCox(production_hour20_ts,lambda)
  diff_series<-diff(production_hour20_ts,1)
  unt_test=ur.kpss(diff_series) 
  summary(unt_test)
  arima_model=auto.arima(diff_series,seasonal=F,stepwise=F,approximation=F,trace=T)
  forecast_ahead=3
  arima_forecast=forecast(arima_model,h=forecast_ahead)
  forecast_table$arima[21]=sum(arima_forecast$mean)+merged20[nrow(merged20)-3,]$production
  
  for (i in c(22:24)) {
    forecast_table$arima[i]=0
  }
  
  return (forecast_table$arima)
}


summarizeForecasts <- function(todays_date, weather_file, production_file, compare_days_before=0){
  lm_sep = LinearRegression_Separated(todays_date, weather_file, production_file)
  sarima_sep = SARIMA_Separated(todays_date, production_file)
  sarima_unsep = SARIMA_Unseparated(todays_date, production_file)
  arima_sep = ARIMA_Separated(todays_date, weather_file, production_file)
  decomposed = Decomposition(todays_date, weather_file, production_file)
  lm_decomposed = decomposed$linear_regression
  arima_decomposed = decomposed$ARIMA
  
  forecast_table=data.table(date=as.Date(todays_date+1),hour=0:23)
  forecast_table[,linear_regression:=lm_sep]
  forecast_table[,SARIMA_separated:=sarima_sep]
  forecast_table[,SARIMA_unseparated:=sarima_unsep]
  forecast_table[,ARIMA_separated:=arima_sep]
  forecast_table[,remainder_lm:=lm_decomposed]
  forecast_table[,remainder_ARIMA:=arima_decomposed]
  l = length(forecast_table)
  forecast_table[,mean:=rowMeans(forecast_table[,c(3:8)])]
  
  if (compare_days_before>0){
    production_data = fread(production_file)
    for (i in 1:compare_days_before){
      day = as.Date(todays_date-i-1)
      prods = production_data[production_data$date==day]$production
      cmd_str = glue("forecast_table[,days_before{i+2}:=prods]")
      eval(parse(text=cmd_str))
    }
  }
  return (forecast_table)
}

res1 = LinearRegression_Separated(todays_date, weather_file, production_file)
res2 = SARIMA_Separated(todays_date, production_file)
res3 = SARIMA_Unseparated(todays_date, production_file)
res4 = decomp = Decomposition(todays_date, weather_file, production_file)
decomp$ARIMA
decomp$linear_regression

forecasts <- summarizeForecasts(todays_date, weather_file, production_file, compare_days_before = 2)
forecasts





forecasts

getResiduals <- function(production_file, date_forecast_done, forecast_table){
  check_day_str = toString(date_forecast_done+3)
  check_prod_file = paste(check_day_str, "production.csv", sep="_")
  actual_prod = fread(check_prod_file)
  actual = actual_prod[date == as.Date(date_forecast_done+1), "production"]
  
  comparison_table=data.table(date=as.Date(date_forecast_done+1),hour=0:23, actual=actual)
  
  lm_sep = forecast_table$linear_regression
  residual1 = lm_sep - actual
    
  sarima_sep = forecast_table$SARIMA_separated
  residual2 = sarima_sep - actual
    
  sarima_unsep = forecast_table$SARIMA_unseparated
  residual3 = sarima_unsep - actual
  
  arima_sep = forecast_table$ARIMA_separated
  residual4 = arima_sep - actual
  
  lm_decomposed = forecast_table$remainder_lm
  residual5 = lm_decomposed - actual
  
  arima_decomposed = forecast_table$remainder_ARIMA
  residual6 = arima_decomposed - actual
  
  means = forecast_table$mean
  residual7 = means - actual
  
  comparison_table[,linear_regression:=lm_sep]
  comparison_table[,residuals1:=residual1]
  comparison_table[,SARIMA_separated:=sarima_sep]
  comparison_table[,residuals2:=residual2]
  comparison_table[,SARIMA_unseparated:=sarima_unsep]
  comparison_table[,residuals3:=residual3]
  comparison_table[,ARIMA_separated:=arima_sep]
  comparison_table[,residuals4:=residual4]
  comparison_table[,remainder_lm:=lm_decomposed]
  comparison_table[,residuals5:=residual5]
  comparison_table[,remainder_ARIMA:=arima_decomposed]
  comparison_table[,residuals6:=residual6]
  comparison_table[,mean:=means]
  comparison_table[,residuals7:=residual7]
  
  return (comparison_table)
}

WMAPE <- function(pred, actual, residual=NULL){
  if (is.null(residual)){
    sum_abs_res <- sum(abs(pred-actual))
  }
  else{
    sum_abs_res <- sum(abs(residual))
  }
  sum_actual <- sum(actual)
  return (sum_abs_res/sum_actual)
}


getWMAPEs <- function(table){
  print(paste("linear regression:", WMAPE(pred=NULL,actual=table$actual.production, residual=table$residuals1)))
  print(paste("SARIMA separated:", WMAPE(pred=NULL,actual=table$actual.production, residual=table$residuals2)))
  print(paste("SARIMA unseparated:", WMAPE(pred=NULL,actual=table$actual.production, residual=table$residuals3)))
  print(paste("ARIMA separated:", WMAPE(pred=NULL,actual=table$actual.production, residual=table$residuals4)))
  print(paste("remainder linear regresion:", WMAPE(pred=NULL,actual=table$actual.production, residual=table$residuals5)))
  print(paste("remainder ARIMA:", WMAPE(pred=NULL,actual=table$actual.production, residual=table$residuals6)))
  print(paste("mean:", WMAPE(pred=NULL,actual=table$actual.production, residual=table$residuals7)))
}


forecasts <- summarizeForecasts(todays_date, weather_file, production_file, compare_days_before = 2)
forecasts
comparison_table = getResiduals(production_file, date_forecast_done=as.Date("2022-05-26"), forecasts)
comparison_table
getWMAPEs(comparison_table)

