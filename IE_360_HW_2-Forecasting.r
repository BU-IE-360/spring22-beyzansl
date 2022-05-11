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
library(stringr)
library(skimr)
library(data.table)

df<-read.csv("IE360_Spring22_HW2_data.csv")


df

col_names=c("Quarter","UGS","RNUV","NLPG","PU","PG","NUGV","NDGV","GNPA","GNPC","GNP")

colnames(df)=col_names

str(df)

for (col in colnames(df)[-1]){
    df[,col]=as.character(df[,col])
    df[,col]<-str_replace_all(df[,col], " ", "")
    df[,col]<-as.numeric(df[,col])
}
df$Quarter=as.character(df$Quarter)
df$Quarter<-str_replace_all(df$Quarter, "_", " ")
df$Quarter<-as.Date(as.yearqtr(df$Quarter))

df_tr=df[is.na(df$UGS)==FALSE,]
df_te=df[is.na(df$UGS)==TRUE,]


skim(df_tr)

str(df_tr)

data_ts <- xts(x = df_tr[,-1], order.by = df_tr$Quarter)
data_ts_te <- xts(x = df_te[,-1], order.by = df_te$Quarter)
glimpse(data_ts)

d_table_tr=as.data.table(data_ts)
d_table_te=as.data.table(data_ts_te)

ggplot(df_tr,aes(x=Quarter,y=UGS)) +  geom_line()+
  xlab("Quarters") + ylab("gasoline sales (in 1000 m3)")+ ggtitle("Quarterly Gasoline Sales from 2000 to 2007")+
  scale_x_date(date_breaks = '3 month', date_labels = '%Y %b', date_minor_breaks = '3 month') +
  geom_smooth(color='red',linetype='solid',fill=NA)+theme(axis.text.x=element_text(angle=90,hjust=1, vjust = 0.5))

acf(df_tr$UGS, lag.max=20)

for (i in colnames(df_tr)){
    AutoCorrelation <- acf(df_tr[,i], lag.max=20, plot = FALSE)
plot(AutoCorrelation, main =i )
}
    

TimeSeries<-ts(df[,-1],frequency=4,start=c(2000,1))
plot(TimeSeries,cex.lab=0.8, cex.axis=0.8, cex.main=1)


ggpairs(data_ts)

ggcorrplot(corr = cor(d_table_tr[c(-4:-1),c(2:11)]),


           type = "upper", lab = TRUE,
           title = "Correlation Matrix",
           colors = c("skyblue4","snow","violet"),
           legend.title = "Correlation",)

fit_1<- lm(UGS~.-index,d_table_tr)
summary(fit_1)

fit_2<- lm(UGS~.-index-RNUV-PG,d_table_tr)
summary(fit_2)

d_table_tr[,trend:=c(1:.N)]

fit_3<- lm(UGS~.-index-RNUV-PG,d_table_tr)
summary(fit_3)

d_table_tr[,seasonality:=as.factor(quarters(d_table_tr$index))]
fit_4<- lm(UGS~.-index-RNUV-PG-trend,d_table_tr)
summary(fit_4)

fit_4<- lm(UGS~.-index-RNUV-PG-trend-GNPA-GNPC-GNP,d_table_tr)
summary(fit_4)

fit_5<- lm(UGS~.-index-RNUV-PG-GNPA-GNPC-GNP,d_table_tr)
summary(fit_5)

d_table_tr[,lag1_UGS:=shift(UGS,type="lag",n=1)]
fit_6<-lm(UGS~+PU+NUGV+NDGV+seasonality+trend+lag1_UGS,d_table_tr)
summary(fit_6)


d_table_tr[,lag4:=shift(UGS,type="lag",n=4)]
fit_7<-lm(UGS~+PU+NUGV+NDGV+trend+seasonality+lag4,d_table_tr)
summary(fit_7)

fit_8<-lm(UGS~+PU+NUGV+NDGV+trend+seasonality+lag4+lag1_UGS,d_table_tr)
summary(fit_8)

d_table_tr[,lag1_PU:=shift(PU,type="lag",n=1)]
d_table_tr[,lag1_NUGV:=shift(NUGV,type="lag",n=1)]
d_table_tr[,lag1_NDGV:=shift(NDGV,type="lag",n=1)]
d_table_tr[,lag1_RNUV:=shift(RNUV,type="lag",n=1)]
fit_9<-lm(UGS~+PU+NUGV+NDGV+trend+seasonality+lag1_NUGV+lag1_UGS,d_table_tr)
summary(fit_9)

d_table_tr[,NUGV_sq:=(d_table_tr$NUGV)^2]
fit_10<-lm(UGS~+PU+NUGV+NDGV+seasonality+trend+NUGV_sq+lag1_UGS, d_table_tr)
summary(fit_10)

library(caret)
ctrl <- trainControl(method = "LOOCV")
fit_4_loocv <- train(UGS~+NUGV+PU+NLPG+NDGV+seasonality , data =d_table_tr[-1,c(1:13)] , method = "lm", trControl = ctrl)
fit_4_loocv

fit_5_loocv <- train(UGS~+NUGV+PU+NLPG+NDGV+seasonality+trend, data =d_table_tr[-1,c(1:13)] , method = "lm", trControl = ctrl)
fit_5_loocv

fit_6_loocv <- train(UGS~+PU+NUGV+NDGV+trend+seasonality+lag1_UGS, data =d_table_tr[-1,-15] , method = "lm", trControl = ctrl)
fit_6_loocv

fit_9_loocv <- train(UGS~+PU+NUGV+NDGV+trend+seasonality+lag1_NUGV+lag1_UGS, data =d_table_tr[-1,-15] , method = "lm", trControl = ctrl)
fit_9_loocv

fit_10_loocv <- train(UGS~+PU+NUGV+NDGV+seasonality+trend+NUGV_sq+lag1_UGS, data =d_table_tr[-1,] , method = "lm", trControl = ctrl)
fit_10_loocv

ggplot(d_table_tr ,aes(x=index)) +
        geom_line(aes(y=UGS,color='real')) + 
        geom_line(aes(y=predict(fit_4,d_table_tr),color='model_4')) + 
        geom_line(aes(y=predict(fit_6,d_table_tr),color='model_6')) +
        geom_line(aes(y=predict(fit_9,d_table_tr),color='model_9'))

checkresiduals(fit_6)

plot(fit_6)

df_merge=d_table_tr[,c(1,2)]
df_merge[,"predicted_UGS"]=predict(fit_6,d_table_tr)
ggplot(df_merge,aes(x=index,y=UGS))+geom_line()+
    geom_point(aes(y=predicted_UGS))+ggtitle("Predicted and Actual Sales of Unleaded Gasoline")+ylab("gasoline sales (in 1000 m3)")+
    xlab("Date (Quarterly)")

d_table_te[,"trend"]=c((nrow(d_table_tr)+1):(nrow(d_table_tr)+nrow(d_table_te)))
d_table_te[,"seasonality"]=as.factor(quarters(d_table_te$index))
d_table_te[1,"lag1_UGS"]=c(d_table_tr$UGS[nrow(d_table_tr)])
d_table_te[1,"predicted_UGS"]=NA

d_table_te

for (i in c(1:nrow(d_table_te))){
    d_table_te$predicted_UGS[i]=predict(fit_6,newdata=d_table_te[i,])
    if(i<nrow(d_table_te)){
        d_table_te$lag1_UGS[i+1]=d_table_te$predicted_UGS[i]
    }
    
}

d_table_te
