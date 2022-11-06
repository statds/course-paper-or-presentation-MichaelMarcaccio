library(readxl)
library(dplyr)
library(writexl)
library(miscTools)
library(tseries)
library(forecast)
library(pls)
library(caret)
library(expsmooth)


setwd("~/GitHub/Project/course-paper-or-presentation-MichaelMarcaccio/Data")
Interaction_Results<- read_excel("Excel Interaction Call Center.xlsx")
Interation_Results<- data.frame(Interaction_Results)
times <- read.csv("Time_Template.csv")
times$date_time<- as.POSIXct(times$date_time,format = "%m/%d/%Y %H:%M")
freq<-data.frame(table(Interaction_Results$START15))
freq$Var1<- as.POSIXct(freq$Var1,tz = "EST",format = "%Y-%m-%d %H:%M")
names(freq) <- c("date_time","Volume")
df <-merge(times,freq,by = "date_time", all.x = TRUE)
df[1:50970,]
df$Volume = coalesce(df$Volume,0)
df$value <- coalesce(df$value,0)
df$new<-pmax(df$value,df$Volume)
ts_freq <- ts(df$new, frequency = 365*24*2, start = c(2020,1585))
ts_freq_train <- ts(ts_freq[1:floor(length(ts_freq)*.7)],  frequency = 365*24*2, start = c(2020,1585))
ts_freq_test <- ts(ts_freq[ceiling(length(ts_freq)*.7):length(ts_freq)],frequency = 365*24*2, end = c(2022,17520))

for (i in 0:5){
  
  for (d in 0:2){
    
    for (m in 0:5){
      
      fit <- arima(ts_freq_train, c(i, d, m),seasonal = list(order = c(1,0,1), period = 7*24*2), method = c('CSS'))
      
      pred <- predict(fit, n.ahead = (length(ts_freq)-ceiling(length(ts_freq)*.7)))
      
      ts_pred = ts(data = pred$pred, frequency = 365*24*2, end = c(2022,17520) )
      
      if(rSquared(ts_freq_test, resid = c(ts_freq_test - pred$pred)) > 0) {
        
        print(i)
        
        print(d)
        
        print(m)
        
        print(rSquared(ts_freq_test, resid = c(ts_freq_test - pred$pred)))}
      
    }}}