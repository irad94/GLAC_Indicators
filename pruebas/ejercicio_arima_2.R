library(readxl)
library(tidyverse)
library(lubridate)
library(forecast)
library(astsa)
library(magrittr)


df <- read_xlsx('C:/Users/IOR_C/Downloads/SampleData.xlsx', sheet = 4) 

plot(as.ts(df$Total))


data <- df %>%  
  rename(date=1) %>% 
  mutate(date = as.Date(date) %>% floor_date(., "1 months")) %>% 
  group_by(date) %>% summarise(value = sum(Total, na.rm = T))

data <- ts(data[,2], frequency = 12, start = c(2018,1))  
  
plot(data, xlab='Years', ylab = 'Office Supplies Sales')


# plot(diff(data),ylab='Differenced Office Supplies Sales')
# plot(diff(log10(data)),ylab='Differenced Log (Office Supplies Sales)')

par(mfrow = c(1,2))
acf(ts(data),main='ACF Office Supplies Sales')
pacf(ts(data),main='PACF Office Supplies Sales')

ARIMAfit = auto.arima(data)
summary(ARIMAfit)

par(mfrow = c(1,1))
pred = predict(ARIMAfit, n.ahead = 3)
pred
plot(pred$pred)
plot.ts(data,type='l',xlim=c(2018,2021), xlab = 'Year',ylab = 'Office Supplies Sales')
lines(pred$pred,col='blue')
lines((pred$pred+2*pred$se),col='orange')
lines((pred$pred-2*pred$se),col='orange')
