library(readxl)
library(tidyverse)
library(reshape)
library(lubridate)
library(forecast)
library(astsa)
library(magrittr)


df <- read_xlsx('C:/Users/IOR_C/Downloads/SampleData.xlsx', sheet = 4) 

plot(as.ts(df$Total))


data <- df %>%  
  dplyr::rename(date=1) %>% 
  mutate(date = as.Date(date) %>% floor_date(., "1 months")) %>% 
  group_by(date) %>% summarise(value = sum(Total, na.rm = T))

data <- ts(data[,2], frequency = 12, start = c(2018,1))  

plot(data, xlab='Years', ylab = 'Office Supplies Sales')

hw <- HoltWinters(data)
plot(hw)

forecast <- predict(hw, n.ahead = 3, prediction.interval = T, level = 0.95)
plot(hw, forecast)


# Función para gráficas suavizadas ----

HWplot3<-function(ts_object,  n.ahead=4,  CI=.95,  error.ribbon='green', line.size=1){
  
  hw_object<-HoltWinters(ts_object)
  
  forecast<-predict(hw_object,  n.ahead=n.ahead,  prediction.interval=T,  level=CI)
  
  
  for_values<-data.frame(time=round(time(forecast),  3),  value_forecast=as.data.frame(forecast)$fit,  dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
  
  fitted_values<-data.frame(time=round(time(hw_object$fitted),  3),  value_fitted=as.data.frame(hw_object$fitted)$xhat)
  
  actual_values<-data.frame(time=round(time(hw_object$x),  3),  Actual=c(hw_object$x))
  
  
  graphset<-merge(actual_values,  fitted_values,  by='time',  all=TRUE)
  graphset<-merge(graphset,  for_values,  all=TRUE,  by='time')
  graphset[is.na(graphset$dev),  ]$dev<-0
  
  graphset$Fitted<-c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),  fitted_values$value_fitted,  for_values$value_forecast)
  
  
  graphset.melt<-melt(graphset[, c('time', 'Actual', 'Fitted')], id='time')
  
  p<-ggplot(graphset.melt,  aes(x=time,  y=value)) + geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill=error.ribbon) + geom_line(aes(colour=variable), size=line.size) + geom_vline(xintercept=max(actual_values$time),  lty=2) + xlab('Time') + ylab('Value') + labs(legend.position='bottom') + scale_colour_hue('')
  return(p)
  
}

HWplot3(data, n.ahead = 2) + 
  labs(title = "Ejemplo de Predicciones Holt-Winters ggplot2")+ 
  scale_x_continuous(breaks = seq(2018, 2020,0.5)) + 
  scale_colour_brewer("Valores", palette = "Set1")+theme_bw()
