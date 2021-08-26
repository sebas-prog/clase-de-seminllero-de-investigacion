# llamar a la carpeta en donde esta la data 
# y la voy a ejecutar 

setwd("C:/Users/daffy/Downloads")
library(readr)
data<- read.csv("positivos_covid.csv",sep=";")

# llamo a las siguientes librerias 

library(lubridate)
library(tidyverse)
library(quantmod)
library(forecast)

data$FECHA_RESULTADO<- data$FECHA_RESULTADO %>% 
                ymd()
#
data1<-data %>% select(DEPARTAMENTO,
                                  FECHA_RESULTADO)  

s<- data1 %>% select(FECHA_RESULTADO) %>% group_by(FECHA_RESULTADO) %>% 
  count()


p
plot(s$n,type="l",col="blue")
lines(rollmean(s$n,7,fill = NA),col="red")
s %>% ggplot(aes(FECHA_RESULTADO,n))+geom_col(color="blue",
                                              alpha=0.5)+
  geom_line(mapping = aes(y=rollmean(s$n,7,fill = NA)),
            color="red",size=1)+theme_minimal()+xlab("")


data %>% select(DEPARTAMENTO,FECHA_RESULTADO)