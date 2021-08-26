# llamar a la carpeta en donde esta la data 
# y la voy a ejecutar 

setwd("C:/Users/atita/Downloads")
library(readr)
data<- read.csv("positivos_covid.csv",sep=";")

# llamo a las siguientes librerias 

library(lubridate)
library(tidyverse)#
library(quantmod)
library(forecast)

data$FECHA_RESULTADO<- data$FECHA_RESULTADO %>% 
                ymd()

data1<-data %>% select(DEPARTAMENTO,
                                  FECHA_RESULTADO)  

s<- data1 %>% select(FECHA_RESULTADO) %>% group_by(FECHA_RESULTADO) %>% 
  count()



plot(s$n,type="l",col="blue")
lines(rollmean(s$n,7,fill = NA),col="red")
s %>% ggplot(aes(FECHA_RESULTADO,n))+geom_col(color="blue",
                                              alpha=0.5)+
  geom_line(mapping = aes(y=rollmean(s$n,7,fill = NA)),
            color="red",size=1)+theme_minimal()+xlab("")


data %>% select(DEPARTAMENTO,FECHA_RESULTADO) %>% 
  filter(DEPARTAMENTO=="PIURA")->piura

piura %>%select(FECHA_RESULTADO) %>% group_by(FECHA_RESULTADO) %>% 
  count() %>% ggplot(aes(FECHA_RESULTADO,n))+
    geom_col(color="blue",alpha=0.5)+
        geom_line(mapping = aes(y=rollmean(n,7,fill = NA)),
                  color="red",size=1)

data %>% select(DEPARTAMENTO) %>% group_by(DEPARTAMENTO) %>% 
  count()  %>% 
  ggplot(aes(reorder(DEPARTAMENTO,n),n,fill=n))+
  geom_col(show.legend = F,position = "dodge")+
  scale_fill_gradient(low = "blue", high = "red")+
  theme_minimal()+  
  geom_text(aes(label = n,y=n+30000)
            , position = position_dodge(width = 1),
            angle=360,size=4,color="black")+
  theme(axis.text.x = element_text(angle=90))+
  labs(x="",y="",
       title = "Evolcuión del covid-19 por departamento",
       subtitle = paste("total de contagiados:",dim(data)[1],
                        " personas"))+
  scale_y_continuous(breaks =c() )-> grafico

x11()
grafico

# Vamos por fallecidos 

fallecidos <- read.csv("fallecidos_covid.csv",sep=";")

fallecidos$FECHA_FALLECIMIENTO<- 
  fallecidos$FECHA_FALLECIMIENTO %>% ymd()



fallecidos %>% select(FECHA_FALLECIMIENTO) %>% 
    group_by(FECHA_FALLECIMIENTO) %>% count() %>% 
    ggplot(aes(FECHA_FALLECIMIENTO,n))+geom_col(color="purple",
                                          alpha=0.5)+
    geom_line(mapping = aes(y=rollmean(n,7,fill = NA)),
              size=1,color="blue")+theme_minimal()

fallecidos %>% select(DEPARTAMENTO) %>% group_by(DEPARTAMENTO) %>% 
  count()  %>% 
  ggplot(aes(reorder(DEPARTAMENTO,n),n,fill=n))+
  geom_col(show.legend = F,position = "dodge")+
  scale_fill_gradient(low = "blue", high = "red")+
  theme_minimal()+  
  geom_text(aes(label = n,y=n+1000)
            , position = position_dodge(width = 1),
            angle=360,size=4,color="black")+
  theme(axis.text.x = element_text(angle=90))+
  labs(x="",y="",
       title = "Evolcuión del covid-19 por departamento",
       subtitle = paste("total de fallecidos:",dim(fallecidos)[1],
                        " personas"))+
  scale_y_continuous(breaks =c() )

f<- fallecidos %>% select(FECHA_FALLECIMIENTO) %>% 
  group_by(FECHA_FALLECIMIENTO) %>% count()

muertes<-fallecidos %>% select(DEPARTAMENTO) %>% 
  group_by(DEPARTAMENTO) %>% 
  count()

contagios<- data %>% select(DEPARTAMENTO) %>% 
  group_by(DEPARTAMENTO) %>% 
  count()

dt<- data.frame("departamentos"=contagios$DEPARTAMENTO,
                "contagiados"=contagios$n,
                "muertes"=muertes$n)


s %>% ggplot(aes(FECHA_RESULTADO,scale(cumsum(n))))+
  geom_line(aes(colour="contagiados"))+geom_line(data = f,mapping = 
                          aes(x=FECHA_FALLECIMIENTO,
                              y=scale(cumsum(n)),colour="muertos"))+
  theme_minimal()+
  scale_colour_manual("", breaks = c("contagiados", "muertos"), 
                      values = c("red", "green"))+
  theme(legend.position ="bottom",
      plot.caption = element_text(hjust = 0))+
  labs(x="",y="",title="Curvas de números de contagios y muertes",
       subtitle = "Datos escalados") ->a

library(ggThemeAssist)
ggThemeAssistGadget(a)
a + theme(plot.subtitle = element_text(family = "NewCenturySchoolbook", 
    size = 10, face = "bold", colour = "gray25", 
    hjust = 0.1), axis.line = element_line(colour = NA), 
    axis.ticks = element_line(colour = NA, 
        linetype = "dashed"), axis.title = element_text(family = "AvantGarde", 
        face = "bold"), axis.text = element_text(family = "mono", 
        face = "bold"), axis.text.x = element_text(family = "serif", 
        vjust = 0.25), plot.title = element_text(family = "AvantGarde", 
        face = "bold", hjust = 0.5), legend.text = element_text(family = "mono"), 
    legend.title = element_text(face = "italic", 
        family = "mono", colour = "beige"), 
    legend.key = element_rect(fill = "gray91", 
        linetype = "dashed"), legend.background = element_rect(fill = "white", 
        linetype = "solid"), legend.direction = "horizontal") +labs(x = NULL, y = NULL)
