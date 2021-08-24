
library(readr)
library(tidyverse)
library(highcharter)
library(xts)
library(lubridate)
library(RColorBrewer)
library(plotly)

# Cargar base de datos

dat<- read_csv("Casos_positivos_de_COVID-19_en_Colombia.csv")


data <-dat %>% filter(`Nombre departamento`=="CESAR")


# Manipulacion de datos

data %<>% rename(fecha=`fecha reporte web`,Estado=Recuperado,
                Tipo_contagio=`Tipo de contagio`,
                Tipo_Recuperacion=`Tipo de recuperación`,
                Estado2=Estado,
                Pertenencia_etnica=`Pertenencia étnica`) %>% 
  mutate(fecha=as.Date.factor(fecha,format="%d/%m/%Y")) 


data %>%  
  mutate(Estado=factor(Estado,labels = c("Activo",
                                         "Fallecido","Fallecido","N/A","Recuperado"))) %>% 
  mutate(across(c(Sexo,Tipo_contagio,Tipo_Recuperacion,Estado2), as.factor))

#Guardar datos
write.csv(x = data,file = "Data_Cesar.csv",row.names = T)


## Recuperados 
data_recuperado <- data %>% 
  filter(Estado=="Recuperado") %>% 
  group_by(fecha) %>% 
  summarise(cantidad=n())


Recup<- as.xts(data_recuperado$cantidad, # `xts modifica las fechas
               order.by =data_recuperado$fecha)

#00FA9A
ptg <- hchart(Recup)
ptg %>% hc_colors("#778899") %>% 
  hc_chart(plotBackgroundColor = NULL, 
           plotBorderWidth =NULL,
           borderWidth = 1
           
  ) %>% 
  hc_yAxis(opposite = F)



## Contagiados

data_contagiado <- data %>% 
  group_by(fecha) %>% summarise(cantidad=n())

Contg<- as.xts(data_contagiado$cantidad,order.by =data_contagiado$fecha)

conw <- hchart(Contg)
conw %>% hc_colors("#778899") %>% 
  hc_chart(plotBackgroundColor = NULL, 
           plotBorderWidth =0,
           plotShadow = FALSE,  
           borderWidth = 1
  ) %>% 
  hc_yAxis(opposite = F)

## Fallecidos

data_fallecido <- data %>% 
  filter(Estado=="Fallecido") %>% 
  group_by(fecha) %>% 
  summarise(cantidad=n())
Fallc<- as.xts(data_fallecido$cantidad,
               order.by =data_fallecido$fecha)


falw <- hchart(Fallc)
falw %>% hc_colors("#778899") %>% 
  hc_chart(plotBackgroundColor = NULL, 
           plotBorderWidth =NULL,
           borderWidth = 1
           
  ) %>% 
  hc_yAxis(opposite = F)



## Casos por sexo

P_sexo <- data  %>%
  group_by(Sexo) %>% summarise(cantidad=n())

levels(P_sexo$Sexo) <- c("M","H" )

P_sexo %>% 
  mutate(Porcentaje=cantidad*100/sum(cantidad)) %>% 
  mutate(Comentario=as.factor(paste(Sexo,sep = " ",
                                    round(Porcentaje,2),"%"))) %>%
  hchart(
    "pie", hcaes(x =Comentario , y = cantidad),
    name = "Casos"
  ) %>%hc_colors(c("#C71585","#191970"))


# Recuperado por edad

recup_ed <- data %>%filter(Estado=="Recuperado") 
hchart(recup_ed$Edad,name="Rango por edad",breaks = 20,color="#C71585")
  
# Fallecido por edad
falle_ed <- data %>% filter(Estado=="Fallecido")
hchart(falle_ed$Edad,name="Rango por edad",breaks = 20,color="#C71585") 

# Contagiado por edad

hchart(data$Edad,name="Rango por edad",breaks = 20,color="#C71585")

# Tipo de contagio

data %>% 
  group_by(Tipo_contagio) %>% summarise(Cantidad=n())  %>% 
  hchart("column",hcaes(y=Cantidad,group=Tipo_contagio)) %>% hc_colors(c("#C71585","#191970","#32CD32","#CD853F"))

# Tipo de Recuperacion

P_Recup <- data %>% 
  group_by(Tipo_Recuperacion) %>% summarise(Cantidad=n()) %>% 
  mutate(Tipo_Recuperacion=factor(c("PCR","Tiempo","N/A"))) %>% 
  mutate(Porcentaje=Cantidad*100/sum(Cantidad)) %>% 
  mutate(Comentario=paste(Tipo_Recuperacion,sep=" ",round(Porcentaje,2),"%"))

P_Recup %>% 
  hchart(
    "pie", hcaes(x =Comentario , y = Cantidad),
    name = "Casos"
  ) %>%hc_colors(c("#C71585","#191970","#32CD32"))

# Condiciones 

data %>% 
  group_by(Estado2) %>% summarise(Cantidad=n(),Porcentaje=round(n()/nrow(data)*100,2)) %>% 
  hchart("column",hcaes(y=Cantidad,group=Estado2)) %>% 
  hc_colors(c("#C71585","#191970","#32CD32","#CD853F","#9400D3","#000000"))


# Etnia

data %>% 
  group_by(Pertenencia_etnica) %>% summarise(Cantidad=n(),Porcentaje=round(n()/nrow(data)*100,3)) %>%  
  hchart("column",hcaes(y=Cantidad,group=Pertenencia_etnica)) %>% 
    hc_colors(c("#C71585","#191970","#32CD32","#CD853F","#9400D3"))


#  Contagiados por edad y sexo
  
Ed_se <- data %>% select(8,10)
L = seq(0,105,5)

Intervalo <- cut(Ed_se$Edad, breaks = L, right = FALSE, include.lowest = TRUE)
Ed_se <- data.frame(Ed_se,Intervalo) 

Mascu<- Ed_se %>% group_by(Intervalo,Sexo) %>% summarise(Cantidad=n())%>% 
  filter(Sexo=="M") %>% mutate(Cantidad=-Cantidad)

Femen <- Ed_se %>% group_by(Intervalo,Sexo) %>% summarise(Cantidad=n()) %>%
  filter(Sexo=="F")

Gener <- rbind.data.frame(Mascu,Femen)

g1 <- ggplot(data = Gener, aes(x = Intervalo,y=Cantidad, fill = Sexo)) +
  geom_bar(data = subset(Gener, Sexo == "F"), stat = "identity") +
  geom_bar(data = subset(Gener, Sexo == "M"), stat = "identity")+
  coord_flip()+scale_y_continuous(limits = c(-5500, 5500),
                                   breaks = seq(-5500,5500,1000),
                                   labels = abs(seq(-5500, 5500, 1000)))+
  labs(x="Edad",y="")+
  theme_minimal()+scale_fill_manual(values=c("#C71585","#191970"))

ggplotly(g1)


# Fallecido por edad y sexo

Ed_se <- data %>% filter(Estado=="Fallecido") %>% select(8,10) 
L = seq(0,105,5)
Intervalo <- cut(Ed_se$Edad, breaks = L, right = FALSE, include.lowest = TRUE)
Ed_se <- data.frame(Ed_se,Intervalo) 

Mascu<- Ed_se %>% group_by(Intervalo,Sexo) %>% summarise(Cantidad=n())%>% 
  filter(Sexo=="M") %>% mutate(Cantidad=-Cantidad)


agg <-data.frame(Intervalo="[5,10)",Sexo="F",Cantidad=0)

Femen <- Ed_se %>% group_by(Intervalo,Sexo) %>% summarise(Cantidad=n()) %>%
  filter(Sexo=="F")

Femen <- rbind(Femen,agg)

Gener <- rbind.data.frame(Mascu,Femen)

g2 <- ggplot(data = Gener, aes(x = Intervalo,y=Cantidad, fill = Sexo)) +
  geom_bar(data = subset(Gener, Sexo == "F"), stat = "identity") +
  geom_bar(data = subset(Gener, Sexo == "M"), stat = "identity")+
  coord_flip()+scale_y_continuous(limits = c(-200, 200),
                                  breaks = seq(-200,200,50),
                                  labels = abs(seq(-200, 200, 50)))+
  labs(x="Edad",y="")+
  theme_minimal()+scale_fill_manual(values=c("#C71585","#191970"))

ggplotly(g2)


