rm(list = ls())
library(readr)
library(ggplot2)
library(plotly)
library(lubridate)
library(dplyr)
library(ggfortify)
library(fpp2)
library(RColorBrewer)
library(tseries)
library(forecast)

bd<-read_csv("C:/Users/Admin/Desktop/HDI/BDM.csv")
colnames(bd)<-c('Fecha','IDU','Precio','Marca','Modelo','Refacción','NRefacción',
                'Tipodetaller','NTipodetaller','Estado','Nestado','Proveedor')


st <- bd %>% select(Fecha, Precio)
prom <- st %>%
  group_by(Fecha) %>%
  summarize(Precio=mean(Precio))
# Representación gráfica interactiva de los costos por día
g1<-ggplot(prom,aes(Fecha,Precio))+
  geom_line(color='brown1')+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(title = 'Serie de tiempo de precios promedios',
       x='Tiempo', y='Valor')+
  theme_minimal()+
  geom_smooth(method = "loess",se = TRUE)
g1<-ggplotly(g1);g1


edo <- bd %>%
  filter(Nestado=="Aguascalientes") %>%
 group_by(Fecha)
summarise(Precio = mean(Precio))





edo<-bd %>%
  filter(Nestado == 'Aguascalientes' )
st <- edo %>% select(Fecha, Precio)
prom <- st %>%
  group_by(Fecha)%>%
  summarize(Precio=mean(Precio))
head(prom)
g1<-ggplot(prom,aes(Fecha, Precio))+geom_line()+
  theme(axis.text.x=element_text(angle=90, hjust=1))
g1<-ggplotly(g1);g1



edo<-bd %>%
  filter(Nestado == 'Aguascalientes' & Refacción=='CALAVERA')






edo<-bd %>%
  filter(Nestado == 'Aguascalientes' & Refacción=='FACIA DELANTERA')

edo<-bd %>%
  filter(Nestado == 'Aguascalientes' & Refacción=='FACIA TRASERA')

edo<-bd %>%
  filter(Nestado == 'Aguascalientes' & Refacción=='FARO')

edo<-bd %>%
  filter(Nestado == 'Aguascalientes' & Refacción=='GUARDAFANGO')

edo<-bd %>%
  filter(Nestado == 'Aguascalientes' & Refacción=='MOLDURA')

edo<-bd %>%
  filter(Nestado == 'Aguascalientes' & Refacción=='PARRILLA')

edo<-bd %>%
  filter(Nestado == 'Aguascalientes' & Refacción=='RIN')

edo<-bd %>%
  filter(Nestado == 'Aguascalientes' & Refacción=='SALPICADERA')

###################
#Análisis Temporal#
###################
# st <- select(bd1,Fecha,Precio)
st <- bd %>% select(Fecha, Precio)

# Separamos la Fecha y Precio
suma <- st %>%
  group_by(Fecha) %>%
  summarize(Precio=sum(Precio, na.rm = TRUE))
#Representación gráfica interactiva de los costos por día
g1<-ggplot(suma,aes(Fecha, Precio))+geom_line()+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  geom_smooth(method = "loess",se = TRUE)
g1<-ggplotly(g1);g1


st <- bd %>% select(Fecha, Precio)
# Separamos la Fecha y precios promedio
promedio <- st %>%
  group_by(Fecha) %>%
  summarize(Precio=mean(Precio))
# Representación gráfica interactiva de los costos por día
# g1<-ggplot(promedio, aes(Fecha, Precio))+geom_line()+
#   theme(axis.text.x=element_text(angle=90, hjust=1))+
#   geom_smooth(method = "loess",se = TRUE)
# g1<-ggplotly(g1);g1
stpre<-ts(promedio$Precio, start = 1, frequency = 12)
autoplot(stpre, ts.colour = "blue")+
         labs(title = 'Serie de tiempo de precios promedios',
              x='Tiempo', y='Costo')+
  theme(axis.text.x=element_text(angle=90, hjust=1))+       
  theme_minimal()
autoplot(acf(stpre, plot = FALSE))
autoplot(stl(stpre, s.window = "periodic"))
autoplot(diff(stpre))
autoplot(acf(diff(stpre), plot = FALSE))
monthplot(diff(stpre), col = "midnightblue")
diff<-diff(stpre)
diff.ts<-diff(stpre, lag = 12)
autoplot(diff.ts)
adf<-adf.test(diff.ts)
 adf$p.value
# Comprbar si la serie es estacionaria
kpss<-kpss.test(diff.ts)
kpss$p.value  # La serie no es estacionaria 
autoplot(acf(diff.ts, plot = FALSE))
autoplot(pacf(diff.ts, plot = FALSE))


arima1<- Arima(stpre, order=c(0,1,2), seasonal=list(order=c(0,1,1),period=12))
arima2<- Arima(stpre, order=c(1,1,0), seasonal=list(order=c(2,1,0),period=12))
arima3<- Arima(stpre, order=c(1,1,2), seasonal=list(order=c(2,1,1),period=12))
# arima4<- Arima(stpre, order=c(1,1,1), seasonal=list(order=c(2,1,1),period=12))
arima5<- Arima(stpre, order=c(1,1,2), seasonal=list(order=c(1,1,1),period=12))
arima6<- Arima(stpre, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12))
arima7<- Arima(stpre, order=c(1,1,0), seasonal=list(order=c(1,1,0),period=12))


AIC(arima1,arima2,arima3,arima5,arima6,arima7)
BIC(arima1,arima2,arima3,arima5,arima6,arima7)
autoplot(acf(arima6$residuals, plot = FALSE))
autoplot(pacf(arima6$residuals, plot = FALSE))
ggtsdiag(arima6)
bp <- Box.test(arima6$residuals) # Test de Box-Pierce
bp$p.value
lb <- Box.test(arima6$residuals, type="Ljung-Box") # Test de Ljung-Box
lb$p.value
jb <- jarque.bera.test(arima6$residuals) # Test de Jarque-Bera
jb$p.value
sht<-shapiro.test(arima6$residuals) $ # Test de Shapiro-Wilk
  sht$p.value
auto.arima(stpre, stepwise = FALSE, approximation = FALSE)
forecast1<-forecast(arima6, level = c(95), h = 50)
g<-autoplot(forecast1)
g<-ggplotly(g);g



forecast1<-forecast(arima1, level = c(95), h = 50)
g<-autoplot(forecast1)
g<-ggplotly(g);g

forecast1<-forecast(arima2, level = c(95), h = 50)
g<-autoplot(forecast1)
g<-ggplotly(g);g

forecast1<-forecast(arima3, level = c(95), h = 50)
g<-autoplot(forecast1)
g<-ggplotly(g);g


forecast1<-forecast(arima5, level = c(95), h = 50)
g<-autoplot(forecast1)
g<-ggplotly(g);g











fit<-decompose(stpre, type = 'additive')
autoplot(fit)

fit<-decompose(stpre, type = 'multiplicative')
autoplot(fit)

autoplot(stpre, series='Serie de Tiempo')+
  autolayer(trendcycle(fit), series='Tendencia')

# naive(stpre, h=20)
# autoplot(naive(stpre, h=20))
# ses(stpre, h=20)
# autoplot(ses(stpre, h=20))
# meanf(stpre, h=20)
# autoplot(meanf(stpre, h=20))
sn<-snaive(stpre, h=20)
autoplot(sn)


autoplot(snaive(stpre, h=20))+autolayer(fitted(snaive(stpre, h=20)), series='Ajuste')
checkresiduals(stpre)



tslm(stpre~trend+season)
forecast(tslm(stpre~trend+season), h=20)
autoplot(forecast(tslm(stpre~trend+season), h=20))
autoplot(forecast(tslm(stpre~season), h=20))
autoplot(forecast(tslm(stpre~trend), h=20))
+autolayer(fitted(forecast(tslm(stpre~trend+season), h=20)), series = 'Ajuste')




edo<-bd %>%
  filter(Nestado == 'Aguascalientes' )
st <- bd %>% select(Fecha, Precio)
prom <- st %>%
  group_by(Fecha) %>%
  summarize(Precio=mean(Precio))
print(paste('El costo promedio de las autopartes en el estado de Aguascalientes es:',round(mean(edo$Precio),3)))
g1<-ggplot(prom,aes(Fecha,Precio))+
  geom_line(color='brown1')+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title = 'Serie de tiempo de precios promedios',
       x='Tiempo', y='Valor')+
  theme_minimal()+
  geom_smooth(method = "loess",se = TRUE)
g1<-ggplotly(g1);g1


stpre<-ts(prom$Precio, start = 1, frequency = 12)
g1<-autoplot(stpre, ts.colour ='brown1')+
  labs(title = 'Serie de tiempo de precios promedios',
       x='Tiempo', y='Valor')+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  theme_minimal()
g1<-ggplotly(g1);g1

print(paste('El costo promedio es:',round(mean(st$Precio),2)))
















#############################################################
# Correlación de precio, refacción, tipo de taller y estado #
#############################################################

st<-bd %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)

##########################
# Correlación por Estado #
##########################

#Aguascalientes
fe<-filter(bd, Estado==1)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Baja California
fe<-filter(bd, Estado==2)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Baja California Sur
fe<-filter(bd, Estado==3)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Campeche
fe<-filter(bd, Estado==4)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Coahuila de Zaragoza
fe<-filter(bd, Estado==5)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Colima
fe<-filter(bd, Estado==6)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Chiapas
fe<-filter(bd, Estado==7)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
# Chihuahua 
fe<-filter(bd, Estado==8)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Distrito Federal
fe<-filter(bd, Estado==9)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Durango
fe<-filter(bd, Estado==10)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Guanajuato
fe<-filter(bd, Estado==11)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Guerrero
fe<-filter(bd, Estado==12)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Hidalgo 
fe<-filter(bd, Estado==13)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Jalisco
fe<-filter(bd, Estado==14)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Estado de México
fe<-filter(bd, Estado==15)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Michoacán de Ocampo 
fe<-filter(bd, Estado==16)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Morelos
fe<-filter(bd, Estado==17)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Nayarit
fe<-filter(bd, Estado==18)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Nuevo León
fe<-filter(bd, Estado==19)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Oaxaca 
fe<-filter(bd, Estado==20)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Puebla
fe<-filter(bd, Estado==21)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Querétaro 
fe<-filter(bd, Estado==22)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
# Quintana Roo 
fe<-filter(bd, Estado==23)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#San Luis Potosí 
fe<-filter(bd, Estado==24)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Sinaloa 
fe<-filter(bd, Estado==25)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Sonora
fe<-filter(bd, Estado==26)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Tabasco 
fe<-filter(bd, Estado==27)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Tamaulipas
fe<-filter(bd, Estado==28)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Tlaxcala 
fe<-filter(bd, Estado==29)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Veracruz de Ignacio de la Llave
fe<-filter(bd, Estado==30)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Yucatán 
fe<-filter(bd, Estado==31)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
#Zacatecas  
fe<-filter(bd, Estado==32)
st<-fe %>% select(Precio,NRefacción, NTipodetaller)
round(cor(st),4)
  
##########################
# Correlación por taller #
##########################

# Taller
fe<-filter(bd, NTipodetaller==1)
st<-fe %>% select(Precio,NRefacción, Estado)
round(cor(st),4)
# Agencia
fe<-filter(bd, NTipodetaller==2)
st<-fe %>% select(Precio,NRefacción, Estado)
round(cor(st),4)
# Agencia Multimarca 
fe<-filter(bd, NTipodetaller==3)
st<-fe %>% select(Precio,NRefacción, Estado)
round(cor(st),4)

#############################################
# Correlación en función de las refacciones #
#############################################

# Calavera 
fe<-filter(bd, NRefacción==1)
st<-fe %>% select(Precio,NTipodetaller, Estado)
round(cor(st),4)
# Facia Delantera 
fe<-filter(bd, NRefacción==2)
st<-fe %>% select(Precio,NTipodetaller, Estado)
round(cor(st),4)
# Facia Trasera
fe<-filter(bd, NRefacción==3)
st<-fe %>% select(Precio,NTipodetaller, Estado)
round(cor(st),4)
# Faro
fe<-filter(bd, NRefacción==4)
st<-fe %>% select(Precio,NTipodetaller, Estado)
round(cor(st),4)
# Guardafango 
fe<-filter(bd, NRefacción==5)
st<-fe %>% select(Precio,NTipodetaller, Estado)
round(cor(st),4)
# Moldura 
fe<-filter(bd, NRefacción==6)
st<-fe %>% select(Precio,NTipodetaller, Estado)
round(cor(st),4)
# Parrilla 
fe<-filter(bd, NRefacción==7)
st<-fe %>% select(Precio,NTipodetaller, Estado)
round(cor(st),4)
# Rin 
fe<-filter(bd, NRefacción==8)
st<-fe %>% select(Precio,NTipodetaller, Estado)
round(cor(st),4)
# Salpicadera
fe<-filter(bd, NRefacción==9)
st<-fe %>% select(Precio,NTipodetaller, Estado)
round(cor(st),4)













#Representación gráfica interactiva del precio promedio
promedio <- st %>%
  group_by(Fecha) %>%
  summarize(Precio=mean(Precio))
g2<-ggplot(promedio, aes(Fecha, Precio))+geom_line()+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  geom_smooth(method = lm,se = TRUE)+
  geom_line(h=mean(Precio))
g2<-ggplotly(g2);g2

fst<-filter(bd,Fecha>='2019/01/01' &  Fecha<='2019/01/31',Refacción=='FARO')


summarise(bd,pm=mean(Precio), n())





