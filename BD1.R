rm(list = ls())
library(readr)
library(ggplot2)
library(plotly)
library(lubridate)
library(dplyr)
library(RColorBrewer)


############################################################################
####                                                                    ####
####                       Análisis Temporal                            ####  
####                                                                    ####
############################################################################


# Cargamos la base de datos
bd1<-read_csv("C:/Users/Admin/Desktop/HDI/BD1.csv")
colnames(bd1)<-c('Fecha','IDU','Precio','Marca','Modelo','Refacción','NRefacción',
                'Tipodetaller','NTipodetaller','Estado','CP' ,'Proveedor')
tail(bd1)
dim(bd1)




# st <- select(bd1,Fecha,Precio)
# Seleccionamos la Fecha y Precios para el análisis temporal
st <- bd1 %>% select(Fecha, Precio)
library(forecast)
AirPassengers
airforecast <- forecast(auto.arima(st), level = 95)

hchart(st)

hchart(st)
basicStats(st)

# Simulación del precio de una acción ####
p<-vector()
p[1]<-1600
p[2]<- p[1]+rlnorm(1)
p[2]
t<-365
for (i in 2:t) {
  p[i]<-p[i-1]+rlnorm(1)
}
p
ts.plot(p)
hist(p)



#Representación gráfica interactiva del precio promedio
promedio <- st %>%
  group_by(Fecha) %>%
  summarize(Precio=mean(Precio))
g2<-ggplot(promedio, aes(Fecha, Precio))+geom_line()+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  geom_smooth(method = lm,se = TRUE)+
  geom_line(h=mean(Precio))
g2<-ggplotly(g2);g2




# Filtro para saber por taller o agencia
Taller<-bd1 %>%
  filter(Tipodetaller == 'TALLER') 
refac<-Taller %>%
  filter(Refacción == 'CALAVERA') 
mod<-refac%>%
  filter(Modelo == '2016') 
mar<-mod%>%
  filter(Marca == 'DODGE')





%>%
  count(Precio, sort = TRUE)
Ag<-bd1 %>%
  filter(Tipodetaller == 'AGENCIA') %>%
  count(Precio, sort = TRUE)
Agm<-bd1 %>%
  filter(Tipodetaller == 'AGENCIA MULTIMARCA') %>%
  count(Precio, sort = TRUE)
count(Agm)
count(Ag)
count(Taller)


edo<-bd1 %>%
  filter(Estado == 'Aguascalientes') %>%
  count(Tipodetaller, sort = TRUE)


# Filtro de estado y precio refacción
pre<-bd1 %>%
  filter(Estado == 'Aguascalientes' & Refacción=='CALAVERA') %>%
  count(Precio, sort = TRUE)
ggplot(pre, aes(x=Refacción, y=n))+geom_point()  

# Filtro de conteo de refacción por estado 
pre<-bd1 %>%
  filter(Refacción=='CALAVERA') %>%
  count(Estado, sort = TRUE)
ggplot(pre, aes(x=Refacción, y=n))+geom_point()  


fst<-filter(bd1,Fecha>='2019/01/01' &  Fecha<='2019/01/31',
            Refacción=='FARO')

ch<-ggplot(fst, aes(Fecha, fill=Precio)) +
  geom_bar(aes(fill = Tipodetaller))+
  coord_flip()+
  labs(title ="Cambio de Calaveras 2019",
       subtitle = "",
       x = "Fecha", y = "Número de Calaveras ")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
ch<-ggplotly(ch);ch



gg<-ggplot() + geom_col(data = fst, aes(x = Fecha, 
          y = Precio, fill = Tipodetaller), position = "dodge")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
gg<-ggplotly(gg);gg


#Facet 
marcas <- count(bd1, Precio, Marca)
glimpse(marcas)

ggplot(data = marcas, aes(Precio, n)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "New Marvel characters by alignment",
       subtitle = "(limited to characters with more than 100 appearances)",
       y = "Count of new characters", x = "") + 
facet_wrap(~ Marca)


mt <- ggplot(bd1, aes(Precio, Marca, colour = factor(Tipodetaller))) +
  geom_point()

mt + facet_grid(vars(Tipodetaller), scales = "free")



g<-ggplot(bd1, aes(Tipodetaller))+geom_bar();g


g<-ggplot(bd1, aes(Tipodetaller))+ geom_bar(aes(weight = Precio));g

g<-ggplot(bd1) + geom_bar(aes(y = Marca));g


st1 <- bd1 %>% select(Marca, Precio)
mod <- st1 %>%
  group_by(Marca) %>%
  summarize(Precio=sum(Precio))
plot(mob, type='l')


promedio <- st %>%
  group_by(Fecha) %>%
  summarize(Precio=mean(Precio))
plot(promedio, type='l')




######################################################################
#####                                                            #####
#####                         TOP n                              #####
#####                                                            #####
######################################################################

# Top 10 en precios
a1<-top_n(bd1, 10, Precio);a1
# Top 10 en Marcas
a2<-top_n(bd1, 10, Marca);a2
# Top 10 en Refacciones
a3<-top_n(bd1, 10, Refacción);a3
# Top 10 en
a4<-top_n(bd1, 10, Estado);a4

summarise(bd1,pm=mean(Precio), n())

filter(bd1, Modelo=='SEAT')


fecha<-filter(bd1,bd1[,1]>='2019/01/01' &  bd1[,1]<='2019/01/06', bd[,]=='DAMA MEXICO')

fecha<-filter(bd1,bd1[,1]>='2019/01/01' &  bd1[,1]<='2019/12/30',bd[,8]=='Aguascalientes')


Fecha<-fecha[,1]
Estilo<-fecha[,3]
ch<-ggplot(fecha, aes(Fecha)) +
  geom_histogram(bins=300, color='darkblue', fill='lightblue')+
  labs(title='Gastos por Estado',
       subtitle=paste(sum(fecha[,3])),
       x ='Fecha', y ='Precios')+
  theme(axis.text.x=element_text(angle=90, hjust=1))
ggplotly(ch)

count( Estilo)

st<-data.frame(c(Fecha, Estilo),ncol=2)

conteo<-ts(Estilo,start = 1,frequency = 12)
plot(conteo)
######################################################################
#####                                                            #####
#####                                                            #####
#####                                                            #####
######################################################################


######################################################################
#####                                                            #####
#####                                                            #####
#####                                                            #####
######################################################################


######################################################################
#####                                                            #####
#####                                                            #####
#####                                                            #####
######################################################################


######################################################################
#####                                                            #####
#####                                                            #####
#####                                                            #####
######################################################################


######################################################################
#####                                                            #####
#####                                                            #####
#####                                                            #####
######################################################################

######################################################################
#####                                                            #####
#####                                                            #####
#####                                                            #####
######################################################################


