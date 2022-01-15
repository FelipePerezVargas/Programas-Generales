rm(list = ls())
library(readr)
library(ggplot2)
library(plotly)
library(lubridate)
library(dplyr)
library(RColorBrewer)

# Proyección del costo medio ( entiéndase éste como la suma del monto 
# entre  la cantidad de piezas, cada registro representa 1 pieza).
# Validar si existe alguna correlación entre la marca - precio - estado, etc.
# Identificar de qué forma se podría optimizar el costo medio.


bd<-read_csv("C:/Users/Admin/Desktop/HDI/BD2.csv")
colnames(bd)<-c('Fecha','IDU','Precio','Marca','Modelo','Refacción',
                'Tipodetaller','Estado','Proveedor')

bd %>% group_by(Marca %>% 
  summarise(avgpr=mean(PrecioRefaccion, na.rm = TRUE)) %>%
  ggplot(aes(x=Marca, y=avgpr)) +geom_col(fill='darkblue')+
  theme(text = element_text(size = 7)) +
  theme(axis.text.x = element_text(angle = 90, hjust=0.5))+
  labs(x = 'Marca (MXN)', y='Gasto promedio (MXN)')


p<-ggplot(bd, aes(Fecha, Precio, colour = Tipodetaller))+
   geom_point()+ labs(colour = "Precio")+
   labs(x = 'Fecha', y='Costo (MXN)')
ggplotly(p)


g <- ggplot(bd, aes(Marca))+
     geom_bar()+
  theme(text = element_text(size = 7)) +
  theme(axis.text.x = element_text(angle = 90, hjust=0.5))
ggplotly(g)


g <- ggplot(bd, aes(Precio))+
  geom_histogram()+
  theme(text = element_text(size = 7)) +
  theme(axis.text.x = element_text(angle = 90, hjust=0.5))
ggplotly(g)



str(bd %>% group_by(PrecioRefaccion
))
as.stri
plot(fecha,bd[,3])


cor(bd$PrecioRefaccion,bd$Estado)

hist(bd$PrecioRefaccion, main='Histrograma FB', ylab='Frecuencia',  
     freq = FALSE, breaks = "FD", col='darkorchid4')
lines(density(bd$PrecioRefaccion), col='red')
abline(v=mean(bd$PrecioRefaccion), col='#5F9EA0', lwd=2)

hist(bd$Estado, main='Histrograma FB', ylab='Frecuencia',  
     freq = TRUE, breaks = "FD", col='darkorchid4')
lines(density(bd$Estado), col='red')

df<-data.frame(fecha, bd$PrecioRefaccion)

e2 <- bd %>%
  ggplot( aes(x=df[,1] , y=df[,2])) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  labs(ylab='Número de Defectos',
       xlab='Mes',
       title =paste( sum(df[,2]),"Reprocesos Histórico "))
#  theme_ipsum()

e2<- ggplotly(e2);e2


df %>% count(df$bd.MarcaUnidad, sort = TRUE)
df %>%  group_by(df$bd.Estado)%>% tally()

summary(bd$PrecioRefaccion)
min(bd$PrecioRefaccion)
max(bd$PrecioRefaccion)
sd(bd$PrecioRefaccion)
var(bd$PrecioRefaccion)
hist(bd$PrecioRefaccion)
plot(fecha,bd$PrecioRefaccion )


bd1<-read_csv("C:/Users/Admin/Desktop/HDI/BD1.csv")
fecha<-ymd(bd1$Fecha)
bd1







