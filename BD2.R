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
bd2<-read_csv("C:/Users/Admin/Desktop/HDI/BDN.csv")

edo<-data.frame(matrix(c(bd2$Precio,bd2$Estado, bd2$Ntipodetaller, bd2$Nrefacción), ncol=4))
 colnames(edo)<-c('Precio', 'Estado','Tipo de Taller', 'Refcacción')
 colnames(edo)<-c('Precio', 'Estado','Tipo de Taller', 'Refcacción')
cor(edo)
 







