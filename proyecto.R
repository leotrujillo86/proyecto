#PROYECTO DE ESTANCIAS INFANTILES EN LA ENTIDAD DE TAMAULIPAS

#librerias que pueden utilizarse (llamar desde el inicio)
install.packages("ggplot2")
install.packages("dplyr")
install.packages("DescTools")
install.packages("plotly")
install.packages("forcats")
install.packages("scales")
install.packages("treemap") #para graficar estatus
install.packages("reshape2")
install.packages("tidyverse")
install.packages("DBI")
install.packages("RMySQL")
library(ggplot2)
library(dplyr)
library(DescTools)
library(plotly)
library(forcats)
library(scales)
library(treemap)       
library(reshape2)
library(tidyverse)
library(DBI)
library(RMySQL)


"Estancias Infantiles Tamaulipas"

#directorio de trabajo y lectura de csv

setwd("c:/Users/leonel.trujillo/Desktop/escuela/R/proyecto/")
getwd()
dir()
read.csv("datos_tamaulipas.csv")  #leer el archivo

dtam <- read.csv("datos_tamaulipas.csv") #asigno a un objeto
dtam

#estructura y analisis

length (dtam) #cantidad de columnas
#tail(dtam) #ultimos 6 registros (6 es por default)
#head(dtam, 10) #primeros 10 registros
dim(dtam) #dimension de mi objeto - base(filas y columnas)
str(dtam) #estructura de mi objeto - base
class(dtam) #Clase de objeto - base
typeof(dtam) #tipo o modeo de almacenamiento de mi objeto - base
names(dtam) #nombre de columnas

#en el str me indica que mi columna de fecha_operacion es caracter y lo tengo que cambiar a fecha:

dtam <- mutate (dtam, fecha_operacion = as.Date(fecha_operacion, "%d/%m/%Y"))
str(dtam)

#datos estadisticos básicos

summary(dtam)
summary(dtam$menores_inscritos)

paste("La media de menores inscritos por estancia es:", 
      mean(dtam$menores_inscritos))
paste("La mediana de menores inscritos por estancia es:", 
      median(dtam$menores_inscritos))
paste("El minimo de menores inscritos es:", 
      min(dtam$menores_inscritos),
      "y El máximo de menores inscritos es:", max(dtam$menores_inscritos))

summary(dtam$capacidad_instalada)

paste("La media de capcidad por estancia es:", 
      mean(dtam$capacidad_instalada))
paste("La mediana de capacidad por estancia es:", 
      median(dtam$capacidad_instalada))

paste("la capacidad minima de un inmueble es:", 
      min(dtam$capacidad_instalada),
      "y la capacidad máxima de un inmueble es:", max(dtam$capacidad_instalada))


#consultas solicitadas por la Secretaría

"Estancias aperturadas despues de  algun ejercicio fiscal (por ejemplo 2012)"
dtam_ef <- dtam[dtam$ejercicio_fiscal > 2012,]
dtam_ef
write.csv(x = dtam_ef, file = "Estancias.EjercicioFiscal.csv", row.names = FALSE, quote = TRUE)
dir()

#1.- cantidad de polizas contratadas por aseguraora
dtam %>% count(aseguradora) #totales
dtam_a <- dtam %>% count(aseguradora) %>% mutate(porcentaje = n/sum(n)) #porcentajes
dtam_a

ggplot(dtam_a, aes(aseguradora, n)) + geom_bar(stat = "identity")

dtam_a2 <- dtam_a %>% #ordenar de mayor a menor
   arrange(desc(n)) %>%
   mutate(aseguradora = factor(aseguradora, levels = aseguradora)) %>%
   ggplot(aes(aseguradora, n)) +
   geom_bar(stat = "identity",
            fill = "blue",
            color = "black")

dtam_a2

#Agregar el texto 
dtam_a2 +
   geom_text (aes(label = comma((n))),
             vjust = -0.3) +
   labs(x = "Aseguradora",
        y = "Frecuencia de Contratación",
        title = "Aseguradoras Contratadas a Nivel Estatal") +
   scale_y_continuous(labels = comma) +
   theme_classic()



#Graficar porcentaje

dtam_a2p <- dtam_a %>%
   arrange(desc(porcentaje)) %>%
   mutate(aseguradora = factor(aseguradora, levels = aseguradora)) %>%
   ggplot(aes(aseguradora, porcentaje)) +
   geom_bar(stat = "identity",
            fill = "dark green",
            color = "black")
   

dtam_a2p
dtam_a2p +
geom_text (aes(label = percent((porcentaje))),
           vjust = -0.3) +
   labs(x = "Aseguradora",
        y = "Frecuencia de Contratación",
        title = "Porcentaje Aseguradoras Contratadas") +
   scale_y_continuous(labels = percent) +
   theme_classic()


#cambiar a circular

ggplot(dtam_a,aes(x="",y=porcentaje, fill=aseguradora))+
   geom_bar(stat = "identity",color="white")+
   coord_polar(theta="y")


#2.- cantidad de estancias aperturadas por ejercici fiscal

dtam %>% count(ejercicio_fiscal)
dtam_e <- dtam %>% count(ejercicio_fiscal) %>% mutate(porcentaje = n/sum(n)) #porcentajes
dtam_e

ggplot(dtam_e, aes(ejercicio_fiscal, n)) + geom_bar(stat = "identity",
                                                    fill = "blue",
                                                    color = "black")

dtam_e2 <- dtam_e %>% 
   mutate(ejercicio_fiscal = factor(ejercicio_fiscal, levels = ejercicio_fiscal)) %>%
   ggplot(aes(ejercicio_fiscal, n)) +
   geom_bar(stat = "identity",
            fill = "blue",
            color = "black")


dtam_e2


dtam_e2 +
   geom_text (aes(label = comma((n))),
              vjust = -0.3) +
   labs(x = "Ejercicio Fiscal",
        y = "Estancias",
        title = "Aperturas por Ejercicio Fiscal") +
   scale_y_continuous(labels = comma) +
   theme_classic()



dtam_e2p <- dtam_e %>%
   mutate(ejercicio_fiscal = factor(ejercicio_fiscal, levels = ejercicio_fiscal)) %>%
   ggplot(aes(ejercicio_fiscal, porcentaje)) +
   geom_bar(stat = "identity",
            fill = "dark green",
            color = "black")


dtam_e2p
dtam_e2p +
   geom_text (aes(label = percent((porcentaje))),
              vjust = -0.3) +
   labs(x = "Ejercicio Fiscal",
        y = "Estancias",
        title = "Aperturas por Ejercicio Fiscal") +
   scale_y_continuous(labels = percent) +
   theme_classic()


#cambiar a circular

ggplot(dtam_e,aes(x="",y=porcentaje, fill=ejercicio_fiscal))+
   geom_bar(stat = "identity",color="white")+
   coord_polar(theta="y")


#3.cantidad de estancias por municipio
dtam %>% count(municipio)
dtam_m <- dtam %>% count(municipio)%>% mutate(porcentaje = n/sum(n))
dtam_m

ggplot(dtam_m, aes(municipio, n)) + geom_bar(stat = "identity",
                                                    fill = "blue",
                                                    color = "black")

dtam_m2 <- dtam_m %>% arrange(desc(n)) %>%
   mutate(municipio = factor(municipio, levels = municipio)) %>%
   ggplot(aes(municipio, n)) +
   geom_bar(stat = "identity",
            fill = "green",
            color = "black")


dtam_m2


dtam_m2 +
   geom_text (aes(label = comma((n))),
              vjust = -0.3) +
   labs(x = "Municipo",
        y = "Estancias",
        title = "Estancias Aperturadas por Municipio") +
   scale_y_continuous(labels = comma) +
   theme_classic()



#otras graficas
# Graficamos "menores_inscritos" en el eje x y "municipio" en el eje de las y

names(dtam)

mi <- ggplot(dtam, aes(x= menores_inscritos , y = municipio)) + 
   geom_point()
mi
ggplotly(mi)

names(dtam)

#Histograma de menosres inscritos

hist(dtam$menores_inscritos)

hmi <- hist(dtam$menores_inscritos, breaks = seq(0,60, 1), 
     main = "Menores Inscritos",
     xlab = "Cantidad de Inscritos",
     ylab = "Frecuencia en Estancias")

hmi

hmi2 <-median(dtam$menores_inscritos)
ggplot(dtam, aes(menores_inscritos))+ 
   geom_histogram(binwidth = 1, col="black", fill = "green") + 
   ggtitle("Histograma de Menores Inscritos", paste("Moda=",hmi2)) +
   ylab("Frecuencia en Estancias") +
   xlab("Cantidad de Inscritos") +
   geom_vline(xintercept =  hmi2, col = "red", lwd = 1, lty =20)+
   theme_light()

# scatterplot menores inscritos vs Capacidad instalada, face wrap ejercicio fiscal.
ggplotly (dtam %>% ggplot( aes(menores_inscritos, capacidad_instalada, colour = capacidad_instalada)) +
   geom_point() + 
   ggtitle("Inscritos/Capacidad") +
   xlab("Inscritos") +
   ylab("Capacidad") +
   facet_wrap("ejercicio_fiscal") +
   theme_dark())


   