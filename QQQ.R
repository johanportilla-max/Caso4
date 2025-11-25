
### entre modelo QQQ
library(tidyverse)

#libreria que te ayuda a extraer los datos
library(quantmod)
#convertir a serie de tiempo 
library(xts)
# carga las librerias para analisis,
#con funciones como auto.arima(), Arima(), checkresiduals(), forecast()
library(fpp2)
#Prueba de Dickey–Fuller aumentada (ADF), Sirve para comprobar si una serie es estacionaria o si tiene raíz unitaria.
#Interpretación:
#H0: la serie NO es estacionaria
#H1: la serie sí es estacionaria.
library(tseries)

serie_QQQ=getSymbols("QQQ", src="yahoo",
                     auto.assign = FALSE, from="2021-01-01") # hasta la fecha si quiero un hasta alguno to=

View(serie_QQQ)
plot(serie_QQQ$QQQ.Close)

## solo cuando es necesario convertir la columna a serie de tiempo 
# te ayuda a convertir a serie de tiempo 
#library(xts)
# La funcio xts parametros-> precio de cierre, ordenar por fecha 
#Precio_cierre=xts(serie_QQQ$`QQQ.Close`, order.by = as.Date(serie_QQQ$date))

Precio=serie_QQQ$`QQQ.Close`
# Numero de datos(dias)

length(Precio)
# la funcion venta te ayuda a separar el conjunto de prueba y el de entrenamiento

Entrenamiento=window(Precio, end="2023-12-31")
Prueba=window(Precio, start = "2024-01-01")

autoplot(Entrenamiento)

# grafico del ACF grafico de auto correlacion 
ggAcf(Entrenamiento) #-> parece tener estacionalidad

adf.test(Entrenamiento) #-> aplico prueba de estacionalidad

dif_Entrenamiento=diff(Entrenamiento) %>% na.omit() #->diferencio la serie
autoplot(dif_Entrenamiento)
# prueba de estacionalidad
adf.test(dif_Entrenamiento)
ggAcf(dif_Entrenamiento)
## se ven muy dispersos aun, aplico segunda diferencia seguro que tiene que ver?

Diferenciada=diff(dif_Entrenamiento) %>% na.omit()
autoplot(Diferenciada)

library(gridExtra)
# crea las graficas juntas de acf y pacf

grid.arrange(ggAcf(dif_Entrenamiento),
             ggPacf(dif_Entrenamiento),
             nrow=1)
# la funcion auto arima te recomienda una configuracion 
ModeloA=auto.arima(Entrenamiento)
# la funcion Arima para de forma manuela ingresar los parametros 
modelo1=Arima(dif_Entrenamiento, order = c(3,0,3))
modelo2=Arima(dif_Entrenamiento, order = c(6,0,6))

# ver por el creiterio de los AICc
ModeloA #-> mejor
modelo1
modelo2

# Criterio de los residuos
#H0 hay auto correlacion 
checkresiduals(ModeloA) #-> mejor 
checkresiduals(modelo1)
checkresiduals(modelo2)

# indicadores de erorr
accuracy(ModeloA)
accuracy(modelo1)
accuracy(modelo2)

modelo2 %>% 
  forecast(h=10) %>%  # (Realizo 5 pronósticos)
  autoplot(include=200) # ultimo n de precios 

