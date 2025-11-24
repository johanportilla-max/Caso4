library(quantmod)

# Descargo la serie de interes desde yahoo

serie <- getSymbols('NFLX',src='yahoo',auto.assign=FALSE,
                    from="2015-01-01")

View(serie)
names(serie)

plot(serie$`NFLX.Close`)



# Actualmente se puede utilizar esta otra librerC-a
library(yahoofinancer)

maxDate = "2005-01-01"

#Serie a descargar
tick<-"NFLX"


accion_down <- Ticker$new(tick)

# Cargo los datos desde la API
prices<- accion_down$get_history(start = maxDate, interval = '1d')


View(prices)


# Esta libreria me permite convertir la data a serie de tiempo
#Notass info
# antes de analisis, contar la histori entender que paso, como se comporta ante una casualidad
# 
library(xts)

# Tomar la columna 'close' y convertirla a xts
close_prices <- xts(prices$close, order.by = as.Date(prices$date))


# Grafico
plot(close_prices, main="Precio de cierre NFLX")


accion<-serie$`NFLX.Close`

length(accion)
# Haciendo particion, para comprobar la prediccion
ventana<-window(accion, end = "2024-06-06") # ventana de entrenamiento
ventana2<-window(accion, start = "2024-06-07") # ventana de prueba


library(fpp2) #Libreria a usar

autoplot(ventana)


ggAcf(ventana) #Tiene auto correlacion, NO es independiente, periodos de tendencia, no es estacionaria 

library(tseries)
adf.test(ventana) #prueba de estacionalidad, no es , serie en niveles es la original 

library(tidyverse)
miserie<-diff(ventana) %>% na.omit() #quitar los na que se generan al diferenciar
autoplot(miserie) # es estacionaria, con una diferencia 
# metodologia arima  d p q<- parametros
library(tseries)
ggAcf(miserie)
adf.test(miserie)

library(gridExtra)

grid.arrange(ggAcf(miserie),
             ggPacf(miserie),
             nrow=1
)



auto.arima(ventana)  # PELIGRO,0,1,0 el cambio entre un periodo y otro es completamente aleatorio, 
# la consoloma me muestra criterios, menor AICc, investigar criterios de seleccion de modelos

#en este escenario la mejor prediccion es el ultimo valor
# p,d,q las bandas son asumidas como el error
# p resagos, contenplar variaciones en el arima, comparar
modelo1 <- Arima(ventana, order = c(0,1,0)) # Importante comparar el AICc con el autoarima

modelo2 <- Arima(ventana, order = c(5,1,5))
modelo2
# criterio de los residuos 
checkresiduals(modelo1) # graficar, pruebas de ljung box

checkresiduals(modelo2) # H0 es que no hay autocorrelacion, H1 hay 

accuracy(modelo1)
accuracy(modelo2)
# indicadores de error, mejor el 2

library(tidyverse)
#Pronostico
modelo1 %>% 
  forecast(h=5,level = 0.95)  # (Realizo 5 pronósticos), con el modelo 1 mo trae info, coge la ultima odservacion 

modelo2 %>% 
  forecast(h=5,level = 0.95) # 

#Miro los valores verdaderos
accion[2374:2378]

#  escenarios, contexto de las series 
#Gráfico
modelo2 %>% 
  forecast(h=5) %>%  # (Realizo 5 pronósticos)
  autoplot(include=80)   # Gráfico los últimos 80 valores + pronóstico (se puede cambiar el 80)


# Con la función xts(serie, order=date) se cargan los datos


## para cargar base de datos de excel, año,mes,dia <- R, en el prebui acomodar la fecha 

library(readxl)
Data <- read_excel("Datos/BaseEjemplo.xlsx", col_types = c("date", 
                                                           "numeric"))# carga la base 
View(Data)


Serie<-xts(Data$Y,order.by = Data$fecha)# pasa a series de tiempo 

autoplot(Serie)
# en el trabajo se puede hacer de yahoo finance, prediccionde 10 odservaciones, hablar de la historia de la serie 
# 




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
ModeloA=auto.arima(dif_Entrenamiento)
# la funcion Arima para de forma manuela ingresar los parametros 
modelo1=Arima(dif_Entrenamiento, order = c(3,1,3))
modelo2=Arima(dif_Entrenamiento, order = c(6,1,6))

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

modelo1 %>% 
  forecast(h=10) %>%  # (Realizo 5 pronósticos)
  autoplot(include=200) # ultimo n de precios 
# NOo