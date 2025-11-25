#33 EC

library(quantmod)

# Descargo la serie de interes desde yahoo

serie <- getSymbols('EC',src='yahoo',auto.assign=FALSE,
                    from="2017-01-01")

View(serie)
names(serie)

plot(serie$`EC.Close`)



# Actualmente se puede utilizar esta otra librerC-a
library(yahoofinancer)

maxDate = "2018-01-01"

#Serie a descargar
tick<-"EC"


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
plot(close_prices, main="Precio de cierre EC")


accion<-serie$`EC.Close`

length(accion)
# Haciendo particion, para comprobar la prediccion
ventana<-window(accion, end = "2021-06-15") # ventana de entrenamiento
ventana2<-window(accion, start = "2021-06-15") # ventana de prueba


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
modelo1 <- Arima(ventana, order = c(0,1,3)) # Importante comparar el AICc con el autoarima

modelo2 <- Arima(ventana, order = c(6,1,6))
modelo2
modelo1
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
accion[0:15]

#  escenarios, contexto de las series 
#Gráfico
modelo1 %>% 
  forecast(h=5) %>%  # (Realizo 5 pronósticos)
  autoplot(include=80)   # Gráfico los últimos 80 valores + pronóstico (se puede cambiar el 80)


# Con la función xts(serie, order=date) se cargan los datos


## para cargar base de datos de excel, año,mes,dia <- R, en el prebui acomodar la fecha 
##3333






library(readxl)
Data <- read_excel("Datos/BaseEjemplo.xlsx", col_types = c("date", 
                                                           "numeric"))# carga la base 
View(Data)


Serie<-xts(Data$Y,order.by = Data$fecha)# pasa a series de tiempo 

autoplot(Serie)
# en el trabajo se puede hacer de yahoo finance, prediccionde 10 odservaciones, hablar de la historia de la serie 
# 






##### modelo QQQ

library(quantmod)

# Descargo la serie de interes desde yahoo

serie <- getSymbols('QQQ',src='yahoo',auto.assign=FALSE,
                    from="2017-01-01")

View(serie)
names(serie)

plot(serie$`EC.Close`)



# Actualmente se puede utilizar esta otra librerC-a
library(yahoofinancer)

maxDate = "2018-01-01"

#Serie a descargar
tick<-"EC"


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
plot(close_prices, main="Precio de cierre EC")


accion<-serie$`EC.Close`

length(accion)
# Haciendo particion, para comprobar la prediccion
ventana<-window(accion, end = "2021-06-15") # ventana de entrenamiento
ventana2<-window(accion, start = "2021-06-15") # ventana de prueba


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
modelo1 <- Arima(ventana, order = c(0,1,3)) # Importante comparar el AICc con el autoarima

modelo2 <- Arima(ventana, order = c(6,1,6))
modelo2
modelo1
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
accion[0:15]

#  escenarios, contexto de las series 
#Gráfico
modelo1 %>% 
  forecast(h=5) %>%  # (Realizo 5 pronósticos)
  autoplot(include=80)   # Gráfico los últimos 80 valores + pronóstico (se puede cambiar el 80)




#####

library(quantmod)

# Descargo la serie de interes desde yahoo

serie <- getSymbols('BTC',src='yahoo',auto.assign=FALSE,
                    from="2017-01-01")

View(serie)
names(serie)

plot(serie$`BTC.Close`)



# Actualmente se puede utilizar esta otra librerC-a
library(yahoofinancer)

maxDate = "2018-01-01"

#Serie a descargar
tick<-"BTC"


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
plot(close_prices, main="Precio de cierre EC")


accion<-serie$`BTC.Close`


length(accion)
# Haciendo particion, para comprobar la prediccion
ventana<-window(accion, end = "2025-05-31") # ventana de entrenamiento
ventana2<-window(accion, start = "2025-06-01") # ventana de prueba


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
modelo1 <- Arima(ventana, order = c(2,1,2)) # Importante comparar el AICc con el autoarima

modelo2 <- Arima(ventana, order = c(3,1,3))
modelo2
modelo1
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
accion[35:50]

#  escenarios, contexto de las series 
#Gráfico
modelo1 %>% 
  forecast(h=10) %>%  # (Realizo 5 pronósticos)
  autoplot(include=200)   # Gráfico los últimos 80 valores + pronóstico (se puede cambiar el 80)
