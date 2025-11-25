
library(quantmod)

# Descargo la serie de interes desde yahoo

serie <- getSymbols('EC',src='yahoo',auto.assign=FALSE,
                    from="2017-01-01")

View(serie)
names(serie)

plot(serie$`EC.Close`)



# Actualmente se puede utilizar esta otra librerC-a
library(yahoofinancer)

maxDate = "2017-01-01"

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
ventana <- window(accion, start = "2019-01-01", end = "2022-01-01")
ventana2 <- window(accion, start = "2022-01-02")

# ventana de prueba


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
library(forecast)

auto.arima(ventana,
           max.p = 5,
           max.q = 5,
           stepwise = FALSE,
           approximation = FALSE,
           seasonal = FALSE)
# PELIGRO,0,1,0 el cambio entre un periodo y otro es completamente aleatorio, 
# la consoloma me muestra criterios, menor AICc, investigar criterios de seleccion de modelos

#en este escenario la mejor prediccion es el ultimo valor
# p,d,q las bandas son asumidas como el error
# p resagos, contenplar variaciones en el arima, comparar
modelo1 <- Arima(ventana, order = c(3,1,2)) # Importante comparar el AICc con el autoarima

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
accion[0:20]

#  escenarios, contexto de las series 
#Gráfico
modelo1 %>% 
  forecast(h=5) %>%  # (Realizo 5 pronósticos)
  autoplot(include=80)   # Gráfico los últimos 80 valores + pronóstico (se puede cambiar el 80)






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
forecast_5 <- forecast(modelo1, h = 5)$mean
real_5 <- ventana2[1:5]

cbind(pronostico = forecast_5, real = real_5)



ventana <- window(accion, end = "2024-01-01")
ventana_test <- window(accion, start = "2024-01-01")
modelo <- auto.arima(ventana)
pron <- forecast(modelo, h = length(ventana_test))
accuracy(pron, ventana_test)











##########################3

# Tu ventana de datos (usando la ventana post-COVID sugerida)
# Importante: Asegúrate que 'accion' esté cargada correctamente.
ventana <- window(accion, start = "2020-06-01", end = "2022-01-01") 

log_ventana <- log(ventana)

miserie_log <- diff(log_ventana) %>% na.omit()
autoplot(miserie_log)

adf.test(miserie_log)

library(gridExtra)
grid.arrange(ggAcf(miserie_log),
             ggPacf(miserie_log),
             nrow=1)
modelo_log <- auto.arima(log_ventana,
                         max.p = 5,
                         max.q = 5,
                         stepwise = FALSE,
                         approximation = FALSE,
                         seasonal = FALSE)

modelo_log
checkresiduals(modelo_log)
pronostico_log <- forecast(modelo_log, h=5, level = 0.95)

pronostico_real <- data.frame(
  Fecha = index(pronostico_log$mean),
  Pronostico = exp(pronostico_log$mean),
  Limite_Inferior = exp(pronostico_log$lower[, "95%"]), # Revertir banda inferior
  Limite_Superior = exp(pronostico_log$upper[, "95%"])  # Revertir banda superior
)

print(pronostico_real)

autoplot(pronostico_log, include=80) + 
  ylab("Precio (escala logarítmica)") 

valores_reales <- head(ventana2, 5)

valores_reales_num <- as.numeric(valores_reales)
comparativa <- cbind(pronostico_real, Real = valores_reales_num)

comparativa$Diferencia <- comparativa$Real - comparativa$Pronostico
comparativa$Error_Absoluto <- abs(comparativa$Diferencia)

comparativa$Error_Pct <- (comparativa$Error_Absoluto / comparativa$Real) * 100

print(comparativa)

rmse <- sqrt(mean(comparativa$Diferencia^2))

mape <- mean(comparativa$Error_Pct)

cat("\nEl modelo se equivocó, en promedio, un", round(mape, 2), "% por día.\n")
cat("En dinero, el error promedio es de:", round(rmse, 2), "pesos/dólares.\n")
