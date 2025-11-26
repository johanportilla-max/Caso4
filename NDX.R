
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

serie_NDX=getSymbols("NDX", src="yahoo",
                     auto.assign = FALSE, from="2024-11-15") # hasta la fecha si quiero un hasta alguno to=

plot(serie_NDX$NDX.Close)

## solo cuando es necesario convertir la columna a serie de tiempo 
# te ayuda a convertir a serie de tiempo 
#library(xts)
# La funcio xts parametros-> precio de cierre, ordenar por fecha 
#Precio_cierre=xts(serie_QQQ$`QQQ.Close`, order.by = as.Date(serie_QQQ$date))

Precio=serie_QQQ$`QQQ.Close`
# Numero de datos(dias)

length(Precio)
# la funcion venta te ayuda a separar el conjunto de prueba y el de entrenamiento

Entrenamiento=window(Precio, end="2025-11-16")
Prueba=window(Precio, start = "2025-11-16")

grafico_particion = ggplot() +
  geom_line(data = data.frame(
    Fecha = index(Entrenamiento),
    Precio = as.numeric(Entrenamiento)
  ), aes(x = Fecha, y = Precio), color = 'black', linewidth = 1, alpha = 0.85) +
  geom_line(data = data.frame(
    Fecha = index(Prueba),
    Precio = as.numeric(Prueba)
  ), aes(x = Fecha, y = Precio), color = '#E74C3C', linewidth = 1.3, alpha = 0.95) +
  geom_vline(xintercept = as.Date("2025-05-01"), linetype = "dashed", 
             color = 'grey50', linewidth = 1, alpha = 0.7) +
  annotate("text", x = as.Date("2023-06-15"), y = max(as.numeric(Precio))*0.98,
           label = "ENTRENAMIENTO (2022-2024)", size = 4, color = "black", fontface = "bold") +
  annotate("text", x = as.Date("2025-05-15"), y = max(as.numeric(Precio))*0.95,
           label = "PRUEBA (2025)", size = 4, color = "#E74C3C", fontface = "bold") +
  annotate("text", x = as.Date("2025-05-15"), y = min(as.numeric(Precio))*1.02,
           label = "Punto de corte", size = 3, color = "grey50", fontface = "italic") +
  ggtitle("QQQ: Partición de Datos para ARIMA",
          subtitle = "Negro=Entrenamiento (2022-2024, 755 obs) | Rojo=Prueba (2025, 275 obs)") +
  xlab("Fecha") +
  ylab("Precio USD") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0, size = 11, color = "grey40"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 11, face = "bold")
  )

print(grafico_particion)

autoplot(Entrenamiento)

# grafico del ACF grafico de auto correlacion 
ggAcf(Entrenamiento) #-> parece tener estacionalidad
adf.test(Entrenamiento) #-> aplico prueba de estacionalidad, no cumple, diferencio 

dif_Entrenamiento=diff(Entrenamiento) %>% na.omit() #->diferencio la serie
autoplot(dif_Entrenamiento)
# prueba de estacionalidad
adf.test(dif_Entrenamiento)
ggAcf(dif_Entrenamiento)

library(gridExtra)
# crea las graficas juntas de acf y pacf

grid.arrange(ggAcf(dif_Entrenamiento),
             ggPacf(dif_Entrenamiento),
             nrow=1)
# la funcion auto arima te recomienda una configuracion 
ModeloQA=auto.arima(Entrenamiento)
# la funcion Arima para de forma manuela ingresar los parametros 

modeloQ1 = Arima(Entrenamiento, order = c(3,1,3))
modeloQ2 = Arima(Entrenamiento, order = c(6,1,6))
modeloQ3 = Arima(Entrenamiento, order = c(1,1,1))
modeloQ4 = Arima(Entrenamiento, order = c(2,1,1))
modeloQ5 = Arima(Entrenamiento, order = c(1,1,2))
modeloQ6 = Arima(Entrenamiento, order = c(2,1,2))
modeloQ7 = Arima(Entrenamiento, order = c(3,1,1))

# ver por el creiterio de los AICc

ModeloQA
modelo1
modelo2 #-> mejor
modelo3
modelo4
modelo5
modelo6 
modelo7

#tabla

tabla = data.frame(
  Modelo = c('Auto-ARIMA', 'ARIMA(3,1,3)', 'ARIMA(6,1,6)', 'ARIMA(1,1,1)', 'ARIMA(2,1,1)', 'ARIMA(1,1,2)', "ARIMA(2,1,2)","ARIMA(3,1,1)"),
  AICc = round(c(ModeloQA$aicc, modeloQ1$aicc, modeloQ2$aicc, modeloQ3$aicc, modeloQ4$aicc, modeloQ5$aicc, modeloQ6$aicc, modeloQ7$aicc), 2)
) %>% arrange(AICc)

print(tabla)
# Criterio de los residuos
#H0 hay auto correlacion 

checkresiduals(ModeloA) 
checkresiduals(modelo1) 
checkresiduals(modelo2) 
checkresiduals(modelo3) 
checkresiduals(modelo4) 
checkresiduals(modelo5) 
checkresiduals(modelo6) #-> mejor
checkresiduals(modelo7)

# indicadores de erorr
accuracy(ModeloA)
accuracy(modelo1)
accuracy(modelo2)
accuracy(modelo6)



modeloQ7 %>% 
  forecast(h=10, level = 0.95) %>%  # (Realizo 5 pronósticos)
  autoplot(include=300) # ultimo n de precios 

modeloQ7# prediccion de los modelos

h_pronosticoQ = length(Prueba)

pronostico_QA = forecast(ModeloQA, h = h_pronosticoQ)
pronostico_Q1 = forecast(modeloQ1, h = h_pronosticoQ)
pronostico_Q2 = forecast(modeloQ2, h = h_pronosticoQ)
pronostico_Q3 = forecast(modeloQ3, h = h_pronosticoQ)
pronostico_Q4 = forecast(modeloQ4, h = h_pronosticoQ)
pronostico_Q5 = forecast(modeloQ5, h = h_pronosticoQ)
pronostico_Q6 = forecast(modeloQ6, h = h_pronosticoQ)
pronostico_Q7 = forecast(modeloQ7, h = h_pronosticoQ)

# Tabla

tabla_con_errores = data.frame(
  Día = 1:10,
  Fecha = format(index(Prueba)[1:10], "%Y-%m-%d"),
  Real = round(as.numeric(Prueba)[1:10], 2),
  Pronóstico = round(as.numeric(pronostico_Q7$mean)[1:10], 2),
  Error_USD = round(as.numeric(Prueba)[1:10] - 
                      as.numeric(pronostico_Q7$mean)[1:10], 2),
  Error_Pct = round(((as.numeric(Prueba)[1:10] - 
                        as.numeric(pronostico_Q7$mean)[1:10]) / 
                       as.numeric(Prueba)[1:10]) * 100, 3)
)

print(tabla_con_errores)

# grafico

fechas_pronosticoQ = seq(as.Date("2025-11-16"), by = "day", length.out = h_pronosticoQ)

ggplot() +
  geom_line(data = data.frame(Fecha = index(Entrenamiento), 
                              Precio = as.numeric(Entrenamiento)),
            aes(x = Fecha, y = Precio), color = 'black', alpha = 0.6) +
  geom_line(data = data.frame(Fecha = index(Prueba), 
                              Precio = as.numeric(Prueba)),
            aes(x = Fecha, y = Precio), color = '#E74C3C', linewidth = 1.2) +
  geom_line(data = data.frame(Fecha = fechas_pronosticoQ,
                              Precio = as.numeric(pronostico_Q2$mean)),
            aes(x = Fecha, y = Precio), color = '#3498DB', linewidth = 1) +
  labs(title = "QQQ: Pronóstico vs Real",
       subtitle = "Negro=Entrenamiento | Rojo=Real | Azul=Pronóstico ARIMA(2,1,2)",
       x = "Fecha", y = "Precio USD") +
  theme_minimal()
