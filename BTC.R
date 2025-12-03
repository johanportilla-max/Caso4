library(yahoofinancer)
library(tseries)
library(fpp2)
library(tidyverse)
library(quantmod)
library(gridExtra)

SerieBTC=getSymbols("BTC", src = "yahoo", auto.assign = FALSE, from="2024-01-01")

BTC=SerieBTC$`BTC.Close`

autoplot(BTC)

length(BTC)
EntreBTC<-window(BTC, end = "2025-11-01") # ventana de entrenamiento
PruebaBTC<-window(BTC, start = "2025-11-01") # ventana de prueba

autoplot(EntreBTC)
ggAcf(EntreBTC)

adf.test(EntreBTC)

BTCd<-diff(EntreBTC) %>% na.omit() 
autoplot

ggAcf(BTCd)
adf.test(BTCd)

grid.arrange(ggAcf(BTCd),
             ggPacf(BTCd),
             nrow=1
)



modeloBTCA=auto.arima(EntreBTC)  
modeloBTC1 <- Arima(EntreBTC, order = c(1,1,1))
modeloBTC2 <- Arima(EntreBTC, order = c(2,1,1))
modeloBTC3 <- Arima(EntreBTC, order = c(2,1,2))
modeloBTC4 <- Arima(EntreBTC, order = c(3,1,2))
modeloBTC5 <- Arima(EntreBTC, order = c(3,1,3))
modeloBTC6 <- Arima(EntreBTC, order = c(3,1,0))
modeloBTC7 <- Arima(EntreBTC, order = c(0,1,3))

modeloBTCA
modeloBTC1
modeloBTC2  
modeloBTC3
modeloBTC4 
modeloBTC5
modeloBTC6
modeloBTC7

tablaBTC=data_frame(
  Modelo=c("Auto-Arima", "Modelo1(1,1,1)", "Modelo2(2,1,1)", "Modelo3(2,1,2)",
           "Modelo4(3,1,2)", "Modelo5(3,1,3)", "Modelo6(3,1,0)","Modelo7(0,1,3)"),
  AICc=round(c(modeloBTCA$aicc, modeloBTC1$aicc, modeloBTC2$aicc, modeloBTC3$aicc,
               modeloBTC4$aicc, modeloBTC5$aicc,modeloBTC6$aicc,modeloBTC7$aicc))) %>% 
  arrange(AICc)

tablaBTC


checkresiduals(modeloBTCA)
checkresiduals(modeloBTC1)

accuracy(modeloBTCA)
accuracy(modeloBTC1)


modeloBTC5 %>% 
  forecast(h=5,level = 0.95)  # (Realizo 5 pronósticos), con el modelo 1 mo trae info, coge la ultima odservacion 

modeloBTC1 %>% 
  forecast(h=5,level = 0.95) # 

BTC[48:50]

#  escenarios, contexto de las series 
#Gráfico
modeloBTC5 %>% 
  forecast(h=10) %>%  # (Realizo 5 pronósticos)
  autoplot(include=200)   # Gráfico los últimos 80 valores + pronóstico (se puede cambiar el 80)



h_pronosticoBTC = length(PruebaBTC)

pronostico_BTCA = forecast(modeloBTCA, h = h_pronosticoBTC)
pronostico_BTC1 = forecast(modeloBTC1, h = h_pronosticoBTC)
pronostico_BTC2 = forecast(modeloBTC2, h = h_pronosticoBTC)
pronostico_BTC3 = forecast(modeloBTC3, h = h_pronosticoBTC)
pronostico_BTC4 = forecast(modeloBTC4, h = h_pronosticoBTC)
pronostico_BTC5 = forecast(modeloBTC5, h = h_pronosticoBTC)
pronostico_BTC6 = forecast(modeloBTC6, h = h_pronosticoBTC)
pronostico_BTC7 = forecast(modeloBTC7, h = h_pronosticoBTC)



fechas_pronosticoBTC = seq(as.Date("2025-11-01"), by = "day", length.out = h_pronosticoBTC)

ggplot() +
  geom_line(data = data.frame(Fecha = index(EntreBTC), 
                              Precio = as.numeric(EntreBTC)),
            aes(x = Fecha, y = Precio), color = 'black', alpha = 0.6) +
  geom_line(data = data.frame(Fecha = index(PruebaBTC), 
                              Precio = as.numeric(PruebaBTC)),
            aes(x = Fecha, y = Precio), color = '#E74C3C', linewidth = 1.2) +
  geom_line(data = data.frame(Fecha = fechas_pronosticoBTC,
                              Precio = as.numeric(pronostico_BTC1$mean)),
            aes(x = Fecha, y = Precio), color = '#3498DB', linewidth = 1) +
  labs(title = "SMH: Pronóstico vs Real",
       subtitle = "Negro=Entrenamiento | Rojo=Real | Azul=Pronóstico Auto Arima",
       x = "Fecha", y = "Precio USD") +
  theme_minimal()

