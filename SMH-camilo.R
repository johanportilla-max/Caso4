library(quantmod)
library(tseries)
library(fpp2)
library(tidyverse)

SMH=getSymbols("SMH", src = "yahoo", auto.assign = FALSE, from="2018-01-01")

names(SMH)
plot(SMH$SMH.Close)

SMHprecios=SMH$SMH.Close

EntrenamientoS=window(SMHprecios, start = "2022-01-01", end = "2025-05-01")
PruebaS=window(SMHprecios, start = "2025-05-01")

plot(EntrenamientoS)
ggAcf(EntrenamientoS)
adf.test(EntrenamientoS)

EntrenamientoSD=diff(EntrenamientoS) %>% na.omit()

plot(EntrenamientoSD)
grid.arrange(ggAcf(EntrenamientoSD),ggPacf(EntrenamientoSD), nrow=1)
adf.test(EntrenamientoSD)

modeloAS=auto.arima(EntrenamientoS)
modeloS1=Arima(EntrenamientoS, order = c(3,1,3))
modeloS2=Arima(EntrenamientoS, order = c(3,1,4))
modeloS3=Arima(EntrenamientoS, order = c(4,1,3))
modeloS4=Arima(EntrenamientoS, order = c(4,1,4))
modeloS5=Arima(EntrenamientoS, order = c(2,1,2))
modeloS6=Arima(EntrenamientoS, order = c(1,1,0))
modeloS7=Arima(EntrenamientoS, order = c(2,1,0))
modeloS8=Arima(EntrenamientoS, order = c(3,1,0))
modeloS9=Arima(EntrenamientoS, order = c(2,1,1))
modeloAS

tablaSm=data.frame(Modelos=c("Auto-Arima","Modelo1(3,1,3)","Modelo2(3,1,4)",
                             "Modelo3(4,1,3)","Modelo4(4,1,4)","Modelo5(2,1,2)",
                             "Modelo6(1,1,0)", "Modelo7(2,1,0)","Modelo8(3,1,0)",
                             "Modelo9(2,1,1)"),
                   AICc=round(c(modeloAS$aicc, modeloS1$aicc,modeloS2$aicc,
                                modeloS3$aicc, modeloS4$aicc, modeloS5$aicc,
                                modeloS6$aicc,modeloS7$aicc, modeloS8$aicc,
                                modeloS9$aicc))) %>% 
  arrange(AICc)
tablaSm


checkresiduals(modeloAS)
accuracy(modeloAS)

modeloAS %>% forecast(h=10) %>% autoplot(include=100)



h_pronosticoS = length(PruebaS)

pronostico_A = forecast(modeloAS, h = h_pronosticoS)
pronostico_1 = forecast(modeloS1, h = h_pronosticoS)
pronostico_2 = forecast(modeloS2, h = h_pronosticoS)
pronostico_3 = forecast(modeloS3, h = h_pronosticoS)
pronostico_4 = forecast(modeloS4, h = h_pronosticoS)
pronostico_5 = forecast(modeloS5, h = h_pronosticoS)
pronostico_6 = forecast(modeloS6, h = h_pronosticoS)
pronostico_7 = forecast(modeloS7, h = h_pronosticoS)
pronostico_8 = forecast(modeloS8, h = h_pronosticoS)
pronostico_9 = forecast(modeloS9, h = h_pronosticoS)


fechas_pronosticoS = seq(as.Date("2025-05-01"), by = "day", length.out = h_pronosticoS)

ggplot() +
  geom_line(data = data.frame(Fecha = index(EntrenamientoS), 
                              Precio = as.numeric(EntrenamientoS)),
            aes(x = Fecha, y = Precio), color = 'black', alpha = 0.6) +
  geom_line(data = data.frame(Fecha = index(PruebaS), 
                              Precio = as.numeric(PruebaS)),
            aes(x = Fecha, y = Precio), color = '#E74C3C', linewidth = 1.2) +
  geom_line(data = data.frame(Fecha = fechas_pronosticoS,
                              Precio = as.numeric(pronostico_A$mean)),
            aes(x = Fecha, y = Precio), color = '#3498DB', linewidth = 1) +
  labs(title = "SMH: Pronóstico vs Real",
       subtitle = "Negro=Entrenamiento | Rojo=Real | Azul=Pronóstico Auto Arima",
       x = "Fecha", y = "Precio USD") +
  theme_minimal()

