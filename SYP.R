library(quantmod)
library(tseries)
library(fpp2)
library(tidyverse)
library(gridExtra)

SNP=getSymbols("SPY", src="yahoo", auto.assign = FALSE, from="2024-01-01")

SPY=SNP$SPY.Close
plot(SPY)

EntrenaSPY=window(SPY, end = "2025-11-15")
PruebaSPY=window(SPY, start = "2025-11-16")

ggAcf(EntrenaSPY)
adf.test(EntrenaSPY)

SPYd=diff(EntrenaSPY) %>% na.omit()

grid.arrange(ggAcf(SPYd), ggPacf(SPYd), nrow=1)
adf.test(SPYd)

modeloSPYA=auto.arima(EntrenaSPY)
modeloSPY1=Arima(EntrenaSPY, order = c(1,1,1))
modeloSPY2=Arima(EntrenaSPY, order = c(1,1,2))
modeloSPY3=Arima(EntrenaSPY, order = c(3,1,3))

modeloSPYA
modeloSPY1
modeloSPY2
modeloSPY3

h_pronosticoSPY = length(PruebaSPY)

pronostico_SPYA = forecast(modeloSPYA, h = h_pronosticoSPY)
pronostico_SPY1 = forecast(modeloSPY1, h = h_pronosticoSPY)
pronostico_SPY2 = forecast(modeloSPY2, h = h_pronosticoSPY)
pronostico_SPY3 = forecast(modeloSPY3, h = h_pronosticoSPY)

modeloSPY3 %>% forecast(h=10) %>% autoplot(include=100)

tabla_con_erroresSPY = data.frame(
  Día = 1:10,
  Fecha = format(index(PruebaSPY)[1:10], "%Y-%m-%d"),
  Real = round(as.numeric(PruebaSPY)[1:10], 2),
  Pronóstico = round(as.numeric(pronostico_SPY3$mean)[1:10], 2),
  Error_USD = round(as.numeric(Prueba)[1:10] - 
                      as.numeric(pronostico_SPY3$mean)[1:10], 2),
  Error_Pct = round(((as.numeric(PruebaSPY)[1:10] - 
                        as.numeric(pronostico_SPY3$mean)[1:10]) / 
                       as.numeric(PruebaSPY)[1:10]) * 100, 3)
)

print(tabla_con_errores)
