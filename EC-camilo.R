library(quantmod)
library(tidyverse)
library(fpp2)
library(tseries)
library(gridExtra)

Ecopetrol=getSymbols("EC", src = "yahoo", auto.assign = FALSE, from="2015-01-01")
names(Ecopetrol)

PreciosE=Ecopetrol$EC.Close

plot(PreciosE)

EntrenamientoE=window(PreciosE, start="2017-01-01",end = "2025-08-31")
PruebaE=window(PreciosE, start = "2025-09-01")

plot(EntrenamientoE)
ggAcf(EntrenamientoE)
adf.test(EntrenamientoE)

EntrenamientoED=diff(EntrenamientoE) %>% na.omit()

plot(EntrenamientoED)
ggAcf(EntrenamientoED)
ggPacf(EntrenamientoED)
adf.test(EntrenamientoED)

grid.arrange(ggAcf(EntrenamientoED),ggPacf(EntrenamientoED), nrow=1)+theme_classic()

modeloEA=auto.arima(EntrenamientoE)

modeloE1=Arima(EntrenamientoE, order = c(4,1,4))
modeloE2=Arima(EntrenamientoE, order = c(3,1,4))
modeloE3=Arima(EntrenamientoE, order = c(2,1,4))
modeloE4=Arima(EntrenamientoE, order = c(3,1,3))
modeloE5=Arima(EntrenamientoE, order = c(4,1,3))
modeloE6=Arima(EntrenamientoE, order = c(5,1,4))
modeloE7=Arima(EntrenamientoE, order = c(2,1,2))


modeloEA
modeloE1
modeloE2  
modeloE3
modeloE4 #-> mejor
modeloE5
modeloE6
modeloE7

tabla=data_frame(
  Modelo=c("Auto-Arima", "Modelo1(4,1,4)", "Modelo2(3,1,4)", "Modelo3(2,1,4)",
           "Modelo4(3,1,3)", "Modelo5(4,1,3)", "Modelo6(5,1,4)","Modelo5(2,1,2)"),
  AICc=round(c(modeloEA$aicc, modeloE1$aicc, modeloE2$aicc, modeloE3$aicc,
               modeloE4$aicc, modeloE5$aicc,modeloE6$aicc,modeloE7$aicc))) %>% 
  arrange(AICc)

tabla

modeloE5 %>% forecast(h=10, level = 0.95) %>% autoplot(include=100)



