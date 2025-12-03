#descruptivo
library(quantmod)
library(tidyquant)
library(tidyverse)
library(ggplot2)
library(moments)     
library(tseries)      
library(packcircles)  
library(zoo)          
serie <- getSymbols('QQQ', src='yahoo', auto.assign=FALSE, from="2010-01-01")
precios <- serie$QQQ.Close
# 2. ANÁLISIS DE LA SERIE ORIGINAL (PRECIOS)
# Gráfico de la Serie de Tiempo
plot(precios, main = "Evolución del Precio de Cierre QQQ")

#  Estadísticas Descriptivas Básicas
media <- mean(precios, na.rm = TRUE)
desviacion <- sd(precios, na.rm = TRUE)
asimetria <- skewness(precios, na.rm = TRUE)
curtosis <- kurtosis(precios, na.rm = TRUE)

cat("\n--- ESTADÍSTICAS DE PRECIOS ---\n")
cat("Media:", media, "\nDesviación:", desviacion, "\nAsimetría:", asimetria, "\nCurtosis:", curtosis, "\n")

# Histograma de Precios
hist(precios, breaks = 40, main = "Histograma de precios QQQ", 
     xlab = "Precio", col = "lightblue", border = "white")

#Composición del ETF (Gráfico de Burbujas)
holdings <- data.frame(
  ticker = c("AAPL", "MSFT", "AMZN", "AVGO", "META", "NVDA", "TSLA", "GOOGL", "GOOG", "COST", "OTROS"),
  empresa = c("Apple", "Microsoft", "Amazon", "Broadcom", "Meta", "NVIDIA", "Tesla", "Alphabet A", "Alphabet C", "Costco", "Resto del QQQ"),
  peso = c(8.7, 8.5, 4.8, 4.4, 4.2, 4.0, 3.5, 3.0, 2.9, 2.2, 53.8), 
  sector = c("Tecnología", "Tecnología", "Consumo", "Tecnología", "Comunicación", "Tecnología", "Consumo", "Comunicación", "Comunicación", "Consumo", "Varios")
)

packing <- circleProgressiveLayout(holdings$peso, sizetype='area')
holdings_pack <- cbind(holdings, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Unir datos para el gráfico
dat.gg$peso <- rep(holdings$peso, each=51)
dat.gg$sector <- rep(holdings$sector, each=51)
dat.gg$ticker <- rep(holdings$ticker, each=51)

grafico_qqq <- ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill = sector), colour = "white", alpha = 0.9) +
  geom_text(data = holdings_pack, aes(x, y, label = paste(ticker, "\n", peso, "%")), size = 3, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Tecnología" = "#0072B2", "Consumo" = "#E69F00", "Comunicación" = "#009E73", "Varios" = "#999999")) + 
  theme_void() + 
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, size=16, face="bold")) +
  coord_equal() +
  labs(title = "Composición del QQQ (Top Holdings)", fill = "Sector Económico")
print(grafico_qqq)

# Descomposición de la Serie (STL: Tendencia + Estacionalidad)
precios_ts <- ts(as.numeric(precios), frequency = 252)
descomposicion <- stl(precios_ts, s.window = "periodic")
plot(descomposicion, main = "Descomposición de la Serie de Precios QQQ")

# Evidencia de NO Estacionariedad en Precios
par(mfrow=c(1, 2))
acf(precios, main = "ACF Precios (No Diferenciada)")
pacf(precios, main = "PACF Precios (No Diferenciada)")
par(mfrow=c(1, 1))

#TRANSFORMACIÓN: CÁLCULO DE RENDIMIENTOS
rendimientos <- dailyReturn(precios, type = 'log')
rendimientos_vector <- as.vector(na.omit(rendimientos))

# ANÁLISIS DE ESTACIONARIEDAD (REQUISITO ARIMA)
# Prueba Formal (Dickey-Fuller Aumentada)
adf_test <- adf.test(rendimientos, alternative = "stationary")
print(adf_test)

if (adf_test$p.value < 0.05) {
  print(">> Conclusión ADF: Los rendimientos son ESTACIONARIOS.")
} else {
  print(">> Conclusión ADF: Los rendimientos NO son estacionarios.")
}

# Identificación de Órdenes ARIMA (ACF y PACF de Rendimientos)
par(mfrow=c(1, 2)) 
acf(rendimientos, main = "ACF Rendimientos (Orden q)")
pacf(rendimientos, main = "PACF Rendimientos (Orden p)")
par(mfrow=c(1, 1))

#ANÁLISIS DE NORMALIDAD

#Visualización: Histograma con Densidad vs Normal
hist(rendimientos_vector, breaks = 50, main = "Distribución de Rendimientos QQQ",
     xlab = "Rendimiento Logarítmico", col = "darkred", border = "white", freq = FALSE)
lines(density(rendimientos_vector), col = "blue", lwd = 2) # Densidad real
mu <- mean(rendimientos_vector)
sigma <- sd(rendimientos_vector)
curve(dnorm(x, mean = mu, sd = sigma), add = TRUE, col = "green", lwd = 2) # Curva Normal teórica
legend("topright", legend=c("Real", "Normal Teórica"), col=c("blue", "green"), lwd=2)

# B. Visualización: Gráfico Q-Q
qqnorm(rendimientos_vector, main = "Gráfico Q-Q de Rendimientos")
qqline(rendimientos_vector, col = "red")

# C. Pruebas Estadísticas
shapiro_test <- shapiro.test(rendimientos_vector) 
jarque_bera_test <- jarque.bera.test(rendimientos_vector)

cat("\n--- PRUEBAS DE NORMALIDAD ---\n")
print(shapiro_test)
print(jarque_bera_test)
# 6. ANÁLISIS DE VOLATILIDAD
# Volatilidad Móvil (30 días)
volatilidad_movil <- rollapply(rendimientos, width = 30, FUN = sd, fill = NA)
plot(volatilidad_movil, main = "Volatilidad Móvil (30 días)", col = "blue", lwd = 1.5, ylab = "Desviación Estándar")

# Clusterización de Volatilidad (Rendimientos al cuadrado)
plot(rendimientos^2, main = "Rendimientos al Cuadrado (Clusters)", ylab = "Rendimiento^2", col = "darkred")

#Autocorrelación de la Volatilidad 
par(mfrow=c(1, 1))
acf(rendimientos^2, main = "ACF de Rendimientos Cuadrados (Efectos ARCH)", ylab = "Autocorrelación")

