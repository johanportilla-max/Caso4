#### analisis descriptivo diego 
##grafico de la serie 
library(quantmod)
library(moments)
library(tidyquant)
library(tidyverse)
library(ggplot2)

serie <- getSymbols('QQQ',src='yahoo',auto.assign=FALSE,
                    from="2010-01-01")
View(serie)
names(serie)
plot(serie$`QQQ.Close`)

##comentario serie
#estadistica basica de las serie 
precios <- serie$QQQ.Close
media <- mean(precios, na.rm = TRUE)
desviacion <- sd(precios, na.rm = TRUE)
asimetria <- skewness(precios, na.rm = TRUE)
curtosis <- kurtosis(precios, na.rm = TRUE)

media
desviacion
asimetria
curtosis

###histograma
precios <- serie$QQQ.Close

hist(precios,
     breaks = 40,
     main = "Histograma de precios QQQ",
     xlab = "Precio",
     col = "lightblue",
     border = "white")



# obtener tabla de componentes del QQQ
componentes <- tq_index("nasdaq100")  

head(componentes)
precios <- componentes %>%
  tq_get(from = "2015-01-01")

ggplot(precios, aes(x = date, y = adjusted)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ symbol, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Componentes del QQQ (NASDAQ 100)",
    subtitle = "Precio ajustado por empresa",
    x = "Fecha",
    y = "Precio"
  )


