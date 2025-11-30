library(tidyverse)
library(quantmod)
library(xts)
library(fpp2)
library(tseries)
library(scales)
library(knitr)
library(kableExtra)

qqq_pal <- list(
  bg        = "#ffffff",
  text_dark = "#1f2937",
  text_gray = "#4b5563",
  primary   = "#00b894",
  secondary = "#06b6d4",
  positive  = "#84cc16",
  negative  = "#e17055",
  grid      = "#f3f4f6"
)

serie_QQQ=getSymbols("QQQ", src="yahoo",
                     auto.assign = FALSE, from="2022-10-07") 

plot(serie_QQQ$QQQ.Close)

Precio=serie_QQQ$`QQQ.Close`

length(Precio)

Entrenamiento=window(Precio, end="2025-09-30")
Prueba=window(Precio, start = "2025-10-01")

df_train <- data.frame(
  Fecha = index(Entrenamiento),
  Precio = as.numeric(Entrenamiento),
  Conjunto = "Entrenamiento"
)

df_test <- data.frame(
  Fecha = index(Prueba),
  Precio = as.numeric(Prueba),
  Conjunto = "Prueba"
)

df_completo <- bind_rows(df_train, df_test)

fecha_corte <- as.Date("2025-10-01")

ggplot(df_completo, aes(x = Fecha, y = Precio)) +
  geom_ribbon(data = df_train, 
              aes(ymin = min(df_completo$Precio) * 0.95, ymax = Precio),
              fill = qqq_pal$primary, alpha = 0.08) +
  geom_ribbon(data = df_test, 
              aes(ymin = min(df_completo$Precio) * 0.95, ymax = Precio),
              fill = qqq_pal$secondary, alpha = 0.15) +
  geom_line(data = df_train, color = qqq_pal$primary, linewidth = 0.9) +
  geom_line(data = df_test, color = qqq_pal$secondary, linewidth = 1.1) +
  geom_vline(xintercept = fecha_corte, 
             linetype = "dashed", color = qqq_pal$negative, linewidth = 0.8) +
  annotate("text", x = fecha_corte, y = max(df_completo$Precio) * 1.02,
           label = "Corte: 01-Oct-2025", hjust = -0.05, vjust = 0,
           color = qqq_pal$negative, fontface = "bold", size = 3.5) +
  annotate("label", 
           x = as.Date("2024-01-01"), 
           y = max(df_completo$Precio) * 0.85,
           label = paste0("ENTRENAMIENTO\n", nrow(df_train), " observaciones"),
           fill = qqq_pal$primary, color = "white", 
           fontface = "bold", size = 3.5, label.padding = unit(0.5, "lines")) +
  annotate("label", 
           x = max(df_test$Fecha) - 10,
           y = min(df_completo$Precio) * 1.15,
           label = paste0("PRUEBA\n", nrow(df_test), " obs."),
           fill = qqq_pal$secondary, color = "white", 
           fontface = "bold", size = 3.2, label.padding = unit(0.4, "lines")) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(labels = dollar_format(prefix = "$"),
                     expand = expansion(mult = c(0.05, 0.08))) +
  labs(
    title = "Partición de Datos: Entrenamiento vs Prueba",
    subtitle = "QQQ (Nasdaq-100 ETF) | Serie de precios de cierre diarios",
    x = NULL,
    y = "Precio de Cierre (USD)",
    caption = paste0("Fuente: Yahoo Finance | Período: ", 
                     min(df_completo$Fecha), " a ", max(df_completo$Fecha))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    
    plot.title = element_text(face = "bold", size = 16, color = qqq_pal$text_dark,
                              margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, color = qqq_pal$secondary,
                                 margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = qqq_pal$text_gray,
                                margin = margin(t = 15), hjust = 0),
    
    axis.title.y = element_text(face = "bold", size = 10, color = qqq_pal$text_gray),
    axis.text = element_text(size = 9, color = qqq_pal$text_gray),
    axis.text.x = element_text(angle = 45, hjust = 1),
    
    panel.grid.major = element_line(color = qqq_pal$grid, linetype = "dashed", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    
    plot.margin = margin(20, 25, 15, 15)
  )

autoplot(Entrenamiento)

# verificar estacionariedad

## grafica 

ggAcf(Entrenamiento) 

acf_data <- acf(Entrenamiento, lag.max = 30, plot = FALSE)

df_acf <- data.frame(
  Lag = acf_data$lag[-1], 
  ACF = acf_data$acf[-1]
)

n <- length(Entrenamiento)
limite_sup <- qnorm(0.975) / sqrt(n)
limite_inf <- -limite_sup

ggplot(df_acf, aes(x = Lag, y = ACF)) +
  
  geom_segment(aes(xend = Lag, yend = 0), 
               color = qqq_pal$primary, linewidth = 0.8) +
  geom_point(color = qqq_pal$primary, size = 2) +
  
  geom_hline(yintercept = limite_sup, linetype = "dashed", 
             color = qqq_pal$secondary, linewidth = 0.7) +
  geom_hline(yintercept = limite_inf, linetype = "dashed", 
             color = qqq_pal$secondary, linewidth = 0.7) +
  geom_hline(yintercept = 0, color = qqq_pal$text_gray, linewidth = 0.5) +
  
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = limite_inf, ymax = limite_sup,
           fill = qqq_pal$secondary, alpha = 0.1) +
  
  annotate("label", x = 20, y = 0.5,
           label = "Decaimiento lento\n→ Serie NO estacionaria",
           fill = qqq_pal$negative, color = "white",
           fontface = "bold", size = 3.5, label.padding = unit(0.5, "lines")) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(limits = c(-0.1, 1.05), breaks = seq(0, 1, 0.25)) +
  labs(
    title = "Función de Autocorrelación (ACF) - Serie en Niveles",
    subtitle = "QQQ: Precio de cierre | Datos de entrenamiento",
    x = "Rezago (Lag)",
    y = "Autocorrelación",
    caption = "Bandas azules: Límites de significancia al 95%"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(face = "bold", size = 14, color = qqq_pal$text_dark),
    plot.subtitle = element_text(size = 10, color = qqq_pal$secondary),
    plot.caption = element_text(size = 9, color = qqq_pal$text_gray, hjust = 0),
    axis.title = element_text(face = "bold", size = 10, color = qqq_pal$text_gray),
    axis.text = element_text(size = 9, color = qqq_pal$text_gray),
    panel.grid.major = element_line(color = qqq_pal$grid, linetype = "dashed", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

## prueba estadistica 

adf.test(Entrenamiento) 

adf_resultado <- adf.test(Entrenamiento)

tabla_adf <- data.frame(
  Métrica = c("Estadístico Dickey-Fuller", 
              "Orden de Rezagos (Lag)", 
              "P-valor",
              "Nivel de Significancia (α)",
              "Hipótesis Nula (H₀)",
              "Decisión"),
  Valor = c(round(adf_resultado$statistic, 4),
            adf_resultado$parameter,
            round(adf_resultado$p.value, 4),
            "0.05",
            "Serie tiene raíz unitaria",
            ifelse(adf_resultado$p.value > 0.05, 
                   "No rechazar H₀", "Rechazar H₀")),
  Interpretación = c("Valor del estadístico de prueba",
                     "Rezagos incluidos en el test",
                     "Probabilidad bajo H₀",
                     "Umbral de decisión",
                     "La serie NO es estacionaria",
                     ifelse(adf_resultado$p.value > 0.05,
                            "Serie NO estacionaria ",
                            "Serie estacionaria ✓"))
)

kable(tabla_adf, 
      caption = "Prueba de Dickey-Fuller Aumentada (ADF) - Serie en Niveles",
      align = c("l", "c", "l")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  row_spec(3, bold = TRUE, color = "#e17055") %>% 
  row_spec(6, bold = TRUE, background = "#fef3f2")



dif_Entrenamiento=diff(Entrenamiento) %>% na.omit()
autoplot(dif_Entrenamiento)

adf.test(dif_Entrenamiento)
ggAcf(dif_Entrenamiento)

library(gridExtra)
grid.arrange(ggAcf(dif_Entrenamiento),
             ggPacf(dif_Entrenamiento),
             nrow=1)
ModeloQA=auto.arima(Entrenamiento)

modeloQ1 = Arima(Entrenamiento, order = c(3,1,3))
modeloQ2 = Arima(Entrenamiento, order = c(6,1,6))
modeloQ3 = Arima(Entrenamiento, order = c(1,1,1))
modeloQ4 = Arima(Entrenamiento, order = c(2,1,1))
modeloQ5 = Arima(Entrenamiento, order = c(1,1,2))
modeloQ6 = Arima(Entrenamiento, order = c(2,1,2))
modeloQ7 = Arima(Entrenamiento, order = c(3,1,1))

ModeloQA
modeloQ1
modeloQ2 
modeloQ3
modeloQ4
modeloQ5
modeloQ6 
modeloQ7


tabla = data.frame(
  Modelo = c('Auto-ARIMA', 'ARIMA(3,1,3)', 'ARIMA(6,1,6)', 'ARIMA(1,1,1)', 'ARIMA(2,1,1)', 'ARIMA(1,1,2)', "ARIMA(2,1,2)","ARIMA(3,1,1)"),
  AICc = round(c(ModeloQA$aicc, modeloQ1$aicc, modeloQ2$aicc, modeloQ3$aicc, modeloQ4$aicc, modeloQ5$aicc, modeloQ6$aicc, modeloQ7$aicc), 2)
) %>% arrange(AICc)

print(tabla)

checkresiduals(ModeloQA) 
checkresiduals(modeloQ1) 
checkresiduals(modeloQ2) 
checkresiduals(modeloQ3) 
checkresiduals(modeloQ4) 
checkresiduals(modeloQ5) 
checkresiduals(modeloQ6) 
checkresiduals(modeloQ7)

accuracy(ModeloQA)
accuracy(modeloQ1)
accuracy(modeloQ2)
accuracy(modeloQ6)



ModeloQA %>% 
  forecast(h=10, level = 0.95) %>% 
  autoplot(include=400)

ModeloQA

h_pronosticoQ = length(Prueba)

pronostico_QA = forecast(ModeloQA, h = h_pronosticoQ)
pronostico_Q1 = forecast(modeloQ1, h = h_pronosticoQ)
pronostico_Q2 = forecast(modeloQ2, h = h_pronosticoQ)
pronostico_Q3 = forecast(modeloQ3, h = h_pronosticoQ)
pronostico_Q4 = forecast(modeloQ4, h = h_pronosticoQ)
pronostico_Q5 = forecast(modeloQ5, h = h_pronosticoQ)
pronostico_Q6 = forecast(modeloQ6, h = h_pronosticoQ)
pronostico_Q7 = forecast(modeloQ7, h = h_pronosticoQ)

tabla_con_errores = data.frame(
  Día = 1:10,
  Fecha = format(index(Prueba)[1:10], "%Y-%m-%d"),
  Real = round(as.numeric(Prueba)[1:10], 2),
  Pronóstico = round(as.numeric(pronostico_QA$mean)[1:10], 2),
  Error_USD = round(as.numeric(Prueba)[1:10] - 
                      as.numeric(pronostico_QA$mean)[1:10], 2),
  Error_Pct = round(((as.numeric(Prueba)[1:10] - 
                        as.numeric(pronostico_QA$mean)[1:10]) / 
                       as.numeric(Prueba)[1:10]) * 100, 3)
)

print(tabla_con_errores)


fechas_pronosticoQ = seq(as.Date("2025-10-01"), by = "day", length.out = h_pronosticoQ)

ggplot() +
  geom_line(data = data.frame(Fecha = index(Entrenamiento), 
                              Precio = as.numeric(Entrenamiento)),
            aes(x = Fecha, y = Precio), color = 'black', alpha = 0.6) +
  geom_line(data = data.frame(Fecha = index(Prueba), 
                              Precio = as.numeric(Prueba)),
            aes(x = Fecha, y = Precio), color = '#E74C3C', linewidth = 1.2) +
  geom_line(data = data.frame(Fecha = fechas_pronosticoQ,
                              Precio = as.numeric(pronostico_QA$mean)),
            aes(x = Fecha, y = Precio), color = '#3498DB', linewidth = 1) +
  labs(title = "QQQ: Pronóstico vs Real",
       subtitle = "Negro=Entrenamiento | Rojo=Real | Azul=Pronóstico ARIMA(2,1,2)",
       x = "Fecha", y = "Precio USD") +
  theme_minimal()

