library(tidyverse)
library(quantmod)
library(xts)
library(fpp2)
library(tseries)
library(scales)
library(knitr)
library(kableExtra)
library(gridExtra)
library(grid)

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


df_diff <- data.frame(
  Fecha = index(dif_Entrenamiento),
  Valor = as.numeric(dif_Entrenamiento)
)

ggplot(df_diff, aes(x = Fecha, y = Valor)) +
  geom_line(color = qqq_pal$secondary, linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = qqq_pal$primary, linewidth = 0.7) +
  annotate("label", 
           x = as.Date("2023-06-01"), 
           y = max(df_diff$Valor) * 0.85,
           label = paste0("Media ≈ ", round(mean(df_diff$Valor), 3)),
           fill = qqq_pal$primary, color = "white",
           fontface = "bold", size = 3.5, label.padding = unit(0.4, "lines")) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.02, 0.03))) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$"),
                     expand = expansion(mult = c(0.05, 0.08))) +
  labs(
    title = "Serie Diferenciada de Primer Orden (d = 1)",
    subtitle = "QQQ: Cambios diarios en precio de cierre | Datos de entrenamiento",
    x = NULL,
    y = "Cambio Diario (USD)",
    caption = paste0("Observaciones: ", nrow(df_diff), 
                     " | Período: ", min(df_diff$Fecha), " a ", max(df_diff$Fecha))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    
    plot.title = element_text(face = "bold", size = 14, color = qqq_pal$text_dark,
                              margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, color = qqq_pal$secondary,
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


adf.test(dif_Entrenamiento)
ggAcf(dif_Entrenamiento)


# GRÁFICO ACF - SERIE DIFERENCIADA

acf_diff_data <- acf(dif_Entrenamiento, lag.max = 30, plot = FALSE)

df_acf_diff <- data.frame(
  Lag = acf_diff_data$lag[-1],
  ACF = acf_diff_data$acf[-1]
)

n_diff <- length(dif_Entrenamiento)
limite_sup_diff <- qnorm(0.975) / sqrt(n_diff)
limite_inf_diff <- -limite_sup_diff

ggplot(df_acf_diff, aes(x = Lag, y = ACF)) +
  geom_segment(aes(xend = Lag, yend = 0), 
               color = qqq_pal$secondary, linewidth = 0.8) +
  geom_point(color = qqq_pal$secondary, size = 2) +
  geom_hline(yintercept = limite_sup_diff, linetype = "dashed", 
             color = qqq_pal$primary, linewidth = 0.7) +
  geom_hline(yintercept = limite_inf_diff, linetype = "dashed", 
             color = qqq_pal$primary, linewidth = 0.7) +
  geom_hline(yintercept = 0, color = qqq_pal$text_gray, linewidth = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = limite_inf_diff, ymax = limite_sup_diff,
           fill = qqq_pal$primary, alpha = 0.1) +
  annotate("label", x = 22, y = 0.12,
           label = "Autocorrelaciones dentro\nde bandas → Estacionaria ✓",
           fill = qqq_pal$positive, color = "white",
           fontface = "bold", size = 3.5, label.padding = unit(0.5, "lines")) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(limits = c(-0.15, 0.2), breaks = seq(-0.1, 0.2, 0.05)) +
  labs(
    title = "Función de Autocorrelación (ACF) - Serie Diferenciada",
    subtitle = "QQQ: Cambios diarios | Verificación de estacionariedad post-diferenciación",
    x = "Rezago (Lag)",
    y = "Autocorrelación",
    caption = "Bandas verdes: Límites de significancia al 95% | La mayoría de rezagos dentro de bandas"
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

# TABLA ADF - SERIE DIFERENCIADA

adf_diff_resultado <- adf.test(dif_Entrenamiento)

tabla_adf_diff <- data.frame(
  Métrica = c("Estadístico Dickey-Fuller", 
              "Orden de Rezagos (Lag)", 
              "P-valor",
              "Nivel de Significancia (α)",
              "Hipótesis Nula (H₀)",
              "Decisión"),
  Valor = c(round(adf_diff_resultado$statistic, 4),
            adf_diff_resultado$parameter,
            round(adf_diff_resultado$p.value, 4),
            "0.05",
            "Serie tiene raíz unitaria",
            ifelse(adf_diff_resultado$p.value < 0.05, 
                   "Rechazar H₀", "No rechazar H₀")),
  Interpretación = c("Valor del estadístico de prueba",
                     "Rezagos incluidos en el test",
                     "Probabilidad bajo H₀",
                     "Umbral de decisión",
                     "La serie NO es estacionaria",
                     ifelse(adf_diff_resultado$p.value < 0.05,
                            "Serie ES estacionaria ✓",
                            "Serie NO estacionaria"))
)

kable(tabla_adf_diff, 
      caption = "Prueba de Dickey-Fuller Aumentada (ADF) - Serie Diferenciada (d=1)",
      align = c("l", "c", "l")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  row_spec(3, bold = TRUE, color = qqq_pal$positive) %>%
  row_spec(6, bold = TRUE, background = "#d4edda")



grid.arrange(ggAcf(dif_Entrenamiento),
             ggPacf(dif_Entrenamiento),
             nrow=1)

# GRÁFICO DUAL: ACF Y PACF - SERIE DIFERENCIADA

acf_data <- acf(dif_Entrenamiento, lag.max = 28, plot = FALSE)
pacf_data <- pacf(dif_Entrenamiento, lag.max = 28, plot = FALSE)

df_acf <- data.frame(
  Lag = as.numeric(acf_data$lag[-1]),
  Valor = as.numeric(acf_data$acf[-1])
)

df_pacf <- data.frame(
  Lag = as.numeric(pacf_data$lag),
  Valor = as.numeric(pacf_data$acf)
)

n <- length(dif_Entrenamiento)
limite <- qnorm(0.975) / sqrt(n)

p_acf <- ggplot(df_acf, aes(x = Lag, y = Valor)) +
  geom_hline(yintercept = 0, color = qqq_pal$text_gray, linewidth = 0.5) +
  geom_hline(yintercept = c(-limite, limite), linetype = "dashed", 
             color = qqq_pal$secondary, linewidth = 0.6) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -limite, ymax = limite,
           fill = qqq_pal$secondary, alpha = 0.08) +
  geom_segment(aes(xend = Lag, yend = 0), color = qqq_pal$primary, linewidth = 0.7) +
  geom_point(color = qqq_pal$primary, size = 1.5) +
  scale_x_continuous(breaks = seq(0, 28, 5)) +
  scale_y_continuous(limits = c(-0.15, 0.12)) +
  labs(
    title = "ACF - Serie Diferenciada",
    subtitle = "Identificación del orden q (MA)",
    x = "Rezago (Lag)",
    y = "ACF"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(face = "bold", size = 12, color = qqq_pal$text_dark),
    plot.subtitle = element_text(size = 9, color = qqq_pal$secondary),
    axis.title = element_text(size = 9, color = qqq_pal$text_gray),
    axis.text = element_text(size = 8, color = qqq_pal$text_gray),
    panel.grid.major = element_line(color = qqq_pal$grid, linetype = "dashed", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )

p_pacf <- ggplot(df_pacf, aes(x = Lag, y = Valor)) +
  geom_hline(yintercept = 0, color = qqq_pal$text_gray, linewidth = 0.5) +
  geom_hline(yintercept = c(-limite, limite), linetype = "dashed", 
             color = qqq_pal$secondary, linewidth = 0.6) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -limite, ymax = limite,
           fill = qqq_pal$secondary, alpha = 0.08) +
  geom_segment(aes(xend = Lag, yend = 0), color = qqq_pal$primary, linewidth = 0.7) +
  geom_point(color = qqq_pal$primary, size = 1.5) +
  scale_x_continuous(breaks = seq(0, 28, 5)) +
  scale_y_continuous(limits = c(-0.15, 0.12)) +
  labs(
    title = "PACF - Serie Diferenciada",
    subtitle = "Identificación del orden p (AR)",
    x = "Rezago (Lag)",
    y = "PACF"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(face = "bold", size = 12, color = qqq_pal$text_dark),
    plot.subtitle = element_text(size = 9, color = qqq_pal$secondary),
    axis.title = element_text(size = 9, color = qqq_pal$text_gray),
    axis.text = element_text(size = 8, color = qqq_pal$text_gray),
    panel.grid.major = element_line(color = qqq_pal$grid, linetype = "dashed", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )

grid.arrange(
  p_acf, p_pacf,
  ncol = 2,
  top = textGrob("Análisis de Correlogramas para Identificación de Parámetros ARIMA", 
                 gp = gpar(fontsize = 13, fontface = "bold", col = qqq_pal$text_dark))
)



ModeloQA=auto.arima(Entrenamiento)

modeloQ1 = Arima(Entrenamiento, order = c(3,1,3))
modeloQ2 = Arima(Entrenamiento, order = c(0,1,0))
modeloQ3 = Arima(Entrenamiento, order = c(2,1,1))
modeloQ4 = Arima(Entrenamiento, order = c(1,1,2))
modeloQ5 = Arima(Entrenamiento, order = c(2,1,2))


tabla = data.frame(
  Modelo = c('Auto-ARIMA', 'ARIMA(3,1,3)', 'ARIMA(0,1,0)', 'ARIMA(2,1,1)', 'ARIMA(1,1,2)', 'ARIMA(2,1,2)'),
  AICc = round(c(ModeloQA$aicc, modeloQ1$aicc, modeloQ2$aicc, modeloQ3$aicc, modeloQ4$aicc, modeloQ5$aicc), 2)
) %>% arrange(AICc)

print(tabla)

# MODELOS CANDIDATOS Y JUSTIFICACIÓN

tabla_candidatos <- data.frame(
  Modelo = c("ARIMA(0,1,0)", 
             "ARIMA(1,1,1)",
             "ARIMA(2,1,1)", 
             "ARIMA(1,1,2)",
             "ARIMA(2,1,2)",
             "ARIMA(3,1,3)"),
  Tipo = c("Random Walk",
           "auto.arima()",
           "Manual",
           "Manual",
           "Manual",
           "Exploratorio"),
  `Observación ACF/PACF` = c(
    "Patrón general cercano a ruido blanco",
    "Selección automática por AICc",
    "Posible estructura en lags 1-2 del PACF",
    "Posible estructura en lags 1-2 del ACF",
    "Combinación de estructuras en ambos correlogramas",
    "Pico marginal en lag 3 de ambos correlogramas"
  ),
  Justificación = c(
    "Benchmark obligatorio: hipótesis de mercado eficiente",
    "Referencia algorítmica para validar selección manual",
    "Extensión AR(2) para capturar persistencia de corto plazo",
    "Extensión MA(2) para capturar estructura de media móvil",
    "Modelo simétrico que combina dinámicas AR y MA",
    "Evaluar si rezagos marginales aportan capacidad predictiva"
  )
)

kable(tabla_candidatos,
      caption = "Modelos ARIMA Candidatos para Evaluación",
      align = c("l", "c", "l", "l"),
      col.names = c("Modelo", "Tipo", "Observación en ACF/PACF", "Justificación")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  column_spec(1, bold = TRUE, color = qqq_pal$primary) %>%
  column_spec(2, color = qqq_pal$secondary) %>%
  column_spec(3, width = "22em") %>%
  column_spec(4, width = "25em") %>%
  row_spec(2, background = "#e8f5e9")

ModeloQA
modeloQ1
modeloQ2 
modeloQ3
modeloQ4
modeloQ5

accuracy(ModeloQA)
accuracy(modeloQ1)
accuracy(modeloQ2)
accuracy(modeloQ3)
accuracy(modeloQ4)
accuracy(modeloQ5)

# TABLA: RESUMEN DE COEFICIENTES POR MODELO 

extraer_coef <- function(modelo, nombre) {
  coefs <- coef(modelo)
  if (nombre %in% names(coefs)) {
    return(round(coefs[nombre], 4))
  } else {
    return("—")
  }
}

tabla_coeficientes <- data.frame(
  Modelo = c("ARIMA(0,1,0)", 
             "ARIMA(1,1,1) + drift", 
             "ARIMA(2,1,1)", 
             "ARIMA(1,1,2)",
             "ARIMA(2,1,2)",
             "ARIMA(3,1,3)"),
  ar1 = c("—", 
          extraer_coef(ModeloQA, "ar1"), 
          extraer_coef(modeloQ3, "ar1"), 
          extraer_coef(modeloQ4, "ar1"),
          extraer_coef(modeloQ5, "ar1"),
          extraer_coef(modeloQ1, "ar1")),
  ar2 = c("—", 
          extraer_coef(ModeloQA, "ar2"), 
          extraer_coef(modeloQ3, "ar2"), 
          extraer_coef(modeloQ4, "ar2"),
          extraer_coef(modeloQ5, "ar2"),
          extraer_coef(modeloQ1, "ar2")),
  ar3 = c("—", 
          extraer_coef(ModeloQA, "ar3"), 
          extraer_coef(modeloQ3, "ar3"), 
          extraer_coef(modeloQ4, "ar3"),
          extraer_coef(modeloQ5, "ar3"),
          extraer_coef(modeloQ1, "ar3")),
  ma1 = c("—", 
          extraer_coef(ModeloQA, "ma1"), 
          extraer_coef(modeloQ3, "ma1"), 
          extraer_coef(modeloQ4, "ma1"),
          extraer_coef(modeloQ5, "ma1"),
          extraer_coef(modeloQ1, "ma1")),
  ma2 = c("—", 
          extraer_coef(ModeloQA, "ma2"), 
          extraer_coef(modeloQ3, "ma2"), 
          extraer_coef(modeloQ4, "ma2"),
          extraer_coef(modeloQ5, "ma2"),
          extraer_coef(modeloQ1, "ma2")),
  ma3 = c("—", 
          extraer_coef(ModeloQA, "ma3"), 
          extraer_coef(modeloQ3, "ma3"), 
          extraer_coef(modeloQ4, "ma3"),
          extraer_coef(modeloQ5, "ma3"),
          extraer_coef(modeloQ1, "ma3")),
  drift = c("—", 
            extraer_coef(ModeloQA, "drift"), 
            extraer_coef(modeloQ3, "drift"), 
            extraer_coef(modeloQ4, "drift"),
            extraer_coef(modeloQ5, "drift"),
            extraer_coef(modeloQ1, "drift")),
  sigma2 = c(round(modeloQ2$sigma2, 2),
             round(ModeloQA$sigma2, 2),
             round(modeloQ3$sigma2, 2),
             round(modeloQ4$sigma2, 2),
             round(modeloQ5$sigma2, 2),
             round(modeloQ1$sigma2, 2))
)

kable(tabla_coeficientes,
      caption = "Coeficientes Estimados por Modelo ARIMA",
      align = c("l", rep("c", 8)),
      col.names = c("Modelo", "AR(1)", "AR(2)", "AR(3)", "MA(1)", "MA(2)", "MA(3)", "Drift", "σ²")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  column_spec(1, bold = TRUE, color = qqq_pal$primary) %>%
  row_spec(2, background = "#e8f5e9") %>%
  footnote(general = "σ² = Varianza de los residuos. El símbolo '—' indica que el parámetro no aplica al modelo.",
           general_title = "Nota: ")


# TABLA: CRITERIOS DE INFORMACIÓN 

comparacion_IC <- data.frame(
  Modelo = c("ARIMA(0,1,0)", 
             "ARIMA(1,1,1) + drift", 
             "ARIMA(2,1,1)", 
             "ARIMA(1,1,2)",
             "ARIMA(2,1,2)",
             "ARIMA(3,1,3)"),
  Parametros = c(length(coef(modeloQ2)) + 1,  # +1 por sigma2
                 length(coef(ModeloQA)) + 1,
                 length(coef(modeloQ3)) + 1,
                 length(coef(modeloQ4)) + 1,
                 length(coef(modeloQ5)) + 1,
                 length(coef(modeloQ1)) + 1),
  AIC = round(c(AIC(modeloQ2), 
                AIC(ModeloQA), 
                AIC(modeloQ3), 
                AIC(modeloQ4),
                AIC(modeloQ5),
                AIC(modeloQ1)), 2),
  AICc = round(c(modeloQ2$aicc, 
                 ModeloQA$aicc, 
                 modeloQ3$aicc, 
                 modeloQ4$aicc,
                 modeloQ5$aicc,
                 modeloQ1$aicc), 2),
  BIC = round(c(BIC(modeloQ2), 
                BIC(ModeloQA), 
                BIC(modeloQ3), 
                BIC(modeloQ4),
                BIC(modeloQ5),
                BIC(modeloQ1)), 2)
)

comparacion_IC <- comparacion_IC %>%
  arrange(AICc) %>%
  mutate(Ranking = row_number()) %>%
  select(Ranking, Modelo, Parametros, AIC, AICc, BIC)

kable(comparacion_IC,
      caption = "Comparación de Modelos por Criterios de Información",
      align = c("c", "l", "c", "c", "c", "c"),
      col.names = c("Ranking", "Modelo", "# Parámetros", "AIC", "AICc", "BIC")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  column_spec(2, bold = TRUE, color = qqq_pal$primary) %>%
  column_spec(5, bold = TRUE) %>%
  row_spec(1, bold = TRUE, background = "#d4edda") %>%
  footnote(general = "Ordenado por AICc (menor es mejor). AICc es el criterio preferido para muestras finitas.",
           general_title = "Nota: ")

# TABLA: MÉTRICAS DE PRECISIÓN (DINÁMICO)

acc_QA <- accuracy(ModeloQA)
acc_Q1 <- accuracy(modeloQ1)
acc_Q2 <- accuracy(modeloQ2)
acc_Q3 <- accuracy(modeloQ3)
acc_Q4 <- accuracy(modeloQ4)
acc_Q5 <- accuracy(modeloQ5)

comparacion_accuracy <- data.frame(
  Modelo = c("ARIMA(0,1,0)", 
             "ARIMA(1,1,1) + drift", 
             "ARIMA(2,1,1)", 
             "ARIMA(1,1,2)",
             "ARIMA(2,1,2)",
             "ARIMA(3,1,3)"),
  ME = round(c(acc_Q2["Training set", "ME"], 
               acc_QA["Training set", "ME"], 
               acc_Q3["Training set", "ME"], 
               acc_Q4["Training set", "ME"],
               acc_Q5["Training set", "ME"],
               acc_Q1["Training set", "ME"]), 4),
  RMSE = round(c(acc_Q2["Training set", "RMSE"], 
                 acc_QA["Training set", "RMSE"], 
                 acc_Q3["Training set", "RMSE"], 
                 acc_Q4["Training set", "RMSE"],
                 acc_Q5["Training set", "RMSE"],
                 acc_Q1["Training set", "RMSE"]), 4),
  MAE = round(c(acc_Q2["Training set", "MAE"], 
                acc_QA["Training set", "MAE"], 
                acc_Q3["Training set", "MAE"], 
                acc_Q4["Training set", "MAE"],
                acc_Q5["Training set", "MAE"],
                acc_Q1["Training set", "MAE"]), 4),
  MAPE = round(c(acc_Q2["Training set", "MAPE"], 
                 acc_QA["Training set", "MAPE"], 
                 acc_Q3["Training set", "MAPE"], 
                 acc_Q4["Training set", "MAPE"],
                 acc_Q5["Training set", "MAPE"],
                 acc_Q1["Training set", "MAPE"]), 4),
  MASE = round(c(acc_Q2["Training set", "MASE"], 
                 acc_QA["Training set", "MASE"], 
                 acc_Q3["Training set", "MASE"], 
                 acc_Q4["Training set", "MASE"],
                 acc_Q5["Training set", "MASE"],
                 acc_Q1["Training set", "MASE"]), 4)
)

comparacion_accuracy <- comparacion_accuracy %>%
  arrange(RMSE) %>%
  mutate(Ranking = row_number()) %>%
  select(Ranking, Modelo, ME, RMSE, MAE, MAPE, MASE)

kable(comparacion_accuracy,
      caption = "Métricas de Precisión sobre Datos de Entrenamiento",
      align = c("c", "l", rep("c", 5))) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center") %>%
  column_spec(2, bold = TRUE, color = qqq_pal$primary) %>%
  column_spec(4, bold = TRUE) %>%
  row_spec(1, background = "#d4edda") %>%
  footnote(general = "ME: Error Medio | RMSE: Raíz del Error Cuadrático Medio | MAE: Error Absoluto Medio | MAPE: Error Porcentual (%) | MASE: Error Escalado",
           general_title = "Métricas: ")


checkresiduals(ModeloQA) 
checkresiduals(modeloQ1) 
checkresiduals(modeloQ2) 
checkresiduals(modeloQ3) 
checkresiduals(modeloQ4) 
checkresiduals(modeloQ5) 

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

