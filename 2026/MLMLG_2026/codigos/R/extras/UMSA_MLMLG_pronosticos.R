# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2026
# ==========================================================
#   Pronostico automatico (S)AR(I)(S)MA y fan charts
# ----------------------------------------------------------

# Paquetes necesarios
if (!require(quantmod)) install.packages("quantmod")
if (!require(forecast)) install.packages("forecast")
if (!require(ggplot2)) install.packages("ggplot2")

library(quantmod)
library(forecast)
library(ggplot2)

# Descargando el Indice de Precios al Consumidor de EE.UU. (mensual) desde FRED
getSymbols("CPIAUCSL", src = "FRED")

# Convertimos a inflacion interanual (%)
cpi <- CPIAUCSL
inflacion <- diff(log(cpi), lag = 12) * 100  # variacion % interanual
inflacion <- na.omit(inflacion)

# Convertimos a serie de tiempo ts() para usar con forecast
serie <- ts(as.numeric(inflacion), start = c(1948, 1), frequency = 12)

# Nos quedamos con los ultimos años para ver mejor el detalle
serie <- window(serie, start = c(2015, 1))

# Ajustamos un modelo (S)AR(I)(S)MA automatico
modelo <- auto.arima(serie,
                     d = 1,      # orden de diferenciacion
                     max.p = 12, # orden AR no estacional maximo
                     max.q = 12,  # orden MA no estacional maximo
                     max.P = 12, # orden AR estacional maximo
                     max.Q = 12  # orden MA estacional maximo
                     )
summary(modelo)

# Horizonte de pronostico
hpron <- 24  # 24 meses = 2 años

# Metodo 1: forecast() - analitico, asume normalidad
f <- forecast(modelo, h = hpron, level = c(40,60,75,90,95))
plot(f)

# Metodo 2: simulacion - empirico, no asume forma funcional
set.seed(666)
sim <- matrix(NA, nrow = hpron, ncol = 1000)
for (i in 1:1000) sim[, i] <- simulate(modelo, nsim = hpron)

# Comparamos el intervalo 95% de ambos metodos en el horizonte hpron
forecast_95 <- c(f$lower[hpron, "95%"], f$upper[hpron, "95%"])
sim_95 <- quantile(sim[hpron, ], probs = c(0.025, 0.975))

cat("Intervalo forecast():", forecast_95, "\n")
cat("Intervalo simulado:  ", sim_95, "\n")

# Calculamos bandas (niveles simetricos, del centro hacia afuera)
niveles <- c(0.025, 0.10, 0.25)
periodos_fcst <- seq(as.numeric(time(serie))[length(serie)] + 1/12,
                     by = 1/12, length.out = hpron)

df_bandas <- do.call(rbind, lapply(niveles, function(p) {
  qs <- apply(sim, 1, quantile, probs = c(p, 1 - p))
  data.frame(tiempo = periodos_fcst, ymin = qs[1, ], ymax = qs[2, ], nivel = p)
}))

mediana <- apply(sim, 1, median)
df_mediana <- data.frame(tiempo = periodos_fcst, valor = mediana)

df_hist <- data.frame(tiempo = as.numeric(time(serie)), valor = as.numeric(serie))

# Punto de union para que el abanico nazca del ultimo dato historico
punto_union <- data.frame(tiempo = df_hist$tiempo[nrow(df_hist)],
                          valor = df_hist$valor[nrow(df_hist)])

# Grafico
ggplot() +
  annotate("rect", xmin = periodos_fcst[1], xmax = max(periodos_fcst),
           ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +
  geom_ribbon(data = df_bandas, aes(x = tiempo, ymin = ymin, ymax = ymax, group = nivel),
              fill = "orangered", alpha = 0.20) +
  geom_line(data = df_hist, aes(x = tiempo, y = valor), color = "black", linewidth = 0.8) +
  geom_line(data = rbind(punto_union, df_mediana),
            aes(x = tiempo, y = valor), color = "orangered", linewidth = 0.9) +
  labs(title = "Pronostico de Inflacion en EE.UU.",
       subtitle = "Fuente: FRED, serie CPIAUCSL | Modelo (S)AR(I)(S)MA",
       x = "Año", y = "% interanual") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10)
  )