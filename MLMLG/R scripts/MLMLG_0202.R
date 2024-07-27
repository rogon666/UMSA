# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2024
# ==========================================================
# ~~~~~ Evaluación y Diagnóstico de Modelos Lineales ~~~~~~~
# ==========================================================
# Load necessary libraries
library(forecast)
library(tseries)
library(FinTS)
library(rugarch)
library(ggplot2)
library(strucchange)

# Cargando datos:
url = 'https://raw.githubusercontent.com/rogon666/UMSA/main/MLMLG/datos/tc_bsusd_blue.csv'
download.file(url, destfile = "tc_bsusd_blue.csv")
data <- read.csv("tc_bsusd_blue.csv")
data$fecha <- as.Date(data$fecha)

# Grafico historico del tipo de cambio
ggplot(data, aes(x = fecha, y = tcblue)) +
  geom_rect(aes(xmin = fecha[golpe == 1][1], xmax = fecha[golpe == 1][length(fecha[golpe == 1])],
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.5) +
  geom_line(color = "blue") +
  labs(title = "Tipo de cambio paralelo (blue) Bs/USD", x = "fecha", y = "Bs/USD")

# Convirtiendo los datos a un objeto de series de tiempo:
ts_tcblue <- ts(data$tcblue, frequency = 365)

# Variaciones del tipo de cambio:
diff_tcblue <- diff(ts_tcblue)
ggplot(data.frame(date = data$fecha[-1], diff_tcblue), aes(x = date, y = diff_tcblue)) +
  geom_line(color = "red") +
  labs(title = "Variaciones del tipo de cambio paralelo (blue) Bs/USD", 
       x = "fecha", y = "diferencias")

# Correlograma 
acf(diff_tcblue, main="ACF")
pacf(diff_tcblue, main="PACF")

# ---------------------------- Modelo ARIMA  -----------------------------------
# Estimando modelos ARIMA:
arimas <- auto.arima(ts_tcblue, d=1,max.p=7,max.q =7, 
                     stepwise = FALSE,
                     approximation = FALSE,
                     trace=TRUE)
summary(arimas)
# Hyndman, RJ and Khandakar, Y (2008) "Automatic time series forecasting: The forecast package for R", Journal of Statistical Software, 26(3).

# Estimacion en paralelo:
tiempo_estimacion<- system.time({
  auto.arima(ts_tcblue, d=1, max.p=7, max.q=7, 
             stepwise=FALSE, approximation=FALSE, 
             parallel=TRUE)
})
print(tiempo_estimacion)

arima <- arima(
  ts_tcblue, 
  order = c(1, 1, 0), 
  include.mean = TRUE)
summary(arima)

# Residuos modelo ARIMA:
residuos_arima <- residuals(arima)

# Test ARCH:
test_arch <- ArchTest(residuos_arima, lags=12)
print(test_arch)

# Correlograma de los residuos
acf(residuos_arima, main="ACF")
pacf(residuos_arima, main="PACF")

# Grafico de residuos:
plot(residuos_arima, main="Residuos ARIMA", ylab="Residuos")

# CUSUM:
cusum_test_arima <- efp(residuos_arima ~ 1, type="OLS-CUSUM")
plot(cusum_test_arima, main="Test CUSUM residuos ARIMA")

# --------------------------- Modelo GARCH -------------------------------------
# Modelo GARCH(1,1): 
spec_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(0,0)),
                         distribution.model = "norm")

ajuste_garch <- ugarchfit(spec = spec_garch, data = residuos_arima)
summary(ajuste_garch)

# Grafico GARCH:
plot(ajuste_garch, which="all")

# Volatilidad condicional GARCH:
volatilidad_garch <- sigma(ajuste_garch)

# Grafico Volatilidad condicional GARCH:
ggplot(data.frame(date = data$fecha, volatilidad_garch), 
       aes(x = date, y = volatilidad_garch)) +
  geom_line(color = "green") +
  labs(title = "Volatilidad condicional GARCH", x = "fecha", y = "volatilidad")

# Residuos  GARCH:
residuos_garch <- residuals(ajuste_garch, standardize = TRUE)

# CUSUM:
cusum_test_arima <- efp(residuos_garch~ 1, type="OLS-CUSUM")
plot(cusum_test_arima, main="Test CUSUM residuos GARCH")

# ------------------------------------------------------------------------------