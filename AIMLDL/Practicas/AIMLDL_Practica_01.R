# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#            Machine Learning y Deep Learning
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#                Practica 1:
#   Machine learning aplicado a series de tiempo: Prophet
# ==========================================================
library(tidyquant)
library(dplyr)
library(prophet)
library(readr)

# ---------------------- Cargando datos ----------------------------------------
url <- "https://raw.githubusercontent.com/rogon666/UMSA/main/AIMLDL/Datos/BTCUSD.csv"
download.file(url, destfile = "bitcoin.csv")
df <- read.csv("bitcoin.csv")

# Convertir la columna de fecha a formato Date
df$Date <- as.Date(df$Date)

# -------------- Preparacion de datos y particion de datos ---------------------
# Renombrar las columnas para adaptarse a Prophet
df <- df %>% rename(ds = Date, y = Close)

# Dividir los datos en entrenamiento y prueba
tamano_muestra_entrenamiento = 0.875  # 87.5%, hold-out en 2024 
train_size <- floor(tamano_muestra_entrenamiento * nrow(df))
train <- df[1:train_size, ]
test <- df[(train_size + 1):nrow(df), ]

train %>%
  ggplot(aes(x = ds, y =y)) +
  geom_candlestick(aes(open = Open, high = High, low = Low, close = y)) +
  labs(title = "Bitcoin/USD muestra train", y = "precios", x = "") +
  theme_tq()

test %>%
  ggplot(aes(x = ds, y =y)) +
  geom_candlestick(aes(open = Open, high = High, low = Low, close = y)) +
  labs(title = "Bitcoin/USD muestra test", y = "precios", x = "") +
  theme_tq()

# ------------- Entrenamiento del modelo Prophet -------------------------------
# Entrenar el modelo Prophet
modelo_profeta_chp20 <- prophet(train,
                                n.changepoints = 20)
modelo_profeta_chp05 <- prophet(train,
                                n.changepoints = 5)

# ----------- Predicciones con el modelo Prophet -------------------------------
# Realizar predicciones
predicciones_chp20 <- make_future_dataframe(modelo_profeta_chp20, periods = nrow(test))
pseudopronostico_chp20 <- predict(modelo_profeta_chp20, predicciones_chp20)

predicciones_chp05 <- make_future_dataframe(modelo_profeta_chp05, periods = nrow(test))
pseudopronostico_chp05 <- predict(modelo_profeta_chp05, predicciones_chp05)

# ------------------ Evaluacion del modelo -------------------------------------
BTC_observado <- test$y

BTC_pronosticado_chp20      <- tail(pseudopronostico_chp20$yhat, nrow(test))
mae_chp20 <- mean(abs(BTC_pronosticado_chp20 - BTC_observado)) # Error medio absoluto
mse_chp20 <- mean((BTC_pronosticado_chp20 - BTC_observado)^2)  # Error cuadratico medio (Promedio del error cuadratico)

BTC_pronosticado_chp05 <- tail(pseudopronostico_chp05$yhat, nrow(test))
mae_chp05 <- mean(abs(BTC_pronosticado_chp05 - BTC_observado)) # Error medio absoluto
mse_chp05 <- mean((BTC_pronosticado_chp05 - BTC_observado)^2)  # Error cuadratico medio (Promedio del error cuadratico)

mae_chp05
mae_chp20

mse_chp05
mse_chp20

# Crear un dataframe para la comparación
comparison_df <- data.frame(
  Date = test$ds,
  BTCUSD_observado = BTC_observado,
  BTCUSD_pronosticado_p05 = BTC_pronosticado_chp05,
  BTCUSD_pronosticado_p20 = BTC_pronosticado_chp20 
)

# Graficar la comparación de observados vs pronosticados
ggplot(comparison_df, aes(x = Date)) +
  geom_line(aes(y = BTCUSD_observado, color = "hold-out")) +
  geom_line(aes(y = BTCUSD_pronosticado_p05, color = "pseudopronostico chp05")) +
  geom_line(aes(y = BTCUSD_pronosticado_p20, color = "pseudopronostico chp20")) +
  labs(title = "Comparación de Valores Observados y Pronosticados",
       y = "Precio de Bitcoin (USD)", x = "") +
  theme_minimal() + 
  theme(
    legend.position = "bottom",          
    legend.title = element_blank()       
  )

# Mejor modelo: chp05
# -----------------------------------------------------------------------------