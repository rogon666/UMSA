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
tamano_muestra_entrenamiento =   # 87.5%, hold-out en 2024 
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
modelo_profeta <- prophet(train,
                          n.changepoints = )
modelo_profeta_htun <- prophet(train,
                               n.changepoints = )

# ----------- Predicciones con el modelo Prophet -------------------------------
# Realizar predicciones
predicciones <- make_future_dataframe(modelo_profeta, periods = nrow(test))
pseudopronostico <- predict(modelo_profeta, predicciones)

predicciones_htun <- make_future_dataframe(modelo_profeta_htun, periods = nrow(test))
pseudopronostico_htun <- predict(modelo_profeta_htun, predicciones_htun)

# ------------------ Evaluacion del modelo -------------------------------------
BTC_observado <- test$y

BTC_pronosticado      <- tail(pseudopronostico$yhat, nrow(test))
mae <- mean(abs(BTC_pronosticado - BTC_observado)) # Error medio absoluto
mse <- mean((BTC_pronosticado - BTC_observado)^2)  # Error cuadratico medio (Promedio del error cuadratico)

BTC_pronosticado_htun <- tail(pseudopronostico_htun$yhat, nrow(test))
mae_htun <- mean(abs(BTC_pronosticado_htun - BTC_observado)) # Error medio absoluto
mse_htun <- mean((BTC_pronosticado_htun - BTC_observado)^2)  # Error cuadratico medio (Promedio del error cuadratico)

mae
mae_htun

mse
mse_htun

# Crear un dataframe para la comparación
comparison_df <- data.frame(
  Date = test$ds,
  BTCUSD_observado = BTC_observado,
  BTCUSD_pronosticado = BTC_pronosticado,
  BTCUSD_pronosticado_htun = BTC_pronosticado_htun
)

# Graficar la comparación de observados vs pronosticados
ggplot(comparison_df, aes(x = Date)) +
  geom_line(aes(y = BTCUSD_observado, color = "hold-out")) +
  geom_line(aes(y = BTCUSD_pronosticado, color = "pseudopronostico")) +
  geom_line(aes(y = BTCUSD_pronosticado_htun, color = "pseudopronostico htun")) +
  labs(title = "Comparación de Valores Observados y Pronosticados",
       y = "Precio de Bitcoin (USD)", x = "") +
  theme_minimal() + 
  theme(
    legend.position = "bottom",          
    legend.title = element_blank()       
  )
# -----------------------------------------------------------------------------