# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#            Machine Learning y Deep Learning
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
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

# -------------------- Exploracion de datos ------------------------------------
df %>%
  ggplot(aes(x = Date, y = )) +
  geom_line() +
  labs(title = "Bitcoin/USD", y = "precio de cierre (USD)", x = "") + 
  theme_tq()

df %>%
  ggplot(aes(x = Date, y = )) +
  geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) +
  labs(title = "Bitcoin/USD", y = "precio (USD)", x = "") +
  theme_tq()

df %>%
  tail(90) %>%
  ggplot(aes(x = Date, y = )) +
  geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) +
  labs(title = "Bitcoin/USD", y = "precios (USD)", x = "") +
  theme_tq()

df %>%
  tail() %>%
  ggplot(aes(x = ds, y = y, 
             open = Open, high = High, low = Low, close = y)) +
  geom_candlestick() +
  geom_bbands(ma_fun = SMA, sd = 2, n = 20) +
  labs(title = "Bitcoin/USD", 
       subtitle = "Bandas de Bollinger y MA",
       y = "precios", x = "") +
  theme_tq()

# -------------- Preparacion de datos y particion de datos ---------------------
# Renombrar las columnas para adaptarse a Prophet
df <- df %>% rename(ds = Date, y = Close)

# Dividir los datos en entrenamiento y prueba
tamano_muestra_entrenamiento =  # 80 %
  train_size <- floor( * nrow(df))
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
modelo_profeta <- prophet(train)

# ----------- Predicciones con el modelo Prophet -------------------------------
# Realizar predicciones
predicciones <- make_future_dataframe(modelo_profeta, periods = nrow(test))
pseudopronostico <- predict(modelo_profeta, predicciones)

# ------------------ Evaluacion del modelo -------------------------------------
BTC_pronosticado <- tail(pseudopronostico$yhat, nrow(test))
BTC_observado <- test$y

mae <- mean(abs(BTC_pronosticado - BTC_observado))
mse <- mean((BTC_pronosticado - BTC_observado)^2)

mae
mse

# Crear un dataframe para la comparación
comparison_df <- data.frame(
  Date = test$ds,
  BTCUSD_observado = BTC_observado,
  BTCUSD_pronosticado = BTC_pronosticado
)

# Graficar la comparación de observados vs pronosticados
ggplot(comparison_df, aes(x = Date)) +
  geom_line(aes(y = BTCUSD_observado, color = "BTC/USD observado")) +
  geom_line(aes(y = BTCUSD_pronosticado, color = "BTC/USD pseudopronosticado")) +
  labs(title = "Comparación de Valores Observados y Pronosticados",
       y = "Precio de Bitcoin (USD)", x = "") +
  theme_minimal() + 
  theme(
    legend.position = "bottom",          
    legend.title = element_blank()       
  )
# -----------------------------------------------------------------------------