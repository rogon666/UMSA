# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#            Machine Learning y Deep Learning
# ----------------------------------------------------------
#         Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#   Practica 2: comparacion de SVM con bosques aleatorios
# ==========================================================

# ----------------- Cargar las librerías necesarias ----------------------------
library(caret)
library(e1071)
library(randomForest)
library(readr)

# -------------------- Cargar el conjunto de datos -----------------------------
url <- 'https://raw.githubusercontent.com/rogon666/UMSA/main/AIMLDL/Datos/enfermedades_cardiacas.csv'
df <- read_csv(url)
# Mostrar las primeras filas del DataFrame
head(df)

# ----------------- Pre-procesamiento y preparación ----------------------------

# Imputar los valores faltantes:
df$talasemia[is.na(df$talasemia)] <- mean(df$talasemia, na.rm = TRUE)
df$numerovasos[is.na(df$numerovasos)] <- mean(df$numerovasos, na.rm = TRUE)

# Preparar los datos para el modelo:
X <- df[, !names(df) %in% c('diagnostico')]
y <- df$diagnostico
y <- ifelse(y == 0, 0, 1)

#------  Dividir el dataset en conjunto de entrenamiento y prueba --------------
semilla = 248
set.seed(semilla)
porcentaje_entrenamiento = 0.7 # 70% de los datos
trainIndex <- createDataPartition(y, 
                                  p = porcentaje_entrenamiento, 
                                  list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# --------------------------- Modelo de SVM ------------------------------------
modelo_svm <- svm(x = X_train, 
                  y = as.factor(y_train), 
                  kernel = "radial",
                  random_state = semilla,
                  probability = TRUE)

# Predecir usando el modelo de SVM
pred_svm <- predict(modelo_svm, X_test)
# Calcular exactitud (accuracy):
exactitud_svm <- sum(pred_svm == y_test) / length(y_test)

# ---------------------- Modelo de Random Forest -------------------------------
bosque_aleatorio <- randomForest(x = X_train, 
                                 ntree = 100,
                                 y = as.factor(y_train), 
                                 random_state = semilla)

# Predecir usando el modelo de Random Forest
pred_rf <- predict(bosque_aleatorio, X_test)
# Calcular exactitud (accuracy):
exactitud_rf <- sum(pred_rf == y_test) / length(y_test)


# --------------------- Comparar las exactitudes -------------------------------
exactitud_rf
exactitud_svm
# Mejor modelo: SVM
# ------------------------------------------------------------------------------