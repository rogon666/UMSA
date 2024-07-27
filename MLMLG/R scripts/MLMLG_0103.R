# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2024
# ==========================================================

# Modelo de prediccion

# Cargar librerías necesarias
library(ggplot2)
library(caret)

# Configurar semilla para reproducibilidad
set.seed(1127)

# Generar datos simulados
n <- 100  # Tamaño de la muestra
mu = 5
sigma2 = 2
x <- rnorm(n, mean = mu, sd = sigma2)  # Variable independiente
# Parametros del modelo de regresion:
beta0 = 3
beta1 = 2
error = rnorm(n, sd = 2)
y <- beta0 + beta1*x + error

# Crear un data frame con los datos
data <- data.frame(x = x, y = y)

# Dividir la muestra en conjuntos de entrenamiento (70%) y prueba (30%)
trainIndex <- createDataPartition(data$y, p = 0.7, list = FALSE)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

# Ajustar el modelo de regresión lineal con los datos de entrenamiento
model <- lm(y ~ x, data = trainData)

# Resumen del modelo
summary(model)

# Realizar predicciones con el conjunto de prueba y calcular intervalos de predicción al 95%
predictions <- predict(model, newdata = testData, interval = "prediction", level = 0.95)

# Calcular el error cuadrático medio (MSE)
mse <- mean((testData$y - predictions[, "fit"])^2)
cat("Error Cuadrático Medio (MSE):", mse, "\n")

# Visualizar los resultados con intervalos de predicción en el conjunto de entrenamiento
ggplot(trainData, aes(x = x, y = y)) +
  geom_point(color = "blue") +
  geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2], color = "red") +
  geom_ribbon(aes(ymin = predict(model, newdata = trainData, interval = "prediction")[, "lwr"], 
                  ymax = predict(model, newdata = trainData, interval = "prediction")[, "upr"]), 
              alpha = 0.2, fill = "gray") +
  labs(title = "Modelo de Regresión Lineal con Intervalos de Predicción: Ajuste en la muestra de aprendizaje (train)",
       x = "Variable Independiente (x)",
       y = "Variable Dependiente (y)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "white")
  )

# Visualizar las predicciones con intervalos de predicción en el conjunto de prueba
ggplot(testData, aes(x = x, y = y)) +
  geom_point(color = "green") +
  geom_point(aes(y = predictions[, "fit"]), color = "red", shape = 1) +
  geom_errorbar(aes(ymin = predictions[, "lwr"], ymax = predictions[, "upr"]), color = "red", alpha = 0.5) +
  labs(title = "Predicciones del Modelo de Regresión Lineal con Intervalos de Predicción",
       x = "Variable Independiente (x)",
       y = "Variable Dependiente (y) / Predicciones") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "white")
  )
