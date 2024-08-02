# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#         Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#  Modelo de Regresión Logistico vs. Gompit and Probit
# ==========================================================
library(ggplot2)

# Generar datos de ejemplo
set.seed(123)
n <- 200
x1 <- rnorm(n)
x2 <- rnorm(n)
beta0 <- -1
beta1 <- 2
beta2 <- -1.5
logit_p <- beta0 + beta1 * x1 + beta2 * x2
p <- 1 / (1 + exp(-logit_p))
y <- rbinom(n, 1, p)

data <- data.frame(y, x1, x2)

# Ajustar modelos
modelo_logit <- glm(y ~ x1 + x2, 
                    family = binomial(link = "logit"), data = data)
modelo_probit <- glm(y ~ x1 + x2, 
                     family = binomial(link = "probit"), data = data)
modelo_gompit <- glm(y ~ x1 + x2, 
                     family = binomial(link = "cloglog"), data = data) # cloglog es equivalente a gompit

# Resumen de los modelos
summary(modelo_logit)
summary(modelo_probit)
summary(modelo_gompit)

# Predicciones
data$pred_logit <- predict(modelo_logit, type = "response")
data$pred_probit <- predict(modelo_probit, type = "response")
data$pred_gompit <- predict(modelo_gompit, type = "response")

# Graficar los resultados
ggplot(data, aes(x = x1, y = pred_logit)) +
  geom_point(aes(y = y), alpha = 0.5) +
  geom_line(color = "blue") +
  labs(title = "Predicción del Modelo Logit",
       x = "x1",
       y = "Probabilidad Predicha") +
  theme_minimal()

ggplot(data, aes(x = x1, y = pred_probit)) +
  geom_point(aes(y = y), alpha = 0.5) +
  geom_line(color = "red") +
  labs(title = "Predicción del Modelo Probit",
       x = "x1",
       y = "Probabilidad Predicha") +
  theme_minimal()

ggplot(data, aes(x = x1, y = pred_gompit)) +
  geom_point(aes(y = y), alpha = 0.5) +
  geom_line(color = "green") +
  labs(title = "Predicción del Modelo Gompit",
       x = "x1",
       y = "Probabilidad Predicha") +
  theme_minimal()

# Crear un rango de valores para x1
x1_range <- seq(min(x1), max(x1), length.out = 100)
x2_mean <- mean(x2)  # Mantener x2 constante en su media

# Crear un nuevo data frame para predicciones
new_data <- data.frame(x1 = x1_range, x2 = x2_mean)

# Obtener predicciones de probabilidad para cada modelo
new_data$pred_logit <- predict(modelo_logit, newdata = new_data, type = "response")
new_data$pred_probit <- predict(modelo_probit, newdata = new_data, type = "response")
new_data$pred_gompit <- predict(modelo_gompit, newdata = new_data, type = "response")

# Graficar las funciones sigmoidales
ggplot(new_data, aes(x = x1)) +
  geom_line(aes(y = pred_logit, color = "Logit"), linewidth = 1) +
  geom_line(aes(y = pred_probit, color = "Probit"), linewidth = 1) +
  geom_line(aes(y = pred_gompit, color = "Gompit"), linewidth = 1) +
  labs(title = "Funciones Sigmoidales: Logit, Probit y Gompit",
       x = "x1",
       y = "Probabilidad Predicha",
       color = "Modelo") +
  theme_minimal()

# El gráfico resultante mostrará las funciones sigmoidales correspondientes a 
# los modelos logit, probit y gompit en función de la variable predictora x1
# Cada curva representará cómo cambia la probabilidad predicha de éxito a medida 
# que cambia x1, manteniendo x2 constante en su media.

# Curva Roja: La función gompit (o cloglog) utiliza la función de distribución 
# acumulativa de Gumbel inversa para transformar la combinación lineal en 
# probabilidades. Esta función es asimétrica y tiene una pendiente más pronunciada
# en los extremos bajos y más suave en los altos.
# Interpretación: La regresión gompit es útil en análisis de supervivencia y 
# eventos extremos. La forma asimétrica de la curva hace que las probabilidades 
# aumenten rápidamente desde valores bajos y luego se suavicen a medida que se 
# acercan a 1. Es preferida en situaciones donde los eventos de interés ocurren 
# de manera abrupta al principio y se estabilizan más tarde.