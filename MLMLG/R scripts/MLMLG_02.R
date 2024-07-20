# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2024
# ==========================================================

# Simulaciones de un modelo lineal multivariado

# Cargar librerías necesarias
library(ggplot2)
library(broom) # install.packages("broom")

# Configurar semilla para reproducibilidad
set.seed(1025)

# Generar datos simulados
n <- 100  # Número de observaciones
x1 <- rnorm(n, mean = 50, sd = 10)  # Variable predictora 1
x2 <- rnorm(n, mean = 30, sd = 5)   # Variable predictora 2

# Parámetros:
beta0 = 5
beta1 = 2
beta2 = 3
error = rnorm(n, mean = 0, sd = 1) # Distribucion Gaussiana (Normal) estandar

y <- beta0 + beta1*x1 + beta2*x2 + error
# y = b0 + b1*x1 + b2*x2 + e

# Graficar histogramas de x1, x2 e y
hist(x1)
hist(x2)
hist(y)

# Combinar x1, x2 e y en un data frame
datos <- data.frame(x1 = x1, x2 = x2, y = y)

# Ajustar el modelo de regresión lineal
modelo_multivariante <- lm(y ~ x1 + x2, data = datos)

# Resumir el modelo
summary(modelo_multivariante)

# Obtener estimaciones intervalicas de los parámetros del modelo
conf_intervals <- confint(modelo_multivariante)

# Mostrar las estimaciones intervalicas
print(conf_intervals)

# Extraer datos para las bandas de predicción
pred_data <- augment(modelo_multivariante, datos)

# Graficar los datos y la línea de regresión con fondo negro, línea azul clara, y puntos amarillos
ggplot(datos, aes(x = x1, y = y)) +
  geom_point(color = "yellow") +
  geom_smooth(method = "lm", aes(color = "x1"), se = TRUE) +
  labs(title = "Modelo de regresión lineal con dos variables predictoras",
       x = "Variable independiente x1",
       y = "Variable dependiente (y)") +
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid.major = element_line(color = "grey40"),
    panel.grid.minor = element_line(color = "grey20"),
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

ggplot(datos, aes(x = x2, y = y)) +
  geom_point(color = "yellow") +
  geom_smooth(method = "lm", aes(color = "x2"), se = TRUE) +
  labs(title = "Modelo de regresión lineal con dos variables predictoras",
       x = "Variable independiente x2",
       y = "Variable dependiente (y)") +
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid.major = element_line(color = "grey40"),
    panel.grid.minor = element_line(color = "grey20"),
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )
