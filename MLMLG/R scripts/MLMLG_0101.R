# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2024
# ==========================================================

# Simulaciones de un modelo lineal univariado 

# Libreria: 
library(ggplot2)

# Definiendo semilla:
set.seed(900)

# Genero datos artificiales aleatorios:
n <- 1000  # numero de observaciones
mu <- 50
sigma <- 10
x <- rnorm(n, mean = mu, sd = sigma)  # Predictor variable N(mu,sigma2)
hist(x)

# y = beta0 + beta1*x + e

errores = rnorm(n, mean = 0, sd = 15)  # Errores: e ~ N(0,15)
error_de_medicion = rnorm(n, mean = 0, sd = 1)

# Parametros poblacionales:
beta0 = 5
beta1 = 2

# y = beta0 + beta1*x + e

ym <- beta0 + beta1*x + errores
y <- ym + error_de_medicion

# Combinar los datos x,y en un dataframe:
datos <- data.frame(x = x, y = y)

# Ajuste de un modelo de regresion lineal con una sola variable explicativa:
modelo_lineal <- lm(y ~ x, data = datos)

# Resultados del modelo
summary(modelo_lineal)

# Grafico de datos:
ggplot(datos, aes(x = x, y = y)) +
  geom_point(color = "yellow") +
  geom_smooth(method = "lm", color = "lightblue") +
  labs(title = "Modelo de regresión lineal",
       x = "variable independiente (x)",
       y = "variable dependiente (y)") +
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid.major = element_line(color = "grey40"),
    panel.grid.minor = element_line(color = "grey20"),
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

# -----------------------------------------------------------------------------
