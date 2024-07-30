# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2024
# ==========================================================
#    Simulación de Consistencia de los Estimadores en un 
#           Modelo de Regresión de Poisson
# ==========================================================
# 
library(ggplot2)

# Función para simular datos y estimar el parámetro beta en un modelo de Poisson
simulacion_consistencia_poisson <- function(n, beta0 = 0.5, beta1 = 1.5) {
  x <- rnorm(n)
  lambda <- exp(beta0 + beta1 * x)
  y <- rpois(n, lambda)
  model <- glm(y ~ x, family = poisson(link = "log"))
  return(coef(model))
}

# Parámetros verdaderos
beta0_true <- 0.5
beta1_true <- 1.5

# Tamaños de muestra para la simulación
sample_sizes <- seq(10, 1000, by = 10)

# Realizar simulación
set.seed(123)
results <- data.frame(sample_size = integer(), 
                      beta0_hat = numeric(), beta1_hat = numeric())

for (n in sample_sizes) {
  coefs <- simulacion_consistencia_poisson(n, beta0_true, beta1_true)
  results <- rbind(results, 
      data.frame(sample_size = n, beta0_hat = coefs[1], beta1_hat = coefs[2]))
}

# Graficar los resultados
ggplot(results, aes(x = sample_size)) +
  geom_line(aes(y = beta0_hat, color = "beta0_hat")) +
  geom_hline(yintercept = beta0_true, linetype = "dashed", color = "blue") +
  geom_line(aes(y = beta1_hat, color = "beta1_hat")) +
  geom_hline(yintercept = beta1_true, linetype = "dashed", color = "red") +
  labs(title = "Consistencia de los Estimadores en un \n Modelo de Regresión de Poisson",
       x = "Tamaño de la Muestra",
       y = "Estimación del Parámetro",
       color = "Parámetro") +
  scale_color_manual(values = c("beta0_hat" = "blue", "beta1_hat" = "red"),
                     labels = c("beta0_hat", "beta1_hat")) +
  theme_minimal()

