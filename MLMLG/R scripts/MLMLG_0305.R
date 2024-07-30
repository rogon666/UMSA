# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2024
# ==========================================================
#  Simulación de la Distribución Asintótica Normal de los
#    Estimadores en un Modelo de Regresión de Poisson
# ==========================================================
library(ggplot2)

# Función para simular datos y estimar los parámetros beta en un modelo de Poisson
simulacion_distribucion_asintotica_poisson <- function(n, beta0 = 0.5, beta1 = 1.5, num_sim = 1000) {
  beta0_hats <- numeric(num_sim)
  beta1_hats <- numeric(num_sim)
  
  for (i in 1:num_sim) {
    x <- rnorm(n)
    lambda <- exp(beta0 + beta1 * x)
    y <- rpois(n, lambda)
    model <- glm(y ~ x, family = poisson(link = "log"))
    coefs <- coef(model)
    beta0_hats[i] = coefs[1]
    beta1_hats[i] = coefs[2]
  }
  
  data.frame(beta0_hat = beta0_hats, beta1_hat = beta1_hats)
}

# Parámetros verdaderos
beta0_true <- 0.5
beta1_true <- 1.5

# Tamaño de muestra grande para observar la normalidad asintótica
sample_size <- 1000

# Realizar simulación
set.seed(123)
results <- simulacion_distribucion_asintotica_poisson(sample_size, 
                                                      beta0_true, 
                                                      beta1_true)

# Graficar los resultados para beta0
ggplot(results, aes(x = beta0_hat)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "blue", linewidth = 1) +
  geom_vline(xintercept = beta0_true, linetype = "dashed", color = "red") +
  stat_function(fun = dnorm, args = list(mean = mean(results$beta0_hat), sd = sd(results$beta0_hat)), 
                color = "black", linetype = "dotted", size = 1) +
  labs(title = "Distribución Asintótica de las Estimaciones de Beta0",
       x = "Estimaciones de Beta0",
       y = "Densidad") +
  theme_minimal()

# Graficar los resultados para beta1
ggplot(results, aes(x = beta1_hat)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "blue", linewidth = 1) +
  geom_vline(xintercept = beta1_true, linetype = "dashed", color = "red") +
  stat_function(fun = dnorm, args = list(mean = mean(results$beta1_hat), sd = sd(results$beta1_hat)), 
                color = "black", linetype = "dotted", size = 1) +
  labs(title = "Distribución Asintótica de las Estimaciones de Beta1",
       x = "Estimaciones de Beta1",
       y = "Densidad") +
  theme_minimal()
