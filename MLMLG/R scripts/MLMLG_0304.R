# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2024
# ==========================================================
#     Simulación de Insesgamiento Asintótico de los 
#    Estimadores en un Modelo de Regresión de Poisson
# ==========================================================

library(ggplot2)

# Función para simular datos y estimar los parámetros beta en un modelo de Poisson
simulate_bias_asymptotic_poisson <- function(n, beta0 = 0.5, beta1 = 1.5, num_sim = 1000) {
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

# Tamaños de muestra para la simulación
sample_sizes <- c(10, 50, 100, 500, 1000)

# Realizar simulación para diferentes tamaños de muestra
set.seed(123)
results <- data.frame()

for (n in sample_sizes) {
  sim_results <- simulate_bias_asymptotic_poisson(n, beta0_true, beta1_true)
  sim_results$sample_size <- n
  results <- rbind(results, sim_results)
}

# Graficar los resultados para beta0
ggplot(results, aes(x = factor(sample_size), y = beta0_hat)) +
  geom_boxplot() +
  geom_hline(yintercept = beta0_true, linetype = "dashed", color = "red") +
  labs(title = "Distribución de las Estimaciones de Beta0",
       x = "Tamaño de la Muestra",
       y = "Estimaciones de Beta0") +
  theme_minimal()

# Graficar los resultados para beta1
ggplot(results, aes(x = factor(sample_size), y = beta1_hat)) +
  geom_boxplot() +
  geom_hline(yintercept = beta1_true, linetype = "dashed", color = "red") +
  labs(title = "Distribución de las Estimaciones de Beta1",
       x = "Tamaño de la Muestra",
       y = "Estimaciones de Beta1") +
  theme_minimal()
