# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2024
# ==========================================================
#    Simulación de Insesgamiento de los Estimadores en un 
#           Modelo de Regresión de Poisson
# ==========================================================

library(ggplot2)

# Función para simular datos y estimar los parámetros beta en un modelo de Poisson
simulacion_insesgamiento_poisson <- function(n, beta0 = 0.5, 
                                     beta1 = 1.5, 
                                     num_sim = 1000) {
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

# Tamaño de muestra
sample_size <- 100

# Realizar simulación
set.seed(123)
results <- simulacion_insesgamiento_poisson(sample_size, beta0_true, beta1_true)

# Graficar los resultados
ggplot(results, aes(x = beta0_hat)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = beta0_true, linetype = "dashed", color = "red") +
  labs(title = "Distribución de las Estimaciones de Beta0",
       x = "Estimaciones de Beta0",
       y = "Densidad") +
  theme_minimal()

ggplot(results, aes(x = beta1_hat)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = beta1_true, linetype = "dashed", color = "red") +
  labs(title = "Distribución de las Estimaciones de Beta1",
       x = "Estimaciones de Beta1",
       y = "Densidad") +
  theme_minimal()
