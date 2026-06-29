# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#         Metodos Bayesianos: Gibbs sampling
# ==========================================================
# Datos simulados
set.seed(666)
n <- 100
x <- rnorm(n, 0, 1)
beta_0_true <- 
beta_1_true <- 
sigma <- 1
y <- beta_0_true + beta_1_true * x + rnorm(n, 0, sigma)

# Inicializar valores
beta_0 <- 0
beta_1 <- 0
n_samples <- 1000

# Almacenar muestras
beta_0_samples <- numeric(n_samples)
beta_1_samples <- numeric(n_samples)

# Parámetros de la prior
mu_0 <- 0
tau_0 <- 10
mu_1 <- 0
tau_1 <- 10

# Gibbs Sampling
for (t in 1:n_samples) {
  # Muestrear beta_0 dado beta_1, y, x
  var_beta_0 <- 1 / (n / sigma^2 + 1 / tau_0^2)
  mean_beta_0 <- var_beta_0 * (sum(y - beta_1 * x) / sigma^2 + mu_0 / tau_0^2)
  beta_0 <- rnorm(1, mean_beta_0, sqrt(var_beta_0))
  
  # Muestrear beta_1 dado beta_0, y, x
  var_beta_1 <- 1 / (sum(x^2) / sigma^2 + 1 / tau_1^2)
  mean_beta_1 <- var_beta_1 * (sum((y - beta_0) * x) / sigma^2 + mu_1 / tau_1^2)
  beta_1 <- rnorm(1, mean_beta_1, sqrt(var_beta_1))
  
  # Almacenar muestras
  beta_0_samples[t] <- beta_0
  beta_1_samples[t] <- beta_1
}

# Graficar las cadenas
par(mfrow = c(2, 1))
plot(beta_0_samples, type = 'l', main = expression(beta[0]), xlab = 'Iteration', ylab = 'Value')
plot(beta_1_samples, type = 'l', main = expression(beta[1]), xlab = 'Iteration', ylab = 'Value')

# Histograma de las muestras
par(mfrow = c(1, 2))
hist(beta_0_samples, breaks = 30, main = expression(beta[0]), xlab = 'Value', probability = TRUE)
hist(beta_1_samples, breaks = 30, main = expression(beta[1]), xlab = 'Value', probability = TRUE)

# Diagrama de contorno
library(MASS)
kde <- kde2d(beta_0_samples, beta_1_samples, n = 50)
contour(kde, main = "Contour Plot of Beta_0 and Beta_1", xlab = expression(beta[0]), ylab = expression(beta[1]))
points(beta_0_samples, beta_1_samples, pch = 20, cex = 0.1, col = rgb(0, 0, 1, 0.3))
