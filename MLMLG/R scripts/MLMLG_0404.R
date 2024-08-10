# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
# Metodos Bayesianos: algoritmo Metropolis-Hastings para MLG
# ==========================================================
# Cargar librerías necesarias
library(MASS)    # Para mvrnorm
library(ggplot2) # Para visualización

# Datos simulados
set.seed(666)
n <- 100
p <- 2
X <- cbind(1, matrix(rnorm(n * p), n, p)) # Matriz de diseño con intercepto
beta_true <- c(-1, 2, -1)
logit <- X %*% beta_true
prob <- 1 / (1 + exp(-logit))
y <- rbinom(n, 1, prob)

# Función de log-verosimilitud
log_likelihood <- function(beta, X, y) {
  logit <- X %*% beta
  ll <- sum(y * log(1 / (1 + exp(-logit))) + (1 - y) * log(1 - 1 / (1 + exp(-logit))))
  return(ll)
}

# Algoritmo de Metropolis-Hastings
metropolis_hastings <- function(initial_beta, X, y, n_samples, proposal_sd) {
  p <- length(initial_beta)
  beta_samples <- matrix(0, n_samples, p)
  beta_samples[1, ] <- initial_beta
  current_beta <- initial_beta
  
  for (t in 2:n_samples) {
    proposal_beta <- mvrnorm(1, current_beta, diag(proposal_sd, p))
    log_alpha <- log_likelihood(proposal_beta, X, y) - log_likelihood(current_beta, X, y)
    
    if (log(runif(1)) < log_alpha) {
      current_beta <- proposal_beta
    }
    
    beta_samples[t, ] <- current_beta
  }
  
  return(beta_samples)
}

# Parámetros del algoritmo
initial_beta <- rep(0, ncol(X))
n_samples <- 10000
proposal_sd <- 0.1

# Ejecutar el algoritmo
set.seed()
beta_samples <- metropolis_hastings(initial_beta, X, y, n_samples, proposal_sd)

# Convertir las muestras a data frames
samples_df <- data.frame(iteration = 1:n_samples, beta_samples)
samples_melted <- melt(samples_df, id.vars = 'iteration', variable.name = 'parameter', value.name = 'value')

# Graficar las cadenas de Markov
ggplot(samples_melted, aes(x = iteration, y = value, color = parameter)) +
  geom_line() +
  facet_wrap(~ parameter, scales = 'free_y') +
  theme_minimal() +
  labs(title = "Cadenas de Markov para Beta", x = "Iteración", y = "Valor")

# Graficar los histogramas
ggplot(samples_melted, aes(x = value, fill = parameter)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5) +
  facet_wrap(~ parameter, scales = 'free') +
  theme_minimal() +
  labs(title = "Histogramas de Muestras para Beta", x = "Valor", y = "Frecuencia")

# Graficar las densidades conjuntas de Beta_1 y Beta_2
ggplot(samples_df, aes(x = X2, y = X3)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_viridis_c() +
  geom_point(alpha = 0.3, color = 'blue', size = 0.5) +
  theme_minimal() +
  labs(title = "Densidad Conjunta de Beta_1 y Beta_2", x = expression(beta[1]), y = expression(beta[2]))
