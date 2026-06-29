# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2024
# ==========================================================
# ~~~~~ Estimación de Modelos Lineales Generalizados ~~~~~~~
# ==========================================================

# ------------------ Estimacion por maxima verosimilitud ----------------------
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  p <- 1 / (1 + exp(-1 - 2 * x))
  y <- rbinom(n, size = 1, prob = p)
  datos <- data.frame(y, x)
  
  # Ajuste del modelo de regresión logística
  modelo_logistico <- glm(y ~ x, data = datos, 
                          family = binomial(link = "logit"))
  
  # Resumen del modelo
  summary(modelo_logistico)
  
  # Intervalos de confianza para los coeficientes
  confint(modelo_logistico)

# -------------- Estimacion por quasi-maxima verosimilitud ---------------------
  
  # Datos simulados
  datos <- data.frame(
    y = c(2, 1, 3, 4, 2, 1, 3, 2, 4, 3),
    x1 = c(1.1, 1.9, 2.8, 3.2, 1.5, 2.6, 1.8, 2.7, 3.0, 1.2),
    x2 = c(0.3, 0.7, 0.6, 0.4, 0.9, 0.5, 0.6, 0.8, 0.4, 0.9)
  )
  
  # Ajuste del modelo de Poisson usando cuasi-verosimilitud
  modelo_poisson <- glm(y ~ x1 + x2, 
                        data = datos, 
                        family = quasipoisson(link = "log"))
  
  # Resumen del modelo
  summary(modelo_poisson)

# -------------- Estimacion por el metodo de los momentos ----------------------

  # Datos simulados
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  beta <- c(0.5, 2)
  lambda <- exp(beta[1] + beta[2]*x)
  y <- rpois(n, lambda)

  # Momentos muestrales
  mu_muestral <- mean(y)
  var_muestral <- var(y)

  # Resolver para beta
  beta_0_hat <- log(mu_muestral)
  beta_1_hat <- (log(var_muestral) - log(mu_muestral)) / mean(x)
  
  # Estimaciones
  beta_hat <- c(beta_0_hat, beta_1_hat)
  beta_hat
