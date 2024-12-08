# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#    Metodos Bayesianos: intervalos de credibilidad
# ==========================================================
# Datos de: Romney, A. K. (1999). Culture Consensus as a 
# Statistical Model. Current Anthropology 40 (Supplement),
# S103-S115.
library(ggplot2)

# Datos de opiniones de 24 mujeres guatemaltecas respecto a si
# ellas consideran que el polio no es una enfermedad contagiosa (1)
# o piensan que el polio es una enfermedad contagiosa (0)
x <- c(1,1,1,1,0,1,1,0,1,0,1,1,1,0,1,1,1,1,1,1,0,0,0,1)
n <- length(x)
soporte <- seq(0,1,length=300)
sum(x)/n
# 71% de las mujeres encuestas piensan que el polio no es una enfermedad contagiosa

# Hipotesis: Creer que las enfermedades específicas son contagiosas 
# es una respuesta cultural aprendida

# ------------------------ PRIORs -------------------------------------
# Se aplica un modelo Beta-Binomial con dos priors
# Primer prior: Beta B(15,2) prior informativo sobre theta
# Segundo prior: Beta B(1,1) prior uniforme (incertidumbre sobre theta)
  
  # Beta(alpha,beta): primer prior informativo
  alpha_prior1 = 15
  beta_prior1 = 2
  prior1 <- dbeta(soporte, shape1 = alpha_prior1, shape2 = beta_prior1)
  data <- data.frame(x = soporte, y = prior1)
  ggplot(data, aes(x = soporte, y = prior1)) +
    geom_line(color = "blue") +
    labs(title = "Prior informativo Beta B(15,2) acerca de theta",
         x = "x",
         y = "Densidad") +
    theme_minimal()

  # Beta(alpha,beta): segundo prior no informativo
  alpha_prior2 = 1
  beta_prior2 = 1
  prior2 <- dbeta(soporte, shape1 = alpha_prior2, shape2 = beta_prior2)
  data <- data.frame(x = soporte, y = prior2)
  ggplot(data, aes(x = soporte, y = prior2)) +
    geom_line(color = "blue") +
    labs(title = "Prior no informativo Beta B(1,1) acerca de theta",
         x = "x",
         y = "Densidad") +
    theme_minimal()

# ---------------------- POSTERIORs ----------------------------------
# La distribucion Beta es conjugada a la binomial, por lo que la 
# distribucion posterior es Beta
  
  # Posterior 1 (Prior informativo):
  alpha_posterior1 = alpha_prior1 + sum(x)
  beta_posterior1 = beta_prior1 + n - sum(x)
  alpha_posterior1
  beta_posterior1
  # Posterior: Beta(32,9)
  EBeta_posterior1 = alpha_posterior1 / (alpha_posterior1 + beta_posterior1)
  EBeta_posterior1
  hpd95_1 <- qbeta(c(0.025,0.975),
                   sum(x)+alpha_posterior1,
                   length(x)-sum(x)+beta_posterior1)
  hpd95_1
  
  # Posterior 2 (Prior no informativo):
  alpha_posterior2 = alpha_prior2 + sum(x)
  beta_posterior2 = beta_prior2 + n - sum(x)
  alpha_posterior2
  beta_posterior2
  # Posterior: Beta(18,8)
  EBeta_posterior2 = alpha_posterior2 / (alpha_posterior2 + beta_posterior2)
  EBeta_posterior2
  hpd95_2 <- qbeta(c(0.025,0.975),
                   sum(x)+alpha_posterior2,
                   length(x)-sum(x)+beta_posterior2)
  hpd95_2
  
  # Resultados con prior informativo:
  # Densidad posterior de la distribución Beta B(32,9)
  posterior1 <- dbeta(soporte, shape1 = alpha_posterior1, shape2 = beta_posterior1)
  ggplot(data) +
    geom_line(aes(x = soporte, y = prior1, color = "Prior B(15,2)")) +
    geom_line(aes(x = soporte, y = posterior1, color = "Posterior B(32,9)")) +
    annotate("segment", x = hpd95_1[1], xend = hpd95_1[2], y = 0, yend = 0, color = "green", size = 1.5) +
    annotate("text", x = mean(hpd95_1), y = 0.02, label = "intervalo HPD al 95%", size = 3, color = "green", vjust = -1) +
    labs(title = "Resultados con prior informativo",
         x = "soporte",
         y = "densidad",
         color = "Distribucion") +
    theme_minimal() +
    scale_color_manual(values = c("Prior B(15,2)" = "blue", "Posterior B(32,9)" = "red"))
  
  # Resultados con prior no informativo:
  # Densidad posterior de la distribución Beta B(18,8)
  posterior2 <- dbeta(soporte, shape1 = 18, shape2 = 8)
  hpd.95 <- qbeta(c(0.025,0.975),sum(x)+18,length(x)-sum(x)+8)
  ggplot(data) +
    geom_line(aes(x = soporte, y = prior2, color = "Prior B(1,1)")) +
    geom_line(aes(x = soporte, y = posterior2, color = "Posterior B(18,8)")) +
    annotate("segment", x = hpd95_2[1], xend = hpd95_2[2], y = 0, yend = 0, color = "green", size = 1.5) +
    annotate("text", x = mean(hpd95_2), y = 0.02, label = "intervalo HPD al 95%", size = 3, color = "green", vjust = -1) +
    labs(title = "Resultados con prior no informativo",
         x = "soporte",
         y = "densidad",
         color = "Distribucion") +
    theme_minimal() +
    scale_color_manual(values = c("Prior B(1,1)" = "blue", "Posterior B(18,8)" = "red"))