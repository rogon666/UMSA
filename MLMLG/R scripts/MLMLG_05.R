# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2024
# ==========================================================

# Load necessary libraries
library(ggplot2)
library(rstanarm)

# Descargar el archivo CSV desde GitHub
url <- "https://raw.githubusercontent.com/rogon666/UMSA/31e44a5cbbbf4eb7fde903fe1c192163bd8dd6ba/MLMLG/datos/burkina.csv"
download.file(url, destfile = "burkina.csv")

# Ajustando un modelo lineal Bayesiano
model <- stan_glm(inflacion ~ PIB, data = data, family = gaussian(), prior = normal(0, 1))

# Resultados
summary(model)

# Distribucion posterior de los coeficientes
posterior <- as.data.frame(as.matrix(model))
ggplot(posterior, aes(x = PIB)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Distribucion posterior del coeficiente del PIB",
       x = "coeficiente del PIB",
       y = "Densidad") +
  theme_minimal()
