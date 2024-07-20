# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2024
# ==========================================================
# Instalar las bibliotecas necesarias
# install.packages("ggplot2")

# Cargar las bibliotecas necesarias
library(ggplot2)

# Descargar el archivo CSV desde GitHub
url <- "https://raw.githubusercontent.com/rogon666/UMSA/31e44a5cbbbf4eb7fde903fe1c192163bd8dd6ba/MLMLG/datos/burkina.csv"
download.file(url, destfile = "burkina.csv")

# Leer el archivo CSV
data <- read.csv("burkina.csv", encoding = "latin1")

# Convertir el año a un objeto Date 
data$date <- as.Date(paste0(data$Año, "-01-01"))

# Gráfico de serie de tiempo para la inflación
ggplot(data, aes(x = date, y = inflacion)) +
  geom_line(color = "blue") +
  labs(title = "Serie de Tiempo de la Inflación en Burkina Faso",
       x = "Fecha",
       y = "Inflación (%)") +
  theme_minimal()

# Gráfico de serie de tiempo para el PIB
ggplot(data, aes(x = date, y = PIB)) +
  geom_line(color = "green") +
  labs(title = "Serie de Tiempo del PIB en Burkina Faso",
       x = "Fecha",
       y = "PIB") +
  theme_minimal()

# Scatter plot de inflación y PIB
ggplot(data, aes(x = PIB, y = inflacion)) +
  geom_point(color = "red") +
  labs(title = "Scatter Plot de Inflación vs. PIB",
       x = "PIB",
       y = "Inflación (%)") +
  theme_minimal()

# Modelo de regresión de inflación como variable dependiente del PIB
model <- lm(inflacion ~ PIB, data = data)

# Resumen del modelo
summary(model)

# Añadir la línea de regresión al scatter plot
ggplot(data, aes(x = PIB, y = inflacion)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatter Plot de Inflación vs. PIB con Línea de Regresión",
       x = "PIB",
       y = "Inflación (%)") +
  theme_minimal()


