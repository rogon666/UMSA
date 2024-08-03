# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#         Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#    Modelo de Regresión Logistico: ejemplo practico 
# ==========================================================
library(ggplot2)
library(dplyr)

# --------------------- Cargar datos ----------------------------------
url = "https://raw.githubusercontent.com/rogon666/UMSA/main/MLMLG/datos/empleados_short.csv"
download.file(url, destfile = "empleados.csv")
datos <- read.csv("empleados.csv")

# Variable dependiente: renuncia 
# Convertir la columna 'renuncia' a binaria (0 = No, 1 = Si)
datos$renuncia_binaria <- ifelse(datos$renuncia == 'Si', 1, 0)
datos$renuncia_binaria <- as.factor(datos$renuncia_binaria)

# Calcular el número de ceros y unos, y sus porcentajes
conteo_renuncias <- datos %>% 
  group_by(renuncia_binaria) %>% 
  summarise(count = n()) %>% 
  mutate(percentage = count / sum(count) * 100,
         renuncia_binaria = as.factor(renuncia_binaria),
         renuncia_binaria = recode(renuncia_binaria, `0` = "No", `1` = "Si"))

# Crear el gráfico
ggplot(conteo_renuncias, aes(x = renuncia_binaria, y = count, fill = renuncia_binaria)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(count, " (", round(percentage, 1), "%)")), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("No" = "#00BFC4", "Si" = "#F8766D")) +
  labs(title = "Distribución de Renuncias de Empleados", 
       x = "Renuncia", 
       y = "Número de Empleados",
       fill = "Renuncia") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5))
