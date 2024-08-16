# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#     Aprendizaje supervisado 
# ==========================================================
rm(list = ls())

# Cargar librerías necesarias
library(ggplot2)

# --------- Cargar los datos desde el archivo CSV de GitHub --------------------
url = "https://raw.githubusercontent.com/rogon666/UMSA/main/AIMLDL/Datos/mortalidad_cochabamba.csv"
download.file(url, destfile = "mortalidad_cochabamba.csv")
df <- read.csv("mortalidad_cochabamba.csv")

# Filtrar los datos para remover la categoría "Natural"
df <- subset(df, causa_muertes != "Natural")

# Calcular los porcentajes
df$causa_muertes <- factor(df$causa_muertes)
df_count <- as.data.frame(table(df$causa_muertes))
df_count$percentage <- (df_count$Freq / sum(df_count$Freq)) * 100

# Crear el gráfico de frecuencias para las causas de muerte con porcentajes
ggplot(df_count, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), 
            vjust = -0.5, 
            color = "black") +
  labs(title = "Frecuencia de Causas de Muerte\n(Excluyendo 'Natural')",
       x = "Causa de Muerte",
       y = "Frecuencia") +
  ylim(0, 40) +  # Establecer el límite del eje Y a 100%
  theme_minimal()