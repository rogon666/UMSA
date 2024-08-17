# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#   Aprendizaje supervisado con validacion cruzada
# ==========================================================
rm(list = ls())

# Cargar librerías necesarias
library(ggplot2)
library(caret) # Matriz confusion

# --------- Cargar los datos desde el archivo CSV de GitHub --------------------
url = "https://raw.githubusercontent.com/rogon666/UMSA/main/AIMLDL/Datos/mortalidad_cochabamba.csv"
download.file(url, destfile = "mortalidad_cochabamba.csv")
df <- read.csv("mortalidad_cochabamba.csv")

# -------------- Pre-procesamiento (preparacion de datos) -------------------------
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

# Convertir la variable causa_muertes en una variable binaria target
df$target <- ifelse(df$causa_muertes == "Suicidio", 1, 0)
table(df$target,df$causa_muertes)
df$target <- factor(df$target, levels = c(0, 1), labels = c("No_Suicidio", "Suicidio"))

# ------------------- Validacion cruzada ---------------------------------------
# Configurar la validación cruzada
train_control <- trainControl(method = "cv", 
                              number = 5)  # Validación cruzada con 10 particiones

# Entrenar el modelo utilizando validación cruzada
model <- train(target ~ edad, 
               data = df, 
               method = "glm", 
               family = "binomial", 
               trControl = train_control)

# Predicciones en los datos de entrenamiento
predictions <- predict(model, newdata = df)

# Convertir las predicciones y el target a factores con los mismos niveles
predictions <- factor(predictions, levels = c("Suicidio","No_Suicidio"))
target <- factor(df$target, levels = c("Suicidio","No_Suicidio"))

# Evaluar el modelo
conf_matrix <- confusionMatrix(predictions, target)
print(conf_matrix)

#-------------------------------------------------------------------------------