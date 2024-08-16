# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#     Aprendizaje supervisado con estimacion rolling
# ==========================================================
rm(list = ls())

# Cargar librerías necesarias
library(ggplot2)

# --------- Cargar los datos desde el archivo CSV de GitHub --------------------
url = "https://raw.githubusercontent.com/rogon666/UMSA/main/AIMLDL/Datos/disdata.csv"
download.file(url, destfile = "discapacidad.csv")
datos <- read.csv("discapacidad.csv")

# ---------------- Pre-procesamiento (preparacion de datos) --------------------
# Espacio de caracteristicas (features): X (variables explicativas):
# Discapacidad 
datos$discapacidad <- 0
datos$discapacidad[datos$Doyouhavedifficultyseeinge %in% c(42, 43, 44) |
                     datos$Doyouhavedifficultyhearing %in% c(46, 47, 48) |
                     datos$Doyouhavedifficultywalkingo %in% c(50, 51, 52) |
                     datos$Doyouhavedifficultyrememberi %in% c(54, 55, 56) |
                     datos$Doyouhavedifficultywithself %in% c(58, 59, 60) |
                     datos$Usingyourusuallanguagecusto %in% c(62, 63, 64)] <- 1

# Discapacidad severa
datos$discapacidad_severa <- 0
datos$discapacidad_severa[datos$Doyouhavedifficultyseeinge %in% c(43, 44) |
                            datos$Doyouhavedifficultyhearing %in% c(47, 48) |
                            datos$Doyouhavedifficultywalkingo %in% c(51, 52) |
                            datos$Doyouhavedifficultyrememberi %in% c(55, 56) |
                            datos$Doyouhavedifficultywithself %in% c(59, 60) |
                            datos$Usingyourusuallanguagecusto %in% c(63, 64)] <- 1

# Nivel educativo
datos$educacion <- datos$Highestlevelofeducationcompl
educacion_labels <- c("ninguno", "primario", "secundario", "vocacional/tecnico", "terciario", "universitario")
datos$educacion <- factor(datos$educacion, levels = 33:38, labels = educacion_labels)

# Sexo
datos$sexo <- ifelse(datos$gender == 1, 0, 1)
sexo_labels <- c("masculino", "femenino")
datos$sexo <- factor(datos$sexo, levels = 0:1, labels = sexo_labels)

# Estado_civil
datos$ecivil <- datos$Maritalstatus
ecivil_labels <- c("soltero", "casado", "separado-divorciado", "viudo")
datos$ecivil <- factor(datos$ecivil, levels = 29:32, labels = ecivil_labels)

# Edad
datos$edad <- datos$age

# Calidad de vida, target, output, variable dependiente: 
datos$calidadvida <- datos$Ingeneralhowisyourliveliho
calidadvida_labels <- c("Mucho peor", "Peor", "Igual", "Mejor", "Mucho mejor")
datos$calidadvida <- factor(datos$calidadvida, levels = 77:81, labels = calidadvida_labels)

# Accesso a servicios financieros
datos$accesofin <- ifelse(datos$Doyoucurrentlyhaveanaccount == 110, 1, 0)
accesofin_labels <- c("no", "si")
datos$accesofin <- factor(datos$accesofin, levels = 0:1, labels = accesofin_labels)

# Eliminar filas donde calidadvida == "Igual"
datos <- subset(datos, calidadvida != "Igual")

# Output, target, etiquetando: 
# Calidad de vida colapsada a dos categorías:
datos$y <- factor(
  ifelse(datos$calidadvida %in% c("Mejor", "Mucho mejor"), 1, 0),
  levels = c(0, 1),
  labels = c("Peor", "Mejor")
)

table(datos$y)

# Submuestra de personas con discapacidad
submuestra_discapacidad <- subset(datos, discapacidad == 1)

# Submuestra de personas con discapacidad severa
submuestra_discapacidad_severa <- subset(datos, discapacidad_severa == 1)

table(submuestra_discapacidad_severa$y)

# ------ APRENDIZAJE SUPERVISADO CON ESTIMACION ROLLING ------------------------

set.seed(926)  # Establecer una semilla para la reproducibilidad

# Crear una lista para almacenar los resultados
resultados <- data.frame(Proporcion_Entrenamiento = numeric(),
                         Exactitud = numeric())

# Bucle para variar el porcentaje de entrenamiento desde 20% a 80%
for (train_prop in seq(0.2, 0.8, by = 0.01)) {
  
  # Índices de muestra de entrenamiento
  train_indices <- sample(1:nrow(submuestra_discapacidad_severa), 
                          size = round(train_prop * nrow(submuestra_discapacidad_severa)))
  
  # Crear los conjuntos de datos de entrenamiento y prueba
  train_data <- datos[train_indices, ]
  test_data <- datos[-train_indices, ]
  
  # Estimar el modelo logit en la muestra de entrenamiento
  modelo_logit_train <- glm(y ~ educacion + edad + ecivil + sexo + accesofin + Treatment,
                            data = train_data, 
                            family = binomial)
  
  # Predicciones en términos de probabilidad en la muestra de prueba
  predicciones_prob_y <- predict(modelo_logit_train, 
                                 newdata = test_data, 
                                 type = "response")
  
  # Convertir las probabilidades a clasificaciones binarias (0 o 1) usando un umbral de 0.5
  predicciones_y_binarias <- ifelse(predicciones_prob_y > 0.5, 1, 0)
  
  # Crear una tabla de contingencia para comparar las predicciones con los valores reales
  matriz_confusion <- table(Prediccion = predicciones_y_binarias, Observado = test_data$y)
  
  # Calcular la exactitud
  exactitud <- sum(diag(matriz_confusion)) / sum(matriz_confusion)
  
  # Extraer el estimador de odds ratio para accesofin
  tratamiento <- exp(coef(modelo_logit_train)["Treatment"])
  
  # Almacenar los resultados en el dataframe
  resultados <- rbind(resultados, data.frame(Proporcion_Entrenamiento = train_prop,
                                             Exactitud = exactitud,
                                             Tratamiento = tratamiento))
}

# Calcular la media de la exactitud
media_exactitud <- mean(resultados$Exactitud)
# Calcular la media de accesofin
media_tratamiento <- mean(resultados$Tratamiento)

# Graficar los resultados de exactitud usando ggplot2 con línea media
ggplot(resultados, aes(x = Proporcion_Entrenamiento, y = Exactitud)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  geom_hline(yintercept = media_exactitud, linetype = "dashed", color = "green") +
  annotate("text", x = 0, y = media_exactitud + 0.01, 
           label = paste("m =", round(media_exactitud, 4)), 
           color = "green", size = 4, hjust = 0) +
  labs(title = "Exactitud del modelo vs Proporción de Entrenamiento",
       x = "Proporción de Entrenamiento",
       y = "Exactitud") +
  theme_minimal()


# Graficar los estimadores de odds ratio para accesofin usando ggplot2
ggplot(resultados, aes(x = Proporcion_Entrenamiento, y = Tratamiento)) +
  geom_line(color = "purple") +
  geom_point(color = "orange") +
  geom_hline(yintercept = media_tratamiento, linetype = "dashed", color = "green") +
  annotate("text", x = 0, y = media_tratamiento + 0.025, 
           label = paste("m =", round(media_tratamiento, 2)), 
           color = "green", size = 4, hjust = 0) +
  labs(title = "Odds Ratio (en train) vs Proporción de Entrenamiento",
       x = "Proporción de Entrenamiento",
       y = "Odds Ratio (tratamiento)") +
  theme_minimal()