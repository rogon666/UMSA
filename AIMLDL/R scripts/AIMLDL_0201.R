# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#       Aprendizaje supervisado y no supervisado
# ==========================================================
library(ggplot2)
# install.packages("klaR")
library(klaR)
# install.packages("randomForest")
library(randomForest)

# --------- Cargar los datos desde el archivo CSV de GitHub --------------------
url = "https://raw.githubusercontent.com/rogon666/UMSA/main/AIMLDL/Datos/disdata.csv"
download.file(url, destfile = "discapacidad.csv")
datos <- read.csv("discapacidad.csv")

# ---------------- Pre-procesamiento (preparacion de datos) --------------------
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


# Tabla de frecuencias para la variable disability
tabla_discapacidad <- table(datos$discapacidad)
print(tabla_discapacidad)

# Tabla de frecuencias para la variable sevdiasb
tabla_sevdiscapacidad<- table(datos$discapacidad_severa)
print(tabla_sevdiscapacidad)

# Gráfico de barras para la variable discapacidad
barplot(, 
        main = "Frecuencia de discapacidad", 
        xlab = "discapacidad", 
        ylab = "Frecuencia", 
        col = "lightblue")

# Gráfico de barras para la variable discapacidad severa
barplot(, 
        main = "Frecuencia de discapacidad severa", 
        xlab = "discapacidad severa", 
        ylab = "Frecuencia", 
        col = "lightcoral")

# Nivel educativo
datos$educacion <- datos$Highestlevelofeducationcompl
educacion_labels <- c("ninguno", "primario", "secundario", "vocacional/tecnico", "terciario", "universitario")
datos$educacion <- factor(datos$educacion, levels = 33:38, labels = educacion_labels)
table(datos$educacion)

# Sexo
datos$sexo <- ifelse(datos$gender == 1, 0, 1)
sexo_labels <- c("masculino", "femenino")
datos$sexo <- factor(datos$sexo, levels = 0:1, labels = sexo_labels)
table(datos$gender, datos$sexo)

# Estado_civil
datos$ecivil<- datos$Maritalstatus
ecivil_labels <- c("soltero", "casado", "separado-divorciado", "viudo")
datos$ecivil <- factor(datos$ecivil, levels = 29:32, labels = ecivil_labels)
table(datos$ecivil)

# Edad
datos$edad <- datos$age
hist(datos$edad)

# Calidad de vida:
datos$calidadvida <- datos$Ingeneralhowisyourliveliho
calidadvida_labels <- c("Mucho peor", "Peor", "Igual", "Mejor", "Mucho mejor")
datos$calidadvida <- factor(datos$calidadvida, levels = 77:81, labels = calidadvida_labels)
table(datos$calidadvida)

# Accesso a servicios financieros
datos$accesofin <- ifelse(datos$Doyoucurrentlyhaveanaccount == 110, 1, 0)
accesofin_labels <- c("no", "si")
datos$accesofin <- factor(datos$accesofin, levels = 0:1, labels = accesofin_labels)
table(datos$accesofin, datos$Doyoucurrentlyhaveanaccount)

# Grupo nanofinanzas
datos$grupo <- datos$GroupNo
table(datos$grupo)

# Distrito
datos$distrito <- datos$DistrictNo
table(datos$distrito)
datos[datos$distrito == 502, ] <- NA
table(datos$distrito)

# ------------------- APRENDIZAJE SUPERVISADO ----------------------------------

# Calidad de vida colapsada a dos categorías:
datos$y <- factor(
  ifelse(datos$calidadvida %in% c("Mejor", "Mucho mejor"), 1, 0),
  levels = c(0, 1),
  labels = c("Peor", "Mejor")
)

  # Submuestra de personas con discapacidad
  submuestra_discapacidad <- subset(datos, discapacidad == )
  
  # Submuestra de personas con discapacidad severa
  submuestra_discapacidad_severa <- subset(datos, discapacidad_severa == )

# Ver la tabla de la nueva variable colapsada
table(datos$y, datos$calidadvida)
tabla_y <- table(datos$y)

# Gráfico de barras para la variable calidad de vida
barplot(tabla_y, 
        main = "Frecuencia de calidad de vida", 
        xlab = "discapacidad severa", 
        ylab = "Frecuencia", 
        col = "lightcoral")

# Modelo logit
modelo_logit <- glm(y ~ educacion +  + ecivil +  + accesofin,
                    data = datos, 
                    family = binomial)

# Resumen del modelo
summary(modelo_logit)

# Convertir log odds a odds
odds <- exp(coef(modelo_logit))
odds

# Gráfico de barras para la variable calidad de vida
barplot(table(submuestra_discapacidad$y), 
        main = "Frecuencia de calidad de vida", 
        xlab = "discapacidad severa", 
        ylab = "Frecuencia", 
        col = "lightcoral")

# Estimar el modelo logit para la submuestra de personas con discapacidad
modelo_logit_discapacidad <- glm(y ~ ,
                                 data = submuestra_discapacidad, 
                                 family = binomial)

# Resumen del modelo
summary(modelo_logit_discapacidad)
exp(coef(modelo_logit_discapacidad))

# Estimar el modelo logit para la submuestra de personas con discapacidad severa
modelo_logit_discapacidad_severa <- glm(y ~ ,
                                        data = submuestra_discapacidad_severa, 
                                        family = binomial)

# Resumen del modelo
summary(modelo_logit_discapacidad_severa)

set.seed()  # Establecer una semilla para la reproducibilidad

# Proporción de la muestra de entrenamiento
train_prop <-

# Índices de muestra de entrenamiento
train_indices <- sample(1:nrow(datos), size = round(train_prop * nrow(datos)))

# Crear los conjuntos de datos de entrenamiento y prueba
train_data <- datos[train_indices, ]
test_data <- datos[-train_indices, ]

# Estimar el modelo logit en la muestra de entrenamiento
modelo_logit_train <- glm(,
                          data = , 
                          family = binomial)

# Resumen del modelo
summary(modelo_logit_train)

# Predicciones en términos de probabilidad en la muestra de prueba
predicciones_prob_y <- predict(modelo_logit_train, 
                               newdata = , 
                               type = "response")

# Convertir las probabilidades a clasificaciones binarias (0 o 1) usando un umbral de 0.5
predicciones_y_binarias <- ifelse(predicciones_prob_y > 0.5, 1, 0)

# Crear una tabla de contingencia para comparar las predicciones con los valores reales
matriz_confusion <- table(Prediccion = predicciones_y_binarias, Observado = test_data$y)
matriz_confusion

# Calcular la exactitud
exactitud <- sum(diag(matriz_confusion)) / sum(matriz_confusion)
print(paste("Exactitud (accuracy): ", round(exactitud, 4)))

aprendizaje_supervisado <- function(datos, formula_modelo, train_proporcion = 0.7, seed = ) {
  # Paso 1: Dividir los datos en muestras de entrenamiento y prueba
  set.seed(seed)
  train_indices <- sample(1:nrow(datos), size = round(train_proporcion * nrow(datos)))
  train_data <- datos[train_indices, ]
  test_data <- datos[-train_indices, ]
  
  # Paso 2: Estimar el modelo logit en la muestra de entrenamiento
  modelo_logit_train <- glm(formula_modelo, data = train_data, family = binomial)
  
  # Paso 3: Hacer predicciones en la muestra de prueba
  predicciones_prob_test <- predict(modelo_logit_train, newdata = test_data, type = "response")
  predicciones_clasificacion_test <- ifelse(predicciones_prob_test > 0.5, 1, 0)
  
  # Paso 4: Calcular la exactitud del modelo logit
  confusion_matrix_logit <- table(Prediccion = predicciones_clasificacion_test, Real = test_data$y)
  exactitud_logit <- sum(diag(confusion_matrix_logit)) / sum(confusion_matrix_logit)
  
  # Paso 5: Estimar un modelo de Random Forest en la muestra de entrenamiento
  modelo_rf_train <- randomForest(formula_modelo, data = train_data)
  
  # Paso 6: Hacer predicciones con Random Forest en la muestra de prueba
  predicciones_rf_test <- predict(modelo_rf_train, newdata = test_data)
  
  # Paso 7: Calcular la exactitud del modelo Random Forest
  confusion_matrix_rf <- table(Prediccion = predicciones_rf_test, Real = test_data$y)
  exactitud_rf <- sum(diag(confusion_matrix_rf)) / sum(confusion_matrix_rf)
  
  # Paso 8: Imprimir los resultados
  print(paste("Exactitud (accuracy) del modelo Logit: ", round(exactitud_logit, 4)))
  print(paste("Exactitud (accuracy) del modelo Random Forest: ", round(exactitud_rf, 4)))
  
  # Devolver las exactitudes
  return(list(Exactitud_Logit = exactitud_logit, Exactitud_RF = exactitud_rf))
}

# Ejemplo de uso de la función
res1 <- aprendizaje_supervisado(datos, 
                                y ~ )
res2 <- aprendizaje_supervisado(submuestra_discapacidad, 
                                y ~ )
res3 <- aprendizaje_supervisado(submuestra_discapacidad_severa, 
                                y ~ )

# ------------------ APRENDIZAJE NO SUPERVISADO --------------------------------

# variables de clusterizacion:
vars_clusterizacion <- c("grupo", "distrito", "", "", "")

# Eliminar filas con NA en las variables categóricas
datos_limpios <- na.omit(datos[,vars_clusterizacion ])

datos_limpios$grupo <- as.factor(datos_limpios$grupo)
datos_limpios$distrito <- as.factor(datos_limpios$distrito)

# Aplicar k-modes con el número seleccionado de clusters, por ejemplo, 3 clusters
set.seed()  # Para reproducibilidad
kmodes_result <- kmodes(datos_limpios[, vars_clusterizacion], modes = 3)

# Agregar los clusters resultantes al dataframe original
datos_limpios$cluster <- kmodes_result$cluster

# Ver la cantidad de elementos en cada cluster
table(datos_limpios$cluster)

# Graficar los clusters
ggplot(datos_limpios, aes(y = educacion, x = edad, color = factor(cluster))) +
  geom_point(size = 3) +
  labs(title = "Análisis no supervisado (k-modas)",
       x = "Edad",
       y = "Educacion",
       color = "Cluster") +
  theme_minimal()
# -------------------------------------------------------------------------------