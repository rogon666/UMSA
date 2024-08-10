# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#   Credit scoring con un modelo MLG y machine learning
# ==========================================================
# Instalar y cargar los paquetes necesarios
# install.packages("tidyverse")
# install.packages("caret")
# install.packages("randomForest")
# install.packages("e1071")
# install.packages("pROC")

library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(pROC)

# Cargar los datos desde el archivo CSV
url <- "https://raw.githubusercontent.com/rogon666/UMSA/main/MLMLG/datos/credit_scoring_db.csv"
download.file(url, destfile = "scoring.csv")
datos <- read.csv("scoring.csv")

# Mostrar las primeras filas del dataset para entender las variables
head(datos)

# Crear la variable dependiente
datos <- datos %>%
  mutate(target = ifelse(dias_mora > 60 & castigo == 1, 1, 0))

# Eliminar las variables 'dias_mora', 'castigo' y 'Monto_credito' del conjunto de datos
datos <- datos %>%
  select(-dias_mora, -castigo, -Monto_credito)

# Calcular WOE manualmente para una variable categórica
calculate_woe <- function(data, feature, target) {
  tab <- table(data[[feature]], data[[target]])
  total_good <- sum(tab[, 1])
  total_bad <- sum(tab[, 2])
  woe_values <- log((tab[, 1] / total_good) / (tab[, 2] / total_bad))
  return(data.frame(Category = rownames(tab), WOE = woe_values))
}

# Calcular y reemplazar WOE:
reemplazar_woe <- function(data, feature, target) {
  tab <- table(data[[feature]], data[[target]])
  total_good <- sum(tab[, 1])
  total_bad <- sum(tab[, 2])
  woe_values <- log((tab[, 1] / total_good) / (tab[, 2] / total_bad))
  woe_table <- data.frame(Category = rownames(tab), WOE = woe_values)
  # Crear un mapeo de los valores de la variable a WOE
  woe_map <- setNames(woe_table$WOE, woe_table$Category)
  # Reemplazar los valores de la variable por WOE
  data[[feature]] <- woe_map[as.character(data[[feature]])]
  data[[feature]] <- as.numeric(data[[feature]])
  return(data)
}

# Aplicar la función a las variables categóricas
categorical_features <- c("Estado_civil", "Edad", "Sexo", "Educacion", "Empleado", "numero_creditos", "Tipo_credito")
for (feature in categorical_features) {
  datos <- reemplazar_woe(datos, feature, "target")
}

# Convertir la variable 'target' a factor para la clasificación
datos$target <- as.factor(datos$target)

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed()
train_index <- createDataPartition(datos$target, p = 0.7, list = FALSE)
train_data <- datos[train_index, ]
test_data <- datos[-train_index, ]

# Ajustar el modelo logit usando las variables transformadas a WOE
modelo_logit <- glm(target ~ ., data = train_data, family = binomial)

# Resultados del modelo
summary(modelo_logit)

# Predicciones y evaluación del modelo logit
logit_pred <- predict(modelo_logit, newdata = test_data, type = "response")
logit_pred_class <- ifelse(logit_pred > 0.5, 1, 0)
confusionMatrix(factor(logit_pred_class), factor(test_data$target))

# Ajustar un modelo Random Forest
modelo_rf <- randomForest(target ~ ., data = train_data, na.action=na.exclude)
rf_pred <- predict(modelo_rf, newdata = test_data)
rf_pred <- factor(rf_pred, levels = levels(test_data$target))
confusionMatrix(rf_pred, test_data$target)

# Ajustar un modelo SVM
modelo_svm <- svm(target ~ ., data = train_data, probability = TRUE, na.action=na.exclude)
svm_pred <- predict(modelo_svm, newdata = test_data)
svm_pred <- factor(svm_pred, levels = levels(test_data$target))
confusionMatrix(svm_pred, test_data$target)