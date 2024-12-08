# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#            Machine Learning y Deep Learning
# ----------------------------------------------------------
#         Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#      Practica final: redes neuronales profundas
# ==========================================================

# ----------------- Cargar las librerías necesarias ----------------------------
# install.packages("deepnet")
# library(mlbench)
# library(deepnet)
library(neuralnet)
library(caTools)  # Para hacer el train-test split
library(ggplot2)

# ----------------------- Cargando los datos -----------------------------------
data("BreastCancer")

# ------------------------- preparacion de datos -------------------------------
# Limpiar filas con datos faltantes
BreastCancer = BreastCancer[which(complete.cases(BreastCancer)==TRUE),]

# Convertir las etiquetas a formato numérico
y = as.matrix(BreastCancer[,11])
y[which(y=="benign")] = 0
y[which(y=="malignant")] = 1
y = as.numeric(y)

# Convertir las características a formato numérico
x = as.numeric(as.matrix(BreastCancer[,2:10]))
x = matrix(as.numeric(x),ncol=9)
df = data.frame(cbind(x, y))

# ---------------------------- Exploracion de datos ----------------------------
# Graficar histogramas y boxplots para la variable de salida y
df_y = data.frame(y = y)
# Histograma de y
ggplot(df_y, aes(x = factor(y))) + 
  geom_bar(fill = "skyblue") +
  labs(title = "Histograma de la variable de salida (y)", x = "Clase", y = "Frecuencia")

# Crear un dataframe con todas las variables de entrada en formato largo (long format)
df_x = data.frame(x)
names(df_x) = paste0("V", 1:9)
df_long_x = reshape2::melt(df_x)
# Boxplot para todas las variables de entrada en un solo gráfico
ggplot(df_long_x, aes(x = variable, y = value)) + 
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Boxplots de las variables de entrada (X)", x = "Variable", y = "Valor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# -------------------------- Particion de datos --------------------------------

# Train-test split (80% entrenamiento, 20% prueba)
set.seed(123)  # Para reproducibilidad
split = sample.split(y, SplitRatio = 0.7)
x_train = x[split, ]
x_test = x[!split, ]
y_train = y[split]
y_test = y[!split]
df_train = df[split, ]
df_test = df[!split, ]

# -------------------- Entrenamiento y testeo de la red neuronal ---------------
# Entrenar la red con neuralnet
nn_neuralnet = neuralnet(y ~ V2 + V3, 
                         hidden = 3, # numero de neuronas
                         data = df_train)

# Predicciones en los datos de prueba
yy_test_neuralnet = compute(nn_neuralnet, df_test[, 1:2])$net.result

# Convertir las predicciones a etiquetas binarias
yhat_test_neuralnet = matrix(0, length(y_test), 1)
yhat_test_neuralnet[which(yy_test_neuralnet > mean(yy_test_neuralnet))] = 1
yhat_test_neuralnet[which(yy_test_neuralnet <= mean(yy_test_neuralnet))] = 0

# Matriz de confusión para neuralnet en los datos de prueba
cm_test_neuralnet = table(y_test, yhat_test_neuralnet)
print(cm_test_neuralnet)

# Calcular la exactitud en el conjunto de prueba para neuralnet
exactitud_test_neuralnet = sum(diag(cm_test_neuralnet)) / sum(cm_test_neuralnet)
print(paste("Exactitud con neuralnet en el conjunto de prueba:", exactitud_test_neuralnet))

# Graficar la red neuronal entrenada
plot(nn_neuralnet)
# ------------------------------------------------------------------------------