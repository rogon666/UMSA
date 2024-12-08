# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Agosto 2024
# ===============================================================
# Aprendizaje no supervisado para fraudes de tarjetas de credito
# ===============================================================
rm(list = ls())

# Cargar librerías necesarias
library(ggplot2)
library(caret)

# --------- Cargar los datos desde el archivo CSV de GitHub --------------------
url = "https://raw.githubusercontent.com/rogon666/UMSA/main/AIMLDL/Datos/fraudes_tarjetascredito.csv"
download.file(url, destfile = "fraudes.csv")
df <- read.csv("fraudes.csv")

# Separar las características (X) y la variable objetivo (y)
X <- df[, -1]
y <- 

# Realizar clustering K-means con 2 grupos
set.seed(42)
kmeans_model <- kmeans(X, centers = 2)

# Añadir los clusters al conjunto de datos
df$cluster <- as.factor(kmeans_model$cluster)

# Usar PCA para reducir a 2 dimensiones si hay más de 2 características
pca <- prcomp(X, scale. = TRUE)
df$PC1 <- pca$x[, 1]
df$PC2 <- pca$x[, 2]

# Obtener las coordenadas de los centroides en el espacio PCA
centroides <- as.data.frame(kmeans_model$centers)
centroides_pca <- predict(pca, newdata = centroides)
centroides_pca <- as.data.frame(centroides_pca)
centroides_pca$cluster <- as.factor(1:nrow(centroides_pca))

# Graficar los clusters y los centroides
ggplot(df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2) +
  geom_point(data = centroides_pca, aes(x = PC1, y = PC2, color = cluster), 
             shape = 8, size = 4, stroke = 2) +
  labs(title = "Clusters K-means y sus Centroides en 2D usando PCA",
       x = "Componente Principal 1",
       y = "Componente Principal 2") +
  theme_minimal()

# Comparar los resultados de los clusters con y
# Como los clusters pueden estar etiquetados de manera diferente, 
# ajustamos la asignación de clusters para encontrar la mejor correspondencia
clusters <- kmeans_model$cluster
conf_matrix <- table(y, clusters)

# Alternativamente, es posible invertir los clusters y comparar de nuevo
clusters_adjusted <- ifelse(clusters == 1, 2, 1)
conf_matrix_adjusted <- table(y, clusters_adjusted)
  
# Mostrar las matrices de confusión
print("Matriz de confusión original:")
print(conf_matrix)
exactitud_1 <- sum(diag()) / sum()
exactitud_1

print("Matriz de confusión ajustada:")
print(conf_matrix_adjusted)
