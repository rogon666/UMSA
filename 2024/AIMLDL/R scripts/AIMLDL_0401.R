# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#            Machine Learning y Deep Learning
# ----------------------------------------------------------
#         Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#         Hypertuning con optimizacion Bayesiana
#     Clasificadores no supervisados basados en DBSCAN
# ==========================================================
library(ParBayesianOptimization)
library(dbscan)
library(scales)
library(caret)
library(cluster)

# ---------------------- Cargando datos ----------------------------------------
url <- "https://raw.githubusercontent.com/rogon666/UMSA/main/AIMLDL/Datos/mortalidad.csv"
download.file(url, destfile = "mortalidad.csv")
df <- read.csv("mortalidad.csv")

table(df$educacion[df$causa_muertes == "Suicidio"])

# Reemplazar los NA en la columna "educacion" con 6 
df$educacion[is.na(df$educacion)] <- 

# ------------------ Preprocesar los datos -------------------------------------
# Convertir variables categóricas en factores
features_categoricos <- c('estado_civil', 'depto_nacimiento','causa_muertes')

df[features_categoricos] <- lapply(df[features_categoricos], as.factor)

# Estandarizar variables numéricas
features_numericos <- c('edad','educacion')
df[features_numericos] <- scale(df[features_numericos])


# Convertir variables categóricas a variables dummy (One-Hot Encoding)
dummies <- dummyVars(~ ., data = df[,features_categoricos])
# Aplicar One-Hot Encoding
X <- as.data.frame(
  predict(dummies, newdata = df[, features_categoricos])
)
X$edadz <- df$edad
X$educacion <- df$educacion

# ------------------- Grafico de k-distancias ----------------------------------
kNNdistplot(X, k = 5)
abline(h = 1.1, lty = 2)

kNNdistplot(X, k = 10)
abline(h = 1.7, lty = 2)

kNNdistplot(X, k = 20)
abline(h = 2.6, lty = 2)

# -------------------------- DBSCAN ---------------------------------------------
# Definir una función para calcular el puntaje de Silhouette con DBSCAN
hiperparametro_eps = 
hiperparametro_pts = 5
clDBSCAN <-dbscan(X,
                  eps=hiperparametro_eps,
                  MinPts = hiperparametro_pts)
table(clDBSCAN$cluster)
hullplot(X,clDBSCAN$cluster, main = "Cluster Hulls convexos")


# Definir la función para calcular el puntaje de Silhouette con DBSCAN
dbscan_scorer <- function(eps, minPts) {
  model <- dbscan(X, eps = eps, minPts = minPts)
  clusters <- model$cluster
  
  # Calcular el puntaje Silhouette solo si hay más de un clúster
  if(length(unique(clusters)) > 1) {
    sil <- silhouette(clusters, dist(X))
    score <- mean(sil[, 3])
  } else {
    score <- -1  # Devolver un puntaje bajo si no se encuentran clústeres significativos
  }
  
  return(list(Score = score))
}

# Definir los confines para la optimización bayesiana
confines <- list(
  eps = c(0.1, 5.0),
  minPts = c(3L, 10L)
)

# Realizar la optimización bayesiana
optimizacion <- bayesOpt(
  FUN = dbscan_scorer,
  bounds = confines,
  initPoints = 5,
  iters.n = 10
)

# Mostrar los mejores parámetros encontrados
hiperparametros_optimos <- getBestPars(optimizacion)
print(hiperparametros_optimos)

hiperparametro_eps = hiperparametros_optimos$eps
hiperparametro_pts = hiperparametros_optimos$minPts
clDBSCAN <-dbscan(X,
                  eps=hiperparametro_eps,
                  MinPts = hiperparametro_pts)
table(clDBSCAN$cluster)

hullplot(X,clDBSCAN$cluster, main = "Cluster Hulls convexos")