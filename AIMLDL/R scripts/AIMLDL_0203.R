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

# -------- Validación cruzada con Random Forests -------------------

# Definir las características (X) y la variable objetivo (y)
X <- datos[, c("educacion", "sexo", "ecivil", "edad", "accesofin")]
y <- datos$y

# Configuración de la validación cruzada
control <- trainControl(method = "cv", 
                        number = 10)  # 10-fold cross-validation

# Entrenamiento del modelo Random Forest con validación cruzada
modelo_rf <- train(y ~ educacion + edad + ecivil + sexo + accesofin, 
                   data = datos, method = "rf", 
                   trControl = control)

# Resumen del modelo
print(modelo_rf)

# Importancia de las variables
importancia <- varImp(modelo_rf)
print(importancia)

# Visualización de la importancia de las variables
plot(importancia)
