# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#   Metodos Bayesianos: MLG logit para credit scoring
# ==========================================================
rm(list = ls())
graphics.off()
library(dplyr)
library(ggplot2)
library(sandwich)
library(lmtest)

# Cargar los datos desde el archivo CSV
url = "https://raw.githubusercontent.com/rogon666/UMSA/main/MLMLG/datos/credit_scoring_db.csv"
download.file(url, destfile = "scoring.csv")
datos <- read.csv("scoring.csv")

# Mostrar las primeras filas del dataset para entender las variables
head(datos)

# Crear la variable dependiente
datos <- datos %>%
  mutate(target = ifelse(dias_mora > & castigo == 1, 1, 0))

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

# Aplicar la función a la variable 'Estado_civil'
woe_estado_civil <- calculate_woe(datos, "Estado_civil", "target")
woe_estado_civil

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

# Ajustar el modelo logit usando las variables transformadas a WOE
modelo_logit <- glm(target ~ ., data = datos, 
                    family = binomial)

# Resultados del modelo
summary(modelo_logit)

# Calcular la matriz de varianza-covarianza robusta
cov_matrix_robusta <- vcovHC(modelo_logit, type = "HC0")

# Realizar pruebas de significancia con la matriz de varianza-covarianza robusta
resultados_robustos <- coeftest(modelo_logit, vcov. = cov_matrix_robusta)

# Mostrar los resultados
print(resultados_robustos)

# Ajustar el modelo logit usando las variables transformadas a WOE
modelo_logit_reducido <- glm(target ~ Edad + Estado_civil + numero_creditos, data = datos, family = binomial)

# Ver los coeficientes del modelo
summary(modelo_logit_reducido)

# Calcular las predicciones lineales (Xb)
predicciones_logit <- predict(modelo_logit_reducido, type = "link")

# Definir el offset y el factor
offset <-   
factor <-   

# Transformar las predicciones en puntuaciones crediticias
scores_crediticios <- offset - factor * predicciones_logit

# Agregar las puntuaciones crediticias al dataset original
datos_final <- datos %>%
  mutate(scores_crediticios = scores_crediticios)

# Ver los datos finales con las puntuaciones crediticias
head(datos_final)

# Histograma de las puntuaciones crediticias con ggplot2
ggplot(datos_final, aes(x = scores_crediticios)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Histograma de Scores Crediticios", x = "Scores Crediticios", y = "Frecuencia") +
  theme_minimal()

# Gráfico de boxplots de los scores crediticios por target
ggplot(datos_final, aes(x = as.factor(target), y = scores_crediticios)) +
  geom_boxplot() +
  labs(title = "Boxplot de Scores Crediticios por Target", x = "Target", y = "Scores Crediticios") +
  theme_minimal()

# Regresion logistica usando brms
library(brms)
modelo_Bayesiano <- brm(
  formula = target ~ Edad + Estado_civil + numero_creditos, 
  data = datos, 
  family = bernoulli(link = "logit"), 
  prior = c(set_prior("normal(0, 5)", class = "b")),
  chains = , 
  iter = , 
  warmup = ,
  seed = 666
)

# Summary of the model
summary(modelo_Bayesiano)

# Plot the model
plot(modelo_Bayesiano)