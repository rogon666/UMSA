# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#         Regularizacion de modelos lineales 
# ==========================================================
# Instalar y cargar los paquetes necesarios
# install.packages(c("caret", "glmnet", "ggplot2", "reshape2"))
library(caret)
library(glmnet)
library(ggplot2)
library(reshape2)

# Cargar los datos desde el archivo CSV
url = "https://raw.githubusercontent.com/rogon666/UMSA/main/MLMLG/datos/salarios.csv"
download.file(url, destfile = "salarios.csv")
data <- read.csv("salarios.csv")

# Crear la variable objetivo (logaritmo del salario)
data$log_salario <- log(data$salario)

# Dividir la muestra en train y test (70%-30%)
set.seed()
trainIndex <- createDataPartition(data$log_salario, p = .7, 
                                  list = FALSE, 
                                  times = 1)
trainData <- data[ trainIndex,]
testData  <- data[-trainIndex,]

# Preparar los datos para el modelado
x_train <- as.matrix(trainData[, c("educacion", "educacionmadre", "edad", 
                                   "antiguedad_ejecutivo", "educacion_grado", 
                                   "antiguedad", "educacion_posgrado", 
                                   "valor_empresa", "beneficios_empresa", 
                                   "ventas_empresa")])
y_train <- trainData$log_salario

x_test <- as.matrix(testData[, c("educacion", "educacionmadre", "edad", 
                                 "antiguedad_ejecutivo", "educacion_grado", 
                                 "antiguedad", "educacion_posgrado", 
                                 "valor_empresa", "beneficios_empresa", 
                                 "ventas_empresa")])
y_test <- testData$log_salario

# Función para calcular MAPE
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# Estimar el modelo lineal simple
linear_model <- lm(log_salario ~ educacion + educacionmadre + edad + 
                     antiguedad_ejecutivo + educacion_grado + antiguedad + 
                     educacion_posgrado + valor_empresa + beneficios_empresa + 
                     ventas_empresa, data = trainData)
linear_pred <- predict(linear_model, newdata = testData)
linear_rmse <- sqrt(mean((y_test - linear_pred)^2))
linear_mae <- mean(abs(y_test - linear_pred))
linear_r2 <- summary(linear_model)$r.squared
linear_mape <- mape(y_test, linear_pred)

# Obtener coeficientes del modelo lineal simple
linear_coefficients <- coef(linear_model)

# Estimar el modelo Lasso
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)
lasso_pred <- predict(lasso_model, s = lasso_model$lambda.min, newx = x_test)
lasso_rmse <- sqrt(mean((y_test - lasso_pred)^2))
lasso_mae <- mean(abs(y_test - lasso_pred))
lasso_r2 <- cor(y_test, lasso_pred)^2
lasso_mape <- mape(y_test, lasso_pred)

# Obtener coeficientes del modelo Lasso
lasso_coefficients <- coef(lasso_model, s = lasso_model$lambda.min)

# Estimar el modelo Ridge
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0)
ridge_pred <- predict(ridge_model, s = ridge_model$lambda.min, newx = x_test)
ridge_rmse <- sqrt(mean((y_test - ridge_pred)^2))
ridge_mae <- mean(abs(y_test - ridge_pred))
ridge_r2 <- cor(y_test, ridge_pred)^2
ridge_mape <- mape(y_test, ridge_pred)

# Obtener coeficientes del modelo Ridge
ridge_coefficients <- coef(ridge_model, s = ridge_model$lambda.min)

# Estimar el modelo Elastic Net
elastic_net_model <- cv.glmnet(x_train, y_train, alpha = 0.5)
elastic_net_pred <- predict(elastic_net_model, s = elastic_net_model$lambda.min, newx = x_test)
elastic_net_rmse <- sqrt(mean((y_test - elastic_net_pred)^2))
elastic_net_mae <- mean(abs(y_test - elastic_net_pred))
elastic_net_r2 <- cor(y_test, elastic_net_pred)^2
elastic_net_mape <- mape(y_test, elastic_net_pred)

# Obtener coeficientes del modelo Elastic Net
elastic_net_coefficients <- coef(elastic_net_model, s = elastic_net_model$lambda.min)

# Comparar los modelos
model_comparison <- data.frame(
  Model = c("Linear", "Lasso", "Ridge", "Elastic Net"),
  RMSE = c(linear_rmse, lasso_rmse, ridge_rmse, elastic_net_rmse),
  MAE = c(linear_mae, lasso_mae, ridge_mae, elastic_net_mae),
  R2 = c(linear_r2, lasso_r2, ridge_r2, elastic_net_r2),
  MAPE = c(linear_mape, lasso_mape, ridge_mape, elastic_net_mape)
)

# Mostrar la comparación
print(model_comparison)

# Graficar la comparación de métricas
model_comparison_long <- melt(model_comparison, id.vars = "Model")

ggplot(model_comparison_long, aes(x = Model, y = value, fill = Model)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Comparación de Métricas entre Modelos",
       x = "Modelo",
       y = "Valor de la Métrica") +
  theme(legend.position = "none")

# Mostrar los coeficientes de los modelos
linear_coefficients
lasso_coefficients
ridge_coefficients
elastic_net_coefficients