# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2024
# ==========================================================
# ~~~~~ Evaluación y Diagnóstico de Modelos Lineales ~~~~~~~
# ==========================================================

library(tseries)
library(lmtest)
library(skedastic)
library(sandwich)
library(car)
library(ggplot2)
library(strucchange)  

# Cargando datos:
url <- "https://raw.githubusercontent.com/rogon666/UMSA/main/MLMLG/datos/salarios.csv"
download.file(url, destfile = "salarios.csv")
salarios <- read.csv("salarios.csv")
salarios['lnsalario'] <-log(salarios['salario'])

# ----------- Estimacion del modelo de regresion lineal MCO --------------------
modelo_lineal_MCO <- lm(lnsalario ~ educacion + edad + antiguedad, 
                        data = salarios)
summary(modelo_lineal_MCO)

# ------- Resultados con matrices varianza-covarianza robustas -----------------

# Prueba de normalidad de Jarque-Bera:
jarque_bera_test <- jarque.bera.test(residuals(modelo_lineal_MCO))
print("Prueba de normalidad de Jarque-Bera")
print(jarque_bera_test)

residuos <- residuals(modelo_lineal_MCO)
df_residuos <- data.frame(residuos)
ggplot(df_residuos, aes(x = residuos)) +
  geom_density(aes(y = after_stat(density)), color = "blue", fill = "blue", alpha = 0.4) +
  stat_function(fun = dnorm, args = list(mean = mean(residuos), sd = sd(residuos)), 
                color = "red", linetype = "dashed") +
  labs(title = "Densidad de los Residuos vs Densidad Normal",
       x = "Residuos",
       y = "Densidad") +
  theme_minimal()

# Test de linealidad Harvey-Collier:
test_harvey_collier <- harvtest(modelo_lineal_MCO)    
print("Test de linealidad Harvey-Collier")
print(test_harvey_collier)

# Prueba de homocedasticidad Breusch-Pagan:
test_breusch_pagan <- bptest(modelo_lineal_MCO)
print("Prueba de homocedasticidad de Breusch-Pagan")
print(test_breusch_pagan)

# Prueba de correlación serial Breusch-Godfrey:
breusch_godfrey_test <- bgtest(modelo_lineal_MCO, order = 1) 
print("Prueba de correlación serial de Breusch-Godfrey")
print(breusch_godfrey_test)

# ------- Resultados con matrices varianza-covarianza robustas -----------------

# Matrices de varianza-covarianza robustas a heterocedasticidad
vcov_robusta <- vcovHC(modelo_lineal_MCO, type = "HC1")
# Opciones: HC0, HC1, HC2, HC3, HC4, HC4m, HC5
# HC0: Matriz de White sin ajuste de grados de libertad
# HC1: Ajusta HC0 para evitar el sesgo en muestras pequenas
# HC2: Ajusta HC0 mediante la corrección de los residuos con los elementos diagonales de la matriz de proyección 
# HC3: Ajusta HC0 con una corrección cuadrática de los residuos, aumentando el ajuste en comparación con HC2.
# HC4, HC4m: Corrección que depende del leverage de cada observación, es decir, de cuán influyente es cada punto de datos.
# Obtener los errores estándar robustos y el test de coeficientes
# HC5: Corrección similar a HC4 para casos extremos de observaciones con alto leverage.
coeftest_robusta <- coeftest(modelo_lineal_MCO, vcov = vcov_robusta)
print("Estimación con matriz varianza-covarianza robusta a heterocedasticidad")
print(coeftest_robusta)

# Matriz de varianza-covarianza robusta de Newey-West
vcov_newey_west <- NeweyWest(modelo_lineal_MCO, lag = 1)
coeftest_newey_west <- coeftest(modelo_lineal_MCO, vcov = vcov_newey_west)
print("Estimación con matriz de varianza-covarianza robusta de Newey-West")
print(coeftest_newey_west)

# ---------- Otras patologias en el modelo de regresión lineal -----------------

# Multicolinealidad: factores de inflación de la varianza (VIF):
vifs <- vif(modelo_lineal_MCO)
print("Factores de Inflación de la Varianza (VIF):")
print(vifs)

# CUSUM:
cusum_test <- efp(lnsalario ~ educacion + edad + antiguedad_ejecutivo, data = salarios, type = "Rec-CUSUM")
plot(cusum_test)

# ------------------------------------------------------------------------------