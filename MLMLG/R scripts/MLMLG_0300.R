# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2024
# ==========================================================
#           Modelo de Regresión de Poisson
# ==========================================================
# Ejemplo de motivacion: crimenes en instituciones post-
# secundarias en los Estados Unidos 
# Fuente: US Department of Education 
# matriculados = personas matriculadas
# tipo = colegio (C) o universidad (U)
# nv = número de delitos violentos cometidos en la universidad o colegio
# nvrate = número de delitos violentos por cada 1000 estudiantes
# matriculados = personas matriculadas, en miles
# region = región del país (C = Central, MO = Medio Oeste, NE = Noreste, SE = Sudeste, SO = Suroeste y O = Oeste)

library(ggplot2)

# --------------------- Cargar datos ----------------------------------
url = "https://raw.githubusercontent.com/rogon666/UMSA/main/MLMLG/datos/crimenes.csv"
download.file(url, destfile = "crimenes_USA.csv")
datos <- read.csv("crimenes_USA.csv")

# ---------------- Exploracion de datos -------------------------------

# Histograma de la variable nv
ggplot(datos, aes(x = nv)) +
  geom_histogram(binwidth = 1, fill = "limegreen", color = "black", alpha = 0.5) +
  labs(title = "Histograma de Número de Crímenes (nv)",
       x = "Número de Crímenes",
       y = "Frecuencia") +
  theme_minimal()

# Grafico de nv contra matriculados
ggplot(datos, aes(x = matriculados, y = nv)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se = FALSE) +
  labs(title = "Número de Crímenes vs Matriculados", x = "Matriculados", y = "Número de Crímenes")

# Grafico de nv contra region
ggplot(datos, aes(x = region, y = nv)) +
  geom_boxplot() +
  labs(title = "Número de Crímenes por Región", x = "Región USA", y = "Número de Crímenes")

# ---------------- Ajuste de regresiones de Poisson --------------------------

# Ajuste de los modelos Poisson
modelo1 <- glm(nv ~ matriculados, data = datos, 
               family = poisson)
modelo2 <- glm(nv ~ region, data = datos, 
               family = poisson)
modelo_completo <- glm(nv ~ matriculados + region, data = datos, 
                       family = poisson)
modelo_nulo <- glm(nv ~ 1, data = datos, family = poisson)

# Devianza de los modelos:
devianza_modelo1 <- with(modelo1, deviance)
devianza_modelo2 <- with(modelo2, deviance)
devianza_completo <- with(modelo_completo, deviance)
devianza_nulo <- with(modelo_nulo, deviance)

# Verosimilitudes logarítmicas de los modelos:
logLik_modelo1 <- logLik(modelo1)
logLik_modelo2 <- logLik(modelo2)
logLik_modelo_completo <- logLik(modelo_completo)
logLik_modelo_nulo <- logLik(modelo_nulo)

# Pseudo R2s basados en la devianza:
pseudoR2_modelo1 <- 1 - devianza_modelo1 / devianza_nulo
pseudoR2_modelo2 <- 1 - devianza_modelo2 / devianza_nulo
pseudoR2_completo <- 1 - devianza_completo / devianza_nulo

# Pseudo R2s de McFadden:
McFpseudoR2_modelo1 <- 1 - as.numeric(logLik_modelo1 / logLik_modelo_nulo)
McFpseudoR2_modelo2 <- 1 - as.numeric(logLik_modelo2 / logLik_modelo_nulo)
McFpseudoR2_completo <- 1 - as.numeric(logLik_modelo_completo / logLik_modelo_nulo)

# Pseudo R2 de Cox y Snell
n <- nrow(datos) # Número de observaciones
L0 <- exp(logLik_modelo_nulo)
L1_M1 <- exp(logLik_modelo1)
L1_M2 <- exp(logLik_modelo2)
L1_MC <- exp(logLik_modelo_completo)
pseudoR2_Cox_Snell_M1 <- as.numeric(1 - (L0 / L1_M1)^(2 / n))
pseudoR2_Cox_Snell_M2 <- as.numeric(1 - (L0 / L1_M2)^(2 / n))
pseudoR2_Cox_Snell_MC <- as.numeric(1 - (L0 / L1_MC)^(2 / n))

# Pseudo R2 de Nagelkerke
pseudoR2_Nagelkerke_M1 <- as.numeric(pseudoR2_Cox_Snell_M1 / (1 - (L0)^(2 / n)))
pseudoR2_Nagelkerke_M2 <- as.numeric(pseudoR2_Cox_Snell_M2 / (1 - (L0)^(2 / n)))
pseudoR2_Nagelkerke_MC <- as.numeric(pseudoR2_Cox_Snell_MC / (1 - (L0)^(2 / n)))

# Pseudo R2 de Efron
mu_modelo_M1 <- predict(modelo1, type = "response")
mu_modelo_M2 <- predict(modelo2, type = "response")
mu_modelo_MC <- predict(modelo_completo, type = "response")
mu_nulo <- predict(modelo_nulo, type = "response")
residuos_M1 <- datos$nv - mu_modelo_M1
residuos_M2 <- datos$nv - mu_modelo_M2
residuos_MC <- datos$nv - mu_modelo_MC
SST <- sum((datos$nv - mu_nulo)^2)
SSR_M1 <- sum(residuos_M1^2)
SSR_M2 <- sum(residuos_M2^2)
SSR_MC <- sum(residuos_MC^2)
pseudoR2_Efron_M1 <- 1 - (SSR_M1 /SST)
pseudoR2_Efron_M2 <- 1 - (SSR_M2 /SST)
pseudoR2_Efron_MC <- 1 - (SSR_MC /SST)

# Imprimir resultados
cat("Devianza del modelo solo con matriculados:", devianza_modelo1, "\n")
cat("Pseudo R2 del modelo solo con matriculados:", pseudoR2_modelo1, "\n\n")
cat("Devianza del modelo solo con region:", devianza_modelo2, "\n")
cat("Pseudo R2 del modelo solo con region:", pseudoR2_modelo2, "\n\n")
cat("Devianza del modelo completo:", devianza_completo, "\n")
cat("Pseudo R2 del modelo completo:", pseudoR2_completo, "\n")
cat("Pseudo R2 de McFadden del modelo solo con matriculados:", McFpseudoR2_modelo1, "\n")
cat("Pseudo R2 de McFadden del modelo solo con region:", McFpseudoR2_modelo2, "\n")
cat("Pseudo R2 de McFadden del modelo completo:", McFpseudoR2_completo, "\n")

# ------------------ Interpretacion de los resultados -------------------------
# Resumen del modelo
summary(modelo_completo)

# Extraer los coeficientes y calcular sus exponentiales
coeficientes <- coef(modelo_completo)
exp_coeficientes <- exp(coeficientes)
prt_coeficientes <- 100*(exp(coeficientes) - 1)

# Cálculo de intervalos de confianza al 95% para los coeficientes
conf_int <- confint(modelo_completo)

# Exponenciar los límites del intervalo de confianza
exp_conf_int <- exp(conf_int)

# Mostrar los resultados
exp_conf_int

# Calcular el cambio porcentual y sus intervalos de confianza
change_pct <- 100 *(exp_conf_int - 1)

# Mostrar los resultados
change_pct

# Mostrar los coeficientes y sus exponentiales
print(coeficientes)
print(exp_coeficientes)
print(prt_coeficientes)

# Interpretación:
# - coeficientes: impacto en el logaritmo de la tasa de ocurrencia
# - exp_coeficientes: cambio en la tasa esperada por unidad de cambio en la variable
# - prt_coeficientes: cambio porcentual en la tasa esperada por unidad de cambio en la variable

# ----------------------- Grafico de ajuste ------------------------------------
# Graficar el modelo ajustado con ambas variables
# Grafico de nv contra matriculados con region como color
ggplot(datos, aes(x = matriculados, y = nv, color = region)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se = FALSE) +
  labs(title = "Número de Crímenes vs Matriculados por Región", x = "Matriculados", y = "Número de Crímenes")

# ---------------------- Medidas de diagnostico --------------------------------
# Residuos de Pearson
residuos_pearson <- residuals(modelo_completo, type = "pearson")

# Gráfico de residuos de Pearson vs. valores ajustados
plot(fitted(modelo_completo), residuos_pearson, 
     xlab = "Valores ajustados", 
     ylab = "Residuos de Pearson", 
     main = "Residuos de Pearson vs. Valores Ajustados")
abline(h = 0, col = "red")

# Sobredispersion
# Estadigrafo de Sobredispersión
Pearson_Chi2 <- sum(residuals(modelo_completo, type = "deviance")^2)
grados_de_libertad <- modelo_completo$df.residual
sobredispersion <- Pearson_Chi2 / grados_de_libertad
if (sobredispersion > 1) {
  cat("Posible sobredispersión detectada. Sobredispersión =", sobredispersion, "\n")
} else {
  cat("No se detectó sobredispersión. Sobredispersión =", sobredispersion, "\n")
}

# Test de sobredispersion basado en el multiplicador de Lagrange:
mmu <- mean(mu_modelo_MC) 
nybar <- n*mmu 
musq <- mu_modelo_MC*mu_modelo_MC
mu2 <- mean(musq)*n
chi_estat <- (mu2 - nybar)^2/(2*mu2) 
pvalue_LM <- pchisq(chi_estat,1,lower.tail = FALSE)
# Null hypothesis: NO EXISTE SOBREDISPERSION
cat("\n Estadigrafo LM (Chi2) = ", chi_estat, "\n p-value (Chi2) = ", pvalue_LM, "\n")

# ---------------------- Modelo binomial negativo ------------------------------
# install.packages("MASS")
library(MASS)

# Ajuste del modelo de binomial negativo
modelo_binomial_negativo <- glm.nb(nv ~ matriculados + region, 
                                   data = datos)

# Resumen del modelo de binomial negativo
summary(modelo_binomial_negativo)

# ---------------------- Modelos con inflacion de ceros ------------------------
# install.packages("pscl")
library(pscl)

# Ajustar el modelo zero-inflated Poisson
modelo_zip <- zeroinfl(nv ~ tipo + region | matriculados, 
                       data = datos, dist = "poisson")

# Ajustar el modelo ZINB
modelo_zinb <- zeroinfl(nv ~ region + tipo | matriculados, 
                        data = datos, dist = "negbin")

# Mostrar el resumen del modelo zero-inflated Poisson (ZIP)
summary(modelo_zip)

# Resumen del modelo ZIP
summary(modelo_zinb)

# --------------------- Modelo de Possion con offset ---------------------------

# Ajustar el modelo de Poisson con un offset
modelo_offset <- glm(nv ~ region + tipo + offset(log(matriculados)), 
                     family = poisson, data = datos)

summary(modelo_offset)

# Estadigrafo de Sobredispersión
Pearson_Chi2 <- sum(residuals(modelo_offset, type = "deviance")^2)
grados_de_libertad <- modelo_offset$df.residual
sobredispersion <- Pearson_Chi2 / grados_de_libertad
if (sobredispersion > 1) {
  cat("Posible sobredispersión detectada. Sobredispersión =", sobredispersion, "\n")
} else {
  cat("No se detectó sobredispersión. Sobredispersión =", sobredispersion, "\n")
}

# Test de sobredispersion basado en el multiplicador de Lagrange:
mu_modelo <- predict(modelo_offset, type = "response")
mmu <- mean(mu_modelo) 
nybar <- n*mmu 
musq <- mu_modelo_MC*mu_modelo
mu2 <- mean(musq)*n
chi_estat <- (mu2 - nybar)^2/(2*mu2) 
pvalue_LM <- pchisq(chi_estat,1,lower.tail = FALSE)
# Null hypothesis: no overdispersion
cat("\n Estadigrafo LM (Chi2) = ", chi_estat, "\n p-value (Chi2) = ", pvalue_LM, "\n")

# ---------- Modelo Poisson con interacciones y transformaciones ---------------

# Estandarizar las variables continuas
datos$zmatriculados <- scale(datos$matriculados)

# Ajuste del Poisson con interacciones
modelo_Poisson_interacciones <- glm(nv ~ zmatriculados * region, 
                                    data = datos, family = poisson)
summary(modelo_Poisson_interacciones)

# Estadigrafo de Sobredispersión
Pearson_Chi2 <- sum(residuals(modelo_Poisson_interacciones, type = "deviance")^2)
grados_de_libertad <- modelo_Poisson_interacciones$df.residual
sobredispersion <- Pearson_Chi2 / grados_de_libertad
if (sobredispersion > 1) {
  cat("Posible sobredispersión detectada. Sobredispersión =", sobredispersion, "\n")
} else {
  cat("No se detectó sobredispersión. Sobredispersión =", sobredispersion, "\n")
}

# Test de sobredispersion basado en el multiplicador de Lagrange:
mu_modelo_interacciones <- predict(modelo_Poisson_interacciones,, type = "response")
mmu <- mean(mu_modelo_interacciones) 
nybar <- n*mmu 
musq <- mu_modelo_MC*mu_modelo_interacciones
mu2 <- mean(musq)*n
chi_estat <- (mu2 - nybar)^2/(2*mu2) 
pvalue_LM <- pchisq(chi_estat,1,lower.tail = FALSE)
# Null hypothesis: no overdispersion
cat("\n Estadigrafo LM (Chi2) = ", chi_estat, "\n p-value (Chi2) = ", pvalue_LM, "\n")

# ----------------------- Modelo de Quasi-Poisson ------------------------------

# Ajuste del modelo quasi-Poisson
modelo_quasi_poisson <- glm(nv ~ matriculados + region, data = datos, 
                            family = quasipoisson)

# Resumen del modelo quasi-Poisson
summary(modelo_quasi_poisson)

# --------------------- Comparacion de modelos no anidados ---------------------
AIC(modelo_zinb, modelo_completo)
BIC(modelo_zinb, modelo_completo)

# Vuong test para comparar modelos no anidados
vuong(modelo_zinb, modelo_completo)