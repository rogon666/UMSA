# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2026
# ==========================================================
# Simulacion de datos de encuesta de hogares con factores de expansion
# (ponderadores de encuesta): ejemplo para el ingreso en funcion de
# educacion, edad y sexo usando el paquete `survey` de R
# -----------------------------------------------------------------------

# install.packages("survey")  # si hace falta
library(survey)

set.seed(123)

n <- 2000

# -----------------------------------------------------------------------
# 1. Simulacion de microdatos
sex <- factor(sample(c("male", "female"), n, replace = TRUE, prob = c(0.49, 0.51)))
age <- round(runif(n, min = 18, max = 65))
education <- factor(
  sample(c("primary", "secondary", "tertiary"), n, replace = TRUE,
         prob = c(0.30, 0.45, 0.25)),
  levels = c("primary", "secondary", "tertiary")
)

# efecto de la educacion (relativo a primaria)
edu_effect <- c(primary = 0, secondary = 4000, tertiary = 12000)

# simular log-ingreso en funcion de educacion, edad (con rendimientos
# decrecientes), sexo, mas ruido
income <- 8000 +
  edu_effect[as.character(education)] +
  80 * age - 0.7 * age^2 +
  ifelse(sex == "male", 1500, 0) +
  rnorm(n, mean = 0, sd = 3000)

income <- pmax(income, 500)  # piso para evitar ingreso negativo o cero

hist(income)
df <- data.frame(
  sex = sex,
  age = age,
  education = education,
  income = income
)
# -----------------------------------------------------------------------
# 2. Simulacion de estratos y factores de expansion (ponderadores)
# -----------------------------------------------------------------------
# factor de expansion = inverso de la probabilidad de
# seleccion que variar por estrato (region)

region <- factor(sample(paste0("region_", 1:4), n, replace = TRUE))
psu    <- factor(sample(paste0("psu_", 1:100), n, replace = TRUE))

# la probabilidad base de seleccion difiere por region -> factor de expansion = 1/p
sel_prob <- c(region_1 = 0.05, region_2 = 0.08, region_3 = 0.04, region_4 = 0.10)
expansion_factor <- 1 / sel_prob[as.character(region)]
expansion_factor <- expansion_factor * runif(n, 0.9, 1.1)

survey_df <- data.frame(
  income, education, age, sex, region, psu, expansion_factor
)

# -----------------------------------------------------------------------
# 3. Declarar el diseno muestral (survey design)
# -----------------------------------------------------------------------
# ids     = identificador de conglomerado/UPM (usar ~1 si no hay conglomerados)
# strata  = variable de estratificacion
# weights = factores de expansion

design <- svydesign(
  ids     = ~psu,
  strata  = ~region,
  weights = ~expansion_factor,
  data    = survey_df,
  nest    = TRUE
)

summary(design)

# -----------------------------------------------------------------------
# 4. Estadisticas descriptivas ponderadas y no ponderadas

# Ponderadas
svymean(~income, design)
svyby(~income, ~education, design, svymean)
svyby(~income, ~sex, design, svymean)

# No ponderadas
mean(df$income, na.rm = TRUE)
aggregate(income ~ education, data = df, FUN = mean, na.rm = TRUE)
aggregate(income ~ sex, data = df, FUN = mean, na.rm = TRUE)

# -----------------------------------------------------------------------
# 5. Regresion ponderada: income ~ education + age + sex
model <- svyglm(
  income ~ education + age + I(age^2) + sex,
  design = design,
  family = gaussian()
)

summary(model)

# -----------------------------------------------------------------------
# 6. Comparaxion con OLS ingenuo (sin ponderar) 

OLS_model <- lm(income ~ education + age + I(age^2) + sex, data = survey_df)
summary(OLS_model)

# -----------------------------------------------------------------------