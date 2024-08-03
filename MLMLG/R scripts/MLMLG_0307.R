# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#         Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#    Modelo de Regresión Logistico: ejemplo practico 
# ==========================================================
library(ggplot2)
library(dplyr)

# --------------------- Cargar datos ----------------------------------
url = "https://raw.githubusercontent.com/rogon666/UMSA/main/MLMLG/datos/empleados_short.csv"
url = "https://raw.githubusercontent.com/rogon666/UMSA/main/MLMLG/datos/empleados_balanceado.csv"
download.file(url, destfile = "empleados.csv")
datos <- read.csv("empleados.csv")

# Variable dependiente: renuncia 
# Convertir la columna 'renuncia' a binaria (0 = No, 1 = Si)
datos$renuncia_binaria <- ifelse(datos$renuncia == 'Si', 1, 0)
datos$renuncia_binaria <- as.factor(datos$renuncia_binaria)

# Calcular el número de ceros y unos, y sus porcentajes
conteo_renuncias <- datos %>% 
  group_by(renuncia_binaria) %>% 
  summarise(count = n()) %>% 
  mutate(percentage = count / sum(count) * 100,
         renuncia_binaria = as.factor(renuncia_binaria),
         renuncia_binaria = recode(renuncia_binaria, `0` = "No", `1` = "Si"))

# Crear el gráfico
ggplot(conteo_renuncias, aes(x = renuncia_binaria, y = count, fill = renuncia_binaria)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(count, " (", round(percentage, 1), "%)")), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("No" = "#00BFC4", "Si" = "#F8766D")) +
  labs(title = "Distribución de Renuncias de Empleados", 
       x = "Renuncia", 
       y = "Número de Empleados",
       fill = "Renuncia") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5))

# Ajustar el modelo logit
modelo_logit <- glm(renuncia_binaria ~ edad + viajes + departamento + 
                      educacion + campo_de_educacion + 
                      satisfaccion_ambiente + genero + 
                      involucracion_laboral + rol_laboral + 
                      satisfaccion_laboral + estado_civil + ingreso_mensual + 
                      num_empresas_trabajadas + horas_extras + 
                      incremento_salarial_porcentaje + calificacion_de_rendimiento + 
                      antiguedad + antiguedad_en_el_puesto + 
                      anios_desde_ultimo_ascenso + anios_con_actual_supervisor,
                    family = binomial(link = "logit"), 
                    data = datos)

# Resumen del modelo ajustado
summary(modelo_logit)

coeficientes <- summary(modelo_logit)$coefficients[, 1]
p_values <- summary(modelo_logit)$coefficients[, 4]

# Coeficientes a numero de veces:
exp_beta <- exp(coeficientes)
exp_beta_percent <- 100 * (exp_beta - 1)

resultados <- data.frame(
  coeficientes = coeficientes,
  OR= exp_beta,
  YPercent = exp_beta_percent,
  P_Values = p_values
)
significativas <- resultados %>% filter(P_Values < 0.05)

print("Variables Significativas:")
print(significativas)

# Separar las clases
clase_mayoritaria <- datos %>% filter(renuncia_binaria == 0)
clase_minoritaria <- datos %>% filter(renuncia_binaria == 1)

# Submuestrear la clase mayoritaria para igualar el tamaño de la clase minoritaria
set.seed(123) # Para reproducibilidad
clase_mayoritaria_submuestreada <- clase_mayoritaria %>% sample_n(nrow(clase_minoritaria))

# Combinar las clases submuestreadas
datos_balanceados <- bind_rows(clase_mayoritaria_submuestreada, clase_minoritaria)

# Ajustar el modelo logit
modelo_logit <- glm(renuncia_binaria ~ edad + viajes + departamento + 
                      educacion + campo_de_educacion + 
                      satisfaccion_ambiente + genero + 
                      involucracion_laboral + rol_laboral + 
                      satisfaccion_laboral + estado_civil + ingreso_mensual + 
                      num_empresas_trabajadas + horas_extras + 
                      incremento_salarial_porcentaje + calificacion_de_rendimiento + 
                      antiguedad + antiguedad_en_el_puesto + 
                      anios_desde_ultimo_ascenso + anios_con_actual_supervisor,
                    family = binomial(link = "logit"), 
                    data = datos_balanceados)

summary(modelo_logit)