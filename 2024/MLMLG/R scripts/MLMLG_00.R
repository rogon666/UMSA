# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2024
# ==========================================================


# Carga la libreria:
library(ggplot2)

# Defino una semilla:
set.seed(333)

# Generar datos aleatorios de una distribucion t de Student
n <- 1000  # muestra
df <- 4   # grados de libertad (k)
t_data <- rt(n, df)

# Media y desviacion estandar de la distribucion t: 
mean_t <- mean(t_data)
sd_t <- sd(t_data)

# Generando datos aleatorios de una distribucion normal:
normal_data <- rnorm(n, mean = mean_t, sd = sd_t)

# Dataframe para graficar
data <- data.frame(
  value = c(t_data, normal_data),
  distribution = rep(c("distribución t", "distribución normal"), each = n)
)

# Grafico de las distribuciones:
ggplot(data, aes(x = value, fill = distribution)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución t de Student vs Normal",
       x = "Valor",
       y = "Densidad") +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "black"),
    legend.key = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    legend.title = element_blank(),
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "white")
  )
# Muestra los estadisticos: 
cat("Media de la distribucion t:", mean_t, "\n")
cat("Desviacion estandar de la distribucion t:", sd_t, "\n")
