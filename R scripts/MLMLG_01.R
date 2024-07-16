# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2024
# ==========================================================

# Load necessary library
library(ggplot2)

# Set seed for reproducibility
set.seed(666)

# Generate simulated data
n <- 100  # Number of observations
x <- rnorm(n, mean = 50, sd = 10)  # Predictor variable
y <- 5 + 2 * x + rnorm(n, mean = 0, sd = 15)  # Response variable with some noise

hist(x)
hist(y)

# Combine x and y into a data frame
data <- data.frame(x = x, y = y)

# Fit the linear regression model
model <- lm(y ~ x, data = data)

# Summarize the model
summary(model)

# Plot the data and the regression line with a black background, light blue line, and yellow points
ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "yellow") +
  geom_smooth(method = "lm", color = "lightblue") +
  labs(title = "Modelo de regresión lineal",
       x = "variable independiente (x)",
       y = "variable dependiente (y)") +
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid.major = element_line(color = "grey40"),
    panel.grid.minor = element_line(color = "grey20"),
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

# -----------------------------------------------------------------------------
