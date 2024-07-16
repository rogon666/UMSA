# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
# Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
# Rolando Gonzales Martinez, Julio 2024
# ==========================================================

# Load necessary library
library(ggplot2)

# Set seed for reproducibility
set.seed(666)

# Generate simulated data
n <- 100  # Number of observations
x <- rnorm(n, mean = 50, sd = 10)  # Predictor variable
y <- 5 + 2 * x + rnorm(n, mean = 0, sd = 5)  # Response variable with some noise

hist(x)
hist(y)

# Combine x and y into a data frame
data <- data.frame(x = x, y = y)

# Fit the linear regression model
model <- lm(y ~ x, data = data)

# Summarize the model
summary(model)

# Plot the data and the regression line
ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Linear Regression Model",
       x = "Predictor (x)",
       y = "Response (y)") +
  theme_minimal()

# -----------------------------------------------------------------------------
