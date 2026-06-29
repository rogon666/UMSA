# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Agosto 2024
# ==========================================================
#    Metodos Bayesianos: integracion de Monte Carlo
# ==========================================================
simulaciones = 1000
c.sample <- rcauchy(,3,2)
 resultados <- atan(c.sample[c.sample > exp(1) & c.sample < pi]^(1/3))
hist(resultados)
mean(atan(c.sample[c.sample > exp(1) & c.sample < pi]^(1/3)))