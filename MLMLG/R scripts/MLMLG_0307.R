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

# --------------------- Cargar datos ----------------------------------
url = "https://raw.githubusercontent.com/rogon666/UMSA/main/MLMLG/datos/empleados_short.csv"
download.file(url, destfile = "empleados.csv")
datos <- read.csv("empleados.csv")