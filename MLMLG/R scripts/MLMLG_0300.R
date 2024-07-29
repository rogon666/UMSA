# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2024
# ==========================================================
#    Simulación de Consistencia de los Estimadores en un 
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


url = "https://raw.githubusercontent.com/rogon666/UMSA/main/MLMLG/datos/crimenes.csv"
download.file(url, destfile = "crimenes_USA.csv")
datos <- read.csv("crimenes_USA.csv")
