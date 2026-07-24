# ==========================================================
# Maestría en Ciencia y Análisis de Datos
# Universidad Mayor de San Andrés
# ----------------------------------------------------------
#   Modelos lineales y modelos lineales generalizados
# ----------------------------------------------------------
#        Rolando Gonzales Martinez, Julio 2026
# ==========================================================
#       Modelos lineales multiecuacionales: VAR(p)
# ==========================================================

# Instalar una sola vez, si fuera necesario:
# install.packages(c("vars", "ggplot2", "tidyr", "dplyr", "scales"))

library(vars)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# ==========================================================
# 1. Importar y preparar los datos
# ==========================================================

url <- paste0(
  "https://raw.githubusercontent.com/rogon666/UMSA/",
  "refs/heads/main/2026/MLMLG_2026/datos/oil_prices.csv"
)

data <- read.csv(url, stringsAsFactors = FALSE)

# Convertir fechas como Jun-06, Jul-06, etc.
data$date <- as.Date(
  paste0("01-", data$date),
  format = "%d-%b-%y"
)

# Ordenar cronológicamente
data <- data[order(data$date), ]
row.names(data) <- NULL

print(head(data))

# ==========================================================
# 2. Graficar precios y variaciones porcentuales
# ==========================================================

precios_largos <- data |>
  dplyr::select(date, Brent, WTI) |>
  tidyr::pivot_longer(
    cols = c(Brent, WTI),
    names_to = "Serie",
    values_to = "Precio"
  )

ggplot(precios_largos, aes(x = date, y = Precio, linetype = Serie)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Precios mensuales del petróleo",
    x = "Fecha",
    y = "Dólares por barril",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Variaciones porcentuales mensuales
variaciones <- data.frame(
  date   = data$date[-1],
  dBrent = 100 * diff(data$Brent) / head(data$Brent, -1),
  dWTI   = 100 * diff(data$WTI) / head(data$WTI, -1)
)

variaciones <- na.omit(variaciones)
print(head(variaciones))

variaciones_largas <- variaciones |>
  pivot_longer(
    cols = c(dBrent, dWTI),
    names_to = "Serie",
    values_to = "Variacion"
  )

ggplot(variaciones_largas, aes(x = date, y = Variacion, linetype = Serie)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Variaciones porcentuales mensuales",
    x = "Fecha",
    y = "Variación porcentual (%)",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# ==========================================================
# 3. Selección del número de rezagos p del VAR(p)
# ==========================================================

variables_var <- variaciones[, c("dBrent", "dWTI")]

seleccion_rezagos <- VARselect(
  y = variables_var,
  lag.max = 12,
  type = "const"
)

print(seleccion_rezagos$selection)
print(seleccion_rezagos$criteria)

# ==========================================================
# 4. Estimación del modelo VAR(p)
# ==========================================================

orden_p_VAR <- 2 # Akaike, FPE

resultado_var <- VAR(
  y = variables_var,
  p = orden_p_VAR,
  type = "const"
)

summary(resultado_var)

# ==========================================================
# 5. Causalidad de Granger
# ==========================================================

# H0: dBrent no causa en el sentido de Granger a dWTI
causalidad_brent_wti <- causality(
  resultado_var,
  cause = "dBrent"
)

print(causalidad_brent_wti$Granger)

# H0: dWTI no causa en el sentido de Granger a dBrent
causalidad_wti_brent <- causality(
  resultado_var,
  cause = "dWTI"
)

print(causalidad_wti_brent$Granger)

# ==========================================================
# 6. Pronósticos de las variaciones porcentuales
# ==========================================================

horizonte <- 12

pronostico_var <- predict(
  resultado_var,
  n.ahead = horizonte,
  ci = 0.95
)

# Fechas futuras mensuales
ultima_fecha <- max(variaciones$date)
fechas_futuras <- seq(
  from = seq(ultima_fecha, by = "month", length.out = 2)[2],
  by = "month",
  length.out = horizonte
)

pronostico_df <- data.frame(
  date = fechas_futuras,
  Pronostico_dBrent = pronostico_var$fcst$dBrent[, "fcst"],
  Pronostico_dWTI   = pronostico_var$fcst$dWTI[, "fcst"]
)

print(pronostico_df)

# Brent: observado y pronosticado en variaciones
brent_variacion_grafico <- bind_rows(
  data.frame(
    date = variaciones$date,
    Valor = variaciones$dBrent,
    Serie = "Brent observado"
  ),
  data.frame(
    date = pronostico_df$date,
    Valor = pronostico_df$Pronostico_dBrent,
    Serie = "Brent pronosticado"
  )
)

ggplot(brent_variacion_grafico, aes(x = date, y = Valor, linetype = Serie)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Pronóstico VAR de la variación porcentual del Brent",
    x = "Fecha",
    y = "Variación porcentual (%)",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# WTI: observado y pronosticado en variaciones
wti_variacion_grafico <- bind_rows(
  data.frame(
    date = variaciones$date,
    Valor = variaciones$dWTI,
    Serie = "WTI observado"
  ),
  data.frame(
    date = pronostico_df$date,
    Valor = pronostico_df$Pronostico_dWTI,
    Serie = "WTI pronosticado"
  )
)

ggplot(wti_variacion_grafico, aes(x = date, y = Valor, linetype = Serie)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Pronóstico VAR de la variación porcentual del WTI",
    x = "Fecha",
    y = "Variación porcentual (%)",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# ==========================================================
# 7. Convertir los pronósticos porcentuales a niveles
# ==========================================================

ultimo_brent <- tail(data$Brent, 1)
ultimo_wti   <- tail(data$WTI, 1)

factor_brent <- 1 + pronostico_df$Pronostico_dBrent / 100
factor_wti   <- 1 + pronostico_df$Pronostico_dWTI / 100

pronostico_df$Pronostico_Brent <- ultimo_brent * cumprod(factor_brent)
pronostico_df$Pronostico_WTI   <- ultimo_wti * cumprod(factor_wti)

print(pronostico_df)

# Brent en niveles
brent_nivel_grafico <- bind_rows(
  data.frame(
    date = data$date,
    Valor = data$Brent,
    Serie = "Brent observado"
  ),
  data.frame(
    date = pronostico_df$date,
    Valor = pronostico_df$Pronostico_Brent,
    Serie = "Brent pronosticado"
  )
)

ggplot(brent_nivel_grafico, aes(x = date, y = Valor, linetype = Serie)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Pronóstico VAR del precio del Brent",
    x = "Fecha",
    y = "Dólares por barril",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# WTI en niveles
wti_nivel_grafico <- bind_rows(
  data.frame(
    date = data$date,
    Valor = data$WTI,
    Serie = "WTI observado"
  ),
  data.frame(
    date = pronostico_df$date,
    Valor = pronostico_df$Pronostico_WTI,
    Serie = "WTI pronosticado"
  )
)

ggplot(wti_nivel_grafico, aes(x = date, y = Valor, linetype = Serie)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Pronóstico VAR del precio del WTI",
    x = "Fecha",
    y = "Dólares por barril",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Gráfico conjunto de Brent y WTI en niveles
niveles_conjuntos <- bind_rows(
  data.frame(date = data$date, Valor = data$Brent, Serie = "Brent observado"),
  data.frame(date = data$date, Valor = data$WTI, Serie = "WTI observado"),
  data.frame(
    date = pronostico_df$date,
    Valor = pronostico_df$Pronostico_Brent,
    Serie = "Brent pronosticado"
  ),
  data.frame(
    date = pronostico_df$date,
    Valor = pronostico_df$Pronostico_WTI,
    Serie = "WTI pronosticado"
  )
)

ggplot(niveles_conjuntos, aes(x = date, y = Valor, linetype = Serie)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Pronósticos VAR de Brent y WTI en niveles",
    x = "Fecha",
    y = "Dólares por barril",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")