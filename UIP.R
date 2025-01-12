# Cargar bibliotecas necesarias
library(gmm)

# Cargar los datos
data <- read.csv("/Users/alfonsomarin/Downloads/UIP LIMPIECITO .csv")

# Preparar las variables
L_S <- data$L_S         # Log del tipo de cambio
L_S_lag <- dplyr::lag(data$L_S_lag, 1) # Log del tipo de cambio rezagado (usando dplyr)
L_S_fwd <- data$L_S_fwd # Log del tipo de cambio adelantado
RS_diff <- data$RS_diff # Diferencial de tasas de interés
L_S_lag2 <- dplyr::lag(data$L_S_lag, 2)  # L_S_lag rezagado una vez más
RS_diff_lag <- dplyr::lag(data$RS_diff, 1)  # RS_diff rezagado

# Crear la matriz de datos, excluyendo NA generados por el lagging
uip_data <- na.omit(cbind(L_S, L_S_lag, L_S_fwd, RS_diff, L_S_lag2, RS_diff_lag))

# Crear función de momentos considerando todos los instrumentos
uip_moment <- function(params, data) {
  e1 <- params[1]
  beta3 <- params[2]
  
  L_S <- data[, 1]       # Variable dependiente
  L_S_lag <- data[, 2]   # Instrumento: L_S rezagado
  L_S_fwd <- data[, 3]   # Forward log exchange rate
  RS_diff <- data[, 4]   # Diferencial de tasas de interés
  L_S_lag2 <- data[, 5]  # Segundo rezago de L_S
  RS_diff_lag <- data[, 6] # Rezago adicional del diferencial de tasas
  
  # Ecuación UIP
  residual <- L_S - (e1 * L_S_lag + (1 - e1) * L_S_fwd + beta3 * RS_diff)
  
  # Moment conditions considerando los instrumentos
  moment_conditions <- cbind(residual * L_S_lag, 
                             residual * L_S_fwd, 
                             residual * RS_diff, 
                             residual * L_S_lag2, 
                             residual * RS_diff_lag)
  return(moment_conditions)
}

# Estimación por GMM
start_params <- c(e1 = 0.5, beta3 = 0.1)
uip_gmm <- gmm(uip_moment, x = uip_data, t0 = start_params)

# Resultados de la estimación
summary(uip_gmm)


