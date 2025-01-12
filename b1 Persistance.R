# Cargar las librerías necesarias
library(readxl)     # Para leer archivos Excel
library(lmtest)     # Para pruebas de hipótesis
library(sandwich)   # Para errores estándar robustos (HAC)
library(stats)      # Para realizar regresiones

# Cargar los datos desde un archivo Excel
file_path <- "/Users/alfonsomarin/Desktop/Everything/UNAM/8vo Semestre/Reto Banxico 2025/UIP.xlsx"  # Cambiar por la ruta adecuada
datos <- read_excel(file_path, sheet = "Datos 2008")

# Extraer la variable de interés (CDS 5 YEAR)
cds_premium <- datos$`CDS 5 YEAR`

# Transformación logarítmica de CDS premium
# La transformación logarítmica se utiliza para estabilizar la varianza y manejar posibles valores extremos.
log_cds_premium <- log(cds_premium)

# Crear la versión rezagada de la variable dependiente
# La inclusión del rezago captura la persistencia temporal, como se especifica en un modelo AR(1).
log_cds_premium_lag <- c(NA, head(log_cds_premium, -1))  # Crear el rezago de la variable dependiente

# Crear un marco de datos eliminando valores NA
cds_data <- data.frame(
  log_cds_premium = log_cds_premium[-1],         # CDS premium en logaritmo (a partir de la fila 2)
  log_cds_premium_lag = log_cds_premium_lag[-1]  # Rezago del CDS premium
)

# MODELO FINAL: Regresión lineal simple con el rezago como única variable explicativa
# Este modelo se seleccionó porque:
# - La variable log_cds_premium_lag fue altamente significativa en modelos previos.
# - La tendencia temporal fue insignificante y no mejoró el modelo.
# - Un modelo AR(1) captura bien la persistencia observada en los datos.
final_model <- lm(log_cds_premium ~ log_cds_premium_lag, data = cds_data)

# Resumen del modelo final
summary(final_model)

# Calcular errores estándar robustos (HAC) para corregir por posibles problemas de heterocedasticidad o autocorrelación
# Los errores HAC son apropiados ya que no asumimos homocedasticidad estricta.
final_model_hac <- coeftest(final_model, vcov = NeweyWest(final_model))
print(final_model_hac)

# Análisis de residuos
# 1. Gráfico de residuos vs valores ajustados
# Este gráfico verifica si hay patrones claros en los residuos que indiquen problemas de especificación o heterocedasticidad.
plot(fitted(final_model), residuals(final_model), 
     main = "Residuos vs Valores Ajustados", 
     xlab = "Valores Ajustados", 
     ylab = "Residuos")
abline(h = 0, col = "red", lwd = 2)

# 2. Prueba de Breusch-Godfrey para autocorrelación de residuos de orden 2
# Esto asegura que los residuos no tengan autocorrelación, un supuesto clave del modelo AR(1).
bg_test_final <- bgtest(final_model, order = 2)
print(bg_test_final)

# Calcular el criterio de información AIC
# El AIC ayuda a comparar diferentes modelos, con valores más bajos indicando mejor ajuste.
final_aic <- AIC(final_model)
cat("AIC del modelo final:", final_aic, "\n")

# RESULTADOS:
# - El modelo final mostró un R-cuadrado ajustado de aproximadamente 0.60, lo que indica un buen ajuste.
# - La variable rezagada (log_cds_premium_lag) fue altamente significativa (p < 0.001).
# - No se detectaron problemas significativos de autocorrelación o heterocedasticidad en los residuos.
# - El AIC del modelo fue -40.22, el más bajo entre los modelos probados.
