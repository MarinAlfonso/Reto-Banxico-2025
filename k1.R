
library(readxl)
library(dplyr)

# Load the data
file_path <- "/Users/alfonsomarin/Desktop/Everything/UNAM/8vo Semestre/Reto Banxico 2025/UIP.xlsx"

data <- read_excel(file_path, sheet = "Datos 2008")

# Set policy rate
policy_rate <- 0.03

# Create Spread and Spread_Lagged variables
data <- data %>%
  mutate(Policy_Rate = policy_rate,
         Spread = TIIE - Policy_Rate,
         Spread_Lagged = lag(Spread))

# Remove missing values caused by lagging
data <- na.omit(data)

# Create differenced variables
data <- data %>%
  mutate(Diff_Spread = Spread - lag(Spread),
         Diff_Spread_Lagged = lag(Diff_Spread))

# Remove missing values caused by differencing
data <- na.omit(data)

# Fit the linear regression model: Diff_Spread ~ Diff_Spread_Lagged
model <- lm(Diff_Spread ~ Diff_Spread_Lagged, data = data)

# Summary of the model
summary(model)

# Augmented Dickey-Fuller (ADF) test on Diff_Spread
library(tseries)
adf_test <- adf.test(data$Diff_Spread, alternative = "stationary")

# Print ADF test results
adf_test

#Aguas aquí porque Dickey fuller apenas pasa a 5% pero a 10 no.
