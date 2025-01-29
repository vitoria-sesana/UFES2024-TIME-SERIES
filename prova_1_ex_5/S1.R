library(forecast)
library(dplyr)

base <- read.table("prova_1/base/BANESPA.txt")


serie <- diff(base)

serie
# serie 

# Visualizar s série diferenciada
plot(ts(serie))

# Calcular e plotar a ACF
acf(serie, main = "Função de Autocorrelação (ACF)")

# Calcular e plotar a PACF
pacf(serie, main = "Função de Autocorrelação Parcial (PACF)")


# diferenciando -----------------------------------------------------------

# Diferenciar a série
serie_diferenciada <- diff(as.numeric(unlist(base)))

# Visualizar a série diferenciada
plot(ts(serie_diferenciada), main = "Série Diferenciada", ylab = "Valores", xlab = "Tempo")

# Calcular e plotar a ACF da série diferenciada
acf(serie_diferenciada, main = "ACF da Série Diferenciada")

# Calcular e plotar a PACF
pacf(serie, main = "PACF da Série Diferenciada")


# modelagem ---------------------------------------------------------------

modelo_1 <- arima(serie, order = c(1, 1, 1))
modelo_2 <- arima(serie, order = c(2, 1, 1))
modelo_3 <- arima(serie, order = c(2, 1, 2))


# Comparar os modelos com base no AIC
modelos <- list(modelo_1, modelo_2, modelo_3)
aic_values <- sapply(modelos, AIC)
names(aic_values) <- c("ARIMA(1,1,1)", "ARIMA(2,1,1)", "ARIMA(2,1,2)")

# Exibir os AICs
print(aic_values)

# Selecionar o melhor modelo
melhor_modelo <- modelos[[which.min(aic_values)]]
print(melhor_modelo)

xtable::xtable(as.table(aic_values))
