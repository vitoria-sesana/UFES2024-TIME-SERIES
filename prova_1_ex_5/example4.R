# Gerar uma série AR(1) como exemplo
set.seed(123) # Para reprodutibilidade
serie_ar1 <- arima.sim(model = list(ar = 0.7), n = 100) # AR(1) com phi = 0.7

# Visualizar a série
plot(serie_ar1, type = "l", main = "Série Temporal AR(1)", ylab = "Valores", xlab = "Tempo")

# Calcular e plotar a ACF
acf(serie_ar1, main = "ACF do Modelo AR(1)")

# Calcular e plotar a PACF
pacf(serie_ar1, main = "PACF do Modelo AR(1)")
