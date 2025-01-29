# exercicio 19 cap 3
# tendencia e sazonalidade, diferenciacao

# analise grafica
# estimação de T e S
# testes para saber se há tendencia ou nao


library(itsmr)
library(forecast)
library(dplyr)

# exemplo do livro --------------------------------------------------------

base <- readxl0::read_xls("prova_1_suavizacao_MA/bases/temperatura.xls")
temp_ubatuba <- base$Ubatuba
serie <- ts(data = temp_ubatuba, frequency = 12 ,start = c(1976, 1))
plot(base)
plot.ts(serie)


# Exemplo 1: wine ---------------------------------------------------------

# Vendas de vinho tinto australiano, janeiro de 1980 a outubro de 1991

plot(itsmr::wine, xlab='Tempo (Meses)', ylab='10³ litros',
     main= 'Vendas de vinho na Australia', pch=18, col='blue')
lines(1:length(itsmr::wine), itsmr::wine, lty = 2, lwd = 1, col='black')

plot.ts(itsmr::wine)

