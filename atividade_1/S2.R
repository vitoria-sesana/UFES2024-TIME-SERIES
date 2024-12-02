library(pacman)
library(dplyr)
library(quantmod)

# Baixar dados do S&P 500
quantmod::getSymbols("^GSPC", src = "yahoo", 
                     from = "1994-05-01", 
                     to = "2001-08-31" )

# Converter para periodicidade mensal
sp500_monthly <- xts::to.monthly(GSPC, indexAt = "lastof", drop.time = TRUE)

# Ver dados mensais
head(sp500_monthly)

# Fechamento ajustado mensal
monthly_close <- quantmod::Cl(sp500_monthly)

# Calcular variação percentual mensal
monthly_returns <- quantmod::Delt(monthly_close) * 100

# Ver variações mensais
retornos <- head(monthly_returns)[-1,]
colnames(retornos) <- c("Retorno")


# análises dos retornos ---------------------------------------------------

# Calcular o retorno mensal (logaritmo das variações de preços)
retornos_mensais <- diff(log(as.numeric(sp500_monthly$GSPC.Close)))


# Calcular o retorno médio mensal
retorno_medio_mensal <- mean(retornos_mensais)

# Anualizar o retorno médio mensal (considerando 12 meses no ano)
retorno_medio_anual <- (1 + retorno_medio_mensal)^12 - 1

# Exibir o retorno médio anual
retorno_medio_anual

