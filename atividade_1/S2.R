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



# retorno medio anual -----------------------------------------------------


n <- sp500_monthly %>% nrow()
n <- n/12

((as.numeric(sp500_monthly$GSPC.Close[88]) / as.numeric(sp500_monthly$GSPC.Close[1])) ^ (1/n)) - 1


# retorno simples anualizado ----------------------------------------------

(mean(retornos_mensais)*12)*100


# valor investido ---------------------------------------------------------

valor_inicial <- 1 
cagr <- 0.1314
anos <- 7
# Valor final
valor_final <- valor_inicial * (1 + cagr)^anos
valor_final
