# Aplicação 1

# bibliotecas -------------------------------------------------------------

if(!require(itsmr)) install.packages("itsmr", repos = "http://cran.us.r-project.org")
if(!require(astsa)) install.packages("astsa", repos = "http://cran.us.r-project.org")
if(!require(TSA)) install.packages("TSA", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")
if(!require(lmtest)) install.packages("lmtest", repos = "http://cran.us.r-project.org")
if(!require(car)) install.packages("car", repos = "http://cran.us.r-project.org")
library(dplyr)
library(tseries)

# descrição ----------------------------------------------------------------
# Vendas de vinho tinto australiano, janeiro de 1980 a outubro de 1991

serie_wine <- ts(itsmr::wine, frequency = 12, start = c(1980, 1))
plot.ts(serie_wine)
plot(decompose(serie_wine))

itsmr::plotc(wine); itsmr::plota(wine)


# tendencia ---------------------------------------------------------------
# teste de Wald-Wolfowitz ou teste de sequências
sequencia_binaria <- diff(serie_wine) > 0  
resultado <- runs.test(as.factor(sequencia_binaria))
print(resultado)

# sazonalidade ------------------------------------------------------------
# teste de Kruskal-Wallis 
dados_ano = cbind(serie = as.vector(serie_wine), ano = factor(rep(1:12, each = 12)))
dados_ano = dados_ano[1:142,]
resultado <- kruskal.test(serie_wine ~ ano, data = dados_ano)
print(resultado)

# diferenciação -----------------------------------------------------------

dserie<-diff(serie_wine)
itsmr::plotc(dserie)
itsmr::plota(dserie)
plot(decompose(dserie))


# médias móveis -----------------------------------------------------------
itsmr::plotc(serie_wine)
model1 <- itsmr::smooth.ma(serie_wine, q = 2)
model2 <- itsmr::smooth.ma(serie_wine, q = 8)
model3 <- itsmr::smooth.ma(serie_wine, q = 12)
itsmr::plotc(model1)
itsmr::plotc(model2)
itsmr::plotc(model3)

# suaviação exponencial ---------------------------------------------------
alpha1 <- forecast::ses(serie_wine, alpha = 0.3, h = 12, initial = "optimal")
alpha2 <- forecast::ses(serie_wine, alpha = 0.5, h = 12)
alpha3 <- forecast::ses(serie_wine, alpha = 0.9, h = 12)

alpha1$model
alpha1
# erro de cada ajuste
list(alpha1, alpha2, alpha3) %>% purrr::map(accuracy) 

plot(alpha1, plot.conf=FALSE, ylab = "", main="", fcol="white")
lines(fitted(alpha1), col="blue")
lines(fitted(alpha2), col="red")
lines(fitted(alpha3), col="green")
lines(alpha1$mean, col="blue", type="o")
lines(alpha2$mean, col="red", type="o")
lines(alpha3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"),
       c("serie original", expression(alpha == 0.3),
         expression(alpha == 0.5),
         expression(alpha == 0.9)),
       pch=1)
