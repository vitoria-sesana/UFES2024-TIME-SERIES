# Aplicação 3 

# descritiva --------------------------------------------------------------

# Os dados clássicos da companhia aérea Box & Jenkins. Totais mensais de 
# passageiros de companhias aéreas internacionais, 1949 a 1960.
# jan de 1949 a dez 1960

serie_passageiros <- AirPassengers
plot.ts(serie_passageiros)
plot(decompose(serie_passageiros))
itsmr::plotc(serie_passageiros); itsmr::plota(serie_passageiros)
 
# tendencia ---------------------------------------------------------------
# teste de Wald-Wolfowitz ou teste de sequências
sequencia_binaria <- diff(serie_passageiros) > 0  
resultado <- runs.test(as.factor(sequencia_binaria))
print(resultado)

# sazonalidade ------------------------------------------------------------
# teste de Kruskal-Wallis 
dados_ano = cbind(serie = as.vector(serie_passageiros), ano = factor(rep(1:12, each = 12)))
resultado <- kruskal.test(serie_passageiros ~ ano, data = dados_ano)
print(resultado)

# médias móveis -----------------------------------------------------------

itsmr::plotc(itsmr::smooth.ma(serie_passageiros, q = 3))
itsmr::plotc(itsmr::smooth.ma(serie_passageiros, q = 12))


# suavização exponencial --------------------------------------------------

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

