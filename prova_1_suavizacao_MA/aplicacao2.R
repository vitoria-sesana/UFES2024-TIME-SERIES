# Aplicação 2

# dados simulados ---------------------------------------------------------

n <- 144
t <- 1:n

# sazonalidade
serie1 <- 2*cos(2*pi*t*6/n) + 3*sin(2*pi*t*6/n)
serie2 <- 4*cos(2*pi*t*10/n) + 5*sin(2*pi*t*10/n)
serie3 <- 6*cos(2*pi*t*40/n) + 7*sin(2*pi*t*40/n)
s <- serie1

# erro
y <- arima.sim(n=n,list(order=c(1,0,0),ar=0.1))

# tendencia 
m <- 3 + 0.3*t

# serie simulada
series_simulada <- m + s + y

base <- cbind(series_simulada, m, y)


# descritiva --------------------------------------------------------------

par(mfrow=c(2,2))
plot.ts(y, main="Comp. estocástica")
plot.ts(m, main="Tendência")
plot.ts(s, main="Sazonalidade")
plot.ts(series_simulada, main="Série gerada")

itsmr::plotc(series_simulada); itsmr::plota(series_simulada)

# tendencia ---------------------------------------------------------------
# teste de Wald-Wolfowitz ou teste de sequências
sequencia_binaria <- diff(series_simulada) > 0  
resultado <- runs.test(as.factor(sequencia_binaria))
print(resultado)

# sazonalidade ------------------------------------------------------------
# teste de Kruskal-Wallis 
dados_ano = cbind(serie = as.vector(series_simulada), ano = factor(rep(1:12, each = 12)))
resultado <- kruskal.test(series_simulada ~ ano, data = dados_ano)
print(resultado)

# médias móveis -----------------------------------------------------------

itsmr::plotc(itsmr::smooth.ma(series_simulada, q = 2))
itsmr::plotc(itsmr::smooth.ma(series_simulada, q = 10))


# suavização exponencial --------------------------------------------------

alpha1 <- forecast::ses(series_simulada, alpha = 0.3, h = 12)
alpha2 <- forecast::ses(series_simulada, alpha = 0.5, h = 12)
alpha3 <- forecast::ses(series_simulada, alpha = 0.9, h = 12)

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

