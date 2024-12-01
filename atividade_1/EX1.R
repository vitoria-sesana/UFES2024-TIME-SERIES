## List 1 - Time series I

zt1<-ts(c(15,19,13,17,22,18,22))
mean(zt1)

# Calcula la autocovariancia
acf_values <- acf(zt1, plot = FALSE)

autocovariancias <- acf_values$acf * var(zt1)  # Multiplica por la varianza para obtener autocovariancias
print(autocovariancias)

ct<-autocovariancias

# Covariancia amostral

ct*6*(7^{-1})

### As correlações

acf_values$acf


###### Exemplo


library(tidyverse)
library(tidymodels)
library(dplyr)
library(tidyquant)
library(TSA)
library(tseries)


library(swirl)


#install.packages("TSA","tseries", "astsa","car","ggplot2","stats","tinytex")


data(iris)


data("larain")
heat(larain)
larain <- data.frame(chuva_anual = as.matrix(larain), date = time(larain))

larain$date %>% class()

as.ts()


larain %>% 
  ggplot(aes(x = date, y = chuva_anual)) + 
  geom_line() + 
  geom_point() + 
  # ggthemes::theme_fivethirtyeight() + 
  labs(x = "", y = "Precipitação em polegadas",
       title = "Chuva Anual em Los Angeles, 1878 - 1992",
       subtitle = "Em polegadas de Chuva por Ano")

summary(larain$chuva_anual)
kurtosis(larain$chuva_anual)
skewness(larain$chuva_anual)


par(mfrow=c(2,1))
acf(larain$chuva_anual, main='Função de autocorrelação- Chuva anual- Los Angeles' )
pacf(larain$chuva_anual, main='Função de autocorrelação Parcial- Chuva anual- Los Angeles' )

par(mfrow=c(2,1))
acf(larain$chuva_anual^2, main='Função de autocorrelação- Chuva anual- Los Angeles' )
pacf(larain$chuva_anual^2, main='Função de autocorrelação Parcial- Chuva anual- Los Angeles' )

#############  

library(gridExtra)
library(tidymodels)
data(ames)
attach(ames)
p1<-qplot(log10(Gr_Liv_Area),log10(Sale_Price),data = ames)
p2<-qplot(Latitude,log10(Sale_Price),data = ames)
p3<-qplot(Longitude,log10(Sale_Price),data = ames)
grid.arrange(p1,p2,p3,nrow(1))

tsplot(log10(ames$Sale_Price))
tsplot(ames$Sale_Price)

####### 
ICV <- read_excel("SeriesTemperatura/SeriesTemporais/ICV.xls")

icv<-ts(ICV[[2]],start = c(1970,1),end = c(1980,6), frequency(12))
ts.plot(icv, main='ICV mensal 1970 -1980', ylab="ICV",xlab= "Ano" )

attach(ICV)
t<-as.Date(ICV$`Mes/ano`)
class(t)
dt<-ICV$ICV
dt
class(dt)


serieICV<-as.ts(serie.icv)
ts.plot(serie.icv)


icv<-as.ts(dt)
ts.plot(icv)

dificv<-diff(icv)
ts.plot(dificv)

Dif2icv<-diff(dificv)
ts.plot(Dif2icv)


#######

par(mfrow=c(2,2))
ts.plot(icv)
ricv=diff(log(icv))
ts.plot(ricv)
hist(ricv, nclass= 15, prob= TRUE)
dens=density(ricv,n=200)
points(dens,type= "l")
qqnorm(ricv)
qqline(ricv)

###### Energia ES

EnerES <- read_excel("SeriesTemperatura/SeriesTemporais/SerieEnergia.xlsx")
enerES<-ts(EnerES[1])

par(mfrow=c(2,2))

ts.plot(enerES, main='Energia mensal', ylab="Energiia -ES",xlab= "meses" )
renerES<-diff(log(enerES))
ts.plot(renerES)
hist(renerES, main= 'Histograma Energia mensal', nclass=20, prob= TRUE)
dens1<-density(renerES,n=200)
points(dens1,type="l")
qqnorm(renerES)
qqline(renerES)
qqplot

##### ozonio

oz<-read_excel("SeriesTemperatura/SeriesTemporais/OZONIO.xls")

oz1<-ts(oz[[3]])

ts.plot(oz1)
roz1<-diff(log(oz1))
ts.plot(roz1, main = 'Serie do Ozonio')
hist(roz1, main= 'Histograma Ozonio', nclass=20, prob= TRUE)
dens1<-density(roz1,n=200)
points(dens1,type="l")
acf(roz1,type ="correlation" )

