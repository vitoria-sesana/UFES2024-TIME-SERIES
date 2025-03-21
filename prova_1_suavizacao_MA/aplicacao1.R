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
library(ggplot2)
# descrição ----------------------------------------------------------------

base <- readxl::read_xls("prova_1_suavizacao_MA/bases/temperatura.xls")$Ubatuba
serie_temp_ubatuba <- ts(base, frequency = 12, start = c(1976, 1))
plot.ts(serie_temp_ubatuba)
plot(decompose(serie_temp_ubatuba))
itsmr::plotc(serie_temp_ubatuba); itsmr::plota(serie_temp_ubatuba)
serie_temp_ubatuba

# médias móveis -----------------------------------------------------------

media_movel <- function(serie, n) {
  suavizado <- rep(NA, length(serie))  
  for (i in n:length(serie)) {
    suavizado[i] <- mean(serie[(i-n+1):i])
  }
  return(suavizado)
}

n_valores <- c(2, 5, 12)

resultado <- data.frame(Original = serie_temp_ubatuba)
for (n in n_valores) {
  resultado[[paste0("Suavizado_n", n)]] <- media_movel(serie_temp_ubatuba, n)
}
print(resultado)

resultado <- resultado %>% 
  mutate(tempo = 1:120)

dados_long <- tidyr::pivot_longer(resultado, cols = -tempo, names_to = "Série", values_to = "Valor")


ggplot(dados_long, aes(x = tempo, y = Valor, color = Série)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  labs(title = "Série Original e Suavizações",
       x = "Tempo",
       y = "Valor") +
  theme_minimal() +  
  scale_color_manual(values = c("black", "blue", "red", "green")) +  
  theme(legend.title = element_blank())  

# suaviação exponencial ---------------------------------------------------
suavizacao_exponencial <- function(serie, alpha) {
  suavizado <- numeric(length(serie_temp_ubatuba))  
  suavizado[1] <- serie_temp_ubatuba[1]  
  
  for (t in 2:length(serie)) {
    suavizado[t] <- alpha * serie_temp_ubatuba[t] + (1 - alpha) * suavizado[t - 1]
  }
  return(suavizado)
}

suavizado <- suavizacao_exponencial(serie_temp_ubatuba, alpha = 0.3)

resultado <- data.frame(Tempo = 1:length(serie_temp_ubatuba), 
                        Original = serie_temp_ubatuba, 
                        Suavizado = suavizado)


print(resultado)

dados_long <- tidyr::pivot_longer(resultado, cols = -Tempo, names_to = "Série", values_to = "Valor")

ggplot(dados_long, aes(x = Tempo, y = Valor, color = Série)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  labs(title = "Série Original e Suavização Exponencial",
       x = "Tempo",
       y = "Valor") +
  theme_minimal() +  
  scale_color_manual(values = c("black", "blue")) +  
  theme(legend.title = element_blank())  
