# Aplicação 3 

# descritiva --------------------------------------------------------------
base <- readxl::read_xls("prova_1_suavizacao_MA/bases/atmosfera.xls")
serie_umidade <- ts(base$umidade, frequency = 12)
plot.ts(serie_umidade)
plot(decompose(serie_umidade))
itsmr::plotc(serie_umidade); itsmr::plota(serie_umidade)
 
# médias móveis -----------------------------------------------------------

media_movel <- function(serie, n) {
  suavizado <- rep(NA, length(serie))  
  for (i in n:length(serie)) {
    suavizado[i] <- mean(serie[(i-n+1):i])
  }
  return(suavizado)
}

n_valores <- c(2, 5, 12)

resultado <- data.frame(Original = serie_umidade)
for (n in n_valores) {
  resultado[[paste0("Suavizado_n", n)]] <- media_movel(serie_umidade, n)
}
print(resultado)

resultado <- resultado %>% 
  mutate(tempo = 1:365)

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
  suavizado <- numeric(length(serie_umidade))  
  suavizado[1] <- serie_umidade[1]  
  
  for (t in 2:length(serie)) {
    suavizado[t] <- alpha * serie_umidade[t] + (1 - alpha) * suavizado[t - 1]
  }
  return(suavizado)
}

suavizado <- suavizacao_exponencial(serie_umidade, alpha = 0.3)

resultado <- data.frame(Tempo = 1:length(serie_umidade), 
                        Original = serie_umidade, 
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
