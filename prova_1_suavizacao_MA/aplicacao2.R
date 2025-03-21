# Aplicação 2

# dados simulados ---------------------------------------------------------

base_2 <- readxl::read_xlsx(
  path = "prova_1_suavizacao_MA/bases/iboovespa.XLSX", 
  col_names = c("ibovespa"),
  ) %>% 
  mutate(
    ibovespa = as.numeric(gsub(",", ".", ibovespa))
    )

serie_ibov <- ts(base_2$ibovespa, frequency = 30)

# descritiva --------------------------------------------------------------

plot.ts(serie_ibov)
plot(decompose(serie_ibov))

itsmr::plotc(serie_ibov); itsmr::plota(serie_ibov)

# médias móveis -----------------------------------------------------------

media_movel <- function(serie, n) {
  suavizado <- rep(NA, length(serie))  
  for (i in n:length(serie)) {
    suavizado[i] <- mean(serie[(i-n+1):i])
  }
  return(suavizado)
}

n_valores <- c(2, 5, 12)

resultado <- data.frame(Original = serie_ibov)
for (n in n_valores) {
  resultado[[paste0("Suavizado_n", n)]] <- media_movel(serie_ibov, n)
}
print(resultado)

resultado <- resultado %>% 
  mutate(tempo = 1:1499)

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
  suavizado <- numeric(length(serie_ibov))  
  suavizado[1] <- serie_ibov[1]  
  
  for (t in 2:length(serie)) {
    suavizado[t] <- alpha * serie_ibov[t] + (1 - alpha) * suavizado[t - 1]
  }
  return(suavizado)
}

suavizado <- suavizacao_exponencial(serie_ibov, alpha = 0.3)

resultado <- data.frame(Tempo = 1:length(serie_ibov), 
                        Original = serie_ibov, 
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
