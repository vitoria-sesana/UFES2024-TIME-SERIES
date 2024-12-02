library(dplyr)
library(ggplot2)

# atividade_01 ------------------------------------------------------------

ibov <- read.csv(file = "atividade_1/base/iBovespa_MENSAL.csv") %>% 
  janitor::clean_names() %>% 
  rename(retorno = change, preco = price) %>%  
  mutate_all(~ gsub("%", "", .)) %>% 
  mutate_all(~ gsub(",", ".", .)) %>%
  mutate_all(~ gsub("[A-Za-z]", "", .)) %>% 
  mutate(retorno = as.numeric(retorno),
         preco = as.numeric(preco),
         log_retorno = log(preco) - lag(log(preco))) %>% 
  select(date, retorno, log_retorno, preco)


estatisticas_serie <- ibov %>%
  slice(-1) %>% 
  summarise(
    media = mean(log_retorno),
    mediana = median(log_retorno),
    variancia = var(log_retorno),
    assimetria = timeDate::skewness(log_retorno),
    curtose =  timeDate::kurtosis(log_retorno),
    maximo = max(log_retorno),
    minimo = min(log_retorno)
  )

min(ibov$date)

hist(ibov$log_retorno, main = "", xlab = "Preço", ylab="Frequência")

preco <- ts(ibov$log_retorno, 
            start = c(2023, 1, 2), 
            end = c(2024, 1, 12),
            frequency = 365)

tabela_latex <- xtable::xtable(estatisticas_serie, caption = "Estatísticas Descritivas")
print(tabela_latex, include.rownames = FALSE)


