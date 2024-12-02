library(dplyr)
library(ggplot2)

# atividade_01 ------------------------------------------------------------

ibov <- read.csv(file = "atividade_1/base/ibovespa_diario.csv") %>% 
  janitor::clean_names() %>% 
  rename(retorno = var, preco = ultimo) %>%  
  mutate_all(~ gsub("%", "", .)) %>% 
  mutate_all(~ gsub(",", ".", .)) %>%
  mutate_all(~ gsub("[A-Za-z]", "", .)) %>% 
  mutate(data = as.Date(data, format = "%d.%m.%Y")) %>%
  mutate(retorno = as.numeric(retorno),
         preco = as.numeric(preco),
         data = as.Date(data),
         log_retorno = log(preco) - lag(log(preco))) %>% 
  select(data, retorno, log_retorno, preco)
  

# análises iniciais -------------------------------------------------------

ibov$data %>% min
ibov$data %>% max

ibov %>% head
ibov %>% tail()

# gráfico

preco <- ts(ibov$preco, 
              start = c(1994, 7, 4), 
              end = c(2020, 8, 19),
              frequency = 365)


retorno <- ts(ibov$retorno, 
                 start = c(1994, 7, 4), 
                 end = c(2020, 8, 19),
                 frequency = 365)

log_retorno <- ts(ibov$retorno, 
                    start = c(1994, 7, 5), 
                    end = c(2020, 8, 19),
                    frequency = 365)

print(preco)
plot.ts(preco, ylab = "Preços", xlab = "Tempo")

# print(retorno)
# plot.ts(retorno)

print(log_retorno)
plot.ts(log_retorno, ylab = "Log retorno", xlab = "Tempo")
IBOV_grafico_serie_LR

# descritiva --------------------------------------------------------------

estatisticas_serie <- ibov %>%
  summarise(
    media = mean(preco),
    mediana = median(preco),
    variancia = var(preco),
    assimetria = timeDate::skewness(preco),
    curtose =  timeDate::kurtosis(preco)
  )

estatisticas_lr <- ibov %>%
  slice(-1) %>% 
  summarise(
    media = mean(log_retorno),
    mediana = median(log_retorno),
    variancia = var(log_retorno),
    assimetria = timeDate::skewness(log_retorno),
    curtose =  timeDate::kurtosis(log_retorno)
  )


estatisticas <- rbind(estatisticas_serie, estatisticas_lr) 
row.names(estatisticas) <- c('Série', "Log-retorno") 
estatisticas
tabela_latex <- xtable::xtable(estatisticas, caption = "Estatísticas Descritivas")
print(tabela_latex, include.rownames = FALSE)


# histograma --------------------------------------------------------------

hist(ibov$preco,
     xlab = "Preços",
     ylab = "Frequência",
     main = "")

hist(rnorm(length(ibov$preco),
           mean = estatisticas_serie$media, 
           sd = estatisticas_serie$variancia),
     xlab = "Normal",
     ylab = "Frequência",
     main = "")


hist(round(ibov$log_retorno,2),
     xlab = "Log-retornos",
     ylab = "Frequência",
     main = "")

hist(rnorm(length(ibov$log_retorno),
           mean = estatisticas_lr$media, 
           sd = estatisticas_lr$variancia),
     xlab = "Normal",
     ylab = "Frequência",
     main = "")


# da serie, do log retorno e dois histograma com as médias e variâncias das mesmas


# qq-plot -----------------------------------------------------------------
qqnorm(ibov$preco)
qqline(ibov$preco, col = "red")

qqnorm(ibov$log_retorno)
qqline(ibov$log_retorno, col = "red")


# Teste do ruído branco ---------------------------------------------------

# Teste de Ljung-Box
resultado <- Box.test(preco, lag = 20, type = "Ljung-Box")

# Exibir resultados
print(resultado)

# -------------------------------- 
# Teste de Ljung-Box
resultado <- Box.test(log_retorno, lag = 20, type = "Ljung-Box")

# Exibir resultados
print(resultado)

