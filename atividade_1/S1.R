library(dplyr)

# atividade_01 ------------------------------------------------------------

ibov <- read.csv(file = "atividade_1/base/ibovespa_diario.csv") %>% 
  janitor::clean_names() %>% 
  rename(retorno = var) %>% 
  arrange(date) %>% # Garantir que os dados estejam ordenados por data
  mutate(
    return_simple = (price / lag(price)) - 1,   # Retorno simples
    return_log = log(price / lag(price))       # Retorno logarítmico
  ) %>% 
  mutate_all(~ gsub("%", "", .)) %>% 
  mutate_all(~ gsub(",", ".", .)) %>%
  mutate_all(~ gsub("[A-Za-z]", "", .)) %>% 
  mutate(data = as.Date(data, format = "%d.%m.%Y")) %>%
  mutate(var = as.numeric(retorno)) %>% 
  select(data, retorno) %>% 
  mutate(data = as.Date(data)) %>% 
  mutate(log_retorno = log(1+retorno))


# análises iniciais -------------------------------------------------------

ibov$data %>% min
ibov$data %>% max

ibov %>% head; ibov %>% tail()

# gráfico

ibov_serie <- ts(ibov_data$var, 
                 start = c(1994, 7, 4), 
                 end = c(2020, 8, 19),
                 frequency = 365)
ibov_serie

print(ibov_serie)

plot.ts(ibov_serie)

ibov_data %>% 
  ggplot(aes(x = data, y = var)) + 
  geom_line() + 
  geom_point() + 
  # ggthemes::theme_fivethirtyeight() + 
  labs(x = "", y = "Precipitação em polegadas",
       title = "Chuva Anual em Los Angeles, 1878 - 1992",
       subtitle = "Em polegadas de Chuva por Ano")

# atividade 3 -------------------------------------------------------------

petr <- read.csv(file = "atividade_1/base/petr4_diario.csv")