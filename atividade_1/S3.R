# atividade 3 -------------------------------------------------------------

petr <- read.csv(file = "atividade_1/base/petr4_diario.csv") %>% 
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


preco <- ts(petr$preco, 
            start = c(2000, 1, 3), 
            end = c(2000, 12, 27),
            frequency = 365)



# retorno simples ---------------------------------------------------------

((petr$preco[2] - petr$preco[1]) / petr$preco[1] )*100


((petr$preco[7] - petr$preco[1]) / petr$preco[1] )*100



# log retorno -------------------------------------------------------------

(log(petr$preco[6] / petr$preco[5])) * 100



(log(petr$preco[10] / petr$preco[5])) * 100



# ACF ---------------------------------------------------------------------

acf(preco, lag.max = 20, main = "")
