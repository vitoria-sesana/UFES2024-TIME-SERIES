library(dplyr)

# atividade_01 ------------------------------------------------------------

ibov <- read.csv(file = "atividade_1/base/ibovespa_diario.csv") %>% 
  janitor::clean_names() %>% 
  mutate_all(~ gsub("%", "", .)) %>% 
  mutate_all(~ gsub(",", ".", .)) %>%
  mutate_all(~ gsub("[A-Za-z]", "", .)) %>% 
  mutate(data = as.Date(data, format = "%d.%m.%Y")) %>% 
  mutate(data = format(data, "%Y%-%m%-%d"))
  
ibov_data <- ibov %>% 
  select(data, var) %>% 
  mutate(data = as.Date(data))

ibov_data %>% head; ibov_data %>% tail()

ibov_serie <- ts(ibov_data$var, start = c(1994, 7, 4), end = c(2020, 8, 19) ,frequency = 365)
ibov_serie
print(ibov_serie)

# gráfico

plot.ts(ibov_serie)

data(sunspot.month)
sunspot.month
plot.ts(sunspot.month)
# atividade 3 -------------------------------------------------------------

petr <- read.csv(file = "atividade_1/base/petr4_diario.csv")