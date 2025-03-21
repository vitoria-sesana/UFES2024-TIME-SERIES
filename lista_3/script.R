# pacotes -------------------------------------------
library(dplyr)
library(tidyverse)
library(tseries)

# série de preços diários de ações da Petrobrás ------

base1 <- 
  read.table("lista_3/bases/d_petro95.00.txt") %>% 
  as_tibble() %>% 
  mutate(V1 = as.numeric(V1), 
         log = log(V1),
         log_diff = c(0,diff(log))
         )

## gráficos ----

plot.ts(
  ts(base1$V1),
  ylab = "Petrobrás"
  )

plot.ts(
  ts(base1$log),
  ylab = "Log Petrobrás"
  )

plot.ts(
  ts(base1$log_diff),
  ylab = "Log-diferenciada Petrobrás"
)

## testes ----------
adf_1 <- adf.test(base1$V1)
pp_1 <- pp.test(base1$V1)


adf_2 <- adf.test(base1$log)
pp_2 <- pp.test(base1$log)


adf_3 <- adf.test(base1$log_diff)
pp_3 <- pp.test(base1$log_diff)


tabela_adf_petro <- rbind(
  c(adf_1$statistic, adf_1$p.value),
  c(adf_2$statistic, adf_2$p.value),
  c(adf_3$statistic, adf_3$p.value)
) %>% 
  as.data.frame() %>% 
  rename("p-valor" = V2) %>% 
  round(4)

tabela_pp_petro <- rbind(
  c(pp_1$statistic, pp_1$p.value),
  c(pp_2$statistic, pp_2$p.value),
  c(pp_3$statistic, pp_3$p.value)
) %>% 
  as.data.frame() %>% 
  rename("p-valor" = V2) %>% 
  round(4)

# índices mensais ibovespa ---------------------------

base2 <- 
  read.table("lista_3/bases/m-ibv94.01.txt") %>% 
  janitor::row_to_names(1) %>% 
  mutate(
    IBOV = as.numeric(IBOV),
    log = log(IBOV),
    log_diff = c(0,diff(IBOV))
  )


## gráficos -----
plot.ts(
  ts(base2$IBOV),
  ylab = "IBOV"
  )

plot.ts(
  ts(log(base2$IBOV)),
  ylab = "log IBOV"
  )

plot.ts(
  ts(base2$log_diff),
  ylab = "Log-diferenciada IBOV"
)

## testes ----------

adf_1_IBOV <- adf.test(base2$IBOV)
pp_1_IBOV <- pp.test(base2$IBOV)


adf_2_IBOV <- adf.test(base2$log)
pp_2_IBOV <- pp.test(base2$log)


adf_3_IBOV <- adf.test(base2$log_diff)
pp_3_IBOV <- pp.test(base2$log_diff)


tabela_adf_IBOV <- rbind(
  c(adf_1_IBOV$statistic, adf_1_IBOV$p.value),
  c(adf_2_IBOV$statistic, adf_2_IBOV$p.value),
  c(adf_3_IBOV$statistic, adf_3_IBOV$p.value)
) %>% 
  as.data.frame() %>% 
  rename("p-valor" = V2) %>% 
  round(4)

tabela_pp_IBOV <- rbind(
  c(pp_1_IBOV$statistic, pp_1_IBOV$p.value),
  c(pp_2_IBOV$statistic, pp_2_IBOV$p.value),
  c(pp_3_IBOV$statistic, pp_3_IBOV$p.value)
) %>% 
  as.data.frame() %>% 
  rename("p-valor" = V2) %>% 
  round(4)



# dados mensais de juros do C-bond brasileiro ---------

base3 <- 
  read.table("lista_3/bases/m-cbond94.01.txt") %>% 
  janitor::row_to_names(1) %>% 
  mutate(
    CBOND = as.numeric(CBOND),
    log = log(CBOND),
    log_diff = c(0,diff(CBOND))
    )

## gráficos -----

plot.ts(
  ts(base3$CBOND),
  ylab = "CBOND"
  )

plot.ts(
  ts(log(base3$CBOND)),
  ylab = "log CBOND"
  )

plot.ts(
  ts(base3$log_diff),
  ylab = "Log-diferenciada CBOND"
)

## testes ----------

adf_1_CBOND <- adf.test(base3$CBOND)
pp_1_CBOND <- pp.test(base3$CBOND)


adf_2_CBOND <- adf.test(base3$log)
pp_2_CBOND <- pp.test(base3$log)


adf_3_CBOND <- adf.test(base3$log_diff)
pp_3_CBOND <- pp.test(base3$log_diff)


tabela_adf_CBOND <- rbind(
  c(adf_1_CBOND$statistic, adf_1_CBOND$p.value),
  c(adf_2_CBOND$statistic, adf_2_CBOND$p.value),
  c(adf_3_CBOND$statistic, adf_3_CBOND$p.value)
) %>% 
  as.data.frame() %>% 
  rename("p-valor" = V2) %>% 
  round(4)

tabela_pp_CBOND <- rbind(
  c(pp_1_CBOND$statistic, pp_1_CBOND$p.value),
  c(pp_2_CBOND$statistic, pp_2_CBOND$p.value),
  c(pp_3_CBOND$statistic, pp_3_CBOND$p.value)
) %>% 
  as.data.frame() %>% 
  rename("p-valor" = V2) %>% 
  round(4)
