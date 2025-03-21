library(xtable)

linha <- c("Série", "Log", "Log-diff") 

rownames(tabela_adf_petro) <- linha
rownames(tabela_pp_petro) <- linha

rownames(tabela_adf_IBOV) <- linha
rownames(tabela_pp_IBOV) <- linha

rownames(tabela_adf_CBOND) <- linha
rownames(tabela_pp_CBOND) <- linha


tabela_adf_petro %>% 
  xtable()
tabela_pp_petro %>% 
  xtable()


tabela_adf_IBOV %>% 
  xtable()
tabela_pp_IBOV %>% 
  xtable()



tabela_adf_CBOND %>% 
  xtable()
tabela_pp_CBOND %>% 
  xtable()
