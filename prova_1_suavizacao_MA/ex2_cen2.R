# Exercício 10 - Capitúlo 4

# realizar previsões
# suavização exponencial


# exemplo do livro --------------------------------------------------------

base <- readxl::read_xls("prova_1_suavizacao_MA/bases/atmosfera.xls",sheet = "Plan1")
umidade <- base$umidade
plot.ts(umidade)

# manchas solares ---------------------------------------------------------

sunspot.month %>% View
data(sunspot.month)
plot.ts(ts(sunspot.month, frequency = 12,start = c(2000,1)))

# Números mensais de manchas solares, provenientes do World Data Center, 
# também conhecido como SIDC. Esta é a versão dos 
# dados que será atualizada ocasionalmente quando novas contagens 
# estiverem disponíveis.
# jan de 1749 to setembro de 2013
