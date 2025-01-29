
library(ggplot2)

dados <- cbind(V1= c(1:6),
      V2= c(-0.82, 0.41, -0.12, 0.08, -0.09, 0.05),
      V3=c(-0.82, -0.43, -0.05, 0.25, -0.20, 0.12)
      )


# Criar o gráfico com barras como linhas
ggplot(dados, aes(x = V1, y = V2)) +
  geom_segment(aes(x = V1, xend = V1, y = 0, yend = V2), 
               color = "blue", lwd = 1) + # Adiciona as barras
  theme_minimal() +
  labs(x = "V1", y = "V2", title = "Gráfico com linhas verticais até o eixo X")

# Criar o gráfico com barras como linhas
ggplot(dados, aes(x = V1, y = V3)) +
  geom_segment(aes(x = V1, xend = V1, y = 0, yend = V2), 
               color = "blue", lwd = 1) + # Adiciona as barras
  theme_minimal() +
  labs(x = "V1", y = "V2", title = "Gráfico com linhas verticais até o eixo X")


library(ggplot2)

dados2 <- cbind(V1= c(1:10),
               V2= c(-0.61, 0.37, -0.05, 0.06, -0.21, 0.11, 0.08, 0.05, 0.12, -0.01)
)


# Criar o gráfico com barras até o eixo X
ggplot(dados2, aes(x = V1, y = V2)) +
  geom_col(width = 0.2, fill = "blue", color = "black") + # Adiciona as barras
  geom_point(size = 3, color = "red") +                  # Adiciona os pontos
  theme_minimal() +
  labs(x = "V1", y = "V2", title = "Gráfico com barras até o eixo X")
