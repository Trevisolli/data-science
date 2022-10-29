
"""
Lista de Exercícios Complementares :  Resolvido na Linguagem R para contato e adaptação à mesma.


2.Um analista do mercado acionário coletou os retornos mensais de duas ações que pretende indicar aos seus
clientes. Calcule as estatísticas descritivas para as duas variáveis, incluindo o coeficiente de correlação entre
os retornos. O banco de dados com
os retornos percentuais mensais está na planilha Lista de Exercício
Complementares: aba Exercício 2.

Paulo Trevisolli
26/10/2022

"""

# Setar a Área de Trabalho:
# Nesta área (pasta) deve se econtrar o arquivo CSV que contém nosso Banco de Dados
# Diretório no Windows: E:\Cursos\USP\MBA Datascience e Analytics\97-Scripts R
setwd("E:/Cursos/USP/MBA Datascience e Analytics/97-Scripts R")
getwd()

# Leitura da Base de Dados
df <- read.csv("Exercicio_02.csv", sep=",", h=T)

head(df)

install.packages("dplyr")
library("dplyr")

# Nº Observações
n = nrow(df[df$Meses,])

# Média
media_acao_1 = round(mean(df$Acao.1),4)
media_acao_2 = round(mean(df$Acao.2),4)

# Mediana
mediana_acao_1 = round(median(df$Acao.1),4)
mediana_acao_2 = round(median(df$Acao.2),4)


# Moda
# No caso, o a função table retorna uma lista com os valores (caso a amostra seja pequena)
# e, a frequência de repetição dos mesmos.
# Abaixo, são duas observações 'Amodais', ou seja, não existe nenhuma observação que, uma 
# de suas variáveis (ação 1 ou ação 2), retorne diferente de 1.

moda_acao_1 = table(df$Acao.1)
moda_acao_2 = table(df$Acao.2)

moda_acao_1 != 1
moda_acao_2 != 1

# 1º Quartil
primeiro_quartil_acao_1 = quantile(df$Acao.1,  probs = 0.25)
primeiro_quartil_acao_2 = quantile(df$Acao.2,  probs = 0.25)


# 3º Quartil
terceiro_quartil_acao_1 = quantile(df$Acao.1,  probs = 0.75)
terceiro_quartil_acao_2 = quantile(df$Acao.2,  probs = 0.75)


# 8º Decil
oitavo_decil_acao_1 = quantile(df$Acao.1,  probs = 0.80)
oitavo_decil_acao_2 = quantile(df$Acao.2,  probs = 0.80)

# 9º Decil
nono_decil_acao_1 = quantile(df$Acao.1,  probs = 0.90)
nono_decil_acao_2 = quantile(df$Acao.2,  probs = 0.90)


# 27º Percentil
vigesimo_setimo_perc_acao_1 = quantile(df$Acao.1,  probs = 0.27)
vigesimo_setimo_perc_acao_2 = quantile(df$Acao.2,  probs = 0.27)


# 64º Percentil
sexagesimo_quarto_perc_acao_1 = quantile(df$Acao.1,  probs = 0.64)
sexagesimo_quarto_perc_acao_2 = quantile(df$Acao.2,  probs = 0.64)

# Recuperando e imprimindo todos acima, de uma única vez, com vetor 
quantile(df$Acao.1, probs = c(0.25, 0.75, 0.80, 0.90, 0.27, 0.64))
quantile(df$Acao.2, probs = c(0.25, 0.75, 0.80, 0.90, 0.27, 0.64))


# Valor Mínimo
min_acao_1 = min(df$Acao.1)
min_acao_2 = min(df$Acao.2)

# Valor Máximo
max_acao_1 = max(df$Acao.1)
max_acao_2 = max(df$Acao.2)


# Amplitude
ampl_acao_1 = (max_acao_1 - min_acao_1)
ampl_acao_2 = (max_acao_2 - min_acao_2)


# Variância
# 1 Analisar a distância de cada dado em relação à média e elevá-lo ao quadrado
desvios_acao_1 <- (df$Acao.1 - mean(df$Acao.1))^2
# 2 Realizar a soma dos desvios e, dividí-los por N-1
var_acao_1 <- sum(desvios_acao_1)/(n-1)

# 1 Analisar a distância de cada dado em relação à média e elevá-lo ao quadrado
desvios_acao_2 <- (df$Acao.2 - mean(df$Acao.2))^2
# 2 Realizar a soma dos desvios e, dividí-los por N-1
var_acao_2 <- sum(desvios_acao_2)/(n-1)

# Desvio Padrão
# Raíz quadrada da variância
desvio_padrao_acao_1 = sqrt(var_acao_1)
desvio_padrao_acao_2 = sqrt(var_acao_2)

# Erro Padrão
# S (desvio padrão) / Raiz de N
erro_padrao_acao_1 = desvio_padrao_acao_1 / sqrt(n)
erro_padrao_acao_2 = desvio_padrao_acao_2 / sqrt(n)


# Coeficiente de Variação
# S (desvio padrão) / Média
coef_var_acao_1 = desvio_padrao_acao_1 / media_acao_1
coef_var_acao_2 = desvio_padrao_acao_2 / media_acao_2

# Correlação
correlacao <- cor(df$Acao.1, df$Acao.2, method = "pearson")

# Estatística T Correlação
estatistica_t <- correlacao/sqrt((1-(correlacao^2))/(n-2))

# p-valor (bicaudal)
p_valor <- 2*(1-pt(estatistica_t,(n-2)))

# Valor Crítico ( t Crítico - 5%)
# Tabela t de Student, 5% e 21 Graus de Liberdade 
t_critico <- -qt(0.05/2, 21) 


Descricao_Analise <-  c("Nº Observações",
  "Média",
  "Mediana",
  "Moda",
  "1º Quartil",
  "3º Quartil",
  "8º Decil",
  "9º Decil",
  "27º Percentil",
  "64º Percentil",
  "Valor Mínimo",
  "Valor Máximo",
  "Amplitude",
  "Variância",
  "Desvio Padrão",
  "Erro Padrão",
  "Coeficiente de Variação",
  "Correlação",
  "Estatística T Correlação",
  "p-valor (bicaudal)",
  "Valor Crítico (5%)")

Valor_Analise_Acao_1 <- c(n, 
                          media_acao_1,
                          mediana_acao_1,
                          "Amodal", 
                          primeiro_quartil_acao_1,
                          terceiro_quartil_acao_1, 
                          oitavo_decil_acao_1,
                          nono_decil_acao_1, 
                          vigesimo_setimo_perc_acao_1, 
                          sexagesimo_quarto_perc_acao_1,
                          min_acao_1, 
                          max_acao_1, 
                          ampl_acao_1, 
                          var_acao_1, 
                          desvio_padrao_acao_1, 
                          erro_padrao_acao_1, 
                          coef_var_acao_1,
                          correlacao, 
                          estatistica_t,
                          p_valor, 
                          t_critico)


Valor_Analise_Acao_2 <- c(n, 
                          media_acao_2,
                          mediana_acao_2,
                          "Amodal", 
                          primeiro_quartil_acao_2,
                          terceiro_quartil_acao_2, 
                          oitavo_decil_acao_2,
                          nono_decil_acao_2, 
                          vigesimo_setimo_perc_acao_2, 
                          sexagesimo_quarto_perc_acao_2,
                          min_acao_2, 
                          max_acao_2, 
                          ampl_acao_2, 
                          var_acao_2, 
                          desvio_padrao_acao_2, 
                          erro_padrao_acao_2, 
                          coef_var_acao_2,
                          correlacao, 
                          estatistica_t,
                          p_valor, 
                          t_critico)

df_analise <- data.frame("Tipo de Análise" = Descricao_Analise, 
                         "Ação 1" = Valor_Analise_Acao_1, 
                         "Ação 2" = Valor_Analise_Acao_2)
