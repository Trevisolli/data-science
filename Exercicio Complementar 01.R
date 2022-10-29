
"""
Lista de Exercícios Complementares :  Resolvido na Linguagem R para contato e adaptação à mesma.


1. Na análise de concessão de empréstimos, uma variável potencialmente importante é a renda da pessoa. O
gerente de um banco coleta uma base de dados de seus correntistas e extrai a variável “renda mensal (R$)”
para 50 pessoas. Embora se trate de uma variável quantitativa, deseja realizar uma análise por meio de tabela
de frequências. Neste sentido, pede-se:

a)Classifique os correntistas em faixas de renda, sendo: 0-2.000; 2.001-4.000; 4.001- 6.000; 6.001-8.000;
8.001-10.000 e 10.001-12.000.

b) Em seguida, elabore a tabela de frequências para as faixas de renda acima.
O banco de dados está na planilha Lista de Exercício Complementares: aba Exercício 1.

Paulo Trevisolli
26/10/2022

"""

# Setar a Área de Trabalho:
# Nesta área (pasta) deve se econtrar o arquivo CSV que contém nosso Banco de Dados
# Diretório no Windows: E:\Cursos\USP\MBA Datascience e Analytics\97-Scripts R\base-dados\
setwd("E:/Cursos/USP/MBA Datascience e Analytics/97-Scripts R/base-dados")

# Criar um Dataframe (Que esteja na Área de Trabalho acima, com o nome: Exercicio_01.csv)
# df <- read.csv("Exercicio_01.csv", sep=",", h=T)

library(readxl)
library(dplyr)

df_excel <- read_excel("Exercicio_01.xlsx")

# Faz o dimensionamento das Faixas 
faixas <- cut(df_excel$`Renda (R$)`,
              breaks =  c(0, 2000, 4000, 6000, 8000, 10000, 12000),
              labels = c("0-2000", "2001-4000", "4001-6000", "6001-8000", "8001-10000", "10000-12000"))

df_excel$Faixa <- faixas


# Quantificar por Faixa  criando o Data Frame 
df <- data.frame( df_excel %>% group_by(Faixa) %>%
  summarize(Count = n()))

# Renomear as colunas no padrão 
df <- rename(df, Faixas.de.Renda = "Faixas de Renda")
df <- rename(df, "Freq.Absoluta" = Count)

# Analisando o Dataframe df
# Show: Visualiza no Console 
show(df)

# View: Visualiza em Modo Gráfico
# Lembrando que o R é Case Sensitive, ou seja, o comando "view" não existe para o R (acusa erro).
View(df)

# Tipos de dados do Dataframe
str(df)


# Resumo do Dataframe
summary(df)

# Exibir o Dataframe 
# Cuidado com o comando "df" em Dataframes muito extensos pois pode causar lentidão etc.
df

# Pra se ter uma noção da estrutura do Dataframe, sem trazer todos os dados, pode-se utilizar o head
head(df)

# Trazer uma coluna específica do Dataframe, por exemplo, Faixas de Renda:
# O comando df[coluna] resulta em um Dataframe 
# Lembrando que o índice do Dataframe se inicia em 1
df[1]
str(df[1])
Col1 = df[1]
# Tipo do Objeto 
str(Col1)
class(Col1)

# Ao selecionar a coluna pelo "$" (Cifrão), o resultado é um CHR  ( e não um Dataframe )
df$Faixas.de.Renda
str(df$Faixas.de.Renda)
Col2 = df$Faixas.de.Renda

# Tipo do Objeto 
str(Col2)
class(Col2)

# Adicionar uma coluna ao Dataframe
# Lembrando que c() que é uma função genérica que combina valores em um vetor ou lista
df$total <- c(0,0,0,0,0,0) 

# Remover uma coluna do Dataframe 
df$total <- NULL
df$V6 <- NULL 

# Soma de uma determinada Coluna
TotalFrequenciaAbsoluta <- sum(df[2])
str(TotalFrequenciaAbsoluta)



# Preenche a Coluna(3) Frequência Relativa (Em %)
df$Freq.Relativa[1] <- ((df$Freq.Absoluta[1] / TotalFrequenciaAbsoluta )*100)
df$Freq.Relativa[2] <- ((df$Freq.Absoluta[2] / TotalFrequenciaAbsoluta )*100)
df$Freq.Relativa[3] <- ((df$Freq.Absoluta[3] / TotalFrequenciaAbsoluta )*100)
df$Freq.Relativa[4] <- ((df$Freq.Absoluta[4] / TotalFrequenciaAbsoluta )*100)
df$Freq.Relativa[5] <- ((df$Freq.Absoluta[5] / TotalFrequenciaAbsoluta )*100)
df$Freq.Relativa[6] <- ((df$Freq.Absoluta[6] / TotalFrequenciaAbsoluta )*100)

# Preenche a Coluna(4) Frequência Acumulada
df$Freq.Acumulada[1] <- df$Freq.Absoluta[1]
df$Freq.Acumulada[2] <- df$Freq.Acumulada[1] + df$Freq.Absoluta[2]
df$Freq.Acumulada[3] <- df$Freq.Acumulada[2] + df$Freq.Absoluta[3]
df$Freq.Acumulada[4] <- df$Freq.Acumulada[3] + df$Freq.Absoluta[4]
df$Freq.Acumulada[5] <- df$Freq.Acumulada[4] + df$Freq.Absoluta[5]
df$Freq.Acumulada[6] <- df$Freq.Acumulada[5] + df$Freq.Absoluta[6]

# Preenche a Coluna(5) Frequência Relativa Acumulada
df$Freq.Relativa.Acumulada[1] <-  df$Freq.Relativa[1]
df$Freq.Relativa.Acumulada[2] <-  df$Freq.Relativa.Acumulada[1] + df$Freq.Relativa[2]
df$Freq.Relativa.Acumulada[3] <-  df$Freq.Relativa.Acumulada[2] + df$Freq.Relativa[3]
df$Freq.Relativa.Acumulada[4] <-  df$Freq.Relativa.Acumulada[3] + df$Freq.Relativa[4]
df$Freq.Relativa.Acumulada[5] <-  df$Freq.Relativa.Acumulada[4] + df$Freq.Relativa[5]
df$Freq.Relativa.Acumulada[6] <-  df$Freq.Relativa.Acumulada[5] + df$Freq.Relativa[6]

# Exibe o Dataframe completo 
df

# Gráfico
# df$Freq.Absoluta -> Dados para criação do gráfico de barras
# main             -> Título do Gráfico 
# col              -> Cor das barras 
# ylim             -> Limite no tamanho das barras (como o valor máximoé 19, setei 0 a 20)
# ylab             -> Texto eixo Y 
# names.arg        -> Descritivo de cada barra (Faixas de Renda)
# las              -> Descritivo da barra na vertical (1=horizontal)
# text             -> Adiciona os valores das barras 
grafico = barplot(df$Freq.Absoluta,
                  main = "Faixa de Renda", 
                  col = "Darkblue", 
                  ylim=c(0,20), 
                  ylab = "Quantidade",
                  names.arg = df$Faixas.de.Renda, las=2)
text(grafico, 1, df$Freq.Absoluta, col = "White")


