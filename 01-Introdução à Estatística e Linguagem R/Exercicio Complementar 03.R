
# 

# Lista de Exercícios Complementares :  Resolvido na Linguagem R para contato e adaptação à mesma.


# 2.Em certo jogo, probabilidade de vitória (sucesso) a cada nova jogada é 1/6. 
# Se forem feitas 10 jogadas,quais são as seguintes probabilidades:
# a) Ter vitória em 4 jogadas.
# b) Ter vitória em pelo menos 7 jogadas.

# Paulo Trevisolli
# 29/10/2022


# Setar a Área de Trabalho:
# Nesta área (pasta) deve se econtrar o arquivo CSV que contém nosso Banco de Dados
# Diretório no Windows: E:\Cursos\USP\MBA Datascience e Analytics\97-Scripts R
setwd("E:/Cursos/USP/MBA Datascience e Analytics/97-Scripts R")
getwd()

# Probabilidade de Sucesso (p)
p <-  (1/6)

# Número de Repetições (n)
n <- 10

# Sucessos de Interesse:
questoes <- c("a", "b")
sucessos <- c(4,7)

resultado <- data.frame(questoes, sucessos)


# A funcao dbinom calcula a probabilidade binomial no R.

# A função completa com os parametros é:
  
#  dbinom(x, size, prob, log = FALSE)

# Onde: x - numero de sucessos desejado, é o “x” na fórmula; size - numero de realizaçoes do evento, ou seja, o “n” da fórmula; prob - probabilidade de sucesso em uma tentativa, o “p” na fórmula.

# Obs.:log=FALSE é o padrão e não precisa ser colocado. Se colocar log=TRUE ele trará o log dos valores.



# Completa o dataframe com os resultados
i <- 1
while(i <= length(resultado$sucessos)) {
  resultado$resposta[i] <- dbinom(x=resultado$sucessos[i], size = n ,prob = p)
  i <- i + 1
}


resultado 

