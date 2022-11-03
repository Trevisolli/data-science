
# 

# Lista de Exercícios Complementares :  Resolvido na Linguagem R para contato e adaptação à mesma.

# 5.(Fonte: Fávero e Belfiore, 2017, Cap. 5) Suponha que, em determinado hospital, 3 clientes são operados
# diariamente de cirurgia do estômago, seguindo uma distribuição Poisson.
# Calcule a probabilidade de que 28
# clientes sejam operados na próxima semana (7 dias úteis).

# ####################################################################### # 
# Distribuição Poisson
# ####################################################################### # 

# Paulo Trevisolli
# 29/10/2022

# λ (lambda) => x
# x = taxa média de ocorrências (3 por dia)
x <- 3

# k (quantidade de sucessos de interesse)
# Abaixo foi feito ajuste pra unidade de dias (28 na próxima semana, 7 dias)
k <- 4 

# Resultado 
r = (exp(-x)*(x^(k)))/factorial(k)

# Resultado em percentual (16.8%)
print(round(r*100,2))

