# 4.(Fonte: Fávero e Belfiore, 2017, Cap. 5) Suponha que um aluno acerte três questões a cada cinco testes.
# Seja X o número de tentativas até o décimo segundo acerto. Determine
# a probabilidade de que o aluno
# precise fazer 20 questões para acertar 12

# p (probabilidade de sucesso) = 3/5 Suposição de 3 acertos à cada 5 testes 
p = (3/5) 

# x (número de ensaios)
x = 20

# k (quantidade de sucessos de interesse)
k = 12

# Resultado 
r = (factorial((x-1))/(factorial((k-1))*factorial((x-1)-(k-1))))*(p^k)*((1-p)^(x-k))

# Resultado em percentual (10.78%)
print(round(r*100,2))


      