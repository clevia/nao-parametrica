# questão 3
#Utilizando o teste Qui-Quadrado para identificar as varáveis que geram atrito

library(readr)
dados <- read_csv("archive/WA_Fn-UseC_-HR-Employee-Attrition.csv")
View(dados)

#testando varíaveis que provavelmente gerem atrito no trabalho

test1 <- chisq.test(dados$Attrition,dados$EnvironmentSatisfaction)
test1
test2 <- chisq.test(dados$Attrition,dados$JobSatisfaction)
test2
test3 <- chisq.test(dados$Attrition,dados$JobInvolvement)
test3
test4 <- chisq.test(dados$Attrition,dados$PerformanceRating)
test4
test5 <- chisq.test(dados$Attrition,dados$WorkLifeBalance)
test5
test6 <- chisq.test(dados$Attrition,dados$Education)
test6

#após os testes foi possível concluir que entre as variaveis testadas
# as que geram atrito de acordo com o teste qui-quadrado são:
#EnvironmentSatisfaction, JobSatisfaction, JobInvolvement e WorkLifeBalance
