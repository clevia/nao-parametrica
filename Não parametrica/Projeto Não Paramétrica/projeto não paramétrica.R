if(!require(dplyr)) install.packages("dplyr") 
library(dplyr)
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)
if(!require(psych)) install.packages("psych") 
library(psych)
if(!require(corrplot)) install.packages("corrplot") 
library(corrplot)
require(readr)
require(MVN)
require(funModeling)
rm(list=ls(all=TRUE))

Training_Data <- read_csv("relatorio NP/Training Data.csv")
View(Training_Data)

#inspecionando os dados 

summary(Training_Data)
glimpse(Training_Data)

attach(Training_Data)
str(Training_Data)

profiling_num(Training_Data)

#frequencia de cada variável 
x1<- freq(Training_Data$married);x1
x2<- freq(Training_Data$Id);x2
x3<- freq(Training_Data$income);x3
x4<- freq(Training_Data$age);x4
x5<- freq(Training_Data$experience);x5
x6<- freq(Training_Data$house_ownership);x6
x7<- freq(Training_Data$car_ownership);x7
x8<- freq(Training_Data$profession);x8
x9<- freq(Training_Data$city);x9
x10<- freq(Training_Data$state);x10
x1.1<- freq(Training_Data$current_job_years);x1.1
x12<- freq(Training_Data$current_house_years);x12
x13<- freq(Training_Data$risk_flag);x13
hist(income)
#agora faremos a frequência de cada variável em relação a variável resposta. 
#assim obtemos a tabela de contingêngia para cada par

tabela1<- table(risk_flag,married); tabela1
tabela2<- table(risk_flag,age); tabela2
tabela3<- table(risk_flag,experience); tabela3
tabela4<- table(risk_flag,house_ownership); tabela4
tabela5<- table(risk_flag,car_ownership); tabela5
tabela6<- table(risk_flag,profession); tabela6
tabela7<- table(risk_flag,city); tabela7
tabela8<- table(risk_flag,state); tabela8
tabela9<- table(risk_flag,current_job_years); tabela9
tabela10<- table(risk_flag,current_house_years); tabela10
tabela11<- table(risk_flag,income); tabela11

##############################################################################
bar1<- barplot(tabela1,
        beside = TRUE,
        xlab = "Estado civil",
        ylab = "Frequência",
        col=c(8,6)
        )
legend("topleft",
       title = "Bandeira de Risco",
       legend = c("adimplente","inadimplente"),
       bty = "n",
       fill=c(8,6))
text(bar1,0,tabela1,cex=1, pos=3
)

#################################################################################
bar2<- barplot(tabela2,
               beside = TRUE,
               xlab = "Idade",
               ylab = "Frequência",
               col= c(8,6)
)
legend("topleft",
       title = "Bandeira de Risco",
       legend = c("adimplente","inadimplente"),
       bty = "n",
       fill= c(8,6))
text(bar2,0,tabela2,cex=1, pos=3
)

################################################################################
bar3<- barplot(tabela3,
               beside = TRUE,
               xlab = "Experiência",
               ylab = "Frequência",
               col= c(8,6)
)
legend("topleft",
       title = "Bandeira de Risco",
       legend = c("adimplente","inadimplente"),
       bty = "n",
       fill= c(8,6))
text(bar3,0,tabela3,cex=1, pos=3
)

################################################################################
bar4<- barplot(tabela4,
               beside = TRUE,
               xlab = "Possuir casa",
               ylab = "Frequência",
               col= c(8,6)
)
legend("topleft",
       title = "Bandeira de Risco",
       legend = c("adimplente","inadimplente"),
       bty = "n",
       fill= c(8,6))
text(bar4,0,tabela4,cex=0.80, pos=3
)

################################################################################
bar5<- barplot(tabela5,
               beside = TRUE,
               xlab = "Possuir carro",
               ylab = "Frequência",
               col= c(8,6)
)
legend("topright",
       title = "Bandeira de Risco",
       legend = c("adimplente","inadimplente"),
       bty = "n",
       fill= c(8,6))
text(bar5,0,tabela5,cex=1, pos=3
)
############################################################################
bar6<- barplot(tabela6,
               beside = TRUE,
               xlab = "Profissão",
               ylab = "Frequência",
               col= c(8,6)
)
legend("topleft",
       title = "Bandeira de Risco",
       legend = c("adimplente","inadimplente"),
       bty = "n",
       fill= c(8,6))
text(bar6,0,tabela6,cex=0.80, pos=3
)
##############################################################################

bar7<- barplot(tabela7,
               beside = TRUE,
               xlab = "Cidade",
               ylab = "Frequência",
               col= c(8,6)
)
legend("topleft",
       title = "Bandeira de Risco",
       legend = c("adimplente","inadimplente"),
       bty = "n",
       fill= c(8,6))
text(bar7,0,tabela7,cex=0.80, pos=3
)

################################################################################

bar8<- barplot(tabela8,
               beside = TRUE,
               xlab = "Estado",
               ylab = "Frequência",
               col= c(8,6)
)
legend("topleft",
       title = "Bandeira de Risco",
       legend = c("adimplente","inadimplente"),
       bty = "n",
       fill= c(8,6))
text(bar8,0,tabela8,cex=0.80, pos=3
)

###############################################################################
bar9<- barplot(tabela9,
               beside = TRUE,
               xlab = "Anos de trabalho",
               ylab = "Frequência",
               col= c(8,6)
)
legend("topright",
       title = "Bandeira de Risco",
       legend = c("adimplente","inadimplente"),
       bty = "n",
       fill= c(8,6))
text(bar9,0,tabela9,cex=0.80, pos=3
)
################################################################################
bar10<- barplot(tabela10,
               beside = TRUE,
               xlab = "Anos de casa",
               ylab = "Frequência",
               col= c(8,6)
)
legend("topleft",
       title = "Bandeira de Risco",
       legend = c("adimplente","inadimplente"),
       bty = "n",
       fill= c(8,6))
text(bar10,0,tabela10,cex=0.80, pos=3
)

#############################################################################

#Hipóteses para o teste qui-quadrado

# h0: as variáveis são independentes
# h1: as variáveis são dependentes

options(scipen=999)

teste1<-chisq.test(tabela1); teste1

#residuos
teste1$residuals

#grafico de correlação 
corrplot(teste1$stdres, is.cor = FALSE,method = "color",
         tl.col = "black", tl.srt = 0)

################################################################################

teste2<-chisq.test(tabela2); teste2

#residuos
teste2$residuals

#grafico de correlação 
#corrplot(teste2$stdres, is.cor = FALSE,method = "color",
#         tl.col = "black", tl.srt = 0)


################################################################################

teste3<-chisq.test(tabela3); teste3

#residuos
teste3$residuals

#grafico de correlação 
corrplot(teste3$stdres, is.cor = FALSE,method = "color",
         tl.col = "black", tl.srt = 0)

################################################################################

teste4<-chisq.test(tabela4); teste4

#residuos
teste4$residuals

#grafico de correlação 
corrplot(teste4$stdres, is.cor = FALSE,method = "pie",
         tl.col = "black", tl.srt = 0)

################################################################################

teste5<-chisq.test(tabela5); teste5

#residuos
teste5$residuals

#grafico de correlação 
corrplot(teste5$stdres, is.cor = FALSE,method = "pie",
         tl.col = "black", tl.srt = 0)

################################################################################

teste6<-chisq.test(tabela6); teste6

#residuos
teste6$residuals

#grafico de correlação 
#corrplot(teste6$stdres, is.cor = FALSE,method = "color",
#         tl.col = "black", tl.srt = 0)

################################################################################

teste7<-chisq.test(tabela7); teste7

#residuos
teste7$residuals

#grafico de correlação 
#corrplot(teste7$stdres, is.cor = FALSE,method = "color",
 #        tl.col = "black", tl.srt = 0)

################################################################################

teste8<-chisq.test(tabela8); teste8

#residuos
teste8$residuals

#grafico de correlação 
corrplot(teste8$stdres, is.cor = FALSE,method = "pie",
         tl.col = "black", tl.srt = 0)

################################################################################

teste9<-chisq.test(tabela9); teste9

#residuos
teste9$residuals

#grafico de correlação 
corrplot(teste9$stdres, is.cor = FALSE,method = "pie",
         tl.col = "black", tl.srt = 0)

################################################################################

teste10<-chisq.test(tabela10); teste10

#residuos
teste10$residuals

#grafico de correlação 
corrplot(teste10$stdres, is.cor = FALSE,method = "pie",
         tl.col = "black", tl.srt = 0)

#################################################################################
teste11<-chisq.test(tabela11); teste11

##########################################################################

wilcox.test(age~risk_flag)
wilcox.test(experience~risk_flag)
wilcox.test(current_job_years~risk_flag)
wilcox.test(current_house_years~risk_flag)
wilcox.test(income~risk_flag)
