H0: media_CA = media_LA = media_LF
H1: pelo menos um média_j é diferente das demais. 


Y_ij = media_i + erro_ij ## Modelo sob 1
Y_ij = media   + erro_ij ## Modelo sob 0 

Y_ij - media_i  =  erro_ij ## Modelo sob 1
Y_ij - media    =  erro_ij ## Modelo sob 0 



media_i = 1/m (sum_i y_ij)

media  = 1/km (sum_jsum_i y_ij)


##decomposição de quadrados

SQDentro = soma_i sum_j (y_ij- media_i)^2 ##  por grupo
 
SQTotal  = soma_i soma_j (y_ij- media)^2   ##  total

SQEntre  = SQTotal - SQDentro   ## entre os grupos  

# quadrado medio total
QMTotal  = SQTotal/(k*m-1) 
QMDentro = SQDentro/(k*m-k)  ## a favor da H0
QMEntre  = SQEntre/(k-1)     ## captura a diferença entre as medias  

QMEntre > QMDentro  => tenho evidências contra H0

F_obs = QMEntre/QMDentro ~ F(k-1, k(m-1)) ## distribuição F

#Suposicoes 
#1. Y_ij sao independentes
#2. Y_ij distr normal
#3. As variâncias sejam iguais (dos grupos): homocedasticidades (suposicao mais forte)


##https://posgraduando.com/como-fazer-analise-de-variancia-one-way-anova-one-way-no-r/

dados <- read.table("anova.txt", header = TRUE)
plot(dados$Abundancia,dados$Local)
boxplot(dados$Abundancia~dados$Local)

anova <- aov(Abundancia~Local,data=dados)
summary(anova)
shapiro.test(resid(anova))
## H0: Os dados sao normais

install.packages(car)
library(car)

#Teste da homogeneidade das variâncias (a homogeneidade é alcançada com valores acima de p acima de 0,05)
leveneTest(Abundancia~Local,data=dados)
## Variancias iguais


# O resultado da ANOVA foi significativo a um p < 0,05, ou seja, é necessário realizar um teste post-hoc para verificar quais grupos diferem entre si

#Teste de Tukey
TukeyHSD(anova)
