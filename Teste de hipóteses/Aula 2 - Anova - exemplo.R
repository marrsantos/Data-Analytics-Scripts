##https://posgraduando.com/como-fazer-analise-de-variancia-one-way-anova-one-way-no-r/

dados <- read.table("anova.txt", header = TRUE)
plot(dados$Abundancia,dados$Local)
boxplot(dados$Abundancia~dados$Local)

anova <- aov(Abundancia~Local,data=dados)
summary(anova)
shapiro.test(resid(anova))

install.packages(car)
library(car)

#Teste da homogeneidade das variâncias (a homogeneidade é alcançada com valores acima de p acima de 0,05)
leveneTest(Abundancia~Local,data=dados)

# O resultado da ANOVA foi significativo a um p < 0,05, ou seja, é necessário realizar um teste post-hoc para verificar quais grupos diferem entre si

#Teste de Tukey
TukeyHSD(anova)

