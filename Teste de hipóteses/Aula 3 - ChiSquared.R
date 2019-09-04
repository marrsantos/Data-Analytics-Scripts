# set.seed(1)
# 
# pop <- rnorm(10000,10,2)
# 
# mean((pop-mean(pop))^2)
# 
# variancia = variancian  = NULL
# 
# for(i in 1:70000)
# {
#     amostra          <- sample(pop,10,TRUE)
#     variancia[i]     <- var(amostra)                    ##n-1
#     variancian[i]    <- mean((amostra-mean(amostra))^2) ##n
# }
# 
# mean(variancia)  ## n-1 e'nao viciada para variancia pop
# mean(variancian) ## n e' nao viciada para variancia pop
# 
# hist(variancia, main = "Distribuicao das Variancias Amostrais")
# 
# mean(variancia)
# var(variancia)
# 
# ## Chi quadrado com n-1 graus de liberdade
# 
# 
# x      <- 0:10 
# ProbX  <- dbinom(x,10,0.5)
# 
# plot(x,ProbX)
# abline(v=1)
# abline(v=9)
# 
# pbinom(1,10,0.5,TRUE)
# dbinom(9,10,0.5)+dbinom(10,10,0.5)
# 
# amostra1 <- rbinom(10,1,0.5)
# amostra2 <- rbinom(10,1,0.8)
# 
# #np>5
# #n*(1-p)>5
# 
# pop   <- rbinom(1000,1,0.5)
# mean(pop)
# 
# media <- NULL
# 
# n = 20
# 
# for(i in 1:10000)
# {
#      amostra    <-  sample(pop,n,TRUE)
#      media[i]   <-  mean(amostra)    
# }
# 
# hist(media)
# 
# mean(media) ## media das proporcoes amostrais
# sd(media)
# 
# 
# mu_antes
# mu_depois
# 
# H_o : mu_antes  = mu_depois  =>  mu_depois - mu_antes = 0
# H_1 : mu_antes != mu_depois  =>  mu_depois - mu_antes != 0
# 
# amostraAntes  <- rnorm(1000,10,2)
# amostraDepois <- rnorm(1000,12,2)
# 
# amostraDif    <- amostraDepois-amostraAntes
# 
# hist(amostraDif)
# 
# #Malsuc 
# nMS = 23
# mediaAmostMS = 47
# sMS = 7.2
# 
# #Bemsuc 
# nBS = 30
# mediaAmostMS = 43.9
# sBS = 5.9
# 
# H_O: mBS=mMS   =>  mMS - mBS  = 0
# H_1: mBS>mMS   =>  mMS - mBS > 0
# 
# ((47-43.9) - 0)/sqrt(7.2^2/23 + 5.9^2/30) ### t observado
# ## t observado 1.6777
# ## t critico   1.717 


## TH para Variância
## 1. Comparar com um valor
## 2. Comparar duas variâncias

### Pre

## Vamos ver a cara de uma ChiSQ_(30).
##

X <- rchisq(5000,30) 

hist(X)

mean(X)  ## média da chi quadrado é igual aos graus de liberdade
var(X)   ## variância é igual a 2*GL.

## Outra coisa:

Z2 <- matrix(NA,5000,30)

for(i in 1:30)
{
  Z2[,i] = (rnorm(5000))^2
}

Chi = rowSums(Z2)  ## somando o quadrado de normais 0 1 

hist(Chi)
mean(Chi)
var(Chi)


##1. Comparar com um valor
##
## H_o: var = var_0
## H_1: var != var_0
##
## X tem que ter distribuição Normal.
##
## V = (n-1)S^2/var_ho ~ ChiSQ_(n-1)  ## estatística de teste
##
## H_o: var = 0.3 
## H_1: var < 0.3
##
## n = 31 e s2 = 0.25 amostral
##
##

set.seed(2)
X <- rnorm(5000,0,sqrt(0.3))
S2Amostra <-NULL 


for(i in 1:20000)
{
  Amostra       <- sample(X, 31, replace = T)
  S2Amostra[i]  <- 30*var(Amostra)/0.3
}

hist(S2Amostra)

mean(S2Amostra)
var(S2Amostra)

## Agora olhando para a distribuição que construimos


set.seed(2)

v_obs  = 30*0.25/0.3  ## chi quadrado observado (estatística de teste)
v_obs

mean(S2Amostra<25)  ## p-valor

quantile(S2Amostra,c(0.05))  ## valor crítico 


#### TH
## H_o: var = 0.3 
## H_1: var < 0.3
##
## n = 31 e s2 = 0.25
##
##
V = 30*0.25/0.3
V
##valor crítico

qchisq(0.05,30)

##Qual a decisão?


install.packages(EnvStats)
library(EnvStats)

set.seed(2)

Amostra <- sample(X, 31, replace = T)

varTest(Amostra, alternative = "less", 0.95, sigma.squared = 0.3)

## Comparação de duas variâncias

## Vamos ver a cara da F(n1-1,n2-1)

Fpop <- rf(10000,30,30) ## fisher divisão de duas ChiSq
   
hist(Fpop)
mean(Fpop)
sd(Fpop)

FAmostra = NULL

for(i in 1:20000)
{
  Amostra1       <- sample(X, 31, replace = T)
  Amostra2       <- sample(X, 31, replace = T)
  
  FAmostra[i]  <-  var(Amostra1)/var(Amostra2)
}

hist(FAmostra)
mean(FAmostra)
sd(FAmostra)

#### TH
## H_o: var1 = var2 
## H_1: var1 != var2


 s2_1 = 0.03 # n = 15
 s2_2 = 0.19 # n = 15

F_obs = s2_1/s2_2  ## estatística de teste F ~ F(n1-1, n2-1)

F_critico1 = qf(0.05,14,14)
F_critico2 = qf(0.95,14,14)

mean(FAmostra<F_obs)  # p-valor multiplico por dois em testes bilaterais

quantile(FAmostra,c(0.05,0.95))

## Decisão?

set.seed(1)

x1 <- rnorm(15,30,sqrt(0.03))
x2 <- rnorm(15,30,sqrt(0.19))

boxplot(x1,x2)  ## já temos indícios? 

## à mão

s1 <- var(x1)
s2 <- var(x2)  

F_obs      <- s1/s2
F_critico1 = qf(0.025,14,14)
F_critico2 = qf(0.975,14,14)

between(F_obs,F_critico1,F_critico2)


var.test(x1, x2, ratio = 1,
         alternative = c("two.sided"),
         conf.level = 0.95)


##p_valor

mean(FAmostra<F_obs)
