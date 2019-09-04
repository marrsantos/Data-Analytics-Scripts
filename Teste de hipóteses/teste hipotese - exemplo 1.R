library(data.table)

pop <- c(1,2,3)

hist(pop)

mediapop <- (1+2+3)/3

# desvio => raiz quadrada da varianca
sdpop <- sqrt(((1-2)^2+(2-2)^2+(3-2)^2)/3)

# retirar todas as possíveis amostras de tamanho dois com rep.
#
amostras<-matrix(data=c(1,1,1,2,1,3,2,1,2,2,2,3,3,1,3,2,3,3),ncol=2,nrow=9,byrow=TRUE)
amostras <- cbind(amostras, rowMeans(amostras))  

set.seed(6)  
populacao <- runif(1000) 
#populacao <- rnorm(n=1000,mean=13,sd=2)
#populacao <- rgamma(1000,2,1)  
#populacao <- rbinom(n=1000,size=1,prob=0.1)
# bernouli (0 ou 1)

hist(populacao)  
media <- mean(populacao) 
dp <- sd(populacao)  
mediaAmostra = NULL 
dpAmostra    = NULL  
for(i in 1:20000){     
  Amostra <- sample(populacao, 30, replace = TRUE)     
  mediaAmostra[i] <- mean(Amostra)     
  dpAmostra[i]    <- sd(Amostra) }  

hist(mediaAmostra)  
mean(mediaAmostra)
sd(mediaAmostra)

##N(0.496,0.09)  exemplo de uso 
mean(mediaAmostra<0.48)  
pnorm(0.48,0.496,0.09)
#
# intervalo de confiança 
#

# a média amostral +- a margem de erro
# a margem de erro tem relação com que confiança eu quero acertar
# IC = precisão vs certeza (de estar acertando ou errando)
# IC = com que confiança eu quero acertar?
# margem de erro > o tanto que eu me permito errar dado uma margem de confiança
# erro padrão = desvio padrão das médias
# sempre que estiver calculando variancia e sd usar n-1

### se eu sei o sd da populacao, eu poderia utilizar a distribuição z
set.seed(1)
populacao <- rnorm(10000,10,2)
media <- mean(populacao) 
dp <- sd(populacao)  
ICI=ICS=mediaAmostra = NULL 
n=10

for(i in 1:10000){     
  Amostra <- sample(populacao, n, replace = TRUE)     
  mediaAmostra[i] <- mean(Amostra)     
  ICI[i] <- mediaAmostra[i]-1.96*sd(populacao)/sqrt(n)
  ICS[i] <- mediaAmostra[i]+1.96*sd(populacao)/sqrt(n)
  }  

hist(mediaAmostra)  
mean(mediaAmostra)
sd(mediaAmostra)
mean(between(mean(populacao),ICI,ICS))
# em 95% dos casos a media estara dentro do intervalo mencionado

### se eu não sei o sd da populacao, eu devo utilizar a distribuição t
set.seed(1)
populacao <- rnorm(10000,10,2)
media <- mean(populacao) 
ICI=ICS=mediaAmostra = NULL 
n=10

for(i in 1:10000){     
  Amostra <- sample(populacao, n, replace = TRUE)     
  mediaAmostra[i] <- mean(Amostra)     
  ICI[i] <- mediaAmostra[i]-2.262*sd(Amostra)/sqrt(n)
  ICS[i] <- mediaAmostra[i]+2.262*sd(Amostra)/sqrt(n)
}  

hist(mediaAmostra)  
mean(between(mean(populacao),ICI,ICS))
# em 95% dos casos a media estara dentro do intervalo mencionado

# >>>>>> home work: fazer o mesmo para proporção <<<<<

##################################
# bootstrap (reamostrar a amostra)
#
media = NULL
for(i in 1:10000)
{
  media[i]<-mean(sample(Amostra,10,TRUE))
}
mean(media)
quantile(media, 0.025)
quantile(media,0.975)
# prova
qnorm(0.025,mean(populacao),sd(populacao/sqrt(n)))
qnorm(0.975,mean(populacao),sd(populacao/sqrt(n)))
# >>>>>> home work: fazer o mesmo para proporção <<<<<

