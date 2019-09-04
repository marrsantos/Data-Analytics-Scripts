library(ggplot2)
pop <- c(1,2,3)
# distribuição da população
hist(pop)
# media população
mediapop <- (1+2+3)/3
# desvio padrao da populaçao (raiz quadrada da variança)
sdpop <- sqrt(((1-2)^2+(2-2)^2+(3-2)^2)/3)
## amostras
# média de todas as amostras possíveis de dois elementos
amostras<-matrix(data=c(1,1,1,2,1,3,2,1,2,2,2,3,3,1,3,2,3,3),ncol=2,nrow=9,byrow=TRUE)
amostras <- cbind(amostras, rowMeans(amostras))  
mediaamostras<-amostras[,3]
# distribuição de frequência de médias
qplot(mediaamostras, geom="histogram") 
