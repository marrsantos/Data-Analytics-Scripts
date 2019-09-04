# entendendo o teste t para igualdade de médias:
#
# H_0: média A = média B
# H_1: média A != média B
#
# assumindo variâncias populacionais iguais, a estatística de teste é:
#
# t_obs = (média A - média B) / sqrt( var A / n_A + var B / n_B)

n <- 20
t <- NULL

for (i in 1:100000){

  metric_a <- rnorm(n,10,2)
  metric_b <- rnorm(n,10,2)

  s_d <- sqrt(var(metric_a)/20 + var(metric_b)/20)

  t[i] <- (mean(metric_a) - mean(metric_b))/s_d

}

hist(t,freq=F)

plot(function(x) dt(x, 2*n-2),-5,5, col="red", add=T)

qt(0.025,2*n-2)
qt(0.975,2*n-2)

abline(v = c(qt(0.025,2*n-2),qt(0.975,2*n-2)), col="blue")

# Exemplo 01: 
#   receita de vendas online utilizando dois layouts diferentes  
#                 de página para finalização da compra.

A <- c(182.51,204.89,162.88,192.62,187.25,194.49,231.87,231.32,210.49,179.07)
B <- c(195.30,198.21,197.27,190.19,188.59,198.81,178.59,194.28,182.49,181.65)

med_A <- mean(A)
var_A <- var(A)
n_A <- 10

med_B <- mean(B)
var_B <- var(B)
n_B <- 10

t_obs <- (med_A - med_B) / sqrt( var_A / n_A + var_B / n_B) # estatistica de teste ~ distribuição t-student com 18 graus de liberdade

plot(function(x) dt(x,18),-5,5)
abline(v = t_obs, col="blue")

# região crítica:
qt(0.025, 18)
qt(0.975, 18)
abline(v = c(qt(0.025, 18), qt(0.975, 18)), col="red")

# p valor:
valor_p <- 2*pt(-t_obs, 18)

#alpha = 0.05
# p-valor > nivel de significancia --> não rejeita H_0
# p-valor < nivel de significancia --> rejeita H_0


# função do R para teste t:

# teste de normalidade
# H_0: distribuição é normal
shapiro.test(A)
shapiro.test(B)

t.test(A,B, var.equal = T)


# Exemplo:
#   0: usuario não efetuou uma compra; 
#   1: usuario efetuou uma compra.
# queremos avaliar a proporção em "1".
#
#         A   B
#     0  35  44
#     1  15   6
# total  50  50


# H_0: proporção de compras em A = proporção de compras em B
# H_1: proporção de compras em A != proporção de compras em B


# teste aprox. Normal:
#
#     só é valido para "grandes amostras": np > 5 e np(1-p) > 5.

p1 <- 15/50
p2 <- 6/50

z_obs <- (p1 - p2)/sqrt(p1*(1-p1)/50 + p2*(1-p2)/50)
z_obs

plot(function(x) dnorm(x),-5,5)
abline(v=c(-z_obs, z_obs), col="blue")
p_valor <- 2*pnorm(-z_obs)
p_valor


# teste exato de Fisher

chi_obs <- (100*(35*6 - 44*15 + (1/2)*100)^2) / (50*50*79*21) # estatistica com correção de continuidade
plot(function(x) dchisq(x,1),0,10)
abline(v=chi_obs, col = "blue")
p_valor <- 1-pchisq(chi_obs,1)

prop.test( c(15,6), c(50,50) , conf.level = 0.99)


