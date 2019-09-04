# tabelaregressao logistica
# Tabela 4 regresão logística bivariada - dados do nascimento
# Prematuro (Sim/Nao)
# 08(30,8)  |  04(05,1)
# 18(69,2)  |  74(94,9)

#p1 = 08
n1<-12
p1<-0.6666667
#p2=18
n2<-92
p2<-0.1956522

SE<-sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))
# 0.142222

ic<-(p2-p1)-1.96*SE
# -0.74978333



