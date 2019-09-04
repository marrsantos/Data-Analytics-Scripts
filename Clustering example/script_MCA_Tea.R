# Import data 
setwd("/home/marrsantos/Dados Oficiais/R scripts _ templates/Clustering example")
tea <- read.table("data_MCA_Tea.csv", header=TRUE, sep=";")
summary(tea)

# Loading FactoMineR
library(FactoMineR)

# Help of the MCA function
?MCA

# MCA with the graphs given by default
res <- MCA(dfcluster, quanti.sup=6)
summary(res, ncp=3)    ## function summary.MCA

# decription of the dimensions
dimdesc(res)

# Graph with some labels
plot(res, label=c("var","quali.sup"), cex=0.7)

# Graphs with selection of elements
plot(res, invisible=c("var","quali.sup"), cex=0.7)
plot(res, invisible=c("ind","quali.sup"),autoLab="y",cex=0.7,title="Active categories")
plot(res, invisible=c("ind","var"),autoLab="y",cex=0.7,title="Supplementary categories")

# Selection of some categories
plot(res, invisible="ind",autoLab="y",cex=0.7,selectMod="cos2 10")
plot(res, invisible="ind",autoLab="y",cex=0.7,selectMod="contrib 20")

# Selection of some individuals
plot(res, invisible=c("var","quali.sup"),autoLab="y",cex=0.7,select="cos2 20")

# Selection of some categories and some individuals
plot(res, autoLab="y",cex=0.7, select="cos2 20", selectMod="cos2 10")

# Graphs of the variables
plot(res, choix="var",xlim=c(0,0.6),ylim=c(0,0.6))
plot(res, choix="var",xlim=c(0,0.6),ylim=c(0,0.6),invisible=c("quali.sup","quanti.sup"))

# Graphs on dimensions 3 and 4
plot(res,invisible=c("var","quali.sup"),cex=0.7,select="contrib 20",axes=3:4)
plot(res, invisible="ind",autoLab="y",cex=0.7,selectMod="cos2 20",axes=3:4)

# Confidence ellipses around the categories for the variables 14 to 17
plotellipses(res,keepvar=c(14:17))

