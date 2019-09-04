# Load datasets
#
data_folder <- paste(getwd(), '/', sep='')
mydata <- read.csv(paste(data_folder,"dados_Q5.csv",sep=''), sep=',', header=T)
#
# Hierarchical clustering
# Scaling data
# excluding the categorial variable
mydatascaled <- scale(mydata)

mydatadist <- dist(mydata)
resultH <- hclust(mydatadist)
plot(resultH)
rect.hclust(resultH, k=3)
# creating a vector with the generated cluster classification
resultcl <- cutree(resultH,3)
resultcl
plot(mydata, col=resultcl)
