donnees<-readRDS("activation2020.rdata")
donnees_cop<-donnees[,-c(1,2,3)]
plot(donnees_cop)

install.packages("corrplot")
library("corrplot")
corrplot(cor(donnees_cop))

