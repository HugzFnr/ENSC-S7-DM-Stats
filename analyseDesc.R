donnees<-readRDS("activation2020.rdata")
donnees_cop<-donnees[,-c(1,2,3)]
donnees_cop1<-donnees[,-c(2,3)]
plot(donnees_cop)

#install.packages("corrplot")
library("corrplot")
corrplot(cor(donnees_cop))

#tentative d'ACP
#install.packages("PCAmixdata")
library(PCAmixdata)
resACP <- PCAmix(donnees_cop1,graph=FALSE)
round(resACP$eig,digit=2)
#ne conserver que les dim où "eignvalue">1
#donc dim 1,2,3,4,5, et 6?
#recu^peration de 68.59% de l'info ou 75.24% si on garde dim 6

plot(resACP,choice="cor")
#on voit que lateralisation hemispherique opposé à l'aire rolandic donc anticorrelés
#"prod_G_angular" droite et gauche fortement corrélés
plot(resACP,choice="ind") #j'arrive pas à nommer les points

#deuxième test sans succès
res<-PCAmix(X.quanti = donnees_cop1[,1:16], graph=TRUE)
round(res$eig, digit=2)
plot(res,choice="ind")