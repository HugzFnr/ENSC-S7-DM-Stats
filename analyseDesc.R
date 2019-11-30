donnees<-readRDS("activation2020.rdata")

###### Visualisation de la corrÈlation des donnÈes #########
donnees_cop<-donnees[,-c(1,2,3)]
donnees_cop1<-donnees[,-c(2,3)]
plot(donnees_cop)

install.packages("corrplot")
library("corrplot")
cor(donnees_cop)
corrplot(cor(donnees_cop))

#Il semberait que Prod_G_Frontal_Inf_Tri_1_L soit corrÈlÈs avecProd_G_Frontal_Inf_Tri_1_R et Prod_S_Sup_Temporal_4_L
cor(donnees$Prod_G_Frontal_Inf_Tri_1_L, donnees$Prod_G_Frontal_Inf_Tri_1_R)
cor(donnees$Prod_G_Frontal_Inf_Tri_1_L, donnees$Prod_S_Sup_Temporal_4_L)


###### ACP ################

install.packages("PCAmixdata")
library(PCAmixdata)
donnees_cop1Numeric <-as.numeric(donnees_cop1)
resACP <- PCAmix(donnees_cop1Numeric,graph=FALSE)
round(resACP$eig,digit=2)
#ne conserver que les dimension "eignvalue">1
#donc dim 1,2,3,4,5, et 6?
#recu^peration de 68.59% de l'info ou 75.24% si on garde dim 6

plot(resACP,choice="cor")
#on voit que lateralisation hemispherique oppos√© √† l'aire rolandic donc anticorrel√©s
#"prod_G_angular" droite et gauche fortement corr√©l√©s
plot(resACP,choice="ind") #j'arrive pas √† nommer les points

#deuxi√®me test sans succ√®s
res<-PCAmix(X.quanti = donnees_cop1[,1:16], graph=TRUE)
round(res$eig, digit=2)
plot(res,choice="ind")



####### Variable qualitatives Boxplots ##########
varExplique <- donnees$Prod_G_Frontal_Inf_Tri_1_L

par(mfrow=c(1,2)) 
boxplot(varExplique~donnees$Preference_Manuelle, xlab="PrÈfÈrence Manuelle", ylab="Aire de Broca", col=c("aliceblue","blanchedalmond"))
boxplot(varExplique~donnees$Sexe, xlab="Sexe", ylab="Aire de Broca", col=c("aliceblue","blanchedalmond"))
testPM <- boxplot(varExplique~donnees$Preference_Manuelle, xlab="PrÈfÈrence Manuelle", ylab="Aire de Broca", col=c("aliceblue","blanchedalmond"), plot=F)$stats
testS <- boxplot(varExplique~donnees$Sexe, xlab="Sexe", ylab="Aire de Broca",col=c("aliceblue","blanchedalmond"), plot=F)$stats

#Test de Student
PM_droite<-donnees[donnees$Preference_Manuelle=="D",colnames(donnees)!="Preference_Manuelle"] 
PM_gauche<-donnees[donnees$Preference_Manuelle=="G",colnames(donnees)!="Preference_Manuelle"] 
t.test(PM_droite$Prod_G_Frontal_Inf_Tri_1_R, PM_gauche$Prod_G_Frontal_Inf_Tri_1_R)


Sexe_F<-donnees[donnees$Sexe=="F",colnames(donnees)!="Sexe"] 
Sexe_H<-donnees[donnees$Sexe=="H",colnames(donnees)!="Sexe"] 
t.test(Sexe_F$Prod_G_Frontal_Inf_Tri_1_R, Sexe_H$Prod_G_Frontal_Inf_Tri_1_R)





#rm(list=ls())