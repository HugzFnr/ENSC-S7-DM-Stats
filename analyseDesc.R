donnees<-readRDS("activation2020.rdata")

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")

rownames(donnees) = donnees$Sujet

###### Visualisation de la corr?lation des donn?es #########

donnees_cop<-donnees[4:18]

plot(donnees_cop)

#install.packages("corrplot")
library("corrplot")
cor(donnees_cop)
corrplot(cor(donnees_cop))

#Il semberait que Prod_G_Frontal_Inf_Tri_1_L soit corr?l?s avecProd_G_Frontal_Inf_Tri_1_R et Prod_S_Sup_Temporal_4_L
cor(donnees$Prod_G_Frontal_Inf_Tri_1_L, donnees$Prod_G_Frontal_Inf_Tri_1_R)
cor(donnees$Prod_G_Frontal_Inf_Tri_1_L, donnees$Prod_S_Sup_Temporal_4_L)


             
###### ACP ################

#install.packages("PCAmixdata")
library(PCAmixdata)


res <- PCAmix(X.quanti = donnees_cop, graph=FALSE)
round(res$eig, digit=2)

#contributions aux composantes principales
round(res$quanti$contrib.pct,digit=2)
res$levels$contrib.pct
par(mfrow=c(1,1))

#graphiques ACP
plot(resACP,axes=c(1,2),choice="cor")
plot(resACP,axes=c(1,2),choice="ind")

##colorer les graphiques en fonction du cos2 : qualité de représentation

#install.packages("FactoMineR")
library("FactoMineR")
library("factoextra")

donnees_cop<-donnees[,-c(1,2,3)]

res.pca <- PCA(donnees_cop, graph = FALSE)

# cercle de covariance
fviz_pca_var(res.pca, axes = c(1, 2),col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

#graphique des individus AVEC labels
fviz_pca_ind (res.pca, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = FALSE # Évite le chevauchement de texte
)




# ####### Variable qualitatives Boxplots ##########
varExplique <- donnees$Prod_G_Frontal_Inf_Tri_1_L

#boxplot 
par(mfrow=c(1,2))
boxplot(varExplique~donnees$Preference_Manuelle, xlab="Pr?f?rence Manuelle", ylab="Aire de Broca", col=c("aliceblue","blanchedalmond"))
boxplot(varExplique~donnees$Sexe, xlab="Sexe", ylab="Aire de Broca", col=c("aliceblue","blanchedalmond"))
testPM <- boxplot(varExplique~donnees$Preference_Manuelle, xlab="Pr?f?rence Manuelle", ylab="Aire de Broca", col=c("aliceblue","blanchedalmond"), plot=F)$stats
testS <- boxplot(varExplique~donnees$Sexe, xlab="Sexe", ylab="Aire de Broca",col=c("aliceblue","blanchedalmond"), plot=F)$stats

#Test de Student
PM_droite<-donnees[donnees$Preference_Manuelle=="D",colnames(donnees)!="Preference_Manuelle"]
PM_gauche<-donnees[donnees$Preference_Manuelle=="G",colnames(donnees)!="Preference_Manuelle"]
t.test(PM_droite$Prod_G_Frontal_Inf_Tri_1_R, PM_gauche$Prod_G_Frontal_Inf_Tri_1_R)

#séparation en fonction des sexes
Sexe_F<-donnees[donnees$Sexe=="F",colnames(donnees)!="Sexe"]
Sexe_H<-donnees[donnees$Sexe=="H",colnames(donnees)!="Sexe"]
t.test(Sexe_F$Prod_G_Frontal_Inf_Tri_1_R, Sexe_H$Prod_G_Frontal_Inf_Tri_1_R)





#rm(list=ls())