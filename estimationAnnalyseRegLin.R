donnees<-readRDS("activation2020.rdata")
donnees_cop<-donnees[,-c(1,2,3)]
donnees_cop1<-donnees[,-c(2,3)]

x1=donnees$Age;
x2=donnees$Volume_Cerebral
x3=donnees$Index_Lateralisation_Hemispherique
x4=donnees$Prod_G_Frontal_Inf_Tri_1_R
x5=donnees$Prod_G_Angular_2_R
x6=donnees$Prod_G_Occipital_Lat_1_R
x7=donnees$Prod_G_Rolandic_Oper_1_R
x8=donnees$Prod_G_Hippocampus_1_R
x9=donnees$Prod_S_Sup_Temporal_4_R
x10=donnees$Prod_G_Angular_2_L
x11=donnees$Prod_G_Occipital_Lat_1_L
x12=donnees$Prod_G_Rolandic_Oper_1_L
x13=donnees$Prod_G_Hippocampus_1_L
x14=donnees$Prod_S_Sup_Temporal_4_L

y=donnees$Prod_G_Frontal_Inf_Tri_1_L


res <-lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14)
summary(res)

par(mfrow=c(1,2))
plot(res$fitted,res$residuals)
abline(h=0,col=2)
plot(res$fitted,y)
abline(0,1,col=2)

shapiro.test(res$residuals)
#p-value<5% donc non rejet de H0

drop1(res)
add1(res,y~x1)

#Fonction step : pas à pas descendant (par)
step(res, ~  x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14, trace = FALSE)

#Fonction step : pas à pas ascendant
res2 <- lm(y~ 1, data=donnees)
step(res2,  ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14)


#a la main descendant :
res <-lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14)
summary(res)
drop1(res)
#on enleve x1 -549,96

res <-lm(y ~ x2+x3+x4 + x5+x6+x7+x8+x9+x10+x11+x12+x13+x14)
drop1(res)
#on enleve x2 -551,60

res <-lm(y ~ +x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14)
drop1(res)
#on enleve x11 -552,60

res <-lm(y ~ +x3+x4+x5+x6+x7+x8+x9+x10+x12+x13+x14)
drop1(res)

#on enleve x9 -553.06

res <-lm(y ~ x3+x4+x5+x6+x7+x8+x10+x12+x13+x14)
drop1(res)

#modele a -553.06, enlever d'autres valeurs ne le minimiserait pas


#a la main ascendant :

res <-lm(y ~1 )
add1(res, ~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14)
#ajout de x4 : -480.44

res <-lm(y ~ x4 )
add1(res, ~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13 +x14)
#ajout de x14

res <-lm(y ~ x14 +x4 )
add1(res, ~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13 +x14)
#ajout de x10


res <-lm(y ~x14 +x4 + x10 )
add1(res, ~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13 +x14)
#ajout de x3


res <-lm(y ~ x14 +x4 +x10 + x3 )
add1(res, ~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13 +x14)
#ajout de x13


res <-lm(y ~ x14 +x4 +x10 + x3 + x13)
add1(res, ~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13 +x14)
#ajout de x8

res <-lm(y ~x14 +x4 +x10 + x3 + x13 + x8)
add1(res, ~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13 +x14)
#ajout de x12


res <-lm(y ~ x14 +x4 +x10 + x3 + x13 + x8 + x12)
add1(res, ~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13 +x14)
#ajout de x6 


res <-lm(y ~ x14 +x10 + x12 +x4 + x3 +x13 + x8 + x6)
add1(res, ~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13 +x14)
#ajout de x7 (-552.94)


res <-lm(y ~ x14 +x10 + x12 +x4 + x3 +x13 + x8 + x6 + x7)
add1(res, ~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13 +x14)
#ajout de x5 (-553.06)


res <-lm(y ~ x14 +x10 + x12 +x4 + x3 +x13 + x8 + x6 + x7 + x5)
add1(res, ~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13 +x14)



########### Femme vs Homme #######

#FEMME 

Sexe_F<-donnees[donnees$Sexe=="F",colnames(donnees)!="Sexe"] 
yF=Sexe_F$Prod_G_Frontal_Inf_Tri_1_L

x1F=Sexe_F$Age;
x2F=Sexe_F$Volume_Cerebral
x3F=Sexe_F$Index_Lateralisation_Hemispherique
x4F=Sexe_F$Prod_G_Frontal_Inf_Tri_1_R
x5F=Sexe_F$Prod_G_Angular_2_R
x6F=Sexe_F$Prod_G_Occipital_Lat_1_R
x7F=Sexe_F$Prod_G_Rolandic_Oper_1_R
x8F=Sexe_F$Prod_G_Hippocampus_1_R
x9F=Sexe_F$Prod_S_Sup_Temporal_4_R
x10F=Sexe_F$Prod_G_Angular_2_L
x11F=Sexe_F$Prod_G_Occipital_Lat_1_L
x12F=Sexe_F$Prod_G_Rolandic_Oper_1_L
x13F=Sexe_F$Prod_G_Hippocampus_1_L
x14F=Sexe_F$Prod_S_Sup_Temporal_4_L



#Fonction step : pas à pas descendant (par)
resFemmeD <-lm(yF ~ x1F+x2F+x3F+x4F+x5F+x6F+x7F+x8F+x9F+x10F+x11F+x12F+x13F+x14F)
step(resFemmeD , ~  x1F+x2F+x3F+x4F+x5F+x6F+x7F+x8F+x9F+x10F+x11F+x12F+x13F+x14F)

#Fonction step : pas à pas ascendant
resFemmeA <- lm(yF~ 1, data=Sexe_F)
step(resFemmeA,  ~ x1F+x2F+x3F+x4F+x5F+x6F+x7F+x8F+x9F+x10F+x11F+x12F+x13F+x14F)


#HOMME 

Sexe_H<-donnees[donnees$Sexe=="H",colnames(donnees)!="Sexe"] 
yH=Sexe_H$Prod_G_Frontal_Inf_Tri_1_L

x1H=Sexe_H$Age;
x2H=Sexe_H$Volume_Cerebral
x3H=Sexe_H$Index_Lateralisation_Hemispherique
x4H=Sexe_H$Prod_G_Frontal_Inf_Tri_1_R
x5H=Sexe_H$Prod_G_Angular_2_R
x6H=Sexe_H$Prod_G_Occipital_Lat_1_R
x7H=Sexe_H$Prod_G_Rolandic_Oper_1_R
x8H=Sexe_H$Prod_G_Hippocampus_1_R
x9H=Sexe_H$Prod_S_Sup_Temporal_4_R
x10H=Sexe_H$Prod_G_Angular_2_L
x11H=Sexe_H$Prod_G_Occipital_Lat_1_L
x12H=Sexe_H$Prod_G_Rolandic_Oper_1_L
x13H=Sexe_H$Prod_G_Hippocampus_1_L
x14H=Sexe_H$Prod_S_Sup_Temporal_4_L



#Fonction step : pas à pas descendant (par)
resHommeD <-lm(yH ~ x1H+x2H+x3H+x4H+x5H+x6H+x7H+x8H+x9H+x10H+x11H+x12H+x13H+x14H)
step(resHommeD , ~ x1H+x2H+x3H+x4H+x5H+x6H+x7H+x8H+x9H+x10H+x11H+x12H+x13H+x14H, trace = FALSE)

#Fonction step : pas à pas ascendant
resHommeA <- lm(yH~ 1, data=Sexe_H)
step(resHommeA,  ~ x1H+x2H+x3H+x4H+x5H+x6H+x7H+x8H+x9H+x10H+x11H+x12H+x13H+x14H)





