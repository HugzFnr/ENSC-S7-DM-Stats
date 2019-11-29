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

#pas à pas ascendant
res <-lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14)
summary(res)
