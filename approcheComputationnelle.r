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

M1 <-lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14)
summary(res)

MSE_M1 = 0.5 * sum((y-M1$fitted.values)^2)
MSE_M1

#Les 100 MSE obtenus par permutation de chacune des variables explicatives
MSE_X1 <- c()
MSE_X2 <- c()
MSE_X3 <- c()
MSE_X4 <- c()
MSE_X5 <- c()
MSE_X6 <- c()
MSE_X7 <- c()
MSE_X8 <- c()
MSE_X9 <- c()
MSE_X10 <- c()
MSE_X11 <- c()
MSE_X12 <- c()
MSE_X13 <- c()
MSE_X14 <- c()

for (iter in 1:101)
{
  s <- sample(x1,replace=FALSE)
  m <- lm (y ~ s+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14)
  MSE_X1 <- append(MSE_X1,0.5 * sum((y-m$fitted.values)^2),length(MSE_X1))
}

for (iter in 1:101)
{
  s <- sample(x2,replace=FALSE)
  m <- lm (y ~ x1+s+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14)
  MSE_X2 <- append(MSE_X2,0.5 * sum((y-m$fitted.values)^2),length(MSE_X2))
}

for (iter in 1:101)
{
  s <- sample(x3,replace=FALSE)
  m <- lm (y ~ x1+x2+s+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14)
  MSE_X3 <- append(MSE_X3,0.5 * sum((y-m$fitted.values)^2),length(MSE_X3))
}

for (iter in 1:101)
{
  s <- sample(x4,replace=FALSE)
  m <- lm (y ~ x1+x2+x3+s+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14)
  MSE_X4 <- append(MSE_X4,0.5 * sum((y-m$fitted.values)^2),length(MSE_X4))
}


for (iter in 1:101)
{
  s <- sample(x5,replace=FALSE)
  m <- lm (y ~ x1+x2+x3+x4+s+x6+x7+x8+x9+x10+x11+x12+x13+x14)
  MSE_X5 <- append(MSE_X5,0.5 * sum((y-m$fitted.values)^2),length(MSE_X5))
}


for (iter in 1:101)
{
  s <- sample(x6,replace=FALSE)
  m <- lm (y ~ x1+x2+x3+x4+x5+s+x7+x8+x9+x10+x11+x12+x13+x14)
  MSE_X6 <- append(MSE_X6,0.5 * sum((y-m$fitted.values)^2),length(MSE_X6))
}


for (iter in 1:101)
{
  s <- sample(x7,replace=FALSE)
  m <- lm (y ~ x1+x2+x3+x4+x5+x6+s+x8+x9+x10+x11+x12+x13+x14)
  MSE_X7 <- append(MSE_X7,0.5 * sum((y-m$fitted.values)^2),length(MSE_X7))
}


for (iter in 1:101)
{
  s <- sample(x8,replace=FALSE)
  m <- lm (y ~ x1+x2+x3+x4+x5+x6+x7+s+x9+x10+x11+x12+x13+x14)
  MSE_X8 <- append(MSE_X8,0.5 * sum((y-m$fitted.values)^2),length(MSE_X8))
}


for (iter in 1:101)
{
  s <- sample(x9,replace=FALSE)
  m <- lm (y ~ x1+x2+x3+x4+x5+x6+x7+x8+s+x10+x11+x12+x13+x14)
  MSE_X9 <- append(MSE_X9,0.5 * sum((y-m$fitted.values)^2),length(MSE_X9))
}


for (iter in 1:101)
{
  s <- sample(x10,replace=FALSE)
  m <- lm (y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+s+x11+x12+x13+x14)
  MSE_X10 <- append(MSE_X10,0.5 * sum((y-m$fitted.values)^2),length(MSE_X10))
}


for (iter in 1:101)
{
  s <- sample(x11,replace=FALSE)
  m <- lm (y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+s+x12+x13+x14)
  MSE_X11 <- append(MSE_X11,0.5 * sum((y-m$fitted.values)^2),length(MSE_X11))
}

for (iter in 1:101)
{
  s <- sample(x12,replace=FALSE)
  m <- lm (y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+s+x13+x14)
  MSE_X12 <- append(MSE_X12,0.5 * sum((y-m$fitted.values)^2),length(MSE_X12))
}
for (iter in 1:101)
{
  s <- sample(x13,replace=FALSE)
  m <- lm (y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+s+x14)
  MSE_X13 <- append(MSE_X13,0.5 * sum((y-m$fitted.values)^2),length(MSE_X13))
}
for (iter in 1:101)
{
  s <- sample(x14,replace=FALSE)
  m <- lm (y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+s)
  MSE_X14 <- append(MSE_X14,0.5 * sum((y-m$fitted.values)^2),length(MSE_X14))
}
etiquettes <- c('M1','x1','x2','x3','x4','x5','x6','x7','x8','x9','x10','x11','x12','x13','x14')
boxplot(MSE_M1,MSE_X1,MSE_X2,MSE_X3,MSE_X4,MSE_X5,MSE_X6,MSE_X7,MSE_X8,MSE_X9,MSE_X10,MSE_X11,MSE_X12,MSE_X13,MSE_X14,names=etiquettes)
title('Boxplots des MSE obtenus par permutations des différentes variables explicatives')

variableAEvaluer = summary(MSE_X1)
diffIQ = variableAEvaluer[5] - variableAEvaluer[2]
diffIQ #différence interquartile
variableAEvaluer[4] #moyenne




