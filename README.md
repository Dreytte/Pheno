# Pheno

library(car)
library(GrapheR) # graphique en clic bouton
library(lme4) # Modèle mixte généralisé
library(nlme) # Modéle mixte général
library(MuMIn)
library(MASS)

#Import fichier
tab1 <- read.delim("tab11.txt")
#déclare ce qui est dans la table
attach(tab1)
summary(tab1)

par(mfrow=c(1,2))
hist(AllBioLAI$LAI3rings)   
qqnorm(AllBioLAI$LAI3rings) 
qqline(AllBioLAI$LAI3rings) # Normale --> regression multiple

par(mfrow=c(1,2))
hist(AllBioLAI$LAI5rings)   
qqnorm(AllBioLAI$LAI5rings) 
qqline(AllBioLAI$LAI5rings) # Aléatoire --> non exploitable

qqp(AllBioLAI$`Wet Biom`,"norm")
qqp(AllBioLAI$`Wet Biom`,"lnorm")
variable<-AllBioLAI$`Wet Biom`
gamma<-fitdistr(variable,"gamma")
qqp(variable, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

#test 2 ajout d'un comment

par(mfrow=c(1,2))
hist(AllBioLAI$`Wet Biom`)   
qqnorm(AllBioLAI$`Wet Biom`) 
qqline(AllBioLAI$`Wet Biom`) # Gamma --> lien = inverse

par(mfrow=c(1,2))
hist(AllBioLAI$`Dry Biom`)   
qqnorm(AllBioLAI$`Dry Biom`) 
qqline(AllBioLAI$`Dry Biom`) # Gamma --> lien = inverse

par(mfrow=c(1,2))
hist(AllBioLAI$Height)   
qqnorm(AllBioLAI$Height) 
qqline(AllBioLAI$Height) # Binomial --> GLM, lien =

qqp(AllBioLAI$Height,"norm")
qqp(AllBioLAI$Height,"lnorm")
variable<-AllBioLAI$Height
gamma<-fitdistr(variable,"gamma")
qqp(variable, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])


