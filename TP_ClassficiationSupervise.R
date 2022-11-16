pacman::p_load(data.table, readxl, readr, FactoMineR, C50, rpart, Factoshiny, tidyverse, dplyr)
setwd(getwd())#Users/33763/OneDrive/Bureau/Git/M2STAT'

P53 <- fread("P53_S.txt.gz", data.table = FALSE)
row.names(P53) <- paste0(P53[,1], factor(duplicated(P53[,1]), labels = c("", "_1")))

P53 <- P53[, -1] # retire la dernière colonne
table(P53[, ncol(P53)]) # selection de la colonne 5409

seed <- 42
set.seed(seed)

# Selection de 50 variables 3D
IndicesVar <- grep("^F3D", colnames(P53))[seed + (51:100)]
IndicesVar <- c(IndicesVar, ncol(P53))

# Test lien var quantitative et qualitative : ANOVA

# Quand on coupe test et training, on fait semblant de pas connaitre
# l'ensemble coupe qui correspond au test

# Autre manière de tester un jeu de donnée, c'est la validation croisé, c a d 
# qu'a un moment donné, tous les sous ensemble servent a un moment, une sorte de
# roulement

# Selection d'individus, tous les individus "active" et un sous ensemble
# des individus "inactive" pour obtenir 1 000 individus


