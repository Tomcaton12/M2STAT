library(Matching) # dans matching se trouve le dataset : lalonde
library(tableone)

data("lalonde")


table <- CreateTableOne(vars = c("age", "educ", "married", "nodegr", "re74", "black", "hisp") , strata = c("treat"), data = lalonde, test = FALSE)
print(table, smd = TRUE)
ExtractSmd(table)
# On s'arrête a re74 car après c'est post traitement, donc c'est un collider (20min26 sur audio 211)

tabUnmatched <-  CreateTableOne(vars = c("age", "educ", "married", "nodegr", "re74", "black", "hisp") , strata = c("treat"), data = lalonde, test = FALSE)
print(tabUnmatched, smd = TRUE)
ExtractSmd(tabUnmatched) # du resultat de cette table, c'est intéressant pour plot
addmargins(table(ExtractSmd(tabUnmatched) > 0.1 )) # nous dit combien de variable on des smd qui dépasse le suil qu'on fixe. Ici 4 qui ont un SMD > 10%. (age, educ, nodegr et hisp)
# Ca veut dire que le groupe re74, y'en a 2 pour 10 000 (diff de moyenne entre les deux groupes, en unité d'écart type car c'est standardisé, on voit que les 2 groupes avait des revenus initiaux échangeables)
# Tant qu'on a pas atteint inférieur a 0.10, on a pas des groupes échangeables

# Design match pour comparer et voir si les groupes sont équilibrés. Valeur du SMD ploté en fonction des variables
library(designmatch)
loveplot(as.matrix(lalonde[, vars]), t_id = which(lalonde$treat == 1), v_line = 0.1, c_id = NULL)

vars = c("age", "educ", "married", "nodegr", "re74", "black", "hisp")
varsFactor = c("treat")
