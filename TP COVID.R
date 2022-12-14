## ---------------------------
##
## Script name: TP COVID
##
## Purpose of script:
##
## Author: Will
##
## Date Created: 2022-11-13
##
## Copyright (c) 
## Email: 
##
## ---------------------------
##
## Notes:
##   
##
## 
## ---------------------------

## load up the packages we will need:
pacman::p_load(readr, readxl, tidyverse, glmmTMB, lme4, lmerTest, nnet,
               broom, broom.mixed, leaps, DataEditR, MASS, VGAM)


## ---------------------------

##%######################################################%##
#                                                          #
####                      Formules                      ####
#                                                          #
##%######################################################%##

# Function for Linear Regression
linear_regression <- function(formula, data){
  model <- lm(formula = formula, data = data)
  # summary(model)
  return(model)
}

# Function for Binomial Regression
binomial_regression <- function(formula, data){
  model <- glm(formula = formula, data = data, family = "binomial")
  # summary(model)
  return(model)
}

##%######################################################%##
#                                                          #
####                   Data gestion                     ####
#                                                          #
##%######################################################%##

# Load origianl dataset
data_COVID <- read_delim("D:/M2 - SMSDS/M2 SMSDS/Modèles linéaires généralisés, modèles mixtes/Cours/TP-20221111/dataset1_smsds_import_raw.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Description of the database
str(data_COVID)

# Summary of the database
summary(data_COVID)

# check of missing values
sapply(data_COVID, function(x) sum(is.na(x)))

# check of the number of unique values
sapply(data_COVID, function(x) length(unique(x)))

# check of the number of rows and columns
dim(data_COVID)

# Trim NA values in the database
data_trim_COVID <- na.omit(data_COVID)

# Recode into 2 binary class and age into 3 class
data_trim_COVID_2 <- data_trim_COVID |> 
  mutate(
    elisa_2 = case_when(
      elisa >=1.1 ~ 1,
      elisa < 1.1 ~ 0
    ),
    age_65 = case_when(
      age >=65 ~ 1,
      age < 65 ~ 0),
    age20 = cut(age, seq(20, 80, 20))
  )

# Recode into 3 class with baseline and ordered
data_trim_COVID_3 <- data_trim_COVID |> 
  mutate(
    elisa_3 = case_when(
    elisa >= 1.1 ~ "pos",
    elisa >= 0.8 ~ "dou",
    elisa < 0.8 ~ "neg"
  ) |> 
  fct_relevel("neg") |> 
  ordered()
)

# split data into train and test COVID_2
set.seed(123)
smp_size_2 <- floor(0.80 * nrow(data_trim_COVID_2))
train_ind_2 <- sample(seq_len(nrow(data_trim_COVID_2)), size = smp_size_2)
train_2 <- data_trim_COVID_2[train_ind_2, ]
test_2 <- data_trim_COVID_2[-train_ind_2, ]

# split data into train and test COVID_3
set.seed(123)
smp_size_3 <- floor(0.80 * nrow(data_trim_COVID_3))
train_ind_3 <- sample(seq_len(nrow(data_trim_COVID_3)), size = smp_size_3)
train_3 <- data_trim_COVID_3[train_ind_3, ]
test_3 <- data_trim_COVID_3[-train_ind_3, ]

## filter and sort the dataset
COVID_2 <- data_trim_COVID_2 %>%
  select(sexe, age, elisa_2, age20)
register("COVID_2", "data_trim_COVID_2")
dtab(COVID_2, dec = 2, nr = 100) %>% render()

## change variable type
COVID_2 <- mutate_at(COVID_2, .vars = vars(sexe), .funs = as_factor)

# add column data_trim_COVID_3$elisa_3 to data_trim_COVID_2
data_trim_COVID_2 <- data_trim_COVID_2 %>% 
  bind_cols(elisa_3 = data_trim_COVID_3$elisa_3)
# 
# ##%######################################################%##
#                                                          #
####                        Plot                        ####
#                                                          #
##%######################################################%##


# Distribution plot of ELISA
ggplot(data_trim_COVID) +
  aes(x = elisa) +
  geom_histogram(bins = 30L, fill = "#515358") +
  labs(
    x = "Valeur de l'ELISA",
    y = "Nombre de participants",
    title = "Distribution de l'ELISA",
    subtitle = "Sans transformation"
  ) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold.italic"))

ggplot(data_trim_COVID) +
  aes(x = elisa) + scale_x_continuous(trans = "log") +
  geom_histogram(bins = 30L, fill = "#515358") +
  labs(
    x = "Valeur de l'ELISA",
    y = "Nombre de participants",
    title = "Distribution de l'ELISA",
    subtitle = "Transformation log"
  ) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold.italic"))

ggplot(data_trim_COVID) +
  aes(x = elisa) + scale_x_continuous(trans = "log2") +
  geom_histogram(bins = 30L, fill = "#515358") +
  labs(
    x = "Valeur de l'ELISA",
    y = "Nombre de participants",
    title = "Distribution de l'ELISA",
    subtitle = "Transformation log2"
  ) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold.italic"))

ggplot(data_trim_COVID) +
  aes(x = elisa) + scale_x_continuous(trans = "log10") +
  geom_histogram(bins = 30L, fill = "#515358") +
  labs(
    x = "Valeur de l'ELISA",
    y = "Nombre de participants",
    title = "Distribution de l'ELISA",
    subtitle = "Transformation log10"
  ) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold.italic"))

##%######################################################%##
#                                                          #
####               Linear regression and                ####
####           Generalized linear regression            ####
#                                                          #
##%######################################################%##

# Models :
model1 <- elisa ~ I(age/10)
model2 <- log10(elisa) ~ I(age/10)
model3 <- log2(elisa) ~ I(age/10)
model4 <- log(elisa) ~ I(age/10)

# Models emboites 5, 6 et 7 (10?) | 8 et 9
model5 <- elisa_2 ~ age_65
model6 <- elisa_2 ~ age_65 + covid
model_6.5 <- dat_outcomes ~ dat_covar, binomial
model7 <- elisa_2 ~ age_65 * covid

model8 <- elisa_2 ~ covid
model9 <- elisa_2 ~ age20 + covid

model10 <- elisa_2 ~ age_65 + covid + sexe

# Models non emboite 6 et 11
model_11 <- elisa_2 ~ covid + sexe

linear_regression(model1, data_trim_COVID)
# |- Age augmente de 10 ans, ELISA diminue en moyenne de  0.04708

linear_regression(model2, data_trim_COVID)
# |- Age augmente de 10 ans, le log 10 de l'ELISA diminue en moyenne de 0.030802
round((((1 - 10^linear_regression(model2, data_trim_COVID)$coefficients[2])) *100), digits = 2)
# |- Age augmente de 10 ans, le titre de l'ELISA diminue en moyenne 6.85 %
round(10^linear_regression(model2, data_trim_COVID)$coefficients[2], digits = 2)
# |- Age augmente de 10 ans, le titre de l'ELISA varie en moyenne d'un facteur de 0.93

linear_regression(model3, data_trim_COVID)
# |- Age augmente de 10 ans, le log 10 de l'ELISA diminue en moyenne de 0.10232
round((((1 - 2^linear_regression(model3, data_trim_COVID)$coefficients[2])) *100), digits = 2)
# |- Age augmente de 10 ans, le titre de l'ELISA diminue en moyenne 6.85 %
round(2^linear_regression(model3, data_trim_COVID)$coefficients[2], digits = 2)
# |- Age augmente de 10 ans, le titre de l'ELISA varie en moyenne d'un facteur de 0.93

linear_regression(model4, data_trim_COVID)
# |- Age augmente de 10 ans, le log 10 de l'ELISA diminue en moyenne de 0.070923 
round(((1 - exp(linear_regression(model3, data_trim_COVID)$coefficients[2])) *100), digits = 2)
# |- Age augmente de 10 ans, le titre de l'ELISA diminue en moyenne de 9.73 %
round(exp(linear_regression(model3, data_trim_COVID)$coefficients[2]), digits = 2)
# |- Age augmente de 10 ans, le titre de l'ELISA varie en moyenne d'un facteur de 0.9

# Create a confusion matrix
xtabs(~ age_65 + elisa_2, data_trim_COVID_2)
# |- On observe, 1347 patients inférieur a 65 ans qui n'ont pas le covid
# |- On observe, 56 patients inférieur a 65 ans qui ont le covid
# |- On observe, 548 patients supérieur a 65 ans qui n'ont pas le covid
# |- On observe, 9 patients supérieur a 65 ans qui ont pas le covid

binomial_regression(model5, data_trim_COVID_2)
# |- Age augmente de 1 an après 65 ans, le titre de l'ELISA diminue en moyenne de 0.9288
plogis(binomial_regression(model5, data_trim_COVID_2)$coefficients[1])
# |- logit-estimates returned by binomial glm, which can be transformed into probabilities
plogis(sum(binomial_regression(model5, data_trim_COVID_2)$coefficients))
# |- Le Risque Absolue (Probabilité de l'Elisa +) chez les >= 65 ans est estimé à 0.016
exp(binomial_regression(model5, data_trim_COVID_2)$coefficients[2])
tidy(binomial_regression(model5, data_trim_COVID_2), conf.int = TRUE, exponentiate = TRUE)
# |- Le Risque Absolue (Probabilité de l'ELISA +), chez les < 65 ans est estimé à 0.395
# # |- L'Odds Ratio à un age de >= 65 ans est estimé à 0.395

binomial_regression(model6, data_trim_COVID_2)
binomial_regression(model7, data_trim_COVID_2)
# |- Model 7 Avec Interraction (AI) : https://www.theanalysisfactor.com/interpreting-interactions-in-regression/#:~:text=Adding%20an%20interaction%20term%20to,for%20different%20values%20of%20Sun.
# L’ajout d’un terme d’interaction à un modèle modifie radicalement
# l’interprétation de tous les coefficients. Sans terme d’interaction, 
# nous interprétons B1 comme l’effet unique de l'âge sur le titre de l'ELISA.
# Mais l’interaction signifie que l’effet de l'âge sur l'ELISA est différent
# pour différentes valeurs du covid.
# Ainsi, l’effet unique de l'age sur l'ELISA ne se limite pas à B1. Cela dépend
# également des valeurs de B3 et du covid. L’effet unique de l'age est représenté
# par tout ce qui est multiplié par l'age dans le modèle: B1 + B3 * covid.
# B1 est maintenant interprété comme l’effet unique de l'age sur l'ELISA uniquement
# lorsque covid = 0.
# 
# L’ajout du terme d’interaction a modifié les valeurs de B1 et B2.
# L’effet de l'âge sur l'ELISA est maintenant de -1.268 + 1.118 * covid.
# Pour l'elisa en lorsque Covid -, covid = 0, donc l’effet de l'âge est de
# -1.268 + 1.118 * 0 = -1.268. Donc, pour deux personnes covid -, nous nous 
# attendons à ce qu’une personne >= 65 ans soit -1.268 moins elevé qu’une
# personne < 65 ans.
# 
# Pour personnes en covid +, cependant, l’effet de l'âge est de -1.268 + 1.118 * 1 = - 0.15.
# Ainsi, pour deux personnes covid +, une personne >= 65 ans devrait être avoir un
# titre ELISA de -0.15 qu’une personne < 65 ans.
# 
# En raison de l’interaction, l’effet d’avoir plus de 65 ans est différent
# si une personne est covid + ou covid -.
# Une autre façon de dire cela est que les pentes des droites de régression
# entre la l'ELISA et l'age sont différentes pour les différentes catégories de covid.
# B3 indique à quel point ces pentes sont différentes.

anova(binomial_regression(model6, data_trim_COVID_2),
      binomial_regression(model7, data_trim_COVID_2),
      test = "LRT"
      )
# Likelihood Ratio Test : on veut le AIC ou le BIC le plus petit
# Le plus petit, le meilleur model ! Non significatif ici

# Meilleur subset possible
regsubsets.out <- regsubsets(elisa_2 ~ .,
             data = COVID_2,
             nbest = 1,      # 1 best model for each number of predictors
             nvmax = NULL,   # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
summary_best_subset <- summary(regsubsets.out)

as.data.frame(summary_best_subset$outmat)
which.max(summary_best_subset$adjr2)
summary_best_subset$which[,]
}

Best_Subset <- function(data, response, max_num_predictors) {
  predictors <- names(data)
  predictors <- predictors[predictors != response]
  n <- nrow(data)
  best_model <- NULL
  best_AIC <- Inf
  best_predictors <- NULL
  for (i in 1:max_num_predictors) {
    model <- NULL
    best_AIC_i <- Inf
    best_predictors_i <- NULL
    for (j in predictors) {
      if (is.null(model)) {
        model <- j
      } else {
        model <- paste(model, j, sep = "+")
      }
      AIC <- AIC(glm(as.formula(paste(response, "~", model)), data = data))
      if (AIC < best_AIC_i) {
        best_AIC_i <- AIC
        best_predictors_i <- model
      }
    }
    if (best_AIC_i < best_AIC) {
      best_AIC <- best_AIC_i
      best_predictors <- best_predictors_i
      best_model <- glm(as.formula(paste(response, "~", best_predictors)), data = data)
    }
    predictors <- predictors[predictors != best_predictors_i]
  }
  return(best_model)
}

binomial_regression(model8, data_trim_COVID_2)
binomial_regression(model9, data_trim_COVID_2)
anova(binomial_regression(model8, data_trim_COVID_2),
      binomial_regression(model9, data_trim_COVID_2),
      test = "LRT") # pareil que "Chisq" |- model9 meilleur

Best_Subset(COVID_2, "elisa_2", 3)

drop1(binomial_regression(model10, data_trim_COVID_2), test = "LRT")

# Comparaison de model non emboité : utiliser anova() et regarder AIC / BIC

# Ecriture binomial et pivot_wider
# binomial_data <- function(data, response, covar1, covar2){
dat_bin <- data_trim_COVID_2 %>%
  filter(!is.na(elisa_2)) %>% 
  count(age_65, covid, elisa_2) %>% 
  pivot_wider(names_from = elisa_2, values_from = n)

dat_outcomes <- tibble(
  pos = dat_bin[, 4],
  neg = dat_bin[, 3]
) %>% 
  as.matrix()

dat_covar <- tibble(
  age_65 = dat_bin[, 1],
  covid = dat_bin[, 2]
) %>% 
  as.matrix()


# Multinomial regression
library(MASS)
summary(multinom(elisa_3 ~ age_65 + covid, data = data_trim_COVID_2))
# Chaque coefficient estime l'effet par rapport à la catégorie négative.
# Les résultats sont proches de ceux qu'on aurait obtenu sur des modèles comparant
# 2 a 2, mais on a ici une vraisemblance pour l'ensemble du modèle !