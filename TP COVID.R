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
pacman::p_load(tictoc, readr, readxl, tidyverse, glmmTMB, lme4, lmerTest, nnet)


## ---------------------------

##%######################################################%##
#                                                          #
####                      Formules                      ####
#                                                          #
##%######################################################%##

# Function for Linear Regression
linear_regression <- function(formula, data){
  model <- lm(formula = formula, data = data)
  summary(model)
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


##%######################################################%##
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

