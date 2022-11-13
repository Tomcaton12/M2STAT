pacman::p_load(readr, readxl, tidyverse)

data_COVID <- read_delim("D:/M2 - SMSDS/M2 SMSDS/Modèles linéaires généralisés, modèles mixtes/Cours/TP-20221111/dataset1_smsds_import_raw.csv", ";", escape_double = FALSE, trim_ws = TRUE)

data_CIG <- readRDS("D:/M2 - SMSDS/M2 SMSDS/Modèles linéaires généralisés, modèles mixtes/Cours/TP-20221111/cig.rds")
  
data_CIG2 <- readRDS("D:/M2 - SMSDS/M2 SMSDS/Modèles linéaires généralisés, modèles mixtes/Cours/TP-20221111/cig2.rds")
  
data_DRUG <- readRDS("D:/M2 - SMSDS/M2 SMSDS/Modèles linéaires généralisés, modèles mixtes/Cours/TP-20221111/drug.rds")

data_DRUG2 <- readRDS("D:/M2 - SMSDS/M2 SMSDS/Modèles linéaires généralisés, modèles mixtes/Cours/TP-20221111/drug2.rds")
  
data_EPI <- readRDS("D:/M2 - SMSDS/M2 SMSDS/Modèles linéaires généralisés, modèles mixtes/Cours/TP-20221111/epilepsy.rds")
  
data_EPI_LONG <- readRDS("D:/M2 - SMSDS/M2 SMSDS/Modèles linéaires généralisés, modèles mixtes/Cours/TP-20221111/epi_long.rds")
  
data_EPI_LONG2 <- readRDS("D:/M2 - SMSDS/M2 SMSDS/Modèles linéaires généralisés, modèles mixtes/Cours/TP-20221111/epi_long2.rds")
  
data_EHPAD <- read_xls("D:/M2 - SMSDS/M2 SMSDS/Modèles linéaires généralisés, modèles mixtes/Cours/TP-20221111/grip.xls")
  
data_EPHAD2 <- read_xls("D:/M2 - SMSDS/M2 SMSDS/Modèles linéaires généralisés, modèles mixtes/Cours/TP-20221111/grippe_ehpad.xls")

data_STIFF <-read_xls("D:/M2 - SMSDS/M2 SMSDS/Modèles linéaires généralisés, modèles mixtes/Cours/TP-20221111/stif.xls")
  

##%######################################################%##
#                                                          #
####                      Formules                      ####
#                                                          #
##%######################################################%##

# Description of the database
str(data_)

# Summary of the database
summary(data_)

# check of missing values
sapply(data_, function(x) sum(is.na(x)))

# check of the number of unique values
sapply(data_, function(x) length(unique(x)))

# check of the number of rows and columns
dim(data_)

# Trim NA values in the database
data_trim_ <- na.omit(data_)

# Function for Linear Regression
linear_regression <- function(formula, data){
  model <- lm(formula = formula, data = data)
  summary(model)
}

# Function for Generalized Linear Regression
glm_regression <- function(formula, data){
  model <- glm(formula = formula, data = data, family = "gaussian")
  summary(model)
}

# Function for Poisson Regression
poisson_regression <- function(formula, data){
  model <- glm(formula = formula, data = data, family = "poisson")
  summary(model)
}

# Function for Negative Binomial Regression
negative_binomial_regression <- function(formula, data){
  model <- glm(formula = formula, data = data, family = "negbin")
  summary(model)
}

# Function for Binomial Regression
binomial_regression <- function(formula, data){
  model <- glm(formula = formula, data = data, family = "binomial")
  summary(model)
}

# Function for Zero-Inflated Poisson Regression
zip_regression <- function(formula, data){
  model <- zeroinfl(formula = formula, data = data, model = "poisson")
  summary(model)
}

# Function for Zero-Inflated Negative Binomial Regression
zinb_regression <- function(formula, data){
  model <- zeroinfl(formula = formula, data = data, model = "negbin")
  summary(model)
}

# Function for Zero Inflated Negative Binomial Regression with log link
ZinbRegression_log <- function(data, formula){
  model <- zeroinfl(formula = formula, data = data, dist = "negbin", link = "log")
  summary(model)
}

# Function for Zero Inflated Poisson Regression with log link
ZipRegression_log <- function(data, formula){
  model <- zeroinfl(formula = formula, data = data, dist = "poisson", link = "log")
  summary(model)
}

# Function for Zero Inflated Poisson Regression with log link and offset
ZipRegression_log_offset <- function(data, formula){
  model <- zeroinfl(formula = formula, data = data, dist = "poisson", link = "log", offset = log(data$pop))
  summary(model)
}

# Function for Zero-Inflated Binomial Regression
zib_regression <- function(formula, data){
  model <- zeroinfl(formula = formula, data = data, model = "binomial")
  summary(model)
}

# Function for Logistic Regression
LogisticRegression <- function(data, formula){
  model <- glm(formula = formula, data = data, family = binomial(link = "logit"))
  summary(model)
}


model_1 <- elisa ~ I(age/10)
model_2 <- log10(elisa) ~ I(age/10)
model_3 <- log2(elisa) ~ I(age/10)
model_4 <- log(elisa) ~ I(age/10)

linear_regression(model_1, data_COVID_trim)
