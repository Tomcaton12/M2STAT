pacman::p_load(readr, readxl, tidyverse, glmmTMB, lme4, lmerTest, nnet, broom, broom.mixed,
               leaps, DataEditR)

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

# split data into train and test
set.seed(123)
smp_size <- floor(0.80 * nrow(data_COVID))
train_ind <- sample(seq_len(nrow(data_COVID)), size = smp_size)
train <- data_COVID[train_ind, ]
test <- data_COVID[-train_ind, ]

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

# Scatter plot of the regression
plot(data_$_, data_$_)


# Encodage d'une variable en 2 classes binaire
data_ <- data_originale |> 
  mutate(
    newvarname = case_when(
      var >=1.1 ~ 1,
      var < 1.1 ~ 0
    )
  )

# Meilleur subset possible
regsubsets.out <- regsubsets(response ~ predictor,
                             data = data,
                             nbest = 1,      # 1 best model for each number of predictors
                             nvmax = NULL,   # NULL for no limit on number of variables
                             force.in = NULL, force.out = NULL,
                             method = "exhaustive")
summary_best_subset <- summary(regsubsets.out)

as.data.frame(summary_best_subset$outmat)
which.max(summary_best_subset$adjr2)
summary_best_subset$which[,]


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
      AIC <- AIC(lm(as.formula(paste(response, "~", model)), data = data))
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



##%######################################################%##
#                                                          #
####                  Interpretations                   ####
#                                                          #
##%######################################################%##

# LM / GLM
# Coefficients:
# (Intercept) covariables(predictor)
# Lorsque le covariable augmente de 'son unité' [souvent 1 unité]
# la variable à prédire, augmente ou diminue en MOYENNE de ...

# Pour avoir la variation en pourcentage % il faut pour :
# log :
round(((1 - exp(lm$coefficients[numero de la covar])) *100), digits = 2)
# log2 : 
round((((1 - 2^lm$coefficients[numero de la covar])) *100), digits = 2)
# pour log10 :
round((((1 - 10^lm$coefficients[numero de la covar])) *100), digits = 2)