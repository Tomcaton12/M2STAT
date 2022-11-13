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


