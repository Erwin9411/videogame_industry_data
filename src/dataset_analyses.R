# Install packages
# install.packages("AER")
# install.packages("systemfit")
# install.packages("moments")
# install.packages("nortest")
# install.packages("dgof")
# install.packages("copula")
# install.packages("plotly")
# install.packages("olsrr")

# Loading Packages
library(readr)
library(dplyr)
library(tidyverse)
library(modelsummary)
library(tidyr)
library(broom)
library(AER)
library(systemfit)


# Creating a folder
folder <- "../gen"

if (file.exists(folder)) {
  
  cat("The folder already exists")
  
} else {
  
  dir.create(folder)
  
}


# Loading data
vg_data <- read.csv("../gen/full_videogame_dataset.csv")

vg_data <- vg_data %>% 
  mutate(ln_USV = log(USV))

vg_data_test <- vg_data %>% 
  filter(OPUSD != 0.00)

vg_data_test <- drop_na(vg_data_test)

#################################################################################
############################ DESCRIPTIVE STATISTICS #############################
#################################################################################

summary(vg_data_test$USV)
SD_USV <- round(sd(vg_data_test$USV),2)

summary(vg_data_test$CONREV)
SD_CONREV <- round(sd(vg_data_test$CONREV),2)

summary(vg_data_test$PROFREV)
SD_PROFREV <- round(sd(vg_data_test$PROFREV),2)

summary(vg_data_test$OPUSD)
SD_OPUSD <- round(sd(vg_data_test$OPUSD),2)

summary(vg_data_test)

SD_FRANC <- round(sd(vg_data_test$FRANC),2)
SD_GEN_AC.AD <- round(sd(vg_data_test$GEN_AC.AD),2)
SD_GEN_RPG <- round(sd(vg_data_test$GEN_RPG),2)
SD_GEN_SP <- round(sd(vg_data_test$GEN_SP),2)
sD_GEN_MI <- round(sd(vg_data_test$GEN_MI),2)
sD_OSIN <- round(sd(vg_data_test$OSIN),2)
sD_OMUL <- round(sd(vg_data_test$OMUL),2)
sD_SP.MP <- round(sd(vg_data_test$SP.MP),2)
sD_PLF_PC <- round(sd(vg_data_test$PLF_PC),2)
sD_ESRB_M <- round(sd(vg_data_test$ESRB_RP),2)

###############################################################################
####### MEAN-CENTERING CONTINUOUS VARIABELS ###################################
###############################################################################

center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# Only keeping the dummy for ESRB_M because it's sufficient
data_prepped <- vg_data_test %>% 
  select(-c(ESRB_E, ESRB_T, ESRB_RP))

data_prepped$CONREV <- center_scale(data_prepped$CONREV)
data_prepped$PROFREV <- center_scale(data_prepped$PROFREV)
data_prepped$OPUSD <- center_scale(data_prepped$OPUSD)
data_prepped$PUBS <- center_scale(data_prepped$PUBS)
data_prepped$SMPRES <- center_scale(data_prepped$SMPRES)
data_prepped$N_LAN <- center_scale(data_prepped$N_LAN)


###############################################################################
############## SIMPLE EFFECT CODING OF DUMMY VARIABLES ########################
###############################################################################

data_prepped$GEN_C_SP <- ifelse(data_prepped$GEN_AC.AD == 1, -.25, ifelse(data_prepped$GEN_SP == 1, .75, ifelse(data_prepped$GEN_RPG == 1, -.25, ifelse(data_prepped$GEN_MI == 1, -.25,0 )))) 
data_prepped$GEN_C_RPG <- ifelse(data_prepped$GEN_AC.AD == 1, -.25, ifelse(data_prepped$GEN_SP == 1, -.25, ifelse(data_prepped$GEN_RPG == 1, .75, ifelse(data_prepped$GEN_MI == 1, -.25,0 ))))
data_prepped$GEN_C_MI <- ifelse(data_prepped$GEN_AC.AD == 1, -.25, ifelse(data_prepped$GEN_SP == 1, -.25, ifelse(data_prepped$GEN_RPG == 1, -.25, ifelse(data_prepped$GEN_MI == 1, .75,0 ))))

data_prepped$OMUL_C <- ifelse(data_prepped$OMUL == "1", 0.66, -0.33)
data_prepped$SP.MP_C <- ifelse(data_prepped$SP.MP == "1", 0.66, -0.33)

data_prepped <- data_prepped %>% 
  rename(GEN_AC.AD_D = GEN_AC.AD,
         GEN_SP_D = GEN_SP,
         GEN_RPG_D = GEN_RPG,
         GEN_MI_D = GEN_MI,
         GEN_SP = GEN_C_SP,
         GEN_RPG = GEN_C_RPG,
         GEN_MI = GEN_C_MI,
         OSIN_D = OSIN,
         OMUL_D = OMUL,
         SP.MP_D = SP.MP,
         OMUL = OMUL_C,
         SP.MP = SP.MP_C)

###############################################################################
######################## LINEAR REGRESSIONS ###################################
###############################################################################

# Linear regression including control variables without copula terms or interactions
model_1A <- lm(ln_USV ~ CONREV +
                 PROFREV +
                 FRANC +
                 GEN_SP +
                 GEN_RPG +
                 GEN_MI +
                 OPUSD +
                 OMUL +
                 SP.MP +
                 PLF_PC +
                 ESRB_M +
                 PUBS +
                 INDIE +
                 soundtrack +
                 SMPRES +
                 N_LAN ,
               data=vg_data_test, na.action = na.omit)


# Linear regression including interaction terms without copulas
model_1B <- lm(ln_USV ~ (CONREV * FRANC) +
                 (CONREV * GEN_SP) +
                 (CONREV * GEN_RPG) +
                 (CONREV * GEN_MI) +
                 (CONREV * OPUSD) +
                 (CONREV * OMUL_D) +
                 (CONREV * SP.MP_D) +
                 (CONREV * PLF_PC) +
                 (CONREV * ESRB_M) +
                 (PROFREV * FRANC) +
                 (PROFREV * GEN_SP) +
                 (PROFREV * GEN_RPG) +
                 (PROFREV * GEN_MI) +
                 (PROFREV * OPUSD) +
                 (PROFREV * OMUL_D) +
                 (PROFREV * SP.MP_D) +
                 (PROFREV * PLF_PC) +
                 (PROFREV * ESRB_M)+
                 PUBS +
                 INDIE +
                 soundtrack +
                 SMPRES +
                 N_LAN,
               data=data_prepped, na.action = na.omit)


###############################################################################
################### GAUSSIAN COPULA ###########################################
###############################################################################

# Installing and loading packages

# install.packages("moments")
# install.packages("nortest")
# install.packages("dgof")
# install.packages("copula")
# install.packages("plotly")
# install.packages("olsrr")
library(moments)
library(nortest)
library(dgof)
library(stats)
library(olsrr)


# Checking for nonnormallity with the Anderson-Darling test
CONREV_AD <- print(ad.test(vg_data_test$CONREV))

PROFREV_AD <- print(ad.test(vg_data_test$PROFREV))

OPUSD_AD <- print(ad.test(vg_data_test$OPUSD))


# Checking the kurtosis and skewness as additional information

CONREV_SKEW <- print(skewness(vg_data_test$CONREV))
PROFREV_SKEW <- print(skewness(vg_data_test$PROFREV))
OPUSD_SKEW <- print(skewness(vg_data_test$OPUSD))

CONREV_KUR <- print(kurtosis(vg_data_test$CONREV))
PROFREV_KUR <- print(kurtosis(vg_data_test$PROFREV))
OPUSD_KUR <- print(kurtosis(vg_data_test$OPUSD))

# Checking for normality of the residuals via the untreated model

untreatedModel2 <- summary(lm(ln_USV ~ CONREV +
                                PROFREV +
                                FRANC +
                                GEN_SP +
                                GEN_RPG +
                                GEN_MI +
                                OPUSD +
                                OMUL +
                                SP.MP +
                                PLF_PC +
                                ESRB_M +
                                PUBS +
                                INDIE +
                                soundtrack +
                                SMPRES +
                                N_LAN ,
                              data=vg_data_test, na.action = na.omit))

untreatedModel3 <- summary(lm(ln_USV ~ (CONREV * FRANC) +
                                (CONREV * GEN_SP) +
                                (CONREV * GEN_RPG) +
                                (CONREV * GEN_MI) +
                                (CONREV * OPUSD) +
                                (CONREV * OMUL_D) +
                                (CONREV * SP.MP_D) +
                                (CONREV * PLF_PC) +
                                (CONREV * ESRB_M) +
                                (PROFREV * FRANC) +
                                (PROFREV * GEN_SP) +
                                (PROFREV * GEN_RPG) +
                                (PROFREV * GEN_MI) +
                                (PROFREV * OPUSD) +
                                (PROFREV * OMUL_D) +
                                (PROFREV * SP.MP_D) +
                                (PROFREV * PLF_PC) +
                                (PROFREV * ESRB_M)+
                                PUBS +
                                INDIE +
                                soundtrack +
                                SMPRES +
                                N_LAN,
                              data=data_prepped, na.action = na.omit))

# Visually investigate the distribution of the residuals of untreatedModel2
# Plot the density of the residuals against the normal distribution
ggplot(data.frame(untreatedModel2$residuals), aes(x=untreatedModel2.residuals)) +
  geom_density(alpha=0.4, fill="red") +
  stat_function(fun = dnorm,
                args = list(mean = mean(untreatedModel2$residuals),
                            sd = sd(untreatedModel2$residuals)), 
                color="blue") +
  xlim(-4,4) +
  xlab("Residuals (Model 1 without interaction)")+
  ylab("Density")+
  theme_classic()

# Visually investigate the distribution of the residuals of untreatedModel3
# Plot the density of the residuals against the normal distribution
ggplot(data.frame(untreatedModel3$residuals), aes(x=untreatedModel3.residuals)) +
  geom_density(alpha=0.4, fill="red") +
  stat_function(fun = dnorm,
                args = list(mean = mean(untreatedModel3$residuals),
                            sd = sd(untreatedModel3$residuals)), 
                color="blue") +
  xlim(-4,4) +
  xlab("Residuals (Model 1 with interaction)")+
  ylab("Density")+
  theme_classic()

# Statistical tests of normal distribution of the residuals of model_1A

# All tests combined (olsrr pack)
ols_test_normality(model_1A)

# Statistical tests of normal distribution of the residuals of model_1B

# All tests combined (olsrr pack)
ols_test_normality(model_1B)


###############################################################################
################### GAUSSIAN COPULA HANNES ####################################
###############################################################################

make_copula <- function(x) {
  if (length(unique(x))==1) return(as.numeric(rep(NA, length(x))))
  return(ifelse(ecdf(x)(x)==1, qnorm(1-.0000001), qnorm(ecdf(x)(x))))
}

vg_data_test$CONREV_STAR <- make_copula(vg_data_test$CONREV)
vg_data_test$PROFREV_STAR <- make_copula(vg_data_test$PROFREV)
vg_data_test$OPUSD_STAR <- make_copula(vg_data_test$OPUSD)

data_prepped$CONREV_STAR <- make_copula(data_prepped$CONREV)
data_prepped$PROFREV_STAR <- make_copula(data_prepped$PROFREV)
data_prepped$OPUSD_STAR <- make_copula(data_prepped$OPUSD)

# ###############################################################################
# ################### GAUSSIAN COPULA BEKKER ####################################
# ###############################################################################
# 
# createCopula <- function(P){
#   H.p <- stats::ecdf(P)
#   H.p <- H.p(P)
#   H.p <- ifelse(H.p==0,0.0000001,H.p)
#   H.p <- ifelse(H.p==1,0.9999999,H.p)
#   U.p <- H.p
#   p.star <- stats::qnorm(U.p)
#   return(p.star)	
# }
# 
# 
# vg_data_test$CONREV_STAR_BE <- make_copula(vg_data_test$CONREV)

###############################################################################
################### GAUSSIAN COPULA MODELS ####################################
###############################################################################

# Linear regression including control variables without copula terms or interactions
model_1A <- lm(ln_USV ~ CONREV +
                 PROFREV +
                 FRANC +
                 GEN_SP +
                 GEN_RPG +
                 GEN_MI +
                 OPUSD +
                 OMUL +
                 SP.MP +
                 PLF_PC +
                 ESRB_M +
                 PUBS +
                 INDIE +
                 soundtrack +
                 SMPRES +
                 N_LAN ,
               data=vg_data_test, na.action = na.omit)


# Linear regression including interaction terms without copulas
model_1B <- lm(ln_USV ~ (CONREV * FRANC) +
                 (CONREV * GEN_SP) +
                 (CONREV * GEN_RPG) +
                 (CONREV * GEN_MI) +
                 (CONREV * OPUSD) +
                 (CONREV * OMUL_D) +
                 (CONREV * SP.MP_D) +
                 (CONREV * PLF_PC) +
                 (CONREV * ESRB_M) +
                 (PROFREV * FRANC) +
                 (PROFREV * GEN_SP) +
                 (PROFREV * GEN_RPG) +
                 (PROFREV * GEN_MI) +
                 (PROFREV * OPUSD) +
                 (PROFREV * OMUL_D) +
                 (PROFREV * SP.MP_D) +
                 (PROFREV * PLF_PC) +
                 (PROFREV * ESRB_M)+
                 PUBS +
                 INDIE +
                 soundtrack +
                 SMPRES +
                 N_LAN,
               data=data_prepped, na.action = na.omit)


# Linear regression including CONREV Gaussian copula terms
model_2A <- lm(ln_USV ~ CONREV +
                 PROFREV +
                 FRANC +
                 GEN_SP +
                 GEN_RPG +
                 GEN_MI +
                 OPUSD +
                 OMUL +
                 SP.MP +
                 PLF_PC +
                 ESRB_M +
                 PUBS +
                 INDIE +
                 soundtrack +
                 SMPRES +
                 N_LAN +
                 CONREV_STAR,
               data=vg_data_test, na.action = na.omit)


# Linear regression including CONREV Gaussian copula terms and interactions
model_2B <- lm(ln_USV ~ (CONREV * FRANC) +
                 (CONREV * GEN_SP) +
                 (CONREV * GEN_RPG) +
                 (CONREV * GEN_MI) +
                 (CONREV * OPUSD) +
                 (CONREV * OMUL_D) +
                 (CONREV * SP.MP_D) +
                 (CONREV * PLF_PC) +
                 (CONREV * ESRB_M) +
                 (PROFREV * FRANC) +
                 (PROFREV * GEN_SP) +
                 (PROFREV * GEN_RPG) +
                 (PROFREV * GEN_MI) +
                 (PROFREV * OPUSD) +
                 (PROFREV * OMUL_D) +
                 (PROFREV * SP.MP_D) +
                 (PROFREV * PLF_PC) +
                 (PROFREV * ESRB_M)+
                 PUBS +
                 INDIE +
                 soundtrack +
                 SMPRES +
                 N_LAN +
                 CONREV_STAR,
               data=data_prepped, na.action = na.omit)

# Linear regression including PROFREV Gaussian copula term
model_3A <- lm(ln_USV ~ CONREV +
                 PROFREV +
                 FRANC +
                 GEN_SP +
                 GEN_RPG +
                 GEN_MI +
                 OPUSD +
                 OMUL +
                 SP.MP +
                 PLF_PC +
                 ESRB_M +
                 PUBS +
                 INDIE +
                 soundtrack +
                 SMPRES +
                 N_LAN +
                 PROFREV_STAR,
               data=vg_data_test, na.action = na.omit)

# Linear regression including PROFREV Gaussian copula term and interactions
model_3B <- lm(ln_USV ~ (CONREV * FRANC) +
                 (CONREV * GEN_SP) +
                 (CONREV * GEN_RPG) +
                 (CONREV * GEN_MI) +
                 (CONREV * OPUSD) +
                 (CONREV * OMUL_D) +
                 (CONREV * SP.MP_D) +
                 (CONREV * PLF_PC) +
                 (CONREV * ESRB_M) +
                 (PROFREV * FRANC) +
                 (PROFREV * GEN_SP) +
                 (PROFREV * GEN_RPG) +
                 (PROFREV * GEN_MI) +
                 (PROFREV * OPUSD) +
                 (PROFREV * OMUL_D) +
                 (PROFREV * SP.MP_D) +
                 (PROFREV * PLF_PC) +
                 (PROFREV * ESRB_M)+
                 PUBS +
                 INDIE +
                 soundtrack +
                 SMPRES +
                 N_LAN +
                 PROFREV_STAR,
               data=data_prepped, na.action = na.omit)

# Linear regression including OPUSD Gaussian copula term
model_4A <- lm(ln_USV ~ CONREV +
                 PROFREV +
                 FRANC +
                 GEN_SP +
                 GEN_RPG +
                 GEN_MI +
                 OPUSD +
                 OMUL +
                 SP.MP +
                 PLF_PC +
                 ESRB_M +
                 PUBS +
                 INDIE +
                 soundtrack +
                 SMPRES +
                 N_LAN +
                 OPUSD_STAR,
               data=vg_data_test, na.action = na.omit)

# Linear regression including OPUSD Gaussian copula term and interactions
model_4B <- lm(ln_USV ~ (CONREV * FRANC) +
                 (CONREV * GEN_SP) +
                 (CONREV * GEN_RPG) +
                 (CONREV * GEN_MI) +
                 (CONREV * OPUSD) +
                 (CONREV * OMUL_D) +
                 (CONREV * SP.MP_D) +
                 (CONREV * PLF_PC) +
                 (CONREV * ESRB_M) +
                 (PROFREV * FRANC) +
                 (PROFREV * GEN_SP) +
                 (PROFREV * GEN_RPG) +
                 (PROFREV * GEN_MI) +
                 (PROFREV * OPUSD) +
                 (PROFREV * OMUL_D) +
                 (PROFREV * SP.MP_D) +
                 (PROFREV * PLF_PC) +
                 (PROFREV * ESRB_M)+
                 PUBS +
                 INDIE +
                 soundtrack +
                 SMPRES +
                 N_LAN +
                 OPUSD_STAR,
               data=data_prepped, na.action = na.omit)

# Linear regression including PROFREV and OPUSD Gaussian copula terms
model_5A <- lm(ln_USV ~ CONREV +
                 PROFREV +
                 FRANC +
                 GEN_SP +
                 GEN_RPG +
                 GEN_MI +
                 OPUSD +
                 OMUL +
                 SP.MP +
                 PLF_PC +
                 ESRB_M +
                 PUBS +
                 INDIE +
                 soundtrack +
                 SMPRES +
                 N_LAN +
                 PROFREV_STAR +
                 OPUSD_STAR,
               data=vg_data_test, na.action = na.omit)


# Linear regression including PROFREV and OPUSD Gaussian copula terms and interactions
model_5B <- lm(ln_USV ~ (CONREV * FRANC) +
                 (CONREV * GEN_SP) +
                 (CONREV * GEN_RPG) +
                 (CONREV * GEN_MI) +
                 (CONREV * OPUSD) +
                 (CONREV * OMUL_D) +
                 (CONREV * SP.MP_D) +
                 (CONREV * PLF_PC) +
                 (CONREV * ESRB_M) +
                 (PROFREV * FRANC) +
                 (PROFREV * GEN_SP) +
                 (PROFREV * GEN_RPG) +
                 (PROFREV * GEN_MI) +
                 (PROFREV * OPUSD) +
                 (PROFREV * OMUL_D) +
                 (PROFREV * SP.MP_D) +
                 (PROFREV * PLF_PC) +
                 (PROFREV * ESRB_M)+
                 PUBS +
                 INDIE +
                 soundtrack +
                 SMPRES +
                 N_LAN +
                 PROFREV_STAR +
                 OPUSD_STAR,
               data=data_prepped, na.action = na.omit)

# Linear regression including all Gaussian copula terms
model_6A <- lm(ln_USV ~ CONREV +
                 PROFREV +
                 FRANC +
                 GEN_SP +
                 GEN_RPG +
                 GEN_MI +
                 OPUSD +
                 OMUL +
                 SP.MP +
                 PLF_PC +
                 ESRB_M +
                 PUBS +
                 INDIE +
                 soundtrack +
                 SMPRES +
                 N_LAN +
                 CONREV_STAR +
                 PROFREV_STAR +
                 OPUSD_STAR,
               data=vg_data_test, na.action = na.omit)

# Linear regression including all Gaussian copula terms and interactions
model_6B <- lm(ln_USV ~ (CONREV * FRANC) +
                 (CONREV * GEN_SP) +
                 (CONREV * GEN_RPG) +
                 (CONREV * GEN_MI) +
                 (CONREV * OPUSD) +
                 (CONREV * OMUL_D) +
                 (CONREV * SP.MP_D) +
                 (CONREV * PLF_PC) +
                 (CONREV * ESRB_M) +
                 (PROFREV * FRANC) +
                 (PROFREV * GEN_SP) +
                 (PROFREV * GEN_RPG) +
                 (PROFREV * GEN_MI) +
                 (PROFREV * OPUSD) +
                 (PROFREV * OMUL_D) +
                 (PROFREV * SP.MP_D) +
                 (PROFREV * PLF_PC) +
                 (PROFREV * ESRB_M)+
                 PUBS +
                 INDIE +
                 soundtrack +
                 SMPRES +
                 N_LAN +
                 CONREV_STAR +
                 PROFREV_STAR +
                 OPUSD_STAR,
               data=data_prepped, na.action = na.omit)


###############################################################################
################### Correlation test ##########################################
###############################################################################

# Library

# install.packages("Hmisc")
# install.packages("writexl")
library("writexl")
library("Hmisc")
library("xtable")

# Prep data
corr_data <- vg_data_test %>% 
  select(c("ln_USV", "CONREV","PROFREV", "FRANC","OPUSD", "GEN_SP", "GEN_RPG", "GEN_MI","OMUL", "SP.MP", "PLF_PC", "ESRB_M"))

pearson <- round(cor(corr_data, method = "pearson", use = "complete.obs"),3)
pearson

pearson_p <- rcorr(as.matrix(corr_data))
pearson_p

corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

pearson_star <- corstarsl(corr_data)

write_xlsx(pearson_star, "../gen/corr_matrix_V5.xlsx")

###############################################################################
################### VIF #######################################################
###############################################################################

library("performance")

# Linear regression without interaction terms
cor_mod_1 <- lm(ln_USV ~ CONREV +
                  PROFREV +
                  FRANC +
                  GEN_SP +
                  GEN_RPG +
                  GEN_MI +
                  OPUSD +
                  OMUL +
                  SP.MP +
                  PLF_PC +
                  ESRB_M +
                  PUBS +
                  INDIE +
                  soundtrack +
                  SMPRES +
                  N_LAN ,
                data=vg_data_test, na.action = na.omit)


# Linear regression including interaction terms
cor_mod_2 <- lm(ln_USV ~ (CONREV * FRANC) +
                  (CONREV * GEN_SP) +
                  (CONREV * GEN_RPG) +
                  (CONREV * GEN_MI) +
                  (CONREV * OPUSD) +
                  (CONREV * OMUL) +
                  (CONREV * SP.MP) +
                  (CONREV * PLF_PC) +
                  (CONREV * ESRB_M) +
                  (PROFREV * FRANC) +
                  (PROFREV * GEN_SP) +
                  (PROFREV * GEN_RPG) +
                  (PROFREV * GEN_MI) +
                  (PROFREV * OPUSD) +
                  (PROFREV * OMUL) +
                  (PROFREV * SP.MP) +
                  (PROFREV * PLF_PC) +
                  (PROFREV * ESRB_M) +
                  PUBS +
                  INDIE +
                  soundtrack +
                  SMPRES +
                  N_LAN,
                data=vg_data_test, na.action = na.omit)

# Linear regression including interaction terms
cor_mod_3 <- lm(ln_USV ~ (CONREV * FRANC) +
                  (CONREV * GEN_SP) +
                  (CONREV * GEN_RPG) +
                  (CONREV * GEN_MI) +
                  (CONREV * OPUSD) +
                  (CONREV * OMUL_D) +
                  (CONREV * SP.MP_D) +
                  (CONREV * PLF_PC) +
                  (CONREV * ESRB_M) +
                  (PROFREV * FRANC) +
                  (PROFREV * GEN_SP) +
                  (PROFREV * GEN_RPG) +
                  (PROFREV * GEN_MI) +
                  (PROFREV * OPUSD) +
                  (PROFREV * OMUL_D) +
                  (PROFREV * SP.MP_D) +
                  (PROFREV * PLF_PC) +
                  (PROFREV * ESRB_M)+
                  PUBS +
                  INDIE +
                  soundtrack +
                  SMPRES +
                  N_LAN,
                data=data_prepped, na.action = na.omit)


# check_collinearity
multicollinearity(cor_mod_1)

multicollinearity(cor_mod_3)


cor_models <- list(cor_mod_2, cor_mod_3)
names(cor_models) <- c("RAW", "Adjusted")

msummary(cor_models, estimate = "estimate", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. cor_models summary", statistic = "{std.error} {p.value}" )



###############################################################################
################### Breusch-Pagan test ########################################
###############################################################################
# install.packages("see")
library("lmtest")
library("sandwich")
library("see")
library("performance")

# Based on model without interaction
bp_test_model5A <- bptest(model_5A)
bp_test_model5A

# Based on model with interaction
bp_test_model3B <- bptest(model_3B)
bp_test_model3B


# Use this one for figures
plot(model_5A)


###############################################################################
################### Copula terms significance #################################
###############################################################################

GC_model_selection <- list(model_2A, model_2B, model_3A, model_3B, model_4A, model_4B, model_5A, model_5B, model_6A
                           , model_6B)
names(GC_model_selection) <- c("model_2A", "model_2B", "model_3A", "model_3B", "model_4A", "model_4B", "model_5A", "model_5B", "model_6A", "model_6B")

msummary(GC_model_selection, output = "table_pvalue_selection3.docx", estimate = "p.value", gof_omit = "AIC|BIC|Log|Pseudo", title = "Gaussian copula terms significance table", statistic = NULL)


###############################################################################
####### MODEL GENERATION Model 5A #############################################
###############################################################################

# Robust standard errors version

RSE_model_5A <- coeftest(model_5A, vcov = vcovHC(model_5A, type = "HC0", save = TRUE))

msummary(RSE_model_5A, output = "../gen/RSE_model5A_coef.docx", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary", statistic = NULL)

msummary(RSE_model_5A, output = "../gen/RSE_model5A_sterror.docx", estimate = "std.error", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary", statistic = NULL)

msummary(RSE_model_5A, output = "../gen/RSE_model5A_pvalue.docx", estimate = "p.value", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary", statistic = NULL)

# Retrieving r^2 etc.
msummary(model_5A, output = "../gen/model5A_coef.docx", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary", statistic = NULL)

# Regular version

msummary(model_5A, output = "../gen/model5A_coef.docx", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary", statistic = NULL)

msummary(model_5A, output = "../gen/model5A_sterror.docx", estimate = "std.error", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary", statistic = NULL)

msummary(model_5A, output = "../gen/model5A_pvalue.docx", estimate = "p.value", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary", statistic = NULL)


###############################################################################
################### H1C TESTING ###############################################
###############################################################################

library(car)

# Testing for H1C of model_5A
H1C_model5A_answer <- linearHypothesis(model_5A, "CONREV - PROFREV = 0")
H1C_model5A_answer



# # Function to get the estimates and test between the difference of two coefficients.
# difftest_lm <- function(x1, x2, model){
#   diffest <- summary(model)$coef[x1,"Estimate"]-summary(model)$coef[x2,"Estimate"]
#   vardiff <- (summary(model)$coef[x1,"Std. Error"]^2 + 
#                 summary(model)$coef[x2,"Std. Error"]^2) - (2*(vcov(model)[x1, x2])) 
#   # variance of x1 + variance of x2 - 2*covariance of x1 and x2
#   diffse <- sqrt(vardiff)
#   tdiff <- (diffest)/(diffse)
#   ptdiff <- 2*(1-pt(abs(tdiff), model$df, lower.tail=T))
#   upr <- diffest + qt(.975, df = model$df)*diffse # will usually be very close to 1.96
#   lwr <- diffest + qt(.025, df = model$df)*diffse
#   df <- model$df
#   return(list(est=round(diffest, digits =2), 
#               t=round(tdiff, digits = 2), 
#               p=round(ptdiff, digits = 4), 
#               lwr=round(lwr, digits = 2), 
#               upr=round(upr, digits = 2),
#               df = df))
# }
# 
# 
# H1C_model5A_answer2 <- difftest_lm("CONREV", "PROFREV", RSE_model_5A)
# H1C_model5A_answer2


###############################################################################
####### MODEL GENERATION Model 3B #############################################
###############################################################################

# Robust standard errors version

RSE_model_3B <- coeftest(model_3B, vcov = vcovHC(model_3B, type = "HC0", save = TRUE))

msummary(RSE_model_3B, output = "../gen/RSE_model3B_coef.docx", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary", statistic = NULL)

msummary(RSE_model_3B, output = "../gen/RSE_model3B_sterror.docx", estimate = "std.error", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary", statistic = NULL)

msummary(RSE_model_3B, output = "../gen/RSE_model3B_pvalue.docx", estimate = "p.value", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary", statistic = NULL)

# Retrieving r^2 etc.
msummary(model_3B, output = "../gen/model3B_coef.docx", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary", statistic = NULL)

# Regular version

msummary(model_3B, output = "../gen/model3B_coef.docx", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary", statistic = NULL)

msummary(model_3B, output = "../gen/model3B_sterror.docx", estimate = "std.error", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary", statistic = NULL)

msummary(model_3B, output = "../gen/model3B_pvalue.docx", estimate = "p.value", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary", statistic = NULL)


###############################################################################
################### REST OF THE MODELS ########################################
###############################################################################

RSE_model_1A <- coeftest(model_1A, vcov = vcovHC(model_1A, type = "HC0", save = TRUE))
RSE_model_1B <- coeftest(model_1B, vcov = vcovHC(model_1B, type = "HC0", save = TRUE))
RSE_model_2A <- coeftest(model_2A, vcov = vcovHC(model_2A, type = "HC0", save = TRUE))
RSE_model_2B <- coeftest(model_2B, vcov = vcovHC(model_2B, type = "HC0", save = TRUE))
RSE_model_3A <- coeftest(model_3A, vcov = vcovHC(model_3A, type = "HC0", save = TRUE))
RSE_model_4A <- coeftest(model_4A, vcov = vcovHC(model_4A, type = "HC0", save = TRUE))
RSE_model_4B <- coeftest(model_4B, vcov = vcovHC(model_4B, type = "HC0", save = TRUE))
RSE_model_5B <- coeftest(model_5B, vcov = vcovHC(model_5B, type = "HC0", save = TRUE))
RSE_model_6A <- coeftest(model_6A, vcov = vcovHC(model_6A, type = "HC0", save = TRUE))
RSE_model_6B <- coeftest(model_6B, vcov = vcovHC(model_6B, type = "HC0", save = TRUE))

# RSE Models without interactions
RSE_models1 <- list(RSE_model_1A, RSE_model_2A,RSE_model_3A,RSE_model_4A,RSE_model_6A)
names(RSE_models1) <- c("model 1A", "model 2A","model 3A", "model 4A","model 6A")

msummary(RSE_models1, output = "../gen/RSE_models1.docx", estimate  = "{estimate} {stars} ({std.error})", gof_omit = "AIC|BIC|Log|Pseu",  stars=T, title = "Table X. Regression models summary", statistic = NULL)

# Getting R^2.
models1 <- list(model_1A, model_2A,model_3A,model_4A,model_6A)
names(models1) <- c("model 1A", "model 2A","model 3A", "model 4A","model 6A")

msummary(models1, output = "../gen/models1.docx",  estimate  = "{estimate}  ({std. error})", gof_omit = "AIC|BIC|Log|Pseu",  stars=T, title = "Table X. Regression models summary", statistic = NULL)



# RSE Models with interactions
RSE_models2 <- list(RSE_model_1B, RSE_model_2B,RSE_model_4B,RSE_model_5B,RSE_model_6B)
names(RSE_models2) <- c("model 1B", "model 2B","model 4B", "model 5B","model 6B")

msummary(RSE_models2, output = "../gen/RSE_models2.docx", estimate  = "{estimate} {stars} ({std.error})", gof_omit = "AIC|BIC|Log|Pseu",  stars=T, title = "Table X. Regression models summary", statistic = NULL)

# Getting R^2.
models2 <- list(model_1B, model_2B,model_4B,model_5B,model_6B)
names(models2) <- c("model 1B", "model 2B","model 4B", "model 5B","model 6B")

msummary(models2, output = "../gen/models2.docx", gof_omit = "AIC|BIC|Log|Pseu",  stars=T, title = "Table X. Regression models summary", statistic = "std.error")

###############################################################################
#################### ROBUSTNESS CHECK MODELS ##################################
###############################################################################


# Regular OLS regression without control variables interactions or copulas
RCM_model_1A <- lm(ln_USV ~ CONREV +
                     PROFREV +
                     FRANC +
                     GEN_SP +
                     GEN_RPG +
                     GEN_MI +
                     OPUSD +
                     OMUL +
                     SP.MP +
                     PLF_PC +
                     ESRB_M,
                   data=vg_data_test, na.action = na.omit)


# Interaction but no covariates or copulas
RCM_model_1B <- lm(ln_USV ~ (CONREV * FRANC) +
                     (CONREV * GEN_SP) +
                     (CONREV * GEN_RPG) +
                     (CONREV * GEN_MI) +
                     (CONREV * OPUSD) +
                     (CONREV * OMUL_D) +
                     (CONREV * SP.MP_D) +
                     (CONREV * PLF_PC) +
                     (CONREV * ESRB_M) +
                     (PROFREV * FRANC) +
                     (PROFREV * GEN_SP) +
                     (PROFREV * GEN_RPG) +
                     (PROFREV * GEN_MI) +
                     (PROFREV * OPUSD) +
                     (PROFREV * OMUL_D) +
                     (PROFREV * SP.MP_D) +
                     (PROFREV * PLF_PC) +
                     (PROFREV * ESRB_M),
                   data=data_prepped, na.action = na.omit)

# Regular OLS regression based on model 5A but only professional reviews
RCM_model_2A <- lm(ln_USV ~ PROFREV +
                     FRANC +
                     GEN_SP +
                     GEN_RPG +
                     GEN_MI +
                     OPUSD +
                     OMUL +
                     SP.MP +
                     PLF_PC +
                     ESRB_M +
                     PUBS +
                     INDIE +
                     soundtrack +
                     SMPRES +
                     N_LAN + 
                     PROFREV_STAR +
                     OPUSD_STAR,
                   data=vg_data_test, na.action = na.omit)

# Regular OLS regression based on model 3B but only professional reviews
RCM_model_2B <- lm(ln_USV ~ 
                     (PROFREV * FRANC) +
                     (PROFREV * GEN_SP) +
                     (PROFREV * GEN_RPG) +
                     (PROFREV * GEN_MI) +
                     (PROFREV * OPUSD) +
                     (PROFREV * OMUL_D) +
                     (PROFREV * SP.MP_D) +
                     (PROFREV * PLF_PC) +
                     (PROFREV * ESRB_M) +
                     PUBS +
                     INDIE +
                     soundtrack +
                     SMPRES +
                     N_LAN + 
                     PROFREV_STAR,
                   data=data_prepped, na.action = na.omit)

# Regular OLS regression but only consumer reviews
RCM_model_3A <- lm(ln_USV ~ CONREV +
                     FRANC +
                     GEN_SP +
                     GEN_RPG +
                     GEN_MI +
                     OPUSD +
                     OMUL +
                     SP.MP +
                     PLF_PC +
                     ESRB_M +
                     PUBS +
                     INDIE +
                     soundtrack +
                     SMPRES +
                     N_LAN + 
                     OPUSD_STAR,
                   data=vg_data_test, na.action = na.omit)

# Regular OLS regression with interaction but only consumer reviews
RCM_model_3B <- lm(ln_USV ~ 
                     (CONREV * FRANC) +
                     (CONREV * GEN_SP) +
                     (CONREV * GEN_RPG) +
                     (CONREV * GEN_MI) +
                     (CONREV * OPUSD) +
                     (CONREV * OMUL_D) +
                     (CONREV * SP.MP_D) +
                     (CONREV * PLF_PC) +
                     (CONREV * ESRB_M) +
                     PUBS +
                     INDIE +
                     soundtrack +
                     SMPRES +
                     N_LAN,
                   data=data_prepped, na.action = na.omit)


# Build up by adding one moderator for interaction at a time, to compile model 3B
RCM_model_4 <- lm(ln_USV ~ (CONREV * FRANC) +
                    (PROFREV * FRANC) +
                    GEN_SP +
                    GEN_RPG +
                    GEN_MI +
                    OPUSD +
                    OMUL_D +
                    SP.MP_D +
                    PLF_PC +
                    ESRB_M +
                    PUBS +
                    INDIE +
                    soundtrack +
                    SMPRES +
                    N_LAN +
                    PROFREV_STAR,
                  data=data_prepped, na.action = na.omit)

# Build up by adding one moderator for interaction at a time, to compile model 3B
RCM_model_5 <- lm(ln_USV ~ (CONREV * FRANC) +
                    (PROFREV * FRANC) +
                    (CONREV * GEN_SP) +
                    (CONREV * GEN_RPG) +
                    (CONREV * GEN_MI) +
                    (PROFREV * GEN_SP) +
                    (PROFREV * GEN_RPG) +
                    (PROFREV * GEN_MI) +
                    OPUSD +
                    OMUL_D +
                    SP.MP_D +
                    PLF_PC +
                    ESRB_M +
                    PUBS +
                    INDIE +
                    soundtrack +
                    SMPRES +
                    N_LAN +
                    PROFREV_STAR,
                  data=data_prepped, na.action = na.omit)

# Build up by adding one moderator for interaction at a time, to compile model 3B
RCM_model_6 <- lm(ln_USV ~ (CONREV * FRANC) +
                    (PROFREV * FRANC) +
                    (CONREV * GEN_SP) +
                    (CONREV * GEN_RPG) +
                    (CONREV * GEN_MI) +
                    (PROFREV * GEN_SP) +
                    (PROFREV * GEN_RPG) +
                    (PROFREV * GEN_MI) +
                    (CONREV * OPUSD) +
                    (PROFREV * OPUSD) +
                    OMUL_D +
                    SP.MP_D +
                    PLF_PC +
                    ESRB_M +
                    PUBS +
                    INDIE +
                    soundtrack +
                    SMPRES +
                    N_LAN +
                    PROFREV_STAR,
                  data=data_prepped, na.action = na.omit)

# Build up by adding one moderator for interaction at a time, to compile model 3B
RCM_model_7 <- lm(ln_USV ~ (CONREV * FRANC) +
                    (PROFREV * FRANC) +
                    (CONREV * GEN_SP) +
                    (CONREV * GEN_RPG) +
                    (CONREV * GEN_MI) +
                    (PROFREV * GEN_SP) +
                    (PROFREV * GEN_RPG) +
                    (PROFREV * GEN_MI) +
                    (CONREV * OPUSD) +
                    (PROFREV * OPUSD) +
                    (CONREV * OMUL_D) +
                    (CONREV * SP.MP_D) +
                    (PROFREV * OMUL_D) +
                    (PROFREV * SP.MP_D) +
                    PLF_PC +
                    ESRB_M +
                    PUBS +
                    INDIE +
                    soundtrack +
                    SMPRES +
                    N_LAN +
                    PROFREV_STAR,
                  data=data_prepped, na.action = na.omit)


# Build up by adding one moderator for interaction at a time, to compile model 3B
RCM_model_8 <- lm(ln_USV ~ (CONREV * FRANC) +
                    (PROFREV * FRANC) +
                    (CONREV * GEN_SP) +
                    (CONREV * GEN_RPG) +
                    (CONREV * GEN_MI) +
                    (PROFREV * GEN_SP) +
                    (PROFREV * GEN_RPG) +
                    (PROFREV * GEN_MI) +
                    (CONREV * OPUSD) +
                    (PROFREV * OPUSD) +
                    (CONREV * OMUL_D) +
                    (CONREV * SP.MP_D) +
                    (PROFREV * OMUL_D) +
                    (PROFREV * SP.MP_D) +
                    (CONREV * PLF_PC) +
                    (PROFREV * PLF_PC) +
                    ESRB_M +
                    PUBS +
                    INDIE +
                    soundtrack +
                    SMPRES +
                    N_LAN +
                    PROFREV_STAR,
                  data=data_prepped, na.action = na.omit)

# Build up by adding one moderator for interaction at a time, to compile model 3B
RCM_model_9 <- lm(ln_USV ~ (CONREV * FRANC) +
                    (PROFREV * FRANC) +
                    (CONREV * GEN_SP) +
                    (CONREV * GEN_RPG) +
                    (CONREV * GEN_MI) +
                    (PROFREV * GEN_SP) +
                    (PROFREV * GEN_RPG) +
                    (PROFREV * GEN_MI) +
                    (CONREV * OPUSD) +
                    (PROFREV * OPUSD) +
                    (CONREV * OMUL_D) +
                    (CONREV * SP.MP_D) +
                    (PROFREV * OMUL_D) +
                    (PROFREV * SP.MP_D) +
                    (CONREV * PLF_PC) +
                    (PROFREV * PLF_PC) +
                    (CONREV * ESRB_M) +
                    (PROFREV * ESRB_M)+
                    PUBS +
                    INDIE +
                    soundtrack +
                    SMPRES +
                    N_LAN +
                    PROFREV_STAR,
                  data=data_prepped, na.action = na.omit)

# Robust standard errors version

RSE_RCMmodel_1A <- coeftest(RCM_model_1A, vcov = vcovHC(RCM_model_1A, type = "HC0", save = TRUE))
RSE_RCMmodel_1B <- coeftest(RCM_model_1B, vcov = vcovHC(RCM_model_1B, type = "HC0", save = TRUE))
RSE_RCMmodel_2A <- coeftest(RCM_model_2A, vcov = vcovHC(RCM_model_2A, type = "HC0", save = TRUE))
RSE_RCMmodel_2B <- coeftest(RCM_model_2B, vcov = vcovHC(RCM_model_2B, type = "HC0", save = TRUE))
RSE_RCMmodel_3A <- coeftest(RCM_model_3A, vcov = vcovHC(RCM_model_3A, type = "HC0", save = TRUE))
RSE_RCMmodel_3B <- coeftest(RCM_model_3B, vcov = vcovHC(RCM_model_3B, type = "HC0", save = TRUE))
RSE_RCMmodel_4 <- coeftest(RCM_model_4, vcov = vcovHC(RCM_model_4, type = "HC0", save = TRUE))
RSE_RCMmodel_5 <- coeftest(RCM_model_5, vcov = vcovHC(RCM_model_5, type = "HC0", save = TRUE))
RSE_RCMmodel_6 <- coeftest(RCM_model_6, vcov = vcovHC(RCM_model_6, type = "HC0", save = TRUE))
RSE_RCMmodel_7 <- coeftest(RCM_model_7, vcov = vcovHC(RCM_model_7, type = "HC0", save = TRUE))
RSE_RCMmodel_8 <- coeftest(RCM_model_8, vcov = vcovHC(RCM_model_8, type = "HC0", save = TRUE))
RSE_RCMmodel_9 <- coeftest(RCM_model_9, vcov = vcovHC(RCM_model_9, type = "HC0", save = TRUE))


# RCM Models 1A - 3B
RSE_RCM <- list(RSE_RCMmodel_1A, RSE_RCMmodel_1B,RSE_RCMmodel_2A,RSE_RCMmodel_2B,RSE_RCMmodel_3A,RSE_RCMmodel_3B)
names(RSE_RCM) <- c("RCM model 1A", "RCM Model 1B","RCM model 2A", "RCM Model 2B","RCM model 3A", "RCM Model 3B")

msummary(RSE_RCM, output = "../gen/RSE_RCM.docx", estimate  = "{estimate} {stars} ({std.error})", gof_omit = "Pseudo",  stars=T, title = "Table X. Regression models summary", statistic = NULL)


# Retrieving r^2 etc.
RCM <- list(RCM_model_1A, RCM_model_1B,RCM_model_2A,RCM_model_2B,RCM_model_3A,RCM_model_3B)
names(RCM) <- c("model 1A", "model 1B","model 2A", "model 2B","model 3A", "model 3B")

msummary(RCM, output = "../gen/RCM.docx", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary", statistic = NULL)


# RCM Models 4 - 9
RSE_RCM2 <- list(RSE_RCMmodel_4, RSE_RCMmodel_5,RSE_RCMmodel_6,RSE_RCMmodel_7,RSE_RCMmodel_8,RSE_RCMmodel_9)
names(RSE_RCM2) <- c("RCM model 4", "RCM Model 5","RCM model 6", "RCM Model 7","RCM model 8", "RCM Model 9")

msummary(RSE_RCM2, output = "../gen/RSE_RCM2.docx", gof_omit = "Pseudo", stars=T, title = "Table X. Regression models summary", statistic = "std.error")


# Retrieving r^2 etc.
RCM2 <- list(RCM_model_4, RCM_model_5,RCM_model_6,RCM_model_7,RCM_model_8,RCM_model_9)
names(RCM2) <- c("model 4", "model 5","model 6", "model 7","model 8", "model 9")

msummary(RCM2, output = "../gen/RCM2.docx", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary", statistic = NULL)


###############################################################################
################### MOdel 3B with raw data ####################################
###############################################################################


# Linear regression including PROFREV Gaussian copula term and interactions
model_3B_raw <- lm(ln_USV ~ (CONREV * FRANC) +
                     (CONREV * GEN_SP) +
                     (CONREV * GEN_RPG) +
                     (CONREV * GEN_MI) +
                     (CONREV * OPUSD) +
                     (CONREV * OMUL) +
                     (CONREV * SP.MP) +
                     (CONREV * PLF_PC) +
                     (CONREV * ESRB_M) +
                     (PROFREV * FRANC) +
                     (PROFREV * GEN_SP) +
                     (PROFREV * GEN_RPG) +
                     (PROFREV * GEN_MI) +
                     (PROFREV * OPUSD) +
                     (PROFREV * OMUL) +
                     (PROFREV * SP.MP) +
                     (PROFREV * PLF_PC) +
                     (PROFREV * ESRB_M)+
                     PUBS +
                     INDIE +
                     soundtrack +
                     SMPRES +
                     N_LAN +
                     PROFREV_STAR,
                   data=vg_data_test, na.action = na.omit)

# Robust standard errors version

RSE_model_3B_raw <- coeftest(model_3B_raw, vcov = vcovHC(model_3B_raw, type = "HC0", save = TRUE))

msummary(RSE_model_3B_raw, output = "../gen/RSE_model3B_coef_raw.docx", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary", statistic = NULL)

msummary(RSE_model_3B_raw, output = "../gen/RSE_model3B_sterror_raw.docx", estimate = "std.error", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary", statistic = NULL)

msummary(RSE_model_3B_raw, output = "../gen/RSE_model3B_pvalue_raw.docx", estimate = "p.value", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary", statistic = NULL)

# Retrieving r^2 etc.
msummary(model_3B_raw, output = "../gen/model3B_coef_raw.docx", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary", statistic = NULL)


###############################################################################
################### EXPLORATORY MODELS ########################################
###############################################################################

# Exploratory linear model including Gaussian copula
exp_mod_1 <- lm(ln_USV ~ (CONREV * FRANC) +
                  (CONREV * GEN_SP) +
                  (CONREV * GEN_RPG) +
                  (CONREV * GEN_MI) +
                  (CONREV * OPUSD) +
                  (CONREV * OMUL_D) +
                  (CONREV * SP.MP_D) +
                  (CONREV * PLF_PC) +
                  (CONREV * ESRB_M) +
                  (PROFREV * FRANC) +
                  (PROFREV * GEN_SP) +
                  (PROFREV * GEN_RPG) +
                  (PROFREV * GEN_MI) +
                  (PROFREV * OPUSD) +
                  (PROFREV * OMUL_D) +
                  (PROFREV * SP.MP_D) +
                  (PROFREV * PLF_PC) +
                  (PROFREV * ESRB_M)+
                  (CONREV * PUBS) +
                  (CONREV * INDIE) +
                  (CONREV * soundtrack) +
                  (CONREV * SMPRES) +
                  (CONREV * N_LAN) +
                  (PROFREV * PUBS) +
                  (PROFREV * INDIE) +
                  (PROFREV * soundtrack) +
                  (PROFREV * SMPRES) +
                  (PROFREV * N_LAN) +
                  PROFREV_STAR,
                data=data_prepped, na.action = na.omit)


RSE_expmod1 <- coeftest(exp_mod_1, vcov = vcovHC(exp_mod_1, type = "HC0", save = TRUE))

msummary(RSE_expmod1, output = "../gen/RSE_expmod1_coef.docx", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)

msummary(RSE_expmod1, output = "../gen/RSE_expmod1_sterror.docx", estimate = "std.error", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)

msummary(RSE_expmod1, output = "../gen/RSE_expmod1_pvalue.docx", estimate = "p.value", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)

# Retrieving r^2 etc
msummary(exp_mod_1, output = "../gen/exp_mod_1_tvalue.docx", estimate = "statistic", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)



###############################################################################
#################### INTERACTION PLOTS ########################################
###############################################################################

# install.packages("interactions")
library("interactions")

# Franchise
int_plot_FRANC1 <- interact_plot(model_3B, pred = CONREV, modx = FRANC)
int_plot_FRANC2 <- interact_plot(model_3B, pred = PROFREV, modx = FRANC)

# Genre sports
int_plot_SP1 <- interact_plot(model_3B, pred = CONREV, modx = GEN_SP)
int_plot_SP2 <- interact_plot(model_3B, pred = PROFREV, modx = GEN_SP)

# Genre RPG
int_plot_RPG1 <- interact_plot(model_3B, pred = CONREV, modx = GEN_RPG)
int_plot_RPG2 <- interact_plot(model_3B, pred = PROFREV, modx = GEN_RPG)

# Genre MI
int_plot_MI1 <- interact_plot(model_3B, pred = CONREV, modx = GEN_MI)
int_plot_MI2 <- interact_plot(model_3B, pred = PROFREV, modx = GEN_MI)

# OPUSD
int_plot_OPUSD1 <- interact_plot(model_3B, pred = CONREV, modx = OPUSD)
int_plot_OPUSD2 <- interact_plot(model_3B, pred = PROFREV, modx = OPUSD)

# Only multiplayer
int_plot_OMUL1 <-interact_plot(model_3B, pred = CONREV, modx = OMUL_D)
int_plot_OMUL2 <-interact_plot(model_3B, pred = PROFREV, modx = OMUL_D)

# Singleplayer and multiplayer
int_plot_SP.MP1 <-interact_plot(model_3B, pred = CONREV, modx = SP.MP_D)
int_plot_SP.MP2 <-interact_plot(model_3B, pred = PROFREV, modx = SP.MP_D)

# Platform
int_plot_PC1 <-interact_plot(model_3B, pred = CONREV, modx = PLF_PC)
int_plot_PC2 <-interact_plot(model_3B, pred = PROFREV, modx = PLF_PC)

# ESRB
int_plot_ESRB1 <-interact_plot(model_3B, pred = CONREV, modx = ESRB_M)
int_plot_ESRB2 <-interact_plot(model_3B, pred = PROFREV, modx = ESRB_M)


int_plot_FRANC1
int_plot_FRANC2
int_plot_SP1
int_plot_SP2
int_plot_RPG1
int_plot_RPG2
int_plot_MI1
int_plot_MI2
int_plot_OPUSD1
int_plot_OPUSD2
int_plot_OMUL1
int_plot_OMUL2
int_plot_SP.MP1
int_plot_SP.MP2
int_plot_PC1 
int_plot_PC2 
int_plot_ESRB1
int_plot_ESRB2


###############################################################################
########## EXTRA MODEL TEST WITH DIFFERENT DUMMIES BASES ######################
###############################################################################

data_prepped2 <- data_prepped

data_prepped2$GEN_AC.AD <- ifelse(data_prepped$GEN_AC.AD_D == 1, .75, ifelse(data_prepped$GEN_SP_D == 1, -.25, ifelse(data_prepped$GEN_RPG_D == 1, -.25, ifelse(data_prepped$GEN_MI_D == 1, -.25,0 ))))
data_prepped2$GEN_SP <- ifelse(data_prepped$GEN_AC.AD_D == 1, -.25, ifelse(data_prepped$GEN_SP_D == 1, .75, ifelse(data_prepped$GEN_RPG_D == 1, -.25, ifelse(data_prepped$GEN_MI_D == 1, -.25,0 )))) 
data_prepped2$GEN_RPG <- ifelse(data_prepped$GEN_AC.AD_D == 1, -.25, ifelse(data_prepped$GEN_SP_D == 1, -.25, ifelse(data_prepped$GEN_RPG_D == 1, .75, ifelse(data_prepped$GEN_MI_D == 1, -.25,0 ))))
data_prepped2$GEN_MI <- ifelse(data_prepped2$GEN_MI > -.25, -.25, -.25)

# Linear regression including PROFREV Gaussian copula term and interactions
model_3B_reversed <- lm(ln_USV ~ (CONREV * FRANC) +
                          (CONREV * GEN_SP) +
                          (CONREV * GEN_RPG) +
                          (CONREV * GEN_AC.AD) +
                          (CONREV * OPUSD) +
                          (CONREV * OMUL_D) +
                          (CONREV * OSIN_D) +
                          (CONREV * PLF_PC) +
                          (CONREV * ESRB_M) +
                          (PROFREV * FRANC) +
                          (PROFREV * GEN_SP) +
                          (PROFREV * GEN_RPG) +
                          (PROFREV * GEN_AC.AD) +
                          (PROFREV * OPUSD) +
                          (PROFREV * OMUL_D) +
                          (PROFREV * OSIN_D) +
                          (PROFREV * PLF_PC) +
                          (PROFREV * ESRB_M)+
                          PUBS +
                          INDIE +
                          soundtrack +
                          SMPRES +
                          N_LAN +
                          PROFREV_STAR,
                        data=data_prepped2, na.action = na.omit)


# Genre sports
int_plot_AC.AD1 <- interact_plot(model_3B_reversed, pred = CONREV, modx = GEN_AC.AD)
int_plot_AC.AD2 <- interact_plot(model_3B_reversed, pred = PROFREV, modx = GEN_AC.AD)
int_plot_AC.AD1
int_plot_AC.AD2

# Only multiplayer
int_plot_OSIN1 <-interact_plot(model_3B_reversed, pred = CONREV, modx = OSIN_D)
int_plot_OSIN2 <-interact_plot(model_3B_reversed, pred = PROFREV, modx = OSIN_D)
int_plot_OSIN1 
int_plot_OSIN2 

RSE_rev_model3B <- coeftest(model_3B_reversed, vcov = vcovHC(model_3B_reversed, type = "HC0", save = TRUE))


msummary(RSE_rev_model3B, output = "../gen/RSE_rev_model3B_coef.docx", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)

msummary(RSE_rev_model3B, output = "../gen/RSE_rev_model3B_sterror.docx", estimate = "std.error", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)

msummary(RSE_rev_model3B, output = "../gen/RSE_rev_model3B_pvalue.docx", estimate = "p.value", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)

# Retrieving r^2 etc
msummary(model_3B_reversed, output = "../gen/model_3B_reversed_tvalue.docx", estimate = "statistic", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)

