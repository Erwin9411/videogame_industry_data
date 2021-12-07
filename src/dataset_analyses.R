# Install packages
# install.packages("AER")
# install.packages("systemfit")
# Loading Packages
library(readr)
library(dplyr)
library(tidyverse)
library(modelsummary)
library(tidyr)
library(broom)
library(AER)
library(systemfit)


# Loading data
vg_data <- read.csv("../gen/full_videogame_dataset.csv")

vg_data <- vg_data %>% 
  mutate(ln_USV = log(USV))

vg_data_test <- vg_data 
  
vg_data_test <- drop_na(vg_data_test)

#################################################################################
############################ DESCRIPTIVE STATISTICS #############################
#################################################################################

summary(vg_data_test$USV)
SD_USV <- sd(vg_data_test$USV)

summary(vg_data_test$CONREV)
SD_CONREV <- sd(vg_data_test$CONREV)

summary(vg_data_test$PROFREV)
SD_PROFREV <- sd(vg_data_test$PROFREV)

summary(vg_data_test$OPUSD)
SD_OPUSD <- sd(vg_data_test$OPUSD)
  
# Boxplot for outliers
boxplot(vg_data$ln_USV, main="Global sales", sub=paste("Outlier rows: ", boxplot.stats(vg_data$ln_USV)$out))

#################################################################################
############################ REGRESSION ANALYSES ################################
#################################################################################

# Linear regression
linmod_1 <- lm(ln_USV ~ CONREV + 
                 PROFREV +
                 FRANC +
                 GEN_AD +
                 GEN_RA +
                 GEN_RP +
                 GEN_SI +
                 GEN_SP +
                 GEN_ST +
                 GEN_MO +
                 GEN_MI +
                 OPUSD +
                 singleplayer +
                 multiplayer +
                 coop +
                 o_coop +
                 pvp +
                 PLF_PC +
                 ESRB_RP +
                 ESRB_E +
                 ESRB_T
                 ,data=vg_data_test, na.action = na.omit)
print(linmod_1)

summary_linmod_1 <- summary(linmod_1)
summary_linmod_1


###############################################################################
################### INCL CONTROL VARIABLES ####################################
###############################################################################

covariates <- cbind(vg_data_test$PUBS, vg_data_test$INDIE, vg_data_test$soundtrack, vg_data_test$SMPRES, vg_data_test$N_LAN, vg_data_test$LCP)

# Linear regression including control variables
linmod_2 <- lm(ln_USV ~ CONREV + 
                 PROFREV +
                 FRANC +
                 GEN_AD +
                 GEN_RA +
                 GEN_RP +
                 GEN_SI +
                 GEN_SP +
                 GEN_ST +
                 GEN_MO +
                 GEN_MI +
                 OPUSD +
                 singleplayer +
                 multiplayer +
                 coop +
                 o_coop +
                 pvp +
                 PLF_PC +
                 ESRB_RP +
                 ESRB_E +
                 ESRB_T +
                 covariates,
                 data=vg_data_test, na.action = na.omit)

print(linmod_2)

summary_linmod_2 <- summary(linmod_2)
summary_linmod_2


###############################################################################
################### INTERACTION TERMS INCLUDED ################################
###############################################################################

# Linear regression including interaction terms
lin_mod_3 <- lm(ln_USV ~ (CONREV * FRANC) +
                  (CONREV * GEN_AD) +
                  (CONREV * GEN_RA) +
                  (CONREV * GEN_RP) +
                  (CONREV * GEN_SI) +
                  (CONREV * GEN_SP) +
                  (CONREV * GEN_ST) +
                  (CONREV * GEN_MO) +
                  (CONREV * GEN_MI) +
                  (CONREV * OPUSD) +
                  (CONREV * singleplayer) +
                  (CONREV * multiplayer) +
                  (CONREV * coop) +
                  (CONREV * o_coop) +
                  (CONREV * pvp) +
                  (CONREV * PLF_PC) +
                  (CONREV * ESRB_RP) +
                  (CONREV * ESRB_E) +
                  (CONREV * ESRB_T) +
                  (PROFREV * FRANC) +
                  (PROFREV * GEN_AD) +
                  (PROFREV * GEN_RA) +
                  (PROFREV * GEN_RP) +
                  (PROFREV * GEN_SI) +
                  (PROFREV * GEN_SP) +
                  (PROFREV * GEN_ST) +
                  (PROFREV * GEN_MO) +
                  (PROFREV * GEN_MI) +
                  (PROFREV * OPUSD) +
                  (PROFREV * singleplayer) +
                  (PROFREV * multiplayer) +
                  (PROFREV * coop) +
                  (PROFREV * o_coop) +
                  (PROFREV * pvp) +
                  (PROFREV * PLF_PC) +
                  (PROFREV * ESRB_RP) +
                  (PROFREV * ESRB_E) +
                  (PROFREV * ESRB_T) +
                  covariates,
                data=vg_data_test, na.action = na.omit)


###############################################################################
################### OUTLIERS DROPPED ##########################################
###############################################################################

# Outliers
boxplot(ln_USV ~ CONREV, data = vg_data_test)
boxplot(ln_USV ~ PROFREV, data = vg_data_test)

outliers_ln_USV <- boxplot(vg_data_test$ln_USV, plot=TRUE)$out
out_test <-vg_data_test
out_test <- out_test[-which(out_test$ln_USV %in% outliers_ln_USV),]


outliers_CONREV <- boxplot(vg_data_test$CONREV, plot=TRUE)$out
out_test <-vg_data_test
out_test <- out_test[-which(out_test$CONREV %in% outliers_ln_USV),]

outliers_PROFREV <- boxplot(vg_data_test$PROFREV, plot=TRUE)$out
out_test <-vg_data_test
out_test <- out_test[-which(out_test$PROFREV %in% outliers_PROFREV),]

# Only outliers detected for PROFREV, these are dropped and the models are ran again below.

# Linear regression
out_linmod_1 <- lm(ln_USV ~ CONREV + 
                 PROFREV +
                 FRANC +
                 GEN_AD +
                 GEN_RA +
                 GEN_RP +
                 GEN_SI +
                 GEN_SP +
                 GEN_ST +
                 GEN_MO +
                 GEN_MI +
                 OPUSD +
                 singleplayer +
                 multiplayer +
                 coop +
                 o_coop +
                 pvp +
                 PLF_PC +
                 ESRB_RP +
                 ESRB_E +
                 ESRB_T
               ,data=out_test, na.action = na.omit)

summary_out_linmod_1 <- summary(out_linmod_1)
summary_out_linmod_1

###############################################################################
################### INCL CONTROL VARIABLES ####################################
###############################################################################

out_covariates <- cbind(out_test$PUBS, out_test$INDIE, out_test$soundtrack, out_test$SMPRES, out_test$N_LAN, out_test$LCP)

# Linear regression including control variables
out_linmod_2 <- lm(ln_USV ~ CONREV + 
                 PROFREV +
                 FRANC +
                 GEN_AD +
                 GEN_RA +
                 GEN_RP +
                 GEN_SI +
                 GEN_SP +
                 GEN_ST +
                 GEN_MO +
                 GEN_MI +
                 OPUSD +
                 singleplayer +
                 multiplayer +
                 coop +
                 o_coop +
                 pvp +
                 PLF_PC +
                 ESRB_RP +
                 ESRB_E +
                 ESRB_T +
                 out_covariates,
               data=out_test, na.action = na.omit)


summary_out_linmod_2 <- summary(out_linmod_2)
summary_out_linmod_2

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

# Making a subset of the endogenous variables, CONREV and PROFREV
GC_nonnormallity <- vg_data_test %>% 
  select(c(CONREV, PROFREV)) %>% 
  mutate(RES_LINMOD3 = residuals(lin_mod_3))

# Checking for nonnormallity with the Anderson-Darling test
CONREV_AD <- print(ad.test(GC_nonnormallity$CONREV))
shapiro.test(GC_nonnormallity$CONREV)

PROFREV_AD <- print(ad.test(GC_nonnormallity$PROFREV))
shapiro.test(GC_nonnormallity$PROFREV)

# CONREV_CVM <- cvm.test(GC_nonnormallity$CONREV)
# PROFREV_CVM <- cvm.test(GC_nonnormallity$PROFREV)

# Both test indicate the endogenous variables are not normally distributed. H0 can be rejected.

# Checking the kurtosis and skewness as additional information

CONREV_SKEW <- print(skewness(GC_nonnormallity$CONREV))
PROFREV_SKEW <- print(skewness(GC_nonnormallity$PROFREV))

CONREV_KUR <- print(kurtosis(GC_nonnormallity$CONREV))
PROFREV_KUR <- print(kurtosis(GC_nonnormallity$PROFREV))

# Visually investigate the distribution of CONREV
# Plot the density of CONREV against the normal distribution
ggplot(GC_nonnormallity, aes(x=CONREV)) +
  geom_density(alpha=0.4, fill="red") +
  stat_function(fun = dnorm,
                args = list(mean = mean(GC_nonnormallity$CONREV),
                            sd = sd(GC_nonnormallity$CONREV)), 
                color="blue") +
  xlim(-0.5,1.5) +
  theme_classic()

# Visually investigate the distribution of PROFREV
# Plot the density of PROFREV against the normal distribution
ggplot(GC_nonnormallity, aes(x=PROFREV)) +
  geom_density(alpha=0.4, fill="red") +
  stat_function(fun = dnorm,
                args = list(mean = mean(GC_nonnormallity$PROFREV),
                            sd = sd(GC_nonnormallity$PROFREV)), 
                color="blue") +
  xlim(-0.5,1.5) +
  theme_classic()

# Checking for normality of the residuals via the untreated model

untreatedModel2 <- summary(lm(ln_USV ~ CONREV + 
                                PROFREV +
                                FRANC +
                                GEN_AD +
                                GEN_RA +
                                GEN_RP +
                                GEN_SI +
                                GEN_SP +
                                GEN_ST +
                                GEN_MO +
                                GEN_MI +
                                OPUSD +
                                singleplayer +
                                multiplayer +
                                coop +
                                o_coop +
                                pvp +
                                PLF_PC +
                                ESRB_RP +
                                ESRB_E +
                                ESRB_T +
                                covariates,
                              data=vg_data_test, na.action = na.omit))

untreatedModel3 <- summary(lm(ln_USV ~ (CONREV * FRANC) +
                                (CONREV * GEN_AD) +
                                (CONREV * GEN_RA) +
                                (CONREV * GEN_RP) +
                                (CONREV * GEN_SI) +
                                (CONREV * GEN_SP) +
                                (CONREV * GEN_ST) +
                                (CONREV * GEN_MO) +
                                (CONREV * GEN_MI) +
                                (CONREV * OPUSD) +
                                (CONREV * singleplayer) +
                                (CONREV * multiplayer) +
                                (CONREV * coop) +
                                (CONREV * o_coop) +
                                (CONREV * pvp) +
                                (CONREV * PLF_PC) +
                                (CONREV * ESRB_RP) +
                                (CONREV * ESRB_E) +
                                (CONREV * ESRB_T) +
                                (PROFREV * FRANC) +
                                (PROFREV * GEN_AD) +
                                (PROFREV * GEN_RA) +
                                (PROFREV * GEN_RP) +
                                (PROFREV * GEN_SI) +
                                (PROFREV * GEN_SP) +
                                (PROFREV * GEN_ST) +
                                (PROFREV * GEN_MO) +
                                (PROFREV * GEN_MI) +
                                (PROFREV * OPUSD) +
                                (PROFREV * singleplayer) +
                                (PROFREV * multiplayer) +
                                (PROFREV * coop) +
                                (PROFREV * o_coop) +
                                (PROFREV * pvp) +
                                (PROFREV * PLF_PC) +
                                (PROFREV * ESRB_RP) +
                                (PROFREV * ESRB_E) +
                                (PROFREV * ESRB_T) +
                                covariates,
                              data=vg_data_test, na.action = na.omit))

# Visually investigate the distribution of the residuals of untreatedModel2
# Plot the density of the residuals against the normal distribution
ggplot(data.frame(untreatedModel2$residuals), aes(x=untreatedModel2.residuals)) +
  geom_density(alpha=0.4, fill="red") +
  stat_function(fun = dnorm,
                args = list(mean = mean(untreatedModel2$residuals),
                            sd = sd(untreatedModel2$residuals)), 
                color="blue") +
  xlim(-4,4) +
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
  theme_classic()



# Statistical tests of normal distribution of the residuals of model 2

# All tests combined (olsrr pack)
ols_test_normality(linmod_2)

# Anderson-Darling
nortest::ad.test(untreatedModel2$residuals)

# Cramer-von Mises
nortest::cvm.test(untreatedModel2$residuals)

# Shapiro-Wilk
shapiro.test(untreatedModel2$residuals)

# Kolmogorov-Smirnov
ks.test(untreatedModel2$residuals, "pnorm")



# Statistical tests of normal distribution of the residuals of model 3

# All tests combined (olsrr pack)
ols_test_normality(lin_mod_3)

# Anderson-Darling
nortest::ad.test(untreatedModel3$residuals)

# Cramer-von Mises
nortest::cvm.test(untreatedModel3$residuals)

# Shapiro-Wilk
shapiro.test(untreatedModel3$residuals)

# Kolmogorov-Smirnov
ks.test(untreatedModel3$residuals, "pnorm")

###############################################################################
################### Similar assumption test but on data without outliers ######
###############################################################################

# Checking for nonnormallity with the Anderson-Darling test
CONREV_AD_out <- print(ad.test(out_test$CONREV))
shapiro.test(out_test$CONREV)

PROFREV_AD_out <- print(ad.test(out_test$PROFREV))
shapiro.test(out_test$PROFREV)

# CONREV_CVM <- cvm.test(GC_nonnormallity$CONREV)
# PROFREV_CVM <- cvm.test(GC_nonnormallity$PROFREV)

# Both test indicate the endogenous variables are not normally distributed. H0 can be rejected.

# Checking the kurtosis and skewness as additional information

CONREV_SKEW_out <- print(skewness(out_test$CONREV))
PROFREV_SKEW_out <- print(skewness(out_test$PROFREV))

CONREV_KUR <- print(kurtosis(out_test$CONREV))
PROFREV_KUR <- print(kurtosis(out_test$PROFREV))

# Visually investigate the distribution of CONREV
# Plot the density of CONREV against the normal distribution
ggplot(out_test, aes(x=CONREV)) +
  geom_density(alpha=0.4, fill="red") +
  stat_function(fun = dnorm,
                args = list(mean = mean(out_test$CONREV),
                            sd = sd(out_test$CONREV)), 
                color="blue") +
  xlim(-0.5,1.5) +
  theme_classic()

# Visually investigate the distribution of PROFREV
# Plot the density of PROFREV against the normal distribution
ggplot(out_test, aes(x=PROFREV)) +
  geom_density(alpha=0.4, fill="red") +
  stat_function(fun = dnorm,
                args = list(mean = mean(out_test$PROFREV),
                            sd = sd(out_test$PROFREV)), 
                color="blue") +
  xlim(-0.5,1.5) +
  theme_classic()

# Checking for normality of the residuals via the untreated model

untreatedModel2_out <- summary(lm(ln_USV ~ CONREV + 
                                    PROFREV +
                                    FRANC +
                                    GEN_AD +
                                    GEN_RA +
                                    GEN_RP +
                                    GEN_SI +
                                    GEN_SP +
                                    GEN_ST +
                                    GEN_MO +
                                    GEN_MI +
                                    OPUSD +
                                    singleplayer +
                                    multiplayer +
                                    coop +
                                    o_coop +
                                    pvp +
                                    PLF_PC +
                                    ESRB_RP +
                                    ESRB_E +
                                    ESRB_T +
                                    out_covariates,
                                  data=out_test, na.action = na.omit))

untreatedModel3_out <- summary(lm(ln_USV ~ (CONREV * FRANC) +
                                    (CONREV * GEN_AD) +
                                    (CONREV * GEN_RA) +
                                    (CONREV * GEN_RP) +
                                    (CONREV * GEN_SI) +
                                    (CONREV * GEN_SP) +
                                    (CONREV * GEN_ST) +
                                    (CONREV * GEN_MO) +
                                    (CONREV * GEN_MI) +
                                    (CONREV * OPUSD) +
                                    (CONREV * singleplayer) +
                                    (CONREV * multiplayer) +
                                    (CONREV * coop) +
                                    (CONREV * o_coop) +
                                    (CONREV * pvp) +
                                    (CONREV * PLF_PC) +
                                    (CONREV * ESRB_RP) +
                                    (CONREV * ESRB_E) +
                                    (CONREV * ESRB_T) +
                                    (PROFREV * FRANC) +
                                    (PROFREV * GEN_AD) +
                                    (PROFREV * GEN_RA) +
                                    (PROFREV * GEN_RP) +
                                    (PROFREV * GEN_SI) +
                                    (PROFREV * GEN_SP) +
                                    (PROFREV * GEN_ST) +
                                    (PROFREV * GEN_MO) +
                                    (PROFREV * GEN_MI) +
                                    (PROFREV * OPUSD) +
                                    (PROFREV * singleplayer) +
                                    (PROFREV * multiplayer) +
                                    (PROFREV * coop) +
                                    (PROFREV * o_coop) +
                                    (PROFREV * pvp) +
                                    (PROFREV * PLF_PC) +
                                    (PROFREV * ESRB_RP) +
                                    (PROFREV * ESRB_E) +
                                    (PROFREV * ESRB_T) +
                                    out_covariates,
                                  data=out_test, na.action = na.omit))

# Visually investigate the distribution of the residuals of untreatedModel2
# Plot the density of the residuals against the normal distribution
ggplot(data.frame(untreatedModel2_out$residuals), aes(x=untreatedModel2_out.residuals)) +
  geom_density(alpha=0.4, fill="red") +
  stat_function(fun = dnorm,
                args = list(mean = mean(untreatedModel2_out$residuals),
                            sd = sd(untreatedModel2_out$residuals)), 
                color="blue") +
  xlim(-4,4) +
  theme_classic()

# Visually investigate the distribution of the residuals of untreatedModel3
# Plot the density of the residuals against the normal distribution
ggplot(data.frame(untreatedModel3_out$residuals), aes(x=untreatedModel3_out.residuals)) +
  geom_density(alpha=0.4, fill="red") +
  stat_function(fun = dnorm,
                args = list(mean = mean(untreatedModel3_out$residuals),
                            sd = sd(untreatedModel3_out$residuals)), 
                color="blue") +
  xlim(-4,4) +
  theme_classic()



# Statistical tests of normal distribution of the residuals of model 2

# All tests combined (olsrr pack)
ols_test_normality(out_linmod_2)

# Anderson-Darling
nortest::ad.test(untreatedModel2_out$residuals)

# Cramer-von Mises
nortest::cvm.test(untreatedModel2_out$residuals)

# Shapiro-Wilk
shapiro.test(untreatedModel2_out$residuals)

# Kolmogorov-Smirnov
ks.test(untreatedModel2_out$residuals, "pnorm")




# Statistical tests of normal distribution of the residuals of model 3

# All tests combined (olsrr pack)
ols_test_normality(out_mod_3)

# Anderson-Darling
nortest::ad.test(untreatedModel3_out$residuals)

# Cramer-von Mises
nortest::cvm.test(untreatedModel3_out$residuals)

# Shapiro-Wilk
shapiro.test(untreatedModel3_out$residuals)

# Kolmogorov-Smirnov
ks.test(untreatedModel3_out$residuals, "pnorm")

# All statistics
ols_test_normality(linmod_2)
ols_test_normality(out_linmod_2)
ols_test_normality(lin_mod_3)
ols_test_normality(out_mod_3)

###############################################################################
################### GAUSSIAN COPULA (HANNES) ####################################
###############################################################################

make_copula <- function(x) {
  if (length(unique(x))==1) return(as.numeric(rep(NA, length(x))))
  return(ifelse(ecdf(x)(x)==1, qnorm(1-.0000001), qnorm(ecdf(x)(x))))
}

vg_data_test$CONREV_STAR <- make_copula(vg_data_test$CONREV)
#vg_data_test$PROFREV_STAR <- make_copula(vg_data_test$PROFREV)

# ###############################################################################
# ################### GAUSSIAN COPULA (BEKKER) ####################################
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

# Linear regression including control variables and including Gaussian copula termsl
GC_mod_1 <- lm(ln_USV ~ CONREV + 
                 PROFREV +
                 FRANC +
                 GEN_AD +
                 GEN_RA +
                 GEN_RP +
                 GEN_SI +
                 GEN_SP +
                 GEN_ST +
                 GEN_MO +
                 GEN_MI +
                 OPUSD +
                 singleplayer +
                 multiplayer +
                 coop +
                 o_coop +
                 pvp +
                 PLF_PC +
                 ESRB_RP +
                 ESRB_E +
                 ESRB_T +
                 CONREV_STAR +
                 covariates,
               data=vg_data_test, na.action = na.omit)

print(GC_mod_1)

summary_GC_mod_1 <- summary(GC_mod_1)
summary_GC_mod_1

# Linear regression including interaction terms and including Gaussian copula terms
GC_mod_2 <- lm(ln_USV ~ (CONREV * FRANC) +
                 (CONREV * GEN_AD) +
                 (CONREV * GEN_RA) +
                 (CONREV * GEN_RP) +
                 (CONREV * GEN_SI) +
                 (CONREV * GEN_SP) +
                 (CONREV * GEN_ST) +
                 (CONREV * GEN_MO) +
                 (CONREV * GEN_MI) +
                 (CONREV * OPUSD) +
                 (CONREV * singleplayer) +
                 (CONREV * multiplayer) +
                 (CONREV * coop) +
                 (CONREV * o_coop) +
                 (CONREV * pvp) +
                 (CONREV * PLF_PC) +
                 (CONREV * ESRB_RP) +
                 (CONREV * ESRB_E) +
                 (CONREV * ESRB_T) +
                 (PROFREV * FRANC) +
                 (PROFREV * GEN_AD) +
                 (PROFREV * GEN_RA) +
                 (PROFREV * GEN_RP) +
                 (PROFREV * GEN_SI) +
                 (PROFREV * GEN_SP) +
                 (PROFREV * GEN_ST) +
                 (PROFREV * GEN_MO) +
                 (PROFREV * GEN_MI) +
                 (PROFREV * OPUSD) +
                 (PROFREV * singleplayer) +
                 (PROFREV * multiplayer) +
                 (PROFREV * coop) +
                 (PROFREV * o_coop) +
                 (PROFREV * pvp) +
                 (PROFREV * PLF_PC) +
                 (PROFREV * ESRB_RP) +
                 (PROFREV * ESRB_E) +
                 (PROFREV * ESRB_T) +
                 CONREV_STAR +
                 covariates,
               data=vg_data_test, na.action = na.omit)

print(GC_mod_2)

summary_GC_mod_2 <- summary(GC_mod_2, diagnostics=TRUE)
summary_GC_mod_2

###############################################################################
################### ENDOGENEITY TEST HAUSMAN ##################################
###############################################################################

# Hausman test for edogeneity of regressors
cf_diff <- coef(GC_mod_1) - coef(linmod_2)
vc_diff <- vcov(GC_mod_1) - coef(linmod_2)
x2_diff <- as.vector(t(cf_diff) %*% solve(vc_diff) %*% cf_diff)
pchisq(x2_diff, df = 2, lower.tail = FALSE)


# Hausman test for edogeneity of regressors for interaction term model, if necessary
cf_diff2 <- coef(GC_mod_2) - coef(lin_mod_3)
vc_diff2 <- vcov(GC_mod_2) - coef(lin_mod_3)
x2_diff2 <- as.vector(t(cf_diff2) %*% solve(vc_diff2) %*% cf_diff2)
pchisq(x2_diff2, df = 2, lower.tail = FALSE)

###############################################################################
################### MODEL GENERATION + GS MODELS (H1A - H1C) ##################
###############################################################################
# install.packages("flextable")
library(flextable)

GC_models <- list(linmod_2, GC_mod_1)
names(GC_models) <- c("Linear model", "GC terms included" )

msummary(GC_models, output = "table_coef3.docx", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)

msummary(GC_models, output = "table_sterror3.docx", estimate = "std.error", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)

msummary(GC_models, output = "table_pvalue3.docx", estimate = "p.value", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)

msummary(GC_models, output = "table_tvalue3.docx", estimate = "statistic", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)


msummary(GC_models, output = "table_coef3.docx", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary for GC models, including interaction terms",  statistic = c("s.e. = {std.error}", 
                                                                                                                                                                                                   "t = {statistic}",
                                                                                                                                                                                                   "p = {p.value}"), statistic_vertical = FALSE)
###############################################################################
####### MODEL GENERATION + GS MODELS FOR INTERACTION (H2 - H7) ################
###############################################################################

GC_models_int <- list(lin_mod_3, GC_mod_2)
names(GC_models_int) <- c("Linear model", "GC terms included")

msummary(GC_models_int, output = "table_coef_int3.docx", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)

msummary(GC_models_int, output = "table_sterror_int3.docx", estimate = "std.error", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)

msummary(GC_models_int, output = "table_pvalue_int3.docx", estimate = "p.value", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)

msummary(GC_models_int, output = "table_tvalue_int3.docx", estimate = "statistic", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)


###############################################################################
################### H1C TESTING ###############################################
###############################################################################

library(car)

# Function to get the estimates and test between the difference of two coefficients.
difftest_lm <- function(x1, x2, model){
  diffest <- summary(model)$coef[x1,"Estimate"]-summary(model)$coef[x2,"Estimate"]
  vardiff <- (summary(model)$coef[x1,"Std. Error"]^2 + 
                summary(model)$coef[x2,"Std. Error"]^2) - (2*(vcov(model)[x1, x2])) 
  # variance of x1 + variance of x2 - 2*covariance of x1 and x2
  diffse <- sqrt(vardiff)
  tdiff <- (diffest)/(diffse)
  ptdiff <- 2*(1-pt(abs(tdiff), model$df, lower.tail=T))
  upr <- diffest + qt(.975, df = model$df)*diffse # will usually be very close to 1.96
  lwr <- diffest + qt(.025, df = model$df)*diffse
  df <- model$df
  return(list(est=round(diffest, digits =2), 
              t=round(tdiff, digits = 2), 
              p=round(ptdiff, digits = 4), 
              lwr=round(lwr, digits = 2), 
              upr=round(upr, digits = 2),
              df = df))
}

# Testing for H1C with the regular linear model
H1C_linmod2_answer2 <- difftest_lm("CONREV", "PROFREV", linmod_2)
H1C_linmod2_answer2

# Same tests but with the GC model
H1C_GC_mod1_answer2 <- difftest_lm("CONREV", "PROFREV", GC_mod_1)
H1C_GC_mod1_answer2

# Extra test for H1C with the linear model plus int terms
H1C_linmod3_answer <- difftest_lm("CONREV", "PROFREV", lin_mod_3)
H1C_linmod3_answer

# Same tests but with the GC model plus int terms
H1C_GC_mod2_answer <- difftest_lm("CONREV", "PROFREV", GC_mod_2)
H1C_GC_mod2_answer

###############################################################################
################### EXPLORATORY MODELS ########################################
###############################################################################

# Exploratory linear model
exp_mod_1 <- lm(ln_USV ~ (CONREV * FRANC) +
                  (CONREV * GEN_AD) +
                  (CONREV * GEN_RA) +
                  (CONREV * GEN_RP) +
                  (CONREV * GEN_SI) +
                  (CONREV * GEN_SP) +
                  (CONREV * GEN_ST) +
                  (CONREV * GEN_MO) +
                  (CONREV * GEN_MI) +
                  (CONREV * OPUSD) +
                  (CONREV * singleplayer) +
                  (CONREV * multiplayer) +
                  (CONREV * coop) +
                  (CONREV * o_coop) +
                  (CONREV * pvp) +
                  (CONREV * PLF_PC) +
                  (CONREV * ESRB_RP) +
                  (CONREV * ESRB_E) +
                  (CONREV * ESRB_T) +
                  (PROFREV * FRANC) +
                  (PROFREV * GEN_AD) +
                  (PROFREV * GEN_RA) +
                  (PROFREV * GEN_RP) +
                  (PROFREV * GEN_SI) +
                  (PROFREV * GEN_SP) +
                  (PROFREV * GEN_ST) +
                  (PROFREV * GEN_MO) +
                  (PROFREV * GEN_MI) +
                  (PROFREV * OPUSD) +
                  (PROFREV * singleplayer) +
                  (PROFREV * multiplayer) +
                  (PROFREV * coop) +
                  (PROFREV * o_coop) +
                  (PROFREV * pvp) +
                  (PROFREV * PLF_PC) +
                  (PROFREV * ESRB_RP) +
                  (PROFREV * ESRB_E) +
                  (PROFREV * ESRB_T) +
                  (CONREV * covariates) +
                  (PROFREV * covariates),
                data=vg_data_test, na.action = na.omit)

# Exploratory linear model including Gaussian copula for CONREV
exp_mod_2 <- lm(ln_USV ~ (CONREV * FRANC) +
                  (CONREV * GEN_AD) +
                  (CONREV * GEN_RA) +
                  (CONREV * GEN_RP) +
                  (CONREV * GEN_SI) +
                  (CONREV * GEN_SP) +
                  (CONREV * GEN_ST) +
                  (CONREV * GEN_MO) +
                  (CONREV * GEN_MI) +
                  (CONREV * OPUSD) +
                  (CONREV * singleplayer) +
                  (CONREV * multiplayer) +
                  (CONREV * coop) +
                  (CONREV * o_coop) +
                  (CONREV * pvp) +
                  (CONREV * PLF_PC) +
                  (CONREV * ESRB_RP) +
                  (CONREV * ESRB_E) +
                  (CONREV * ESRB_T) +
                  (PROFREV * FRANC) +
                  (PROFREV * GEN_AD) +
                  (PROFREV * GEN_RA) +
                  (PROFREV * GEN_RP) +
                  (PROFREV * GEN_SI) +
                  (PROFREV * GEN_SP) +
                  (PROFREV * GEN_ST) +
                  (PROFREV * GEN_MO) +
                  (PROFREV * GEN_MI) +
                  (PROFREV * OPUSD) +
                  (PROFREV * singleplayer) +
                  (PROFREV * multiplayer) +
                  (PROFREV * coop) +
                  (PROFREV * o_coop) +
                  (PROFREV * pvp) +
                  (PROFREV * PLF_PC) +
                  (PROFREV * ESRB_RP) +
                  (PROFREV * ESRB_E) +
                  (PROFREV * ESRB_T) +
                  (CONREV * covariates) +
                  (PROFREV * covariates)  +
                  CONREV_STAR,
                data=vg_data_test, na.action = na.omit)

exp_models <- list(exp_mod_1, exp_mod_2)
names(exp_models) <- c("Linear model", "GC term included")

msummary(exp_models, output = "table_coef_exp.docx", gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)

msummary(exp_models, output = "table_sterror_exp.docx", estimate = "std.error", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)

msummary(exp_models, output = "table_pvalue_exp.docx", estimate = "p.value", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)

msummary(exp_models, output = "table_tvalue_exp.docx", estimate = "statistic", gof_omit = "AIC|BIC|Log|Pseudo", title = "Table X. Regression models summary for GC models, including interaction terms", statistic = NULL)
