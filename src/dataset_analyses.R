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

vg_data_test <- vg_data %>% 
  select(-c(na_sales_m, eu_sales_m, jp_sales_m, other_sales_m)) 
  
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
                 plf_pc +
                 ESRB_RP +
                 ESRB_E +
                 ESRB_T
                 ,data=vg_data_test, na.action = na.omit)
print(linmod_1)

summary_linmod_1 <- summary(linmod_1)
summary_linmod_1

coeffs_linmod_1 <- summary_linmod_1$coefficients
beta.estimate_CONREV <- coeffs_linmod_1["meta_user_score", "Estimate"]
std.error_CONREV <- coeffs_linmod_1["meta_user_score", "Std. Error"]
t_value_CONREV <- beta.estimate_CONREV/std.error_CONREV
p_value_CONREV <- 2*pt(-abs(t_value_CONREV), df=nrow(vg_data)-ncol(vg_data))
f_statistic_CONREV <- linmod_1$fstatistic[1]
f <- summary(linmod_1)$fstatistic
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
print(model_p)

AIC_linmod_1 <- AIC(linmod_1)
BIC_linmod_1 <- BIC(linmod_1)

print(summary_linmod_1)

tidy_linmod_1 <- tidy(linmod_1, conf.int = T)
tidy_linmod_1

###############################################################################
################### INCL CONTROL VARIABLES ####################################
###############################################################################

covariates <- cbind(vg_data_test$INDIE, vg_data_test$soundtrack, vg_data_test$SMPRES, vg_data_test$N_LAN, vg_data_test$LCP)

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
                 plf_pc +
                 ESRB_RP +
                 ESRB_E +
                 ESRB_T +
                 covariates,
                 data=vg_data_test, na.action = na.omit)

print(linmod_2)

summary_linmod_2 <- summary(linmod_2)
summary_linmod_2

tidy_linmod_2 <- tidy(linmod_2, conf.int = T)
tidy_linmod_2

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
                  (CONREV * plf_pc) +
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
                  (PROFREV * plf_pc) +
                  (PROFREV * ESRB_RP) +
                  (PROFREV * ESRB_E) +
                  (PROFREV * ESRB_T) +
                  covariates,
                data=vg_data_test, na.action = na.omit)

###############################################################################
################### WEIGHTED LINEAR MODEL #####################################
###############################################################################

wt <- 1 / lm(abs(linmod_2$residuals) ~ linmod_2$fitted.values)$fitted.values^2

wls_mod_2 <- lm(ln_USV ~ CONREV + 
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
                  plf_pc +
                  ESRB_RP +
                  ESRB_E +
                  ESRB_T +
                  covariates,
                  data=vg_data_test,
                  weights = wt, na.action = na.omit)

print(wls_mod_2)

summary_wls_mod_2 <- summary(wls_mod_2)
summary_wls_mod_2

tidy_wls_mod_2 <- tidy(wls_mod_2, conf.int = T)
tidy_wls_mod_2



###############################################################################
################### INTERACTION TERMS INCLUDED ################################
###############################################################################

# Linear regression including interaction terms
wls_mod_3 <- lm(ln_USV ~ (CONREV * FRANC) +
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
                 (CONREV * plf_pc) +
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
                 (PROFREV * plf_pc) +
                 (PROFREV * ESRB_RP) +
                 (PROFREV * ESRB_E) +
                 (PROFREV * ESRB_T) +
                 covariates,
                 data=vg_data_test, weights = wt, na.action = na.omit)

print(wls_mod_3)

summary_wls_mod_3 <- summary(wls_mod_3)
summary_wls_mod_3



tidy_wls_mod_3 <- tidy(wls_mod_3, conf.int = T)
tidy_wls_mod_3

###############################################################################
################### MODEL SUMMARIES ###########################################
###############################################################################

models <- list(linmod_1, linmod_2, wls_mod_2, wls_mod_3)
names(models) <- c("OLS", "OLS + COV", "WLS", "WLS + interaction")

msummary(models, gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary", statistic = "statistic")

# Model reporting via Stargazer
library(stargazer)

stargazer(models,
          title = "Table X. Regression models summary",
          dep.var.caption = "USV",  
          dep.var.labels = "",  
          covariate.labels = ,  
          column.labels = c("OLS", "OLS + COV", "WLS", "WLS + interaction"),
          notes.label = "Significance levels",  
          type="text",
          out="output.html"  
)

###############################################################################
################### 2SLS REGRESSION TEST ######################################
###############################################################################

instr_var <- cbind(vg_data_test$plf_pc)
endo_var <- cbind(vg_data_test$CONREV, vg_data_test$PROFREV)
exo_var <- cbind(vg_data_test$FRANC, vg_data_test$GEN_AD, vg_data_test$GEN_RA, vg_data_test$GEN_RP, vg_data_test$GEN_SI, vg_data_test$GEN_SP, vg_data_test$GEN_SI, vg_data_test$GEN_MO, vg_data_test$GEN_MI, vg_data_test$OPUSD, vg_data_test$singleplayer, vg_data_test$multiplayer, vg_data_test$coop, vg_data_test$o_coop, vg_data_test$pvp, vg_data_test$ESRB_RP, vg_data_test$ESRB_E, vg_data_test$ESRB_T, covariates)


two_sls_mod_1 <- ivreg(ln_USV ~ endo_var + exo_var | exo_var + instr_var, data=vg_data_test, na.action = na.omit)
summary(two_sls_mod_1)

# standard ols regression needed for testing

ols_test_0 <- lm(ln_USV ~ CONREV + 
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
                 ESRB_RP +
                 ESRB_E +
                 ESRB_T +
                 covariates,
               data=vg_data_test, na.action = na.omit)

# Detailed 2SLS regression tests

ols_test_1 <- lm(CONREV ~ PROFREV +
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
                   ESRB_RP +
                   ESRB_E +
                   ESRB_T +
                   plf_pc +
                   covariates,
                 data=vg_data_test, na.action = na.omit)

summary(ols_test_1)
CONREV_hat <- fitted(ols_test_1)

ols_test_2 <- lm(PROFREV ~ CONREV +
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
                   ESRB_RP +
                   ESRB_E +
                   ESRB_T +
                   plf_pc +
                   covariates,
                 data=vg_data_test, na.action = na.omit)

summary(ols_test_2)
PROFREV_hat <- fitted(ols_test_2)

ols_test_3 <- lm(ln_USV ~ CONREV_hat + 
                     PROFREV_hat +
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
                     o_co op +
                     pvp +
                     ESRB_RP +
                     ESRB_E +
                     ESRB_T +
                     covariates,
                   data=vg_data_test, na.action = na.omit)

summary(ols_test_3)

models_2 <- list(linmod_1, linmod_2, wls_mod_2, wls_mod_3, ols_test_3)
names(models_2) <- c("OLS", "OLS + COV", "WLS", "WLS + interaction", "2SLS")

msummary(models_2, gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary", statistic = "statistic")

# Hausman test for edogeneity of regressors
cf_diff <- coef(ols_test_3) - coef(ols_test_0)
vc_diff <- vcov(ols_test_3) - coef(ols_test_0)
x2_diff <- as.vector(t(cf_diff) %*% solve(vc_diff) %*% cf_diff)
pchisq(x2_diff, df = 2, lower.tail = FALSE)

###############################################################################
################### ALL VARIABLES INCLUDED TEST ###############################
###############################################################################

# Linear regression including control variables
test_wls_mod_1 <- lm(ln_USV ~ (CONREV * FRANC) +
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
                       (CONREV * plf_pc) +
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
                       (PROFREV * plf_pc) +
                       (PROFREV * ESRB_RP) +
                       (PROFREV * ESRB_E) +
                       (PROFREV * ESRB_T) +
                       (CONREV * covariates) +
                       (PROFREV * covariates),
                     data=vg_data_test, weights = wt, na.action = na.omit)

summary_test_wls_mod_1 <- summary(test_wls_mod_1)
summary_test_wls_mod_1

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
                 plf_pc +
                 ESRB_RP +
                 ESRB_E +
                 ESRB_T
               ,data=out_test, na.action = na.omit)

summary_out_linmod_1 <- summary(out_linmod_1)
summary_out_linmod_1

###############################################################################
################### INCL CONTROL VARIABLES ####################################
###############################################################################

out_covariates <- cbind(out_test$INDIE, out_test$soundtrack, out_test$SMPRES, out_test$N_LAN, out_test$LCP)

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
                 plf_pc +
                 ESRB_RP +
                 ESRB_E +
                 ESRB_T +
                 out_covariates,
               data=out_test, na.action = na.omit)


summary_out_linmod_2 <- summary(out_linmod_2)
summary_out_linmod_2

###############################################################################
################### WEIGHTED LINEAR MODEL #####################################
###############################################################################

out_wt <- 1 / lm(abs(out_linmod_2$residuals) ~ out_linmod_2$fitted.values)$fitted.values^2

out_wls_mod_2 <- lm(ln_USV ~ CONREV + 
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
                  plf_pc +
                  ESRB_RP +
                  ESRB_E +
                  ESRB_T +
                  out_covariates,
                data=out_test,
                weights = out_wt, na.action = na.omit)

summary_out_wls_mod_2 <- summary(out_wls_mod_2)
summary_out_wls_mod_2

###############################################################################
################### INTERACTION TERMS INCLUDED ################################
###############################################################################

# Linear regression including interaction terms
out_wls_mod_3 <- lm(ln_USV ~ (CONREV * FRANC) +
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
                  (CONREV * plf_pc) +
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
                  (PROFREV * plf_pc) +
                  (PROFREV * ESRB_RP) +
                  (PROFREV * ESRB_E) +
                  (PROFREV * ESRB_T) +
                  out_covariates,
                data=out_test, weights = out_wt, na.action = na.omit)


summary_out_wls_mod_3 <- summary(out_wls_mod_3)
summary_out_wls_mod_3

###############################################################################
################### MODEL SUMMARIES ###########################################
###############################################################################

out_models <- list(out_linmod_1, out_linmod_2, out_wls_mod_2, out_wls_mod_3)
names(out_models) <- c("OLS", "OLS + COV", "WLS", "WLS + interaction")

msummary(out_models, gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary", statistic = "statistic")

# Model reporting via Stargazer
library(stargazer)

stargazer(models,
          title = "Table X. Regression models summary",
          dep.var.caption = "USV",  
          dep.var.labels = "",  
          covariate.labels = ,  
          column.labels = c("OLS", "OLS + COV", "WLS", "WLS + interaction"),
          notes.label = "Significance levels",  
          type="text",
          out="output.html"  
)


###############################################################################
################### H1A - H1C MODEL SUMMARIES ###################################
###############################################################################

h1_h2_model <- list(linmod_1, linmod_2, wls_mod_2)
names(h1_h2_model) <- c("OLS", "OLS + COV", "WLS")

msummary_h1_h2 <- msummary(h1_h2_model, gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary for H1A - H1C", statistic = "statistic")
msummary_h1_h2

out_h1_h2_model <- list(out_linmod_1, out_linmod_2, out_wls_mod_2)
names(out_h1_h2_model) <- c("OLS", "OLS + COV", "WLS")

msummary(out_h1_h2_model, gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary for H1A - H1C, outliers excluded", statistic = "statistic")

###############################################################################
################### H2 - H7 MODEL SUMMARIES ###################################
###############################################################################

h3_h7_models <- list(lin_mod_3, wls_mod_3, out_wls_mod_3 )
names(h3_h7_models) <- c("OLS", "WLS", "WLS (outliers excluded" )

msummary(h3_h7_models, gof_omit = "AIC|BIC|Log|Pseudo", stars=T, title = "Table X. Regression models summary for H2 - H7, including interaction terms", statistic = "statistic")
