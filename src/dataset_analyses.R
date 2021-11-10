# Loading Packages
library(readr)
library(dplyr)
library(tidyverse)


# Loading data
vg_data <- read.csv("../gen/full_videogame_dataset.csv")

vg_data <- vg_data %>% 
  filter(global_sales_m > 0.00) %>% 
  mutate(ln_usv = log(global_sales_m))


#################################################################################
############################ REGRESSION ANALYSES ################################
#################################################################################

# Boxplot for outliers
boxplot(vg_data$ln_usv, main="Global sales", sub=paste("Outlier rows: ", boxplot.stats(vg_data$ln_usv)$out))

# Linear regression
linmod_1 <- lm(ln_usv ~ meta_user_score + 
                 meta_critic_score +
                 franchise +
                 gen_action +
                 gen_adventure +
                 gen_racing +
                 gen_roleplaying +
                 gen_simulation +
                 gen_sports +
                 gen_strategy +
                 gen_mmo +
                 gen_misc +
                 originalcost_. +
                 singleplayer +
                 multiplayer +
                 coop +
                 plf_con +
                 plf_pc +
                 esrb_everyone +
                 esrb_teens +
                 esrb_adults +
                 esrb_pending
                 ,data=vg_data)
print(linmod_1)

summary_linmod_1 <- summary(linmod_1)
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

###############################################################################
################### INTERACTION TERMS INCLUDED ################################
###############################################################################

# Linear regression including interaction terms
linmod_2 <- lm(ln_usv ~ meta_user_score + 
                 meta_critic_score +
                 franchise +
                 gen_action +
                 gen_adventure +
                 gen_racing +
                 gen_roleplaying +
                 gen_simulation +
                 gen_sports +
                 gen_strategy +
                 gen_mmo +
                 gen_misc +
                 originalcost_. +
                 singleplayer +
                 multiplayer +
                 coop +
                 plf_con +
                 plf_pc +
                 esrb_everyone +
                 esrb_teens +
                 esrb_adults +
                 esrb_pending +
                 (meta_user_score * franchise) +
                 (meta_user_score * gen_action) +
                 (meta_user_score * gen_adventure) +
                 (meta_user_score * gen_racing) +
                 (meta_user_score * gen_roleplaying) +
                 (meta_user_score * gen_simulation) +
                 (meta_user_score * gen_sports) +
                 (meta_user_score * gen_strategy) +
                 (meta_user_score * gen_mmo) +
                 (meta_user_score * gen_misc) +
                 (meta_user_score * originalcost_.) +
                 (meta_user_score * singleplayer) +
                 (meta_user_score * multiplayer) +
                 (meta_user_score * coop) +
                 (meta_user_score * plf_con) +
                 (meta_user_score * plf_pc) +
                 (meta_user_score * esrb_everyone) +
                 (meta_user_score * esrb_teens) +
                 (meta_user_score * esrb_adults) +
                 (meta_user_score * esrb_pending) +
                 (meta_critic_score * franchise) +
                 (meta_critic_score * gen_action) +
                 (meta_critic_score * gen_adventure) +
                 (meta_critic_score * gen_racing) +
                 (meta_critic_score * gen_roleplaying) +
                 (meta_critic_score * gen_simulation) +
                 (meta_critic_score * gen_sports) +
                 (meta_critic_score * gen_strategy) +
                 (meta_critic_score * gen_mmo) +
                 (meta_critic_score * gen_misc) +
                 (meta_critic_score * originalcost_.) +
                 (meta_critic_score * singleplayer) +
                 (meta_critic_score * multiplayer) +
                 (meta_critic_score * coop) +
                 (meta_critic_score * plf_con) +
                 (meta_critic_score * plf_pc) +
                 (meta_critic_score * esrb_everyone) +
                 (meta_critic_score * esrb_teens) +
                 (meta_critic_score * esrb_adults) +
                 (meta_critic_score * esrb_pending)
               ,data=vg_data)

print(linmod_2)
