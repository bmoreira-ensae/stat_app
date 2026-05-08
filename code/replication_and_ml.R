##### SETUP

library(causalToolbox)
library(dplyr)
library(GenericML)
library(haven)
library(lmtest)
library(mlr3)
library(mlr3learners)
library(sandwich)
library(stargazer)

wave2 <- read_dta("/home/bruno/Desktop/Data/waveII.dta")
wave3 <- read_dta("/home/bruno/Desktop/Data/waveIII.dta")
wave3_young_women_sample <- read_dta("/home/bruno/Desktop/Data/waveIII_young_women_sample.dta")

##### REPLICATION


controls <- c("older_sister", "bl_still_in_school", "bl_education_mother", 
              "bl_HHsize", "bl_public_transit", "bl_age10", "bl_age11", 
              "bl_age12", "bl_age13", "bl_age14", "bl_age15", "bl_age16", 
              "bl_age17", "older_sister_miss", "bl_still_in_school_miss", 
              "bl_education_mother_miss", "bl_HHsize_miss", 
              "bl_public_transit_miss")

eq <- paste("anyemp + anyoil + oil_kk + factor(third) + factor(unionID) + ", 
            controls, collapse = " + ")

dfwave2 <- wave2[wave2$midline == 1 & wave2$washedout == 0 &
                   wave2$before_miss == 0 & wave2$bl_age_reported >= 14 &
                   wave2$bl_age_reported <= 16,]

dfwave2_15 <- dfwave2[dfwave2$bl_age_reported == 14,]

f_mmid <- as.formula(paste0("ml_ever_married ~ ", eq))

regr4 <- lm(f_mmid, data = dfwave2)
regr5 <- lm(f_mmid, data = dfwave2_15)

dfwave3 <- wave3[wave3$endline == 1 & wave3$washedout == 0 &
                   wave3$before_miss == 0 & wave3$bl_ever_married == 0 &
                   wave3$bl_age_reported >= 14 & wave3$bl_age_reported <= 16,]

dfwave3_15 <- dfwave3[dfwave3$bl_age_reported == 14,]

f_18 <- as.formula(paste0("under_18 ~ ", eq))
f_16 <- as.formula(paste0("under_16 ~ ", eq))
f_mend <- as.formula(paste0("ever_married ~ ", eq))
f_mage <- as.formula(paste0("marriage_age ~ ", eq))
f_20 <- as.formula(paste0("ever_birth_20 ~ ", eq))

regr1 <- lm(f_18,  data = dfwave3)
regr2 <- lm(f_18,  data = dfwave3_15)
regr3 <- lm(f_16,  data = dfwave3_15)
regr6 <- lm(f_mend,  data = dfwave3)
regr7 <- lm(f_mend,  data = dfwave3_15)
regr8 <- lm(f_mage, data = dfwave3)
regr9 <- lm(f_mage, data = dfwave3_15)
regr10 <- lm(f_20, data = dfwave3)
regr11 <- lm(f_20, data = dfwave3_15)

models <- mget(paste0("regr", 1:11))

se_list <- lapply(models, function(m) {sqrt(diag(vcovCL(m, cluster = ~CLUSTER, type = "HC1")))})

stargazer(models, se = se_list, type = "text", keep = c("anyemp", "anyoil", 
                                                        "oil_kk"), 
          omit = c("factor\\(third\\)", "factor\\(unionID\\)"), 
          omit.stat = c("f", "ser", "rsq", "adj.rsq"))

### ML: GENERAL

dfwave3 <- wave3[wave3$endline == 1 & wave3$washedout == 0 &
                   wave3$before_miss == 0 & wave3$bl_ever_married == 0 &
                   wave3$bl_age_reported >= 14 & wave3$bl_age_reported <= 16, ]

Z_vars <- c("bl_still_in_school", "bl_education_mother", "older_sister", 
            "bl_HHsize")

##### UNDER_18

df_ml <- dfwave3 %>% filter(!is.na(under_18), !is.na(oil))

for (v in Z_vars) {if (any(is.na(df_ml[[v]]))) {
  df_ml[[paste0(v, "_miss")]] <- as.numeric(is.na(df_ml[[v]]))
  df_ml[[v]][is.na(df_ml[[v]])] <- 0}}

Z_final <- c(Z_vars, intersect(paste0(Z_vars, "_miss"), names(df_ml)))

Y <- as.numeric(df_ml$under_18)
D <- as.numeric(df_ml$oil)
Z <- as.matrix(df_ml[, Z_final])

m_18   <- lm(under_18 ~ oil, data = df_ml)
ols_18 <- coeftest(m_18, vcov = vcovCL(m_18, cluster = ~CLUSTER, type = "HC1"))

set.seed(123)

fit_18 <- GenericML(Z = Z, D = D, Y = Y,
                    learners_GenericML = c(
                      "lasso", "random_forest", 
                      "mlr3::lrn('cv_glmnet', s = 'lambda.min', alpha = 0.5)"),
                    num_splits = 100,
                    quantile_cutoffs = c(0.2, 0.4, 0.6, 0.8))

best_18        <- get_best(fit_18)
blp_18         <- get_BLP(fit_18)
gates_18       <- get_GATES(fit_18)
clan_school_18 <- get_CLAN(fit_18, variable = "bl_still_in_school")
clan_educ_18   <- get_CLAN(fit_18, variable = "bl_education_mother")
clan_sister_18 <- get_CLAN(fit_18, variable = "older_sister")
clan_hh_18     <- get_CLAN(fit_18, variable = "bl_HHsize")

##### UNDER_16

df_ml <- dfwave3 %>% filter(!is.na(under_16), !is.na(oil))

for (v in Z_vars) {if (any(is.na(df_ml[[v]]))) {
  df_ml[[paste0(v, "_miss")]] <- as.numeric(is.na(df_ml[[v]]))
  df_ml[[v]][is.na(df_ml[[v]])] <- 0}}

Z_final <- c(Z_vars, intersect(paste0(Z_vars, "_miss"), names(df_ml)))

Y <- as.numeric(df_ml$under_16)
D <- as.numeric(df_ml$oil)
Z <- as.matrix(df_ml[, Z_final])

m_16   <- lm(under_16 ~ oil, data = df_ml)
ols_16 <- coeftest(m_16, vcov = vcovCL(m_16, cluster = ~CLUSTER, type = "HC1"))

set.seed(123)

fit_16 <- GenericML(Z = Z, D = D, Y = Y,
                    learners_GenericML = c("lasso", "random_forest",
                                           "mlr3::lrn('cv_glmnet', s = 'lambda.min', alpha = 0.5)"),
                    num_splits = 100,
                    quantile_cutoffs = c(0.25, 0.5, 0.75))

best_16        <- get_best(fit_16)
blp_16         <- get_BLP(fit_16)
gates_16       <- get_GATES(fit_16)
clan_school_16 <- get_CLAN(fit_16, variable = "bl_still_in_school")
clan_educ_16   <- get_CLAN(fit_16, variable = "bl_education_mother")
clan_sister_16 <- get_CLAN(fit_16, variable = "older_sister")
clan_hh_16     <- get_CLAN(fit_16, variable = "bl_HHsize")