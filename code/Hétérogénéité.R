# Importing libraries
library(dplyr)
library(tidyr)
library(haven)
library(fixest)
library(stargazer)
library(car)
setwd("D:/ENSAE/SEMESTRE_1/STATAPP/Heterogeneity")

controls <- c("older_sister", "bl_still_in_school", "bl_education_mother", "bl_HHsize", "bl_public_transit", "bl_age10", "bl_age11", "bl_age12", "bl_age13",
              "bl_age14", "bl_age15", "bl_age16", "bl_age17", "older_sister_miss", "bl_still_in_school_miss", "bl_education_mother_miss", "bl_HHsize_miss", 
              "bl_public_transit_miss")
eq <- paste("anyemp + anyoil + oil_kk + factor(third) + ", controls, collapse = " + ")

# Importing data
wave3 <- read_dta("waveIII.dta") # endline
wave3_y_sample <- read_dta("waveIII_young_women_sample.dta")

# Sampling at the endline
dfw3_all <- wave3[wave3$endline == 1 & wave3$washedout == 0 & wave3$before_miss == 0 & wave3$bl_ever_married == 0 & wave3$bl_age_reported >= 14
                  & wave3$bl_age_reported <= 16,]
dfw3_15 <- dfw3_all[dfw3_all$bl_age_reported == 14, ]

dfw3_YS <- wave3_y_sample[wave3_y_sample$endline == 1 & wave3_y_sample$washedout == 0 & wave3_y_sample$before_miss == 0 & wave3_y_sample$bl_ever_married == 0 & wave3_y_sample$bl_age_reported >= 14
                 & wave3_y_sample$bl_age_reported <= 16,]



# Faits stylisés sur l'hétérogénéité 
## I- Regressions séparées

### 1- Tranformation des traitements 
dfw3_all$anyemp_exc <- ifelse(dfw3_all$oil_kk == 1, 0, dfw3_all$anyemp)
dfw3_all$anyoil_exc <- ifelse(dfw3_all$oil_kk == 1, 0, dfw3_all$anyoil)

### 2- Regressions séparées selon la variable "Still_in_school"

#### a) Separating data frames
Still_in_school <- dfw3_all[dfw3_all$bl_still_in_school==1,]
No_school <- dfw3_all[dfw3_all$bl_still_in_school==0,]

#### b- Running separate regressions
controls_school <- c("older_sister", "bl_education_mother", "bl_HHsize", "bl_public_transit", "bl_age10", "bl_age11", "bl_age12", "bl_age13",
                     "bl_age14", "bl_age15", "bl_age16", "bl_age17", "older_sister_miss", "bl_still_in_school_miss", "bl_education_mother_miss", "bl_HHsize_miss", 
                     "bl_public_transit_miss")

eq_school <- paste("anyemp_exc + anyoil_exc + oil_kk + factor(third) + ", controls_school , collapse = " + ")

f_u18_school <- as.formula(paste0("under_18 ~ ", eq_school, " | unionID"))
Still_in_school <- feols(f_u18_school,  data = Still_in_school, cluster = ~CLUSTER)
No_school <- feols(f_u18_school,  data = No_school, cluster = ~CLUSTER)

#### c- Showing 

etable(Still_in_school, No_school, keep = c("%anyemp_exc","%anyoil_exc","%oil_kk"),
       dict = c(anyemp_exc = "Empowerment", anyoil_exc = "Incentive", oil_kk = "Incen.*Empow.", under_18="Married under 18"),
       tex = TRUE,                              # Demande le format LaTeX
       title = "Regressions séparées (1)",     # Titre du tableau
       label = "tab:SR_still_school",           # Label pour Overleaf
       replace = TRUE)                          # Nettoie un peu la sortie


### 2- Regressions séparées selon l'état de scolarisation de la mère

#### a- Separating data frames
Schooled <- dfw3_all[dfw3_all$bl_mother_schooled==1,]
No_Schooled <- dfw3_all[dfw3_all$bl_mother_schooled==0,]

#### b- Running separate regressions
controls_secondary <- c("older_sister", "bl_still_in_school", "bl_HHsize", "bl_public_transit", "bl_age10", "bl_age11", "bl_age12", "bl_age13",
                        "bl_age14", "bl_age15", "bl_age16", "bl_age17", "older_sister_miss", "bl_still_in_school_miss", "bl_education_mother_miss", "bl_HHsize_miss", 
                        "bl_public_transit_miss")

eq_schooled <- paste("anyemp_exc + anyoil_exc + oil_kk + factor(third) + ", controls_secondary , collapse = " + ")

f_u18_m_schooled <- as.formula(paste0("under_18 ~ ", eq_schooled, " | unionID"))
Schooled <- feols(f_u18_m_schooled,  data = Schooled, cluster = ~CLUSTER)
No_Schooled <- feols(f_u18_m_schooled,  data = No_Schooled, cluster = ~CLUSTER)

#### c- Showing 

etable(Schooled, No_Schooled, keep = c("%anyemp_exc","%anyoil_exc","%oil_kk"),
       dict = c(anyemp_exc = "Empowerment", anyoil_exc = "Incentive", oil_kk = "Incen.*Empow.", under_18="Married under 18"),
       tex = TRUE,                              # Demande le format LaTeX
       title = "Regressions séparées (2)",     # Titre du tableau
       label = "tab:SR_m_school",           # Label pour Overleaf
       replace = TRUE)                          # Nettoie un peu la sortie



## II-  Regression avec interactions



### 1- Hétérogénéité selon l'état de scolarisation de la jeune fille à la baseline

#### a-Running regressions 
IR_still_school <- feols(
  under_18 ~ oil_kk*bl_still_in_school,
  data = dfw3_YS,
  cluster = ~CLUSTER
)

#### b- Diplay results
etable(IR_still_school,
       dict = c(oil_kk = "Incen.+Empow.", under_18="Married under 18", bl_still_in_school = "Schooled at baseline"),
       tex = TRUE,                              # Demande le format LaTeX
       title = "Regression d'interactions suivant la scolarisation de la jeune fille",     # Titre du tableau
       label = "tab:IR_still_school",           # Label pour Overleaf
       replace = TRUE)                          # Nettoie un peu la sortie

### 2- Hétérogénéité selon l'éducation de la mère

#### a-Running regressions 
IR_m_school <- feols(
  under_18 ~ anyemp*bl_mother_schooled,
  data = dfw3_YS,
  cluster = ~CLUSTER
)

#### b- Diplay results
etable(IR_m_school,
       dict = c(anyemp = "Empowerment", under_18="Married under 18", bl_mother_schooled = "Mother Schooled at baseline"),
       tex = TRUE,                              # Demande le format LaTeX
       title = "Regression d'interactions suivant l'éducation de la mère",     # Titre du tableau
       label = "tab:IR_m_school",           # Label pour Overleaf
       replace = TRUE)                          # Nettoie un peu la sortie




### 3- Hétérogénéité selon le conservatisme de la fille

#### a-Running regressions 
dfw3_YS$high_g_cons = as.factor(dfw3_YS$high_g_cons)

IR_g_cons <- feols(
  under_18 ~ anyemp*high_g_cons,
  data = dfw3_YS,
  cluster = ~CLUSTER
)


#### b- Diplay results
etable(IR_g_cons,
       dict = c(anyemp = "Empowerment", under_18="Married under 18", high_g_cons = "Girl Social Conservatism"),
       tex = TRUE,                              # Demande le format LaTeX
       title = "Regression d'interactions suivant le conservatisme de la jeune fille",     # Titre du tableau
       label = "tab:IR_g_cons",           # Label pour Overleaf
       replace = TRUE)                          # Nettoie un peu la sortie


### 4- Hétérogénéité selon le conservatisme du père

#### a-Running regressions 
dfw3_YS$high_p_cons = as.factor(dfw3_YS$high_p_cons)

IR_p_cons <- feols(
  under_18 ~ anyoil*high_p_cons,
  data = dfw3_YS,
  cluster = ~CLUSTER
)


#### b- Diplay results
etable(IR_p_cons,
       dict = c(anyoil = "Incentive", under_18="Married under 18", high_p_cons1 = "Parent Social Conservatism"),
       tex = TRUE,                              # Demande le format LaTeX
       title = "Regression d'interactions suivant le conservatisme de la jeune fille",     # Titre du tableau
       label = "tab:IR_p_cons",           # Label pour Overleaf
       replace = TRUE)                          # Nettoie un peu la sortie


