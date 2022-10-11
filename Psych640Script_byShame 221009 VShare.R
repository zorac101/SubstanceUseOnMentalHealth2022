
#--- PSYCH 640 Zora & Yadu

# ==== 1. Core Features ====

# Loading Libraries
library(readxl) 

Packages <- c("psych", "sjPlot", "corrplot", "plyr", "readxl", "dplyr", "ggplot2", "lme4",
              "tidyverse", "brant", "MASS", "VGAM", "polycor", "car", "survey")
lapply(Packages, require, character.only = TRUE)

# ==== 2. Dataframe and variables ====

## 2.1 Importing CSV Files
df <- read.csv("Data_03Filtered_R-Code_20220921.csv", header = TRUE)

## 2.2 Creating new dataframe
df.sub <- df %>% 
  dplyr::select(ALC_0, CAN_0, OPI_0, NumRel_0,
                LS, EDU_0, EMPT_0, SEX_0, MH, Shame_High)

df.sub$LS = as.numeric(df.sub$LS)


# ==== 3. Data distribution and visualize ====

## 3.1 descriptives of individual variables
describe(df.sub)
df.sub %>% count(ALC_0)
df.sub %>% count(CAN_0)
df.sub %>% count(OPI_0)
df.sub %>% count(NumRel_0)
df.sub %>% count(LS)
df.sub %>% count(EDU_0)
df.sub %>% count(EMPT_0)
df.sub %>% count(SEX_0)
df.sub %>% count(Age)
df.sub %>% count(MH)

df.highshame %>% count(EDU_0)
df.highshame %>% count(EMPT_0)

df.lowshame %>% count(EDU_0)
df.lowshame %>% count(EMPT_0)

# ==== 4. Logistic ordinal regression ====
# Convert outcome variable to be a factored, ordered data type
df.sub$MH = factor(df.sub$MH, ordered = TRUE)

df.lowshame <- dplyr::filter(df.sub, Shame_High==0)
df.highshame <- dplyr::filter(df.sub, Shame_High==1)

# Low Shame, Male
LowShameMale_1 <- polr(MH ~ EMPT_0+EDU_0+
                    ALC_0+CAN_0+OPI_0, 
                  data = df.lowshame, 
                  subset = SEX_0==0,
                  Hess = TRUE, 
                  method = c("logistic"))
summary(LowShameMale_1)
tab_model(LowShameMale_1)

LowShameMale_2 <- polr(MH ~ EMPT_0+EDU_0+
                      ALC_0+CAN_0+OPI_0+LS+NumRel_0, 
                    data = df.lowshame, 
                    subset = SEX_0==0,
                    Hess = TRUE, 
                    method = c("logistic"))
summary(LowShameMale_2)
tab_model(LowShameMale_2)

LowShameMale_3 <- polr(MH ~ EMPT_0+EDU_0+
                      ALC_0+CAN_0+OPI_0+LS+NumRel_0+
                      ALC_0*LS + ALC_0*NumRel_0 +
                      CAN_0*LS + CAN_0*NumRel_0 +
                      OPI_0*LS + OPI_0*NumRel_0, 
                    data = df.lowshame, 
                    subset = SEX_0==0,
                    Hess = TRUE, 
                    method = c("logistic"))
summary(LowShameMale_3)
tab_model(LowShameMale_3)


# Low shame, Female
LowShameFemale_1 <- polr(MH ~ EMPT_0+EDU_0+
                    SEX_0+ALC_0+CAN_0+OPI_0, 
                  data = df.lowshame, 
                  subset = SEX_0==1,
                  Hess = TRUE, 
                  method = c("logistic"))
summary(LowShameFemale_1)
tab_model(LowShameFemale_1)

LowShameFemale_2 <- polr(MH ~ EMPT_0+EDU_0+
                      ALC_0+CAN_0+OPI_0+LS+NumRel_0, 
                    data = df.lowshame, 
                    subset = SEX_0==1,
                    Hess = TRUE, 
                    method = c("logistic"))
summary(LowShameFemale_2)
tab_model(LowShameFemale_2)

LowShameFemale_3 <- polr(MH ~ EMPT_0+EDU_0+
                      ALC_0+CAN_0+OPI_0+LS+NumRel_0+
                      ALC_0*LS + ALC_0*NumRel_0 +
                      CAN_0*LS + CAN_0*NumRel_0 +
                      OPI_0*LS + OPI_0*NumRel_0, 
                    data = df.lowshame, 
                    subset = SEX_0==1,
                    Hess = TRUE, 
                    method = c("logistic"))
summary(LowShameFemale_3)
tab_model(LowShameFemale_3)

df.lowshame$CAN_1 <- mapvalues(df.lowshame$CAN_0, from = c(0, 1, 2, 3, 4), to = c(-1, 0, 1, 2, 3))
df.lowshame$CAN_2 <- mapvalues(df.lowshame$CAN_0, from = c(0, 1, 2, 3, 4), to = c(-2, -1, 0, 1, 2))
df.lowshame$CAN_3 <- mapvalues(df.lowshame$CAN_0, from = c(0, 1, 2, 3, 4), to = c(-3, -2, -1, 0, 1))
df.lowshame$CAN_4 <- mapvalues(df.lowshame$CAN_0, from = c(0, 1, 2, 3, 4), to = c(-4, -3, -2, -1, 0))

LowShameFemale_3_CAN_1 <- polr(MH ~ EMPT_0+EDU_0+
                           ALC_0+CAN_1+OPI_0+LS+NumRel_0+
                           ALC_0*LS + ALC_0*NumRel_0 +
                           CAN_1*LS + CAN_0*NumRel_0 +
                           OPI_0*LS + OPI_0*NumRel_0, 
                         data = df.lowshame, 
                         subset = SEX_0==1,
                         Hess = TRUE, 
                         method = c("logistic"))
summary(LowShameFemale_3_CAN_1)
tab_model(LowShameFemale_3_CAN_1)


LowShameFemale_3_CAN_2 <- polr(MH ~ EMPT_0+EDU_0+
                                 ALC_0+CAN_2+OPI_0+LS+NumRel_0+
                                 ALC_0*LS + ALC_0*NumRel_0 +
                                 CAN_2*LS + CAN_0*NumRel_0 +
                                 OPI_0*LS + OPI_0*NumRel_0, 
                               data = df.lowshame, 
                               subset = SEX_0==1,
                               Hess = TRUE, 
                               method = c("logistic"))
summary(LowShameFemale_3_CAN_2)
tab_model(LowShameFemale_3_CAN_2)

LowShameFemale_3_CAN_3 <- polr(MH ~ EMPT_0+EDU_0+
                                 ALC_0+CAN_3+OPI_0+LS+NumRel_0+
                                 ALC_0*LS + ALC_0*NumRel_0 +
                                 CAN_3*LS + CAN_0*NumRel_0 +
                                 OPI_0*LS + OPI_0*NumRel_0, 
                               data = df.lowshame, 
                               subset = SEX_0==1,
                               Hess = TRUE, 
                               method = c("logistic"))
summary(LowShameFemale_3_CAN_3)
tab_model(LowShameFemale_3_CAN_3)

LowShameFemale_3_CAN_4 <- polr(MH ~ EMPT_0+EDU_0+
                                 ALC_0+CAN_4+OPI_0+LS+NumRel_0+
                                 ALC_0*LS + ALC_0*NumRel_0 +
                                 CAN_4*LS + CAN_0*NumRel_0 +
                                 OPI_0*LS + OPI_0*NumRel_0, 
                               data = df.lowshame, 
                               subset = SEX_0==1,
                               Hess = TRUE, 
                               method = c("logistic"))
summary(LowShameFemale_3_CAN_4)
tab_model(LowShameFemale_3_CAN_4)


# High Shame, Male
HighShameMale_1 <- polr(MH ~ EMPT_0+EDU_0+
                         ALC_0+CAN_0+OPI_0, 
                       data = df.highshame, 
                       subset = SEX_0==0,
                       Hess = TRUE, 
                       method = c("logistic"))
summary(HighShameMale_1)
tab_model(HighShameMale_1)

HighShameMale_2 <- polr(MH ~ EMPT_0+EDU_0+
                         ALC_0+CAN_0+OPI_0+LS+NumRel_0, 
                       data = df.highshame, 
                       subset = SEX_0==0,
                       Hess = TRUE, 
                       method = c("logistic"))
summary(HighShameMale_2)
tab_model(HighShameMale_2)

HighShameMale_3 <- polr(MH ~ EMPT_0+EDU_0+
                         ALC_0+CAN_0+OPI_0+LS+NumRel_0+
                         ALC_0*LS + ALC_0*NumRel_0 +
                         CAN_0*LS + CAN_0*NumRel_0 +
                         OPI_0*LS + OPI_0*NumRel_0, 
                       data = df.highshame, 
                       subset = SEX_0==0,
                       Hess = TRUE, 
                       method = c("logistic"))
summary(HighShameMale_3)
tab_model(HighShameMale_3)

# High shame, Female
HighShameFemale_1 <- polr(MH ~ EMPT_0+EDU_0+
                           SEX_0+ALC_0+CAN_0+OPI_0, 
                         data = df.highshame, 
                         subset = SEX_0==1,
                         Hess = TRUE, 
                         method = c("logistic"))
summary(HighShameFemale_1)
tab_model(HighShameFemale_1)

HighShameFemale_2 <- polr(MH ~ EMPT_0+EDU_0+
                           ALC_0+CAN_0+OPI_0+LS+NumRel_0, 
                         data = df.highshame, 
                         subset = SEX_0==1,
                         Hess = TRUE, 
                         method = c("logistic"))
summary(HighShameFemale_2)
tab_model(HighShameFemale_2)

HighShameFemale_3 <- polr(MH ~ EMPT_0+EDU_0+
                           ALC_0+CAN_0+OPI_0+LS+NumRel_0+
                           ALC_0*LS + ALC_0*NumRel_0 +
                           CAN_0*LS + CAN_0*NumRel_0 +
                           OPI_0*LS + OPI_0*NumRel_0, 
                         data = df.highshame, 
                         subset = SEX_0==1,
                         Hess = TRUE, 
                         method = c("logistic"))
summary(HighShameFemale_3)
tab_model(HighShameFemale_3)

## Test interaction of OPI * NumRel for HighShame Female
df.highshame$OPI_1 <- mapvalues(df.highshame$OPI_0, from = c(0, 1, 2), to = c(-1, 0, 1))
df.highshame$OPI_2 <- mapvalues(df.highshame$OPI_0, from = c(0, 1, 2), to = c(-2, -1, 0))

HighShameFemale_3_OPI_1 <- polr(MH ~ EMPT_0+EDU_0+
                            ALC_0+CAN_0+OPI_1+LS+NumRel_0+
                            ALC_0*LS + ALC_0*NumRel_0 +
                            CAN_0*LS + CAN_0*NumRel_0 +
                            OPI_0*LS + OPI_1*NumRel_0, 
                          data = df.highshame, 
                          subset = SEX_0==1,
                          Hess = TRUE, 
                          method = c("logistic"))
summary(HighShameFemale_3_OPI_1)
tab_model(HighShameFemale_3_OPI_1)
# NumRel: OR = 1.71, 95% CI = 1.32 - 2.23, p < .001

HighShameFemale_3_OPI_2 <- polr(MH ~ EMPT_0+EDU_0+
                                  ALC_0+CAN_0+OPI_2+LS+NumRel_0+
                                  ALC_0*LS + ALC_0*NumRel_0 +
                                  CAN_0*LS + CAN_0*NumRel_0 +
                                  OPI_0*LS + OPI_2*NumRel_0, 
                                data = df.highshame, 
                                subset = SEX_0==1,
                                Hess = TRUE, 
                                method = c("logistic"))
summary(HighShameFemale_3_OPI_2)
tab_model(HighShameFemale_3_OPI_2)
# NumRel: OR = 2.19, 95% CI = 1.41 - 3.37, p < .001

# ==== 5 Assumptions====

df.sub$MH = as.numeric(df.sub$MH)

# 5.1: Multi-collinearity assumption (VIF)
cors<-cor(df.sub[, c(1:9)])
corrplot(cors, method="number")

# check VIF
vif_model <- lm(MH ~ ALC_0 + CAN_0 + OPI_0 + NumRel_0 + 
                  LS + EDU_0 + EMPT_0 + SEX_0 + Shame_High,
           data = df.sub)
vif(vif_model, type="marginal")

# 5.2: Proportional odds assumption 
df.sub$MH = factor(df.sub$MH, ordered = TRUE)

# model with proportional odds assumption
poa_model1 <- vglm(MH ~ ALC_0 + CAN_0 + OPI_0 + NumRel_0 + 
               LS + EDU_0 + EMPT_0 + SEX_0 + Shame_High +
               ALC_0*LS+CAN_0*LS+OPI_0*LS+
               ALC_0*NumRel_0+CAN_0*NumRel_0+OPI_0*NumRel_0, data=df.sub,
             family=cumulative(parallel=T))
summary(poa_model1)

# model without proportional odds assumption
poa_model2 <- vglm(MH ~ ALC_0 + CAN_0 + OPI_0 + NumRel_0 + 
                     LS + EDU_0 + EMPT_0 + SEX_0 + Shame_High +
                     ALC_0*LS+CAN_0*LS+OPI_0*LS+
                     ALC_0*NumRel_0+CAN_0*NumRel_0+OPI_0*NumRel_0, data=df.sub,
             family=cumulative(parallel=F))

summary(poa_model2)

# chi-square test to check difference of deviances and residual degrees of freedom
# abs since the values could be negative. Negative difference of degrees of freedom would produce p=NaN

pchisq(abs(deviance(poa_model1)-deviance(poa_model2)), abs(df.residual(poa_model2)-df.residual(poa_model1)))
# chi-square = 1. That is, the proportional odds assumption was not violated.

library(MASS)
poa_model <- polr(
  formula = MH ~ ALC_0 + CAN_0 + OPI_0 + NumRel_0 + 
    LS + EDU_0 + EMPT_0 + SEX_0 + Shame_High +
    ALC_0*LS+CAN_0*LS+OPI_0*LS+
    ALC_0*NumRel_0+CAN_0*NumRel_0+OPI_0*NumRel_0, 
  data = df.sub)

library(brant)
brant::brant(poa_model)
