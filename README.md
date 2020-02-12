---
title: "SASSI 2-10-20 Contract"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Look for errors in each data set
```{r}
library(prettyR)
library(MBESS)
library(descr)
library(coefficientalpha)
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/SASSI/Data/2-10-20_contract")
clinical_sample = read.csv("clinical_sample.csv", header = TRUE, na.strings = 255)
cross_validation_sample = read.csv("cross_validation_sample.csv", header = TRUE, na.strings = 255)
development_sample = read.csv("development_sample.csv", header = TRUE, na.strings = 255)
normative_sample = read.csv("normative_sample.csv", header= TRUE, na.strings = 255)
stability_sample = read.csv("stability_sample.csv", header = TRUE, na.strings = 255)

```
Review for errors and descriptives
```{r}
dim(clinical_sample)
dim(cross_validation_sample)
dim(development_sample)
dim(normative_sample)
dim(stability_sample)

describe(clinical_sample)
## Do this to get counts if you are having trouble
apply(clinical_sample[,c(3:4,5)], 2, function(x){describe.factor(x)})


describe(development_sample)
describe(cross_validation_sample)
describe(normative_sample)
```
Fisher test's for differences in development and cross validation sample
```{r}
cross_validation_sample_demos = cross_validation_sample[,c(2:13)]
cross_validation_sample_demos$ID = rep(0, dim(cross_validation_sample_demos)[1])


development_sample_demos = development_sample[,c(2:13)]
development_sample_demos$ID = rep(1, dim(development_sample_demos)[1])

cross_development_demos = rbind(cross_validation_sample_demos, development_sample_demos)
ID = cross_development_demos$ID
cross_development_demos$ID = NULL
fisher.test(cross_development_demos$AGE, ID)
### Need to collapse  ETHN, LIVING_SIT, MANDATED, REFERRAL
describe.factor(cross_development_demos$ETHN)
cross_development_demos$ETHN = ifelse(cross_development_demos$ETHN == 7, 1,0)
### Causing problems drop
describe.factor(cross_development_demos$LIVING_SIT)
#cross_development_demos$LIVING_SIT = ifelse(cross_development_demos$LIVING_SIT == 9,1,0)
cross_development_demos$LIVING_SIT = NULL
describe.factor(cross_development_demos$MANDATED)
describe.factor(cross_development_demos$REFERRAL)
cross_development_demos$REFERRAL = ifelse(cross_development_demos$REFERRAL == 7,1,0)
head(cross_development_demos)
describe.factor(cross_development_demos$DWI_ARREST)
### Not enough to include
cross_development_demos$DWI_ARREST = NULL
### Employment status too few
describe.factor(cross_development_demos$EMP_STATUS)
cross_development_demos$EMP_STATUS = NULL
describe.factor(cross_development_demos$TOT_ARREST)
cross_development_demos$TOT_ARREST = ifelse(cross_development_demos$TOT_ARREST == 0,1,0)
describe.factor(cross_development_demos$PRIOR_TREAT)
cross_development_demos$PRIOR_TREAT = ifelse(cross_development_demos$PRIOR_TREAT == 1, 1,0)
head(cross_development_demos)
#cross_development_demos = as.list(cross_development_demos)
######### Try testing each one
head(cross_development_demos)
fisher.test(cross_development_demos$AGE, ID)
fisher.test(cross_development_demos$SEX, ID)
fisher.test(cross_development_demos$ETH, ID)
compmeans(cross_development_demos$ETHN, ID)
fisher.test(cross_development_demos$TOT_ARREST, ID)
fisher.test(cross_development_demos$PRIOR_TREAT, ID)
## Not enough variation
#fisher.test(cross_development_demos$YEARS_ED, ID)
fisher.test(cross_development_demos$CLIENTSETTING, ID)

### Significant at .01 alpha
fisher.test(cross_development_demos$MANDATED, ID)
compmeans(cross_development_demos$MANDATED, ID)
fisher.test(cross_development_demos$REFERRAL, ID)
compmeans(cross_development_demos$REFERRAL, ID)

```
Ok set up for stability samples
```{r}
head(stability_sample)
dim(stability_sample)
dim(clinical_sample)
stability_sample
```
Ok do omegas from clinicial samples
```{r}
library(psych)
head(clinical_sample)
head(clinical_sample[,15:101])
ci.reliability(clinical_sample[,15:101])
head(clinical_sample[,102:115])

#FVA  = SUM (A1 TO A14).
FVA = ci.reliability(clinical_sample[,102:115])
FVA
#FVOD = SUM (D1 TO D20).
FVOD = ci.reliability(clinical_sample[,116:135])
FVOD
#summary(omega(clinical_sample[,116:135]))
#####################
# frisk = s2,s21,s26,s55,s66 (1)  AND  s53 (2).
FRISK = ci.reliability(clinical_sample[,c(16, 35, 40, 69, 80, 67)])
FRISK
FRISK_boot = bootstrap(clinical_sample[,c(16, 35, 40, 69, 80, 67)], type = "omega")
FRISK_boot
summary(omega(clinical_sample[,c(16, 35, 40, 69, 80, 67)]))
################################
#S18,s29,s35,s42,s51,s60,s65,s70 (1).
att = data.frame(S18 = clinical_sample$S18, S29 = clinical_sample$S29, S35 = clinical_sample$S35, S42 = clinical_sample$S42, S51 = clinical_sample$S51, S60 = clinical_sample$S60, S65 = clinical_sample$S65, S70 = clinical_sample$S70)
att_omega = ci.reliability(att)
att_omega
att_boot =  bootstrap(att)
att_boot
head(clinical_sample)
########################
# Symptoms sym = S16,S28,s31,s37,s38,s39,S49,S58,S63,s74,s75,s77,s80,s81,s82,s83,s84,s85,s87 (1).
sym = data.frame(S16 = clinical_sample$S16, S28 = clinical_sample$S28, S31 = clinical_sample$S31, S37 = clinical_sample$S37, S38 = clinical_sample$S38, S39 = clinical_sample$S39, S49 = clinical_sample$S49, S58 = clinical_sample$S58, S63 = clinical_sample$S63, S74 = clinical_sample$S74, S75 = clinical_sample$S75, S77 = clinical_sample$S77, S80 = clinical_sample$S80, S81 = clinical_sample$S81, S82 = clinical_sample$S82, S83 = clinical_sample$S83, S84 = clinical_sample$S84, S85 = clinical_sample$S85, S87 = clinical_sample$S87)
sym_omega = ci.reliability(sym)
sym_omega
#######################
# Obvious Attributes s8,S25,s41,s45,s69,s79,s86 (1)  AND  s10,s14,s27,s30 (2).
oat = data.frame(S8 = clinical_sample$S8, S25 = clinical_sample$S25, S41 = clinical_sample$S41, S45 = clinical_sample$S45, S69 = clinical_sample$S69, S79 = clinical_sample$S79, S86 = clinical_sample$S86, S10 = clinical_sample$S10, S14 = clinical_sample$S14, S27 = clinical_sample$S27, S30 = clinical_sample$S30)
dim(oat)
oat_complete = na.omit(oat)
head(oat)
oat_omega = ci.reliability(oat)
dim(oat_complete)
oat_omega
summary(omega(oat_complete))
bootstrap(oat)
library(ltm)
cronbach.alpha(oat_complete, CI = TRUE)
###########################################
# Subtle Attributes s6,s13, s19,S33,s44,s46,s50,s64, s66,s78 (1)  AND   S11,s15,s20,s53,s61,s67 (2).
sat = data.frame(S6 = clinical_sample$S6, S13 = clinical_sample$S13, S19 = , S33 = clinical_sample$S33, S44 = clinical_sample$S44, S46 = clinical_sample$S46, S50 = clinical_sample$S50, S64 = clinical_sample$S64, S66 = clinical_sample$S66, S78 = clinical_sample$S78, S11 = clinical_sample$S11, S15 = clinical_sample$S15, S20 = clinical_sample$S20, S53 = clinical_sample$S53, S61 = clinical_sample$S61, S67 = clinical_sample$S67)
head(sat)

#S11,s15,s20,s53,s61,s67 (2).
sat[,10:15] = apply(sat[,10:15], 2, function(x){ifelse(x == 2,1,2)})

sat_omega = ci.reliability(sat)
summary(omega(sat))
sat_omega_complete = na.omit(sat)
cronbach.alpha(sat_omega_complete, CI = TRUE)
#####################
## Defensiveness S5,S14,S22,S24,S30  (1)   AND   S2,S4,S9,S17,S23,S68,S69 (2).
def = data.frame(S5 = clinical_sample$S5, S14 = clinical_sample$S14, S22 = clinical_sample$S22, S24 = clinical_sample$S24, S30 = clinical_sample$S30, S2 = clinical_sample$S2, S4 = clinical_sample$S4, S9 = clinical_sample$S9, S17 = clinical_sample$S17, S23 = clinical_sample$S23, S68 = clinical_sample$S68, S69 = clinical_sample$S69)
library(coefficientalpha)
def_omega = ci.reliability(def)
def_omega
summary(omega(def))
def_complete = na.omit(def)
cronbach.alpha(def_complete, CI = TRUE)
library(psych)
alpha(def_complete)
library(psy)
cronbach(def)
######################
## Correctional
FRISK = data.frame(clinical_sample[,c(16, 35, 40, 69, 80, 67)])
head(FRISK)
FRISK$S53 = ifelse(FRISK$S53 == 2,1,2)

FRISK_omega = ci.reliability(FRISK)
FRISK_omega


```
Just try screening outcomes
```{r}
head(development_sample)
library(caret)

development_sample$SASSDR_recode = ifelse(development_sample$SASSDR== 1, 1, 0)
development_sample$SASSDR_recode = as.factor(development_sample$SASSDR_recode)
development_sample$NODIAG = as.factor(development_sample$NODIAG)
confusionMatrix(development_sample$SASSDR_recode, development_sample$NODIAG, positive = "1")


```
