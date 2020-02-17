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

#describe(clinical_sample)
## Do this to get counts if you are having trouble
apply(clinical_sample[,c(3:4,5)], 2, function(x){describe.factor(x)})


#describe(development_sample)
#describe(cross_validation_sample)
#describe(normative_sample)


### NODIAG change to 1 = NODIAG; 0 = DIAG; 
### Change SASSDR  2 low prob = 0, 1 high prob = 1 
clinical_sample$NODIAG = ifelse(clinical_sample$NODIAG == 1,0,1)
clinical_sample$SASSDR = ifelse(clinical_sample$SASSDR == 2,0,1)

cross_validation_sample$NODIAG = ifelse(cross_validation_sample$NODIAG == 1,0,1)
cross_validation_sample$SASSDR = ifelse(cross_validation_sample$SASSDR == 2,0,1)

development_sample$NODIAG = ifelse(development_sample$NODIAG == 1,0,1)
development_sample$SASSDR = ifelse(development_sample$SASSDR == 2,0,1)

normative_sample$SASSDR = ifelse(normative_sample$SASSDR == 2,0,1) 
stability_sample$SASSDR = ifelse(stability_sample$SASSDR == 2,0,1)

```


Recode the vars (see AR3 scoring)
```{r}
#### Get rid of the "R" at the end of the S vars in stability sample
test_sample =  stability_sample
test_sample = stability_sample[,8:94]

names(test_sample) = gsub("R", "", names(test_sample))
stability_sample[,8:94] =  NULL

stability_sample = data.frame(stability_sample, test_sample)


#### ### Recode the variables
#frisk =   s53 (2).
clinical_sample$S53 = ifelse(clinical_sample$S53 == 2,1,2)
cross_validation_sample$S53 = ifelse(cross_validation_sample$S53 == 2,1,2)
development_sample$S53 = ifelse(development_sample$S53 == 2,1,2)
normative_sample$S53 = ifelse(normative_sample$S53 == 2,1,2)
stability_sample$S53 = ifelse(stability_sample$S53 == 2,1,2)

#oat =  s10,s14,s27,s30 (2).
clinical_sample$S10 = ifelse(clinical_sample$S10 == 2,1,2)
cross_validation_sample$S10 = ifelse(cross_validation_sample$S10 == 2,1,2)
development_sample$S10 = ifelse(development_sample$S10 == 2,1,2)
normative_sample$S10 = ifelse(normative_sample$S10 == 2,1,2)
stability_sample$S10 = ifelse(stability_sample$S10 == 2,1,2)

clinical_sample$S14 = ifelse(clinical_sample$S14 == 2,1,2)
cross_validation_sample$S14 = ifelse(cross_validation_sample$S14 == 2,1,2)
development_sample$S14 = ifelse(development_sample$S14 == 2,1,2)
normative_sample$S14 = ifelse(normative_sample$S14 == 2,1,2)
stability_sample$S14 = ifelse(stability_sample$S14 == 2,1,2)

clinical_sample$S27 = ifelse(clinical_sample$S27 == 2,1,2)
cross_validation_sample$S27 = ifelse(cross_validation_sample$S27 == 2,1,2)
development_sample$S27 = ifelse(development_sample$S27 == 2,1,2)
normative_sample$S27 = ifelse(normative_sample$S27 == 2,1,2)
stability_sample$S27 = ifelse(stability_sample$S27 == 2,1,2)

clinical_sample$S30 = ifelse(clinical_sample$S30 == 2,1,2)
cross_validation_sample$S30 = ifelse(cross_validation_sample$S30 == 2,1,2)
development_sample$S30 = ifelse(development_sample$S30 == 2,1,2)
normative_sample$S30 = ifelse(normative_sample$S30 == 2,1,2)
stability_sample$S30 = ifelse(stability_sample$S30 == 2,1,2)


#### sat = S11,s15,s20,s53,s61,s67 (2).
clinical_sample$S11 = ifelse(clinical_sample$S11 == 2,1,2)
cross_validation_sample$S11 = ifelse(cross_validation_sample$S11 == 2,1,2)
development_sample$S11 = ifelse(development_sample$S11 == 2,1,2)
normative_sample$S11 = ifelse(normative_sample$S11 == 2,1,2)
stability_sample$S11 = ifelse(stability_sample$S11 == 2,1,2)

clinical_sample$S15 = ifelse(clinical_sample$S15 == 2,1,2)
cross_validation_sample$S15 = ifelse(cross_validation_sample$S15 == 2,1,2)
development_sample$S15 = ifelse(development_sample$S15 == 2,1,2)
normative_sample$S15 = ifelse(normative_sample$S15 == 2,1,2)
stability_sample$S15 = ifelse(stability_sample$S15 == 2,1,2)

clinical_sample$S61 = ifelse(clinical_sample$S61 == 2,1,2)
cross_validation_sample$S61 = ifelse(cross_validation_sample$S61 == 2,1,2)
development_sample$S61 = ifelse(development_sample$S61 == 2,1,2)
normative_sample$S61 = ifelse(normative_sample$S61 == 2,1,2)
stability_sample$S61 = ifelse(stability_sample$S61 == 2,1,2)


#DEF  = S5,S14,S22,S24,S30  (1)   AND   S2,S4,S9,S17,S23,S68,S69 (2).
## Recode S14, S30
clinical_sample$S14_DEF = ifelse(clinical_sample$S14 == 1,2,1)
cross_validation_sample$S14_DEF = ifelse(cross_validation_sample$S14 == 1,2,1)
development_sample$S14_DEF = ifelse(development_sample$S14 == 1,2,1)
normative_sample$S14_DEF = ifelse(normative_sample$S14 == 1,2,1)
stability_sample$S14_DEF = ifelse(stability_sample$S14 == 1,2,1)

clinical_sample$S30_DEF = ifelse(clinical_sample$S30 == 1,2,1)
cross_validation_sample$S30_DEF = ifelse(cross_validation_sample$S30 == 1,2,1)
development_sample$S30_DEF = ifelse(development_sample$S30 == 1,2,1)
normative_sample$S30_DEF = ifelse(normative_sample$S30 == 1,2,1)
stability_sample$S30_DEF = ifelse(stability_sample$S30 == 1,2,1)

clinical_sample$S2_DEF = ifelse(clinical_sample$S2 == 2,1,2)
cross_validation_sample$S2_DEF = ifelse(cross_validation_sample$S2 == 2,1,2)
development_sample$S2_DEF = ifelse(development_sample$S2 == 2,1,2)
normative_sample$S2_DEF = ifelse(normative_sample$S2 == 2,1,2)
stability_sample$S2_DEF = ifelse(stability_sample$S2 == 2,1,2)

##########################

clinical_sample$S4 = ifelse(clinical_sample$S4 == 2,1,2)
cross_validation_sample$S4 = ifelse(cross_validation_sample$S4 == 2,1,2)
development_sample$S4 = ifelse(development_sample$S4 == 2,1,2)
normative_sample$S4 = ifelse(normative_sample$S4 == 2,1,2)
stability_sample$S4 = ifelse(stability_sample$S4 == 2,1,2)

clinical_sample$S9 = ifelse(clinical_sample$S9 == 2,1,2)
cross_validation_sample$S9 = ifelse(cross_validation_sample$S9 == 2,1,2)
development_sample$S9 = ifelse(development_sample$S9 == 2,1,2)
normative_sample$S9 = ifelse(normative_sample$S9 == 2,1,2)
stability_sample$S9 = ifelse(stability_sample$S9 == 2,1,2)

clinical_sample$S17 = ifelse(clinical_sample$S17 == 2,1,2)
cross_validation_sample$S17 = ifelse(cross_validation_sample$S17 == 2,1,2)
development_sample$S17 = ifelse(development_sample$S17 == 2,1,2)
normative_sample$S17 = ifelse(normative_sample$S17 == 2,1,2)
stability_sample$S17 = ifelse(stability_sample$S17 == 2,1,2)

clinical_sample$S23 = ifelse(clinical_sample$S23 == 2,1,2)
cross_validation_sample$S23 = ifelse(cross_validation_sample$S23 == 2,1,2)
development_sample$S23 = ifelse(development_sample$S23 == 2,1,2)
normative_sample$S23 = ifelse(normative_sample$S23 == 2,1,2)
stability_sample$S23 = ifelse(stability_sample$S23 == 2,1,2)

clinical_sample$S68 = ifelse(clinical_sample$S68 == 2,1,2)
cross_validation_sample$S68 = ifelse(cross_validation_sample$S68 == 2,1,2)
development_sample$S68 = ifelse(development_sample$S68 == 2,1,2)
normative_sample$S68 = ifelse(normative_sample$S68 == 2,1,2)
stability_sample$S68 = ifelse(stability_sample$S68 == 2,1,2)
### Change for DEF
clinical_sample$S69_DEF = ifelse(clinical_sample$S69 == 2,1,2)
cross_validation_sample$S69_DEF = ifelse(cross_validation_sample$S69 == 2,1,2)
development_sample$S69_DEF = ifelse(development_sample$S69 == 2,1,2)
normative_sample$S69_DEF = ifelse(normative_sample$S69 == 2,1,2)
stability_sample$S69_DEF = ifelse(stability_sample$S69 == 2,1,2)

# sam s11 
clinical_sample$S11 = ifelse(clinical_sample$S11 == 2,1,2)
cross_validation_sample$S11 = ifelse(cross_validation_sample$S11 == 2,1,2)
development_sample$S11 = ifelse(development_sample$S11 == 2,1,2)
normative_sample$S11 = ifelse(normative_sample$S11 == 2,1,2)
stability_sample$S11 = ifelse(stability_sample$S11 == 2,1,2)

# COR= s3,s15,s27 (2).
clinical_sample$S3 = ifelse(clinical_sample$S3 == 2,1,2)
cross_validation_sample$S3 = ifelse(cross_validation_sample$S3 == 2,1,2)
development_sample$S3 = ifelse(development_sample$S3 == 2,1,2)
normative_sample$S3 = ifelse(normative_sample$S3 == 2,1,2)
stability_sample$S3 = ifelse(stability_sample$S3 == 2,1,2)

#VAL    = S15,S27,S30,S36,S47    (1)  AND    S1,S23,S25,S32,S51,S54 (2).

### Reverse code for VAL S15, S27, 30
clinical_sample$S15_VAL = ifelse(clinical_sample$S15 == 1,2,1)
cross_validation_sample$S15_VAL = ifelse(cross_validation_sample$S15 == 1,2,1)
development_sample$S15_VAL = ifelse(development_sample$S15 == 1,2,1)
normative_sample$S15_VAL = ifelse(normative_sample$S15 == 1,2,1)
stability_sample$S15_VAL = ifelse(stability_sample$S15 == 1,2,1)

clinical_sample$S27_VAL = ifelse(clinical_sample$S27 == 1,2,1)
cross_validation_sample$S27_VAL = ifelse(cross_validation_sample$S27 == 1,2,1)
development_sample$S27_VAL = ifelse(development_sample$S27 == 1,2,1)
normative_sample$S27_VAL = ifelse(normative_sample$S27 == 1,2,1)
stability_sample$S27_VAL = ifelse(stability_sample$S27 == 1,2,1)

clinical_sample$S30_VAL = ifelse(clinical_sample$S30 == 1,2,1)
cross_validation_sample$S30_VAL = ifelse(cross_validation_sample$S30 == 1,2,1)
development_sample$S30_VAL = ifelse(development_sample$S30 == 1,2,1)
normative_sample$S30_VAL = ifelse(normative_sample$S30 == 1,2,1)
stability_sample$S30_VAL = ifelse(stability_sample$S30 == 1,2,1)

### This is a 2 for VAL and 1 for other vars
clinical_sample$S25_VAL = ifelse(clinical_sample$S25 == 2,1,2)
cross_validation_sample$S25_VAL = ifelse(cross_validation_sample$S25 == 2,1,2)
development_sample$S25_VAL = ifelse(development_sample$S25 == 2,1,2)
normative_sample$S25_VAL = ifelse(normative_sample$S25 == 2,1,2)
stability_sample$S25_VAL = ifelse(stability_sample$S25 == 2,1,2)

clinical_sample$S32_VAL = ifelse(clinical_sample$S32 == 2,1,2)
cross_validation_sample$S32_VAL = ifelse(cross_validation_sample$S32 == 2,1,2)
development_sample$S32_VAL = ifelse(development_sample$S32 == 2,1,2)
normative_sample$S32_VAL = ifelse(normative_sample$S32 == 2,1,2)
stability_sample$S32_VAL = ifelse(stability_sample$S32 == 2,1,2)

### These are just two for VAL
clinical_sample$S51_VAL = ifelse(clinical_sample$S51 == 2,1,2)
cross_validation_sample$S51_VAL = ifelse(cross_validation_sample$S51 == 2,1,2)
development_sample$S51_VAL = ifelse(development_sample$S51 == 2,1,2)
normative_sample$S51_VAL = ifelse(normative_sample$S51 == 2,1,2)
stability_sample$S51_VAL = ifelse(stability_sample$S51 == 2,1,2)

clinical_sample$S54 = ifelse(clinical_sample$S54 == 2,1,2)
cross_validation_sample$S54 = ifelse(cross_validation_sample$S54 == 2,1,2)
development_sample$S54 = ifelse(development_sample$S54 == 2,1,2)
normative_sample$S54 = ifelse(normative_sample$S54 == 2,1,2)
stability_sample$S54 = ifelse(stability_sample$S54 == 2,1,2)

clinical_sample$S1 = ifelse(clinical_sample$S1 == 2,1,2)
cross_validation_sample$S1 = ifelse(cross_validation_sample$S1 == 2,1,2)
development_sample$S1 = ifelse(development_sample$S1 == 2,1,2)
normative_sample$S1 = ifelse(normative_sample$S1 == 2,1,2)
stability_sample$S1 = ifelse(stability_sample$S1 == 2,1,2)

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
fisher.test(cross_development_demos$TOT_ARREST, ID)
fisher.test(cross_development_demos$PRIOR_TREAT, ID)
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
#sassi_omega =  ci.reliability(clinical_sample[,15:101])

head(clinical_sample[,102:115])

#FVA  = SUM (A1 TO A14).
FVA_omega = ci.reliability(clinical_sample[,102:115])
FVA_omega
#FVOD = SUM (D1 TO D20).
FVOD_omega = ci.reliability(clinical_sample[,116:135])
FVOD_omega
#summary(omega(clinical_sample[,116:135]))
#####################
# frisk = s2,s21,s26,s55,s66 (1)  AND  s53 (2).
FRISK_omega = ci.reliability(clinical_sample[,c(16, 35, 40, 69, 80, 67)])
FRISK_omega
################################
#S18,s29,s35,s42,s51,s60,s65,s70 (1).
att = data.frame(S18 = clinical_sample$S18, S29 = clinical_sample$S29, S35 = clinical_sample$S35, S42 = clinical_sample$S42, S51 = clinical_sample$S51, S60 = clinical_sample$S60, S65 = clinical_sample$S65, S70 = clinical_sample$S70)
att_omega = ci.reliability(att)
att_omega
########################
# Symptoms sym = S16,S28,s31,s37,s38,s39,S49,S58,S63,s74,s75,s77,s80,s81,s82,s83,s84,s85,s87 (1).
sym = data.frame(S16 = clinical_sample$S16, S28 = clinical_sample$S28, S31 = clinical_sample$S31, S37 = clinical_sample$S37, S38 = clinical_sample$S38, S39 = clinical_sample$S39, S49 = clinical_sample$S49, S58 = clinical_sample$S58, S63 = clinical_sample$S63, S74 = clinical_sample$S74, S75 = clinical_sample$S75, S77 = clinical_sample$S77, S80 = clinical_sample$S80, S81 = clinical_sample$S81, S82 = clinical_sample$S82, S83 = clinical_sample$S83, S84 = clinical_sample$S84, S85 = clinical_sample$S85, S87 = clinical_sample$S87)
sym_omega = ci.reliability(sym)
sym_omega
#######################
# Obvious Attributes s8,S25,s41,s45,s69,s79,s86 (1)  AND  s10,s14,s27,s30 (2).
oat = data.frame(S8 = clinical_sample$S8, S25 = clinical_sample$S25, S41 = clinical_sample$S41, S45 = clinical_sample$S45, S69 = clinical_sample$S69, S79 = clinical_sample$S79, S86 = clinical_sample$S86, S10 = clinical_sample$S10, S14 = clinical_sample$S14, S27 = clinical_sample$S27, S30 = clinical_sample$S30)
oat_omega = ci.reliability(oat)
oat_omega
###########################################
# Subtle Attributes s6,s13, s19,S33,s44,s46,s50,s64, s66,s78 (1)  AND   S11,s15,s20,s53,s61,s67 (2).
sat = data.frame(S6 = clinical_sample$S6, S13 = clinical_sample$S13, S19 = clinical_sample$S19, S33 = clinical_sample$S33, S44 = clinical_sample$S44, S46 = clinical_sample$S46, S50 = clinical_sample$S50, S64 = clinical_sample$S64, S66 = clinical_sample$S66, S78 = clinical_sample$S78, S11 = clinical_sample$S11, S15 = clinical_sample$S15, S20 = clinical_sample$S20, S53 = clinical_sample$S53, S61 = clinical_sample$S61, S67 = clinical_sample$S67)
sat_omega = ci.reliability(sat)
sat_omega
summary(omega(sat))
sat_omega_complete = na.omit(sat)
cronbach.alpha(sat_omega_complete, CI = TRUE)
#####################
## Defensiveness S5,S14,S22,S24,S30  (1)   AND   S2,S4,S9,S17,S23,S68,S69 (2).
def = data.frame(S5 = clinical_sample$S5, S14 = clinical_sample$S14_DEF, S22 = clinical_sample$S22, S24 = clinical_sample$S24, S30 = clinical_sample$S30_DEF, S2 = clinical_sample$S2_DEF, S4 = clinical_sample$S4, S9 = clinical_sample$S9, S17 = clinical_sample$S17, S23 = clinical_sample$S23, S68 = clinical_sample$S68, S69 = clinical_sample$S69_DEF)
def_omega = ci.reliability(def)
def_omega
def_complete = na.omit(def)
cronbach.alpha(def_complete, CI = TRUE)
########
### sam s16,S21,s26,s38,s42,s46,s51,s60,s63,s70,s84 (1)   AND  s11 (2).
sam = data.frame(S16 =  clinical_sample$S16, S21 = clinical_sample$S21, S26 = clinical_sample$S26, S38 = clinical_sample$S38, S42 = clinical_sample$S42, S51 = clinical_sample$S51, S60 = clinical_sample$S60, S63 = clinical_sample$S63, S70 = clinical_sample$S70, S84 = clinical_sample$S84, S11 = clinical_sample$S11)
sam_omega = ci.reliability(sam)
sam_omega
######################
## Correctional s16,s19,s25,s26,s31,s32,s33,s49,s50,s56,s58,s65,s81,s83 (1)  AND  s3,s15,s27 (2).
correct = data.frame(S16 = clinical_sample$S16, S19 = clinical_sample$S19, S25 = clinical_sample$S25, S26 = clinical_sample$S26, S31 = clinical_sample$S31, S32 = clinical_sample$S32, S33 = clinical_sample$S33, S49 = clinical_sample$S49, S50= clinical_sample$S50, S56 = clinical_sample$S56, S58 = clinical_sample$S58, S65 = clinical_sample$S65, S81 = clinical_sample$S81, S83 = clinical_sample$S83, S3 = clinical_sample$S3, S15 = clinical_sample$S15, S27 = clinical_sample$S27)
correct_omega = ci.reliability(correct)
correct_omega

## VAL    = S15,S27,S30,S36,S47    (1)  AND    S1,S23,S25,S32,S51,S54 (2).
val = data.frame(S15 = clinical_sample$S15_VAL, S27 = clinical_sample$S27_VAL, S30 = clinical_sample$S30_VAL, S36 = clinical_sample$S36, S1 = clinical_sample$S1, S23 = clinical_sample$S23, S32 = clinical_sample$S32_VAL, S51 = clinical_sample$S51_VAL, S54 = clinical_sample$S54)
val_omega = ci.reliability(val)
val_omega

```
Stability coefficents
```{r}
### Need merge those with client in stability with normative

dim(stability_sample)
names(stability_sample)[1] = "ID"
names(normative_sample)[1] = "ID"
dim(stability_sample)
stable_norm = merge(stability_sample, normative_sample, by = "ID", all.x = TRUE)
dim(stable_norm)
stable_norm

SASDR_stable = cor.test(stable_norm$SASSDR.x, stable_norm$SASSDR.y)
SASDR_stable

FVA_stable =  cor.test(stable_norm$FVA.x, stable_norm$FVA.y)
FVA_stable

FVOD_stable = cor.test(stable_norm$FVOD.x, stable_norm$FVOD.y)
FVOD_stable

FRISK_stable = cor.test(stable_norm$frisk.x, stable_norm$frisk.y)
FRISK_stable

att_stable = cor.test(stable_norm$att.x, stable_norm$att.y)
att_stable

sym_stable = cor.test(stable_norm$sym.x, stable_norm$sym.y)
sym_stable

oat_stable = cor.test(stable_norm$oat.x, stable_norm$oat.y)
oat_stable

sat_stable = cor.test(stable_norm$sat.x, stable_norm$sat.y)
sat_stable

def_stable = cor.test(stable_norm$DEF.x, stable_norm$DEF.y)
def_stable

sam_stable = cor.test(stable_norm$sam.x, stable_norm$sam.y)
sam_stable

cor_stable = cor.test(stable_norm$COR.x, stable_norm$COR.y)
cor_stable

val_stable = cor.test(stable_norm$VAL.x, stable_norm$VAL.y)
val_stable




```


Table 2 results
```{r}
sassi_omega
FVA_omega
FVOD_omega
FRISK_omega
att_omega
sym_omega
oat_omega
sat_omega
def_omega
sam_omega
correct_omega
val_omega
### Stabilities
SASDR_stable
FVA_stable
FVOD_stable
FRISK_stable
att_stable
sym_stable
oat_stable
sat_stable
def_stable
sam_stable
cor_stable
val_stable
```


Tables 3 through 5
```{r}
head(development_sample)
library(caret)
library(DescTools)


development_sample$SASSDR = as.factor(development_sample$SASSDR)
development_sample$NODIAG = as.factor(development_sample$NODIAG)
SASSDR_development =  confusionMatrix(development_sample$SASSDR, development_sample$NODIAG, positive = "1")
n_correct_SASSDR_development =  sum(SASSDR_development$table[1,1], SASSDR_development$table[2,2])
n_correct_SASSDR_development

SASSDR_development_totals = data.frame(test_p = sum(SASSDR_development$table[,1]), test_n = sum(SASSDR_development$table[,2]), criteria_p = sum(SASSDR_development$table[1,]), criteria_n = sum(SASSDR_development$table[2,]))


cramer_v_SASSDR_development = CramerV(SASSDR_development$table, conf.level = .95)
cramer_v_SASSDR_development



cross_validation_sample$SASSDR = as.factor(cross_validation_sample$SASSDR)
cross_validation_sample$NODIAG = as.factor(cross_validation_sample$NODIAG)
SASSDR_cross_validation =  confusionMatrix(cross_validation_sample$SASSDR, cross_validation_sample$NODIAG, positive = "1")
n_correct_SASSDR_cross_validation =  sum(SASSDR_cross_validation$table[1,1], SASSDR_cross_validation$table[2,2])
n_correct_SASSDR_cross_validation

cross_validation_sample_totals = data.frame(test_p = sum(SASSDR_cross_validation$table[,1]), test_n = sum(SASSDR_cross_validation$table[,2]), criteria_p = sum(SASSDR_cross_validation$table[1,]), criteria_n = sum(SASSDR_cross_validation$table[2,]))
cross_validation_sample_totals

cramer_v_SASSDR_cross_validation = CramerV(SASSDR_cross_validation$table, conf.level = .95)
cramer_v_SASSDR_cross_validation

clinical_sample$SASSDR = as.factor(clinical_sample$SASSDR)
clinical_sample$NODIAG = as.factor(clinical_sample$NODIAG)
SASSDR_clinical=  confusionMatrix(clinical_sample$SASSDR, clinical_sample$NODIAG, positive = "1")

n_correct_SASSDR_clinical=  sum(SASSDR_clinical$table[1,1], SASSDR_clinical$table[2,2])
n_correct_SASSDR_clinical

clinical_sample_totals = data.frame(test_p = sum(SASSDR_clinical$table[,1]), test_n = sum(SASSDR_clinical$table[,2]), criteria_p = sum(SASSDR_clinical$table[1,]), criteria_n = sum(SASSDR_clinical$table[2,]))
clinical_sample_totals

cramer_v_SASSDR_clinical= CramerV(SASSDR_clinical$table, conf.level = .95)
cramer_v_SASSDR_clinical


```
Tables 3 through 5 results
```{r}
## 3
SASSDR_development
n_correct_SASSDR_development
SASSDR_development_totals
cramer_v_SASSDR_development
## 4
SASSDR_cross_validation
n_correct_SASSDR_cross_validation
cross_validation_sample_totals
cramer_v_SASSDR_cross_validation
## 5
SASSDR_clinical
n_correct_SASSDR_clinical
clinical_sample_totals
cramer_v_SASSDR_clinical

```
Table 6

As shown in Table 6, overall SASSI-A3 screening accuracy was XX.X% for cases where participants’ DEF scores were one standard deviation above the normative sample DEF scale mean score or lower (i.e., DEF scale scores of 7 or less).
```{r}
def_criteria =  round(mean(normative_sample$DEF)+sd(normative_sample$DEF))
def_criteria
### DEF 9
clinical_sample_def_9 = subset(clinical_sample, DEF <= 9)
dim(clinical_sample_def_9)
SASSDR_clinical_def_9=  confusionMatrix(clinical_sample_def_9$SASSDR, clinical_sample_def_9$NODIAG, positive = "1")
SASSDR_clinical_def_9

n_correct_SASSDR_clinical_def_9=  sum(SASSDR_clinical_def_9$table[1,1], SASSDR_clinical_def_9$table[2,2])
n_correct_SASSDR_clinical_def_9

SASSDR_clinical_def_9_totals = data.frame(test_p = sum(SASSDR_clinical_def_9$table[,1]), test_n = sum(SASSDR_clinical_def_9$table[,2]), criteria_p = sum(SASSDR_clinical_def_9$table[1,]), criteria_n = sum(SASSDR_clinical_def_9$table[2,]))


cramer_v_SASSDR_clinical_def_9= CramerV(SASSDR_clinical_def_9$table, conf.level = .95)
cramer_v_SASSDR_clinical_def_9


#### DEF 10
clinical_sample_def_10 = subset(clinical_sample, DEF <= 10)
dim(clinical_sample_def_10)
SASSDR_clinical_def_10=  confusionMatrix(clinical_sample_def_10$SASSDR, clinical_sample_def_10$NODIAG, positive = "1")
SASSDR_clinical_def_10

n_correct_SASSDR_clinical_def_10=  sum(SASSDR_clinical_def_10$table[1,1], SASSDR_clinical_def_10$table[2,2])
n_correct_SASSDR_clinical_def_10

SASSDR_clinical_def_10_totals = data.frame(test_p = sum(SASSDR_clinical_def_10$table[,1]), test_n = sum(SASSDR_clinical_def_10$table[,2]), criteria_p = sum(SASSDR_clinical_def_10$table[1,]), criteria_n = sum(SASSDR_clinical_def_10$table[2,]))
SASSDR_clinical_def_10_totals

cramer_v_SASSDR_clinical_def_10= CramerV(SASSDR_clinical_def_10$table, conf.level = .95)
cramer_v_SASSDR_clinical_def_10


```
Table 6 and 7 results
```{r}

### Table 6
SASSDR_clinical_def_9
n_correct_SASSDR_clinical_def_9
SASSDR_clinical_def_9_totals
cramer_v_SASSDR_clinical_def_9

### Table 7
SASSDR_clinical_def_10
n_correct_SASSDR_clinical_def_10
cramer_v_SASSDR_clinical_def_10
SASSDR_clinical_def_10_totals


```
Table 8 data prep and analysis
```{r}
clinical_sample_def_0_4 = subset(clinical_sample, DEF <= 4)
dim(clinical_sample_def_0_4)
SASSDR_clinical_def_0_4=  confusionMatrix(clinical_sample_def_0_4$SASSDR, clinical_sample_def_0_4$NODIAG, positive = "1")
SASSDR_clinical_def_0_4

n_correct_SASSDR_clinical_def_0_4=  sum(SASSDR_clinical_def_0_4$table[1,1], SASSDR_clinical_def_0_4$table[2,2])
n_correct_SASSDR_clinical_def_0_4

clinical_sample_def_5 = subset(clinical_sample, DEF <= 5)
dim(clinical_sample_def_5)
SASSDR_clinical_def_5=  confusionMatrix(clinical_sample_def_5$SASSDR, clinical_sample_def_5$NODIAG, positive = "1")
SASSDR_clinical_def_5

n_correct_SASSDR_clinical_def_5=  sum(SASSDR_clinical_def_5$table[1,1], SASSDR_clinical_def_5$table[2,2])
n_correct_SASSDR_clinical_def_5

clinical_sample_def_6 = subset(clinical_sample, DEF <= 6)
dim(clinical_sample_def_6)
SASSDR_clinical_def_6=  confusionMatrix(clinical_sample_def_6$SASSDR, clinical_sample_def_6$NODIAG, positive = "1")
SASSDR_clinical_def_6

n_correct_SASSDR_clinical_def_6=  sum(SASSDR_clinical_def_6$table[1,1], SASSDR_clinical_def_6$table[2,2])
n_correct_SASSDR_clinical_def_6


clinical_sample_def_7 = subset(clinical_sample, DEF <= 7)
dim(clinical_sample_def_7)
SASSDR_clinical_def_7=  confusionMatrix(clinical_sample_def_7$SASSDR, clinical_sample_def_7$NODIAG, positive = "1")
SASSDR_clinical_def_7

n_correct_SASSDR_clinical_def_7=  sum(SASSDR_clinical_def_7$table[1,1], SASSDR_clinical_def_7$table[2,2])
n_correct_SASSDR_clinical_def_7

clinical_sample_def_8 = subset(clinical_sample, DEF <= 8)
dim(clinical_sample_def_8)
SASSDR_clinical_def_8=  confusionMatrix(clinical_sample_def_8$SASSDR, clinical_sample_def_8$NODIAG, positive = "1")
SASSDR_clinical_def_8

n_correct_SASSDR_clinical_def_8=  sum(SASSDR_clinical_def_8$table[1,1], SASSDR_clinical_def_8$table[2,2])
n_correct_SASSDR_clinical_def_8

clinical_sample_def_9 = subset(clinical_sample, DEF <= 9)
dim(clinical_sample_def_9)
SASSDR_clinical_def_9=  confusionMatrix(clinical_sample_def_9$SASSDR, clinical_sample_def_9$NODIAG, positive = "1")
SASSDR_clinical_def_9

n_correct_SASSDR_clinical_def_9=  sum(SASSDR_clinical_def_9$table[1,1], SASSDR_clinical_def_9$table[2,2])
n_correct_SASSDR_clinical_def_9

clinical_sample_def_10_11 = subset(clinical_sample, DEF == 10 | DEF == 11)
dim(clinical_sample_def_10_11)

SASSDR_clinical_def_10_11=  confusionMatrix(clinical_sample_def_10_11$SASSDR, clinical_sample_def_10_11$NODIAG, positive = "1")
SASSDR_clinical_def_10_11

n_correct_SASSDR_clinical_def_10_11=  sum(SASSDR_clinical_def_10_11$table[1,1], SASSDR_clinical_def_10_11$table[2,2])
n_correct_SASSDR_clinical_def_10_11

```
Table 8 results 
```{r}
SASSDR_clinical_def_0_4
n_correct_SASSDR_clinical_def_0_4
SASSDR_clinical_def_5
n_correct_SASSDR_clinical_def_5
SASSDR_clinical_def_6
n_correct_SASSDR_clinical_def_6
SASSDR_clinical_def_7
n_correct_SASSDR_clinical_def_7
SASSDR_clinical_def_8
n_correct_SASSDR_clinical_def_8
SASSDR_clinical_def_9
n_correct_SASSDR_clinical_def_9
SASSDR_clinical_def_10_11
n_correct_SASSDR_clinical_def_10_11
```
Table 9 data analysis
```{r}
table_9_fva = ifelse(clinical_sample$FVA >= 7, 1, ifelse(clinical_sample$FVOD >= 12, 1,ifelse(clinical_sample$sym >= 6, 1,0)))
table_9_fva = as.factor(table_9_fva)
clinical_sample$NODIAG = as.factor(clinical_sample$NODIAG)

table_9_fva_results =  confusionMatrix(table_9_fva, clinical_sample$NODIAG, positive = "1")
table_9_fva_results

table_9_fva_n_correct=  sum(table_9_fva_results$table[1,1], table_9_fva_results$table[2,2])
table_9_fva_n_correct

table_9_fva_totals = data.frame(test_p = sum(table_9_fva_results$table[,1]), test_n = sum(table_9_fva_results$table[,2]), criteria_p = sum(table_9_fva_results$table[1,]), criteria_n = sum(table_9_fva_results$table[2,]))
table_9_fva_totals

table_9_fva_cramer_v= CramerV(table_9_fva_results$table, conf.level = .95)
table_9_fva_cramer_v
```
Table 9 Results
```{r}
table_9_fva_results
table_9_fva_n_correct
table_9_fva_totals
table_9_fva_cramer_v
```
Table 10 data analysis
Rule 1: FVA 12 or FVOD 12 or more

Rule 2: FRISK 5 or more

Rule 3: SYM 5 or more

Face Valid Classification: Rules 1 or 2 or 3

Rule 4: SAT 9 or more

Rule 5: OAT 4 and DEF 10 or more

Rule 6: OAT 7 or more and SAT 6 or more and DEF 2 or more and SAM 4 or more

Rule 7: FVA or FVOD 7 or more and FRISK or ATT or SAM 3 or more and OAT 5 or more

Rule 7: FVA or FVOD 5 or more and OAT 4 or more and DEF 7 or more

Rule 8: FVA or FVOD 5 or more and SAT 3 or more and DEF 4 or more and SAM 3 or more

 
```{r}
rule1 = as.factor(ifelse(clinical_sample$FVA >= 7, 1,ifelse(clinical_sample$FVOD >= 12,1,0)))
clinical_sample$NODIAG = as.factor(clinical_sample$NODIAG)
rule1_results =  confusionMatrix(rule1, clinical_sample$NODIAG, positive = "1")
rule1_results

rule1_test_p = sum(rule1_results$table[2,])
rule1_diag_p = sum(rule1_results$table[,2])
rule1_accurate = rule1_test_p /rule1_diag_p 
rule1_accurate

rule1_totals = data.frame(test_p = sum(rule1_results$table[,1]), test_n = sum(rule1_results$table[,2]), criteria_p = sum(rule1_results$table[1,]), criteria_n = sum(rule1_results$table[2,]))
rule1_totals


rule2 = as.factor(ifelse(clinical_sample$frisk >=5,1,0))

rule2_results =  confusionMatrix(rule2, clinical_sample$NODIAG, positive = "1")
rule2_results

rule2_test_p = sum(rule2_results$table[2,])
rule2_diag_p = sum(rule2_results$table[,2])
rule2_accurate = rule2_test_p /rule2_diag_p 
rule2_accurate

rule2_totals = data.frame(test_p = sum(rule2_results$table[,1]), test_n = sum(rule2_results$table[,2]), criteria_p = sum(rule2_results$table[1,]), criteria_n = sum(rule2_results$table[2,]))
rule2_totals


rule3 = as.factor(ifelse(clinical_sample$sym >=6,1,0))

rule3_results =  confusionMatrix(rule3, clinical_sample$NODIAG, positive = "1")
rule3_results

rule3_test_p = sum(rule3_results$table[2,])
rule3_diag_p = sum(rule3_results$table[,2])
rule3_accurate = rule3_test_p /rule3_diag_p 
rule3_accurate

rule3_totals = data.frame(test_p = sum(rule3_results$table[,1]), test_n = sum(rule3_results$table[,2]), criteria_p = sum(rule3_results$table[1,]), criteria_n = sum(rule3_results$table[2,]))
rule3_totals


rule4 = as.factor(ifelse(clinical_sample$sat >=11,1,0))

rule4_results =  confusionMatrix(rule4, clinical_sample$NODIAG, positive = "1")
rule4_results

rule4_test_p = sum(rule4_results$table[2,])
rule4_diag_p = sum(rule4_results$table[,2])
rule4_accurate = rule4_test_p /rule4_diag_p 
rule4_accurate

rule4_totals = data.frame(test_p = sum(rule4_results$table[,1]), test_n = sum(rule4_results$table[,2]), criteria_p = sum(rule4_results$table[1,]), criteria_n = sum(rule4_results$table[2,]))
rule4_totals



rule5 = as.factor(ifelse(clinical_sample$oat >= 8 &  clinical_sample$sat >=6 & clinical_sample$DEF >= 2 & clinical_sample$sam >= 4, 1,0))
rule5_results =  confusionMatrix(rule5, clinical_sample$NODIAG, positive = "1")
rule5_results

rule5_test_p = sum(rule5_results$table[2,])
rule5_diag_p = sum(rule5_results$table[,2])
rule5_accurate = rule5_test_p /rule5_diag_p 
rule5_accurate

rule5_totals = data.frame(test_p = sum(rule5_results$table[,1]), test_n = sum(rule5_results$table[,2]), criteria_p = sum(rule5_results$table[1,]), criteria_n = sum(rule5_results$table[2,]))
rule5_totals



rule6 = as.factor(ifelse(clinical_sample$FVOD >= 7 & clinical_sample$frisk >= 3 | clinical_sample$att >= 3 | clinical_sample$sam >= 3 & clinical_sample$oat >= 5,1,0))
rule6
rule6_results =  confusionMatrix(rule6, clinical_sample$NODIAG, positive = "1")
rule6_results

rule6_test_p = sum(rule6_results$table[2,])
rule6_diag_p = sum(rule6_results$table[,2])
rule6_accurate = rule6_test_p /rule6_diag_p 
rule6_accurate

rule6_totals = data.frame(test_p = sum(rule6_results$table[,1]), test_n = sum(rule6_results$table[,2]), criteria_p = sum(rule6_results$table[1,]), criteria_n = sum(rule6_results$table[2,]))
rule6_totals



rule7 = as.factor(ifelse(clinical_sample$FVA >= 5 | clinical_sample$FVOD >= 5 & clinical_sample$oat >= 4 & clinical_sample$DEF >= 7,1,0))
rule7
rule7_results =  confusionMatrix(rule7, clinical_sample$NODIAG, positive = "1")
rule7_results

rule7_test_p = sum(rule7_results$table[2,])
rule7_diag_p = sum(rule7_results$table[,2])
rule7_accurate = rule7_test_p /rule7_diag_p 
rule7_accurate

rule7_totals = data.frame(test_p = sum(rule7_results$table[,1]), test_n = sum(rule7_results$table[,2]), criteria_p = sum(rule7_results$table[1,]), criteria_n = sum(rule7_results$table[2,]))
rule7_totals



rule8 = as.factor(ifelse(clinical_sample$FVA >= 5 | clinical_sample$FVOD >= 5 & clinical_sample$oat >= 4 & clinical_sample$DEF >= 4 & clinical_sample$sam >=3,1,0))
rule8
rule8_results =  confusionMatrix(rule8, clinical_sample$NODIAG, positive = "1")
rule8_results


rule8_test_p = sum(rule8_results$table[2,])
rule8_diag_p = sum(rule8_results$table[,2])
rule8_accurate = rule8_test_p /rule8_diag_p 
rule8_accurate

rule8_totals = data.frame(test_p = sum(rule8_results$table[,1]), test_n = sum(rule8_results$table[,2]), criteria_p = sum(rule8_results$table[1,]), criteria_n = sum(rule8_results$table[2,]))
rule8_totals



```
Table 10 results
```{r}
rule1_test_p
rule1_diag_p
rule1_accurate

rule2_test_p
rule2_diag_p
rule2_accurate

rule3_test_p
rule3_diag_p
rule3_accurate

rule4_test_p
rule4_diag_p
rule4_accurate

rule5_test_p
rule5_diag_p
rule5_accurate

rule6_test_p
rule6_diag_p
rule6_accurate

rule7_test_p
rule7_diag_p
rule7_accurate

rule8_test_p
rule8_diag_p
rule8_accurate


```
Try second rules
Proposed alt Set Decision Rule
Rule 1: FVA 7 or FVOD 12 or more
Rule 2: FRISK 6 or more
Rule 3: SYM 5 or more
Face Valid Classification: Rules 1 or 2 or 3
Rule 4: SAT 7 or more
Rule 5: OAT 7 or more and SAT 6 or more and DEF 2 or more and SAM 4 or more
Rule 6: FVOD 7 or more and FRISK or ATT or SAM 3 or more and OAT 5 or more
Rule 7: FVA or FVOD 5 or more and OAT 4 or more and DEF 7 or more
Rule 8: FVA or FVOD 5 or more and SAT 3 or more and DEF 4 or more and SAM 3 or more
```{r}
rule1 = as.factor(ifelse(clinical_sample$FVA >= 7, 1,ifelse(clinical_sample$FVOD >= 12,1,0)))
clinical_sample$NODIAG = as.factor(clinical_sample$NODIAG)
rule1_results =  confusionMatrix(rule1, clinical_sample$NODIAG, positive = "1")
rule1_results

rule1_test_p = sum(rule1_results$table[2,])
rule1_diag_p = sum(rule1_results$table[,2])
rule1_accurate = rule1_test_p /rule1_diag_p 
rule1_accurate

rule1_totals = data.frame(test_p = sum(rule1_results$table[,1]), test_n = sum(rule1_results$table[,2]), criteria_p = sum(rule1_results$table[1,]), criteria_n = sum(rule1_results$table[2,]))
rule1_totals


rule2 = as.factor(ifelse(clinical_sample$frisk >=6,1,0))

rule2_results =  confusionMatrix(rule2, clinical_sample$NODIAG, positive = "1")
rule2_results

rule2_test_p = sum(rule2_results$table[2,])
rule2_diag_p = sum(rule2_results$table[,2])
rule2_accurate = rule2_test_p /rule2_diag_p 
rule2_accurate

rule2_totals = data.frame(test_p = sum(rule2_results$table[,1]), test_n = sum(rule2_results$table[,2]), criteria_p = sum(rule2_results$table[1,]), criteria_n = sum(rule2_results$table[2,]))
rule2_totals


rule3 = as.factor(ifelse(clinical_sample$sym >=5,1,0))

rule3_results =  confusionMatrix(rule3, clinical_sample$NODIAG, positive = "1")
rule3_results

rule3_test_p = sum(rule3_results$table[2,])
rule3_diag_p = sum(rule3_results$table[,2])
rule3_accurate = rule3_test_p /rule3_diag_p 
rule3_accurate

rule3_totals = data.frame(test_p = sum(rule3_results$table[,1]), test_n = sum(rule3_results$table[,2]), criteria_p = sum(rule3_results$table[1,]), criteria_n = sum(rule3_results$table[2,]))
rule3_totals


rule4 = as.factor(ifelse(clinical_sample$sat >=7,1,0))

rule4_results =  confusionMatrix(rule4, clinical_sample$NODIAG, positive = "1")
rule4_results

rule4_test_p = sum(rule4_results$table[2,])
rule4_diag_p = sum(rule4_results$table[,2])
rule4_accurate = rule4_test_p /rule4_diag_p 
rule4_accurate

rule4_totals = data.frame(test_p = sum(rule4_results$table[,1]), test_n = sum(rule4_results$table[,2]), criteria_p = sum(rule4_results$table[1,]), criteria_n = sum(rule4_results$table[2,]))
rule4_totals


#Rule 5: OAT 7 or more and SAT 6 or more and DEF 2 or more and SAM 4 or more
rule5 = as.factor(ifelse(clinical_sample$oat >= 7 &  clinical_sample$sat >=6 & clinical_sample$DEF >= 2 & clinical_sample$sam >= 4, 1,0))
rule5_results =  confusionMatrix(rule5, clinical_sample$NODIAG, positive = "1")
rule5_results

rule5_test_p = sum(rule5_results$table[2,])
rule5_diag_p = sum(rule5_results$table[,2])
rule5_accurate = rule5_test_p /rule5_diag_p 
rule5_accurate

rule5_totals = data.frame(test_p = sum(rule5_results$table[,1]), test_n = sum(rule5_results$table[,2]), criteria_p = sum(rule5_results$table[1,]), criteria_n = sum(rule5_results$table[2,]))
rule5_totals

#Rule 6: FVOD 7 or more and FRISK or ATT or SAM 3 or more and OAT 5 or more

rule6 = as.factor(ifelse(clinical_sample$FVOD >= 7 & clinical_sample$frisk >= 3 | clinical_sample$att >= 3 | clinical_sample$sam >= 3 & clinical_sample$oat >= 5,1,0))
rule6
rule6_results =  confusionMatrix(rule6, clinical_sample$NODIAG, positive = "1")
rule6_results

rule6_test_p = sum(rule6_results$table[2,])
rule6_diag_p = sum(rule6_results$table[,2])
rule6_accurate = rule6_test_p /rule6_diag_p 
rule6_accurate

rule6_totals = data.frame(test_p = sum(rule6_results$table[,1]), test_n = sum(rule6_results$table[,2]), criteria_p = sum(rule6_results$table[1,]), criteria_n = sum(rule6_results$table[2,]))
rule6_totals

#Rule 7: FVA or FVOD 5 or more and OAT 4 or more and DEF 7 or more


rule7 = as.factor(ifelse(clinical_sample$FVA >= 5 | clinical_sample$FVOD >= 5 & clinical_sample$oat >= 4 & clinical_sample$DEF >= 7,1,0))
rule7
rule7_results =  confusionMatrix(rule7, clinical_sample$NODIAG, positive = "1")
rule7_results

rule7_test_p = sum(rule7_results$table[2,])
rule7_diag_p = sum(rule7_results$table[,2])
rule7_accurate = rule7_test_p /rule7_diag_p 
rule7_accurate

rule7_totals = data.frame(test_p = sum(rule7_results$table[,1]), test_n = sum(rule7_results$table[,2]), criteria_p = sum(rule7_results$table[1,]), criteria_n = sum(rule7_results$table[2,]))
rule7_totals

#Rule 8: FVA or FVOD 5 or more and SAT 3 or more and DEF 4 or more and SAM 3 or more


rule8 = as.factor(ifelse(clinical_sample$FVA >= 5 | clinical_sample$FVOD >= 5 & clinical_sample$sat >= 3 & clinical_sample$DEF >= 4 & clinical_sample$sam >=3,1,0))
rule8_results =  confusionMatrix(rule8, clinical_sample$NODIAG, positive = "1")
rule8_results


rule8_test_p = sum(rule8_results$table[2,])
rule8_diag_p = sum(rule8_results$table[,2])
rule8_accurate = rule8_test_p /rule8_diag_p 
rule8_accurate

rule8_totals = data.frame(test_p = sum(rule8_results$table[,1]), test_n = sum(rule8_results$table[,2]), criteria_p = sum(rule8_results$table[1,]), criteria_n = sum(rule8_results$table[2,]))
rule8_totals
```



Table 11
```{r}
accurate_var = ifelse(clinical_sample$SASSDR == clinical_sample$NODIAG,1,0)
accurate_var = as.factor(accurate_var)
cramer_v_table11 = CramerV(clinical_sample$CLIENTSETTING, accurate_var,  conf.level = .95)
cramer_v_table11

describe.factor(clinical_sample$CLIENTSETTING)
percent_table11 = tapply(accurate_var,clinical_sample$CLIENTSETTING,function(x){prop.table(table(x))})

n_table11 =  tapply(accurate_var,clinical_sample$CLIENTSETTING,function(x){table(x)})

```
Table 11 results
```{r}
percent_table11
n_table11

```
Table 12 through 17 data cleaning
Criminal justice program = 1
Social services = 2
Medical pan clinic = 3
Medical facility other = 4
6 = Substance use treatment
7 = other
```{r}

#### Criminal justice programs
clinical_sample_crime = subset(clinical_sample, CLIENTSETTING == 1)
clinical_sample_crime_total_n = dim(clinical_sample_crime)[1]
SASSDR_clinical_crime=  confusionMatrix(as.factor(clinical_sample_crime$SASSDR), as.factor(clinical_sample_crime$NODIAG), positive = "1")
SASSDR_clinical_crime

n_correct_SASSDR_clinical_crime=  sum(SASSDR_clinical_crime$table[1,1], SASSDR_clinical_crime$table[2,2])
n_correct_SASSDR_clinical_crime

SASSDR_clinical_crime_totals = data.frame(test_p = sum(SASSDR_clinical_crime$table[,1]), test_n = sum(SASSDR_clinical_crime$table[,2]), criteria_p = sum(SASSDR_clinical_crime$table[1,]), criteria_n = sum(SASSDR_clinical_crime$table[2,]))
SASSDR_clinical_crime_totals



#### Social services programs
clinical_sample_social_services = subset(clinical_sample, CLIENTSETTING == 2)

clinical_sample_social_services_total_n = dim(clinical_sample_social_services)[1]

SASSDR_clinical_social_services=  confusionMatrix(as.factor(clinical_sample_social_services$SASSDR), as.factor(clinical_sample_social_services$NODIAG), positive = "1")
SASSDR_clinical_social_services

n_correct_SASSDR_clinical_social_services=  sum(SASSDR_clinical_social_services$table[1,1], SASSDR_clinical_social_services$table[2,2])
n_correct_SASSDR_clinical_social_services

SASSDR_clinical_social_services_totals = data.frame(test_p = sum(SASSDR_clinical_social_services$table[,1]), test_n = sum(SASSDR_clinical_social_services$table[,2]), criteria_p = sum(SASSDR_clinical_social_services$table[1,]), criteria_n = sum(SASSDR_clinical_social_services$table[2,]))
SASSDR_clinical_social_services_totals


###
#### medical pain clinics
clinical_sample_pain_clinic = subset(clinical_sample, CLIENTSETTING == 3)
dim(clinical_sample_pain_clinic)[1]
### None

## other pain clinic
clinical_sample_other_pain_clinic = subset(clinical_sample, CLIENTSETTING == 4)
dim(clinical_sample_other_pain_clinic)


## substance use facilities
#### substance use clinics
clinical_sample_substance_use = subset(clinical_sample, CLIENTSETTING == 6)
clinical_sample_substance_use_total_n =  dim(clinical_sample_substance_use)[1]
SASSDR_clinical_substance_use=  confusionMatrix(as.factor(clinical_sample_substance_use$SASSDR), as.factor(clinical_sample_substance_use$NODIAG), positive = "1")


n_correct_SASSDR_clinical_substance_use=  sum(SASSDR_clinical_substance_use$table[1,1], SASSDR_clinical_substance_use$table[2,2])
n_correct_SASSDR_clinical_substance_use

SASSDR_clinical_substance_use_totals = data.frame(test_p = sum(SASSDR_clinical_substance_use$table[,1]), test_n = sum(SASSDR_clinical_substance_use$table[,2]), criteria_p = sum(SASSDR_clinical_substance_use$table[1,]), criteria_n = sum(SASSDR_clinical_substance_use$table[2,]))
SASSDR_clinical_substance_use_totals

#### other
clinical_sample_other = subset(clinical_sample, CLIENTSETTING == 7)
clinical_sample_other_total_n =  dim(clinical_sample_other)[1]
SASSDR_clinical_other=  confusionMatrix(as.factor(clinical_sample_other$SASSDR), as.factor(clinical_sample_other$NODIAG), positive = "1")
SASSDR_clinical_other

n_correct_SASSDR_clinical_other=  sum(SASSDR_clinical_other$table[1,1], SASSDR_clinical_other$table[2,2])
n_correct_SASSDR_clinical_other

SASSDR_clinical_other_totals = data.frame(test_p = sum(SASSDR_clinical_other$table[,1]), test_n = sum(SASSDR_clinical_other$table[,2]), criteria_p = sum(SASSDR_clinical_other$table[1,]), criteria_n = sum(SASSDR_clinical_other$table[2,]))
SASSDR_clinical_other_totals


```
Tables 12 through 17 results
```{r}
SASSDR_clinical_crime
clinical_sample_crime_total_n
n_correct_SASSDR_clinical_crime
SASSDR_clinical_crime_totals

SASSDR_clinical_social_services
clinical_sample_social_services_total_n
n_correct_SASSDR_clinical_social_services
SASSDR_clinical_social_services_totals

SASSDR_clinical_substance_use
clinical_sample_substance_use_total_n
n_correct_SASSDR_clinical_substance_use
SASSDR_clinical_substance_use_totals

SASSDR_clinical_other
clinical_sample_other_total_n
n_correct_SASSDR_clinical_other
SASSDR_clinical_other_totals
```



Go back and clean up table 1 answers from Scarlett


