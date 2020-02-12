---
title: "SASSI 2-10-20 Contract"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Look for errors in each data set
```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/SASSI/Data/2-10-20_contract")
clinical_sample = read.csv("clinical_sample.csv", header = TRUE)
cross_validation_sample_sample = read.csv("cross_validation_sample_sample.csv", header = TRUE)
development_sample = read.csv("development_sample.csv", header = TRUE)
normative_sample = read.csv("normative_sample.csv", header= TRUE)
stability_sample = read.csv("stability_sample.csv", header = TRUE)
```
Review for errors
```{r}
#describe(clinical_sample)
#describe(stability_sample)
describe(development_sample)
describe(cross_validation_sample_sample)
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
cross_development_demos = as.list(cross_development_demos)
### Need to collapse  ETHN, LIVING_SIT, MANDATED, REFERRAL
describe.factor(cross_development_demos$ETHN)

p_value_develop_cross_val_results = list()

for(i in 1:length(cross_development_demos)){
  p_value_develop_cross_val_results[[i]] = fisher.test(cross_development_demos[[i]], ID)
  p_value_develop_cross_val_results[[i]] = p_value_develop_cross_val_results[[i]]$p.value
}
p_value_develop_cross_val_results = data.frame(p_value_develop_cross_val_results)
names(p_value_develop_cross_val_results) = names(cross_development_demos)
p_value_develop_cross_val_results

describe.factor(cross_development_demos$LIVING_SIT)
library(descr)

compmeans(cross_development_demos$ETHN, ID)
```

