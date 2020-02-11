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
cross_validation = read.csv("cross_validation.csv", header = TRUE)
development_sample = read.csv("development_sample.csv", header = TRUE)
normative_sample = read.csv("normative_sample.csv", header= TRUE)
stability_sample = read.csv("stability_sample.csv", header = TRUE)
```
Review for errors
```{r}
#describe(clinical_sample)
#describe(stability_sample)
describe(development_sample)
describe(cross_validation)
describe(normative_sample)
```

