---
title: 'Data Exploration and Predictive Modeling: Spontaneous Abortion Prediction'
author: "Peace Maddox"
output:
  html_document:
    df_print: paged
---

# Introduction

This notebook is based on this ([Esophageal Cancer](https://pjournal.github.io/boun01-canaytore/assignment3_esoph)) project. These techniques are important for contextualizing data and creating predictions based on modeling and visualizations. The data set used for this project is from the ([Induced abortion and secondary infertility](https://obgyn.onlinelibrary.wiley.com/doi/10.1111/j.1471-0528.1976.tb00904.x)) study.

## Objective

-   Exploring the data set ([infert](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/infert.html)) which comes in the "R" data sets package.

-   Here is a data usage example below:

```{r echo=FALSE}
require(stats)
model1 <- glm(case ~ spontaneous+induced, data = infert, family = binomial())
summary(model1)
## adjusted for other potential confounders:
summary(model2 <- glm(case ~ age+parity+education+spontaneous+induced,
                     data = infert, family = binomial()))
## Really should be analysed by conditional logistic regression
## which is in the survival package
if(require(survival)){
  model3 <- clogit(case ~ spontaneous+induced+strata(stratum), data = infert)
  print(summary(model3))
  detach()  # survival (conflicts)
}
```
