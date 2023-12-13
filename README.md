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
-   Visualizing the relationship between spontaneous abortion case occurrence and age / education / induced abortions.

-   Identifying the groups at risk via useful analyzes and graphs.

-   Building a well-developed generalized linear model.

-   Predicting spontaneous abortion percentages among the groups.

-   Testing the robustness of the model via leave-one-out cross validation.

  Refer to the pdf document for the full mardown!

# Resources

[Esophageal Cancer Project](https://pjournal.github.io/boun01-canaytore/assignment3_esoph)

[Induced abortion and secondary infertility study](https://obgyn.onlinelibrary.wiley.com/doi/10.1111/j.1471-0528.1976.tb00904.x)

[Infertility data](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/infert.html)

[Linear Regression Analysis](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2992018/)

[Practical advice on variable selection and reporting using Akaike information criterion](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10523071/)

[Common pitfalls in statistical analysis: Logistic regression](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5543767/)

[Cross-validation](https://en.wikipedia.org/wiki/Cross-validation_(statistics))

[Cross-validation under separate sampling: strong bias and how to correct it](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4296143/){.uri}

[Impact of the Choice of Cross-Validation Techniques on the Results of Machine Learning-Based Diagnostic Applications](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8369053/){.uri}

[How to Calculate RMSE in R](https://www.statology.org/how-to-calculate-rmse-in-r/)
