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

# Data exploration

```{r packages}
library(tidyverse)
library(ggplot2)
library(knitr)
library(MASS)
```

## Data set overview

-   The data comes from a study investigating the role of induced (and spontaneous) abortions in the etiology of secondary sterility.

-   Obstetric and gynecologic histories were obtained from 100 women with secondary infertility admitted to the First Department of Obstetrics and Gynecology of the University of Athens Medical School and to the Division of Fertility and Sterility of that Department.

-   For every patient, researchers tried to find two healthy control subjects from the same hospital with matching for age, parity, and level of education.

-   Two control subjects each were found for 83 of the index patients.

-   Data frame with 248 records for `education/ age/ parity/ induced/ case/ spontaneous/ stratum/ pooled.stratum`.

```{r data exploration}
head(infert)
summary(infert)
str(infert)
```

## Data visualization

### Data grappling

```{r age_bins}
infert2 <- infert %>% 
  mutate(
    # Create categories
    age_group = dplyr::case_when(
      age <= 25            ~ "0-25",
      age > 25 & age <= 30 ~ "25-30",
      age > 30 & age <= 35 ~ "30-35",
      age > 35 & age <= 40 ~ "35-40",
      age > 40             ~ "> 40"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("0-25", "25-30","30-35","35-40", "> 40")
    )
  )
infert2 <- na.omit(infert2)
head(infert2)
```

```         
```

```{r strip_chart}
# Create strip chart (works better with bins)
stripchart(spontaneous ~ age_group, data=infert2)
stripchart(induced ~ age_group, data=infert2)
```

*Observations*

-   We can say that age has an effect on the amount `spontaneous` and `induced` abortions.

### Abortion Case Proportions

```{r}
infert2 %>%
 group_by(age_group) %>%
 summarise(spontaneous_cases = sum(spontaneous), 
 cases = sum(case),
 percentage = 100 * cases / (cases+spontaneous_cases)) %>%
 ggplot(., aes(x = age_group, y = percentage, fill = age_group)) +
 geom_bar(stat = "identity", position = "dodge") +
 labs(title = "Proportion of Spontaneous Abortion Cases over Age Groups", subtitle = "Data Source:
`infert2`", x = 'Ages', y = "% of Spontaneous Cases") +
 theme_minimal() +
 theme(legend.position = "none") +
 geom_text(aes(label = paste(format(percentage,digits=1), "%")), size=4.5, position =
position_stack(vjust = 0.5))

```

```{r}
infert2 %>%
 group_by(age_group) %>%
 summarise(induced_cases = sum(induced), 
 cases = sum(case),
 percentage = 100 * cases / (cases+induced_cases)) %>%
 ggplot(., aes(x = age_group, y = percentage, fill = age_group)) +
 geom_bar(stat = "identity", position = "dodge") +
 labs(title = "Proportion of Induced Abortion Cases over Age Groups", subtitle = "Data Source:
`infert2`", x = 'Ages', y = "% of Induced Cases") +
 theme_minimal() +
 theme(legend.position = "none") +
 geom_text(aes(label = paste(format(percentage,digits=1), "%")), size=4.5, position =
position_stack(vjust = 0.5))

```

```{r}
infert2 %>%
 group_by(education) %>%
 summarise(induced_cases = sum(induced), 
 cases = sum(case),
 percentage = 100 * cases / (cases+induced_cases)) %>%
 ggplot(., aes(x = education, y = percentage, fill = education)) +
 geom_bar(stat = "identity", position = "dodge") +
 labs(title = "Proportion of Induced Abortion Cases vs. Education", subtitle = "Data Source:
`infert2`", x = 'Education', y = "% of Induceds Cases") +
 theme_minimal() +
 theme(legend.position = "none") +
 geom_text(aes(label = paste(format(percentage,digits=1), "%")), size=4.5, position =
position_stack(vjust = 0.5))
```

```{r}
infert2 %>%
 group_by(education) %>%
 summarise(spontaneous_cases = sum(spontaneous), 
 cases = sum(case),
 percentage = 100 * cases / (cases+spontaneous_cases)) %>%
 ggplot(., aes(x = education, y = percentage, fill = education)) +
 geom_bar(stat = "identity", position = "dodge") +
 labs(title = "Proportion of Spontaneous Abortion Cases vs. Education", subtitle = "Data Source:
`infert2`", x = 'Education', y = "% of Spontaneous Cases") +
 theme_minimal() +
 theme(legend.position = "none") +
 geom_text(aes(label = paste(format(percentage,digits=1), "%")), size=4.5, position =
position_stack(vjust = 0.5))
```

### Abortion Case Distribution

```{r}
infert2 %>% 
  group_by(age_group, induced) %>%
  summarize(total_cases = sum(case)) %>%
  group_by(age_group) %>%
  mutate(percentage = 100 * total_cases / sum(total_cases)) %>%
  filter(percentage != "NaN" & percentage != 0) %>%
  ggplot(., aes(x = age_group, y = percentage, fill = induced)) +
  geom_col(stat = "identity", position = "fill") +
  theme_minimal() +
  geom_text(aes(label = paste(format(percentage,digits=1), "%")), size=4, position = "fill", hjust = 0.5, vjust = 1.1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Stacked Bar Chart of Case Distribution of Induced Abortions by Age Groups", subtitle = "Data Source: `infert2`", x = "Age Groups", y = "% of Abortion Cases", fill = "Induced")
```

```{r}
infert2 %>% 
  group_by(age_group, spontaneous) %>%
  summarize(total_cases = sum(case)) %>%
  group_by(age_group) %>%
  mutate(percentage = 100 * total_cases / sum(total_cases)) %>%
  filter(percentage != "NaN" & percentage != 0) %>%
  ggplot(., aes(x = age_group, y = percentage, fill = spontaneous)) +
  geom_col(stat = "identity", position = "fill") +
  theme_minimal() +
  geom_text(aes(label = paste(format(percentage,digits=1), "%")), size=4, position = "fill", hjust = 0.5, vjust = 1.1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Stacked Bar Chart of Case Distribution of Spontaneous Abortions by Age Groups", subtitle = "Data Source: `infert2`", x = "Age Groups", y = "% of Abortion Cases", fill = "Spontaneous")
```

```{r}
infert2 %>% 
  group_by(education, induced) %>%
  summarize(total_cases = sum(case)) %>%
  group_by(education) %>%
  mutate(percentage = 100 * total_cases / sum(total_cases)) %>%
  filter(percentage != "NaN" & percentage != 0) %>%
  ggplot(., aes(x = education, y = percentage, fill = induced)) +
  geom_col(stat = "identity", position = "fill") +
  theme_minimal() +
  geom_text(aes(label = paste(format(percentage,digits=1), "%")), size=4, position = "fill", hjust = 0.5, vjust = 1.1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Stacked Bar Chart of Case Distribution of Induced Abortions by Education", subtitle = "Data Source: `infert2`", x = "Education", y = "% of Abortion Cases", fill = "Induced")
```

```{r}
infert2 %>% 
  group_by(education, spontaneous) %>%
  summarize(total_cases = sum(case)) %>%
  group_by(education) %>%
  mutate(percentage = 100 * total_cases / sum(total_cases)) %>%
  filter(percentage != "NaN" & percentage != 0) %>%
  ggplot(., aes(x = education, y = percentage, fill = spontaneous)) +
  geom_col(stat = "identity", position = "fill") +
  theme_minimal() +
  geom_text(aes(label = paste(format(percentage,digits=1), "%")), size=4, position = "fill", hjust = 0.5, vjust = 1.1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Stacked Bar Chart of Case Distribution of Spontaneous Abortions by Education", subtitle = "Data Source: `infert2`", x = "Education", y = "% of Abortion Cases", fill = "Spontaneous")
```

### Heat-map if Abortion Case Distribution

```{r}
infert2 %>% 
  group_by(age_group) %>%
  mutate(total_cases = sum(case), 
            total_stratum = sum(stratum),
            percentage = 100 * total_cases / (total_cases+total_stratum)) %>%
  ggplot(., aes(x = induced, y = spontaneous, fill = percentage)) +
  geom_tile() +
  facet_wrap(~age_group) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_gradient2(low="white", high="red3", guide="colorbar") +
  labs(title = "Heatmap of Abortion Cases", x = "Induced", subtitle = "Data Source: `infert2`", y = "Spontaneous", fill = "Cases (%)")

```

# Data modeling

Data models are used to describe the relationship between variables.

## Linear models

Regression analysis is an important statistical method for the analysis of medical data. It enables the identification and characterization of relationships among multiple factors. It also enables the identification of prognostically relevant risk factors and the calculation of risk scores for individual prognostication ([NIH, 2010](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2992018/)).

### ANOVA test (change to show the affect of everything on spontaneous)

```{r}
infert2$percentage_s <- infert2$case / (infert2$spontaneous+infert2$case) #create a spontaneous percentage column
infert2

model <- lm(percentage_s ~ age_group + education + induced, data = infert2) #Linear model is created in order to apply anova test
anova(model)
```

According to the results of the ANOVA test, it was observed that `age_group` , and amount of `induced` abortions had the greatest effect on the amount of spontaneous abortions.

### Akaike's Information Criterion

The Akaike's information criterion model (AIC), achieves parsimony via a fit-complexity trade-off and is used as a relative measure to compare and rank several competing models fit to the same data, where the model with the lowest AIC is considered the best ([NIH, 2023](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10523071/)). This script will help use decide if we should remove `education` from the model.

```{r}
AIC(glm(percentage_s ~  age_group + education + induced, data = infert2, family = binomial(link = "logit"))) #with all
AIC(glm(percentage_s ~  education + induced, data = infert2, family = binomial(link = "logit"))) 
AIC(glm(percentage_s ~  age_group + induced, data = infert2, family = binomial(link = "logit"))) #best model
AIC(glm(percentage_s ~  age_group + education, data = infert2, family = binomial(link = "logit")))
AIC(glm(percentage_s ~ age_group, data = infert2, family = binomial(link = "logit"))) #with age_group
AIC(glm(percentage_s ~  education, data = infert2, family = binomial(link = "logit"))) #with education
AIC(glm(percentage_s ~  induced, data = infert2, family = binomial(link = "logit"))) #with induced 
```

The third model containing `age_group` and `induced` data produced the lowest AIC.

### Logistic Regression

Logistic regression analysis is a statistical technique to evaluate the relationship between various predictor variables (either categorical or continuous) and an outcome which is binary (dichotomous) ([NIH, 2017](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5543767/)). Logistic regression is an important research tool used for disease prediction.

```{r}
model <- glm(percentage_s ~ age_group + induced, data = infert2, family = binomial(link = "logit")) #Logistic regression
summary(model)
```

-   The `induced` p-value shows that it makes a significant difference in this model.
-   An ideal model would include multiple p-values of less than 5 percent ([check out this example](https://pjournal.github.io/boun01-canaytore/assignment3_esoph#Akaike%E2%80%99s_Information_Criterion:~:text=to%20our%20model.-,Logistic%20Regression,-model%20%3C%2D%20glm))

# Test the model

## Predicted Spontaneous Abortion Risk Percentages among Induced, Education, and Age Groups

After creating the model we can visualize the predicted spontaneous abortion cases.

### Age Group

```{r}
predict_spontaneous_percentages <- data.frame()
for (i in 1:5) {
  for (j in 1:4) {
    predict_spontaneous_percentages[i,j] <- plogis(predict(model, data.frame(age_group = unique(infert2$age_group)[i], induced = unique(infert2$induced)[j]))) #Prediction
  }
}
pivot_longer(predict_spontaneous_percentages, cols=everything(), names_to = "Induced_Abortion", values_to = "Spontaneous_Percentage") %>%
  add_column(.before="Induced_Abortion", Age_Group = c(rep("0-25",4), rep("25-30",4),rep("30-35",4),rep("35-40",4),rep("40<",4))) %>%
  ggplot(.,aes(x=Age_Group, y=Spontaneous_Percentage, fill = Induced_Abortion)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_discrete(name = "Induced_Abortion", labels = c("0", "1", "2")) +
  labs(title = "Predicted Spontaneous Abortion Risk Percentages among Induced and Age Groups", subtitle = "Prediction Based on Logistic Regression", x = "Age Groups", y = "Predicted Percentage")

```

-   These predictions represent the estimated spontaneous abortion percentages by age group and induced abortions.
-   According to the predictions, patients with one induced abortion and in the 25-30yr age group have the highest percentage of spontaneous abortions.
-   According to the predictions, patients with more than one induced abortions and in the 30-35yr age group have the lowest percentage of spontaneous abortion.
-   The risk levels between the 0-25yr, 30-35yr, and 40\< are very similar.

```{r}
predict_spontaneous_percentages <- data.frame(row.names = c("0-25 years","25-30 years","30-35 years","35-40 years","40< years"))
for (i in 1:5) {
  for (j in 1:4) {
    predict_spontaneous_percentages[i,j] <- paste(round(100*(plogis(predict(model, data.frame(age_group = unique(infert2$age_group)[i], induced = unique(infert2$induced)[j])))),0),"%",sep="")
  }
}
colnames(predict_spontaneous_percentages) <- c("0","1","2")
kable(predict_spontaneous_percentages, caption = "Predicted Spontaneous Abortion Percentages corresp. to Age and Induced Abortion Groups")
```

-   This table shows the percentage values.

### Leave-one-out Cross Validation

Cross-validation is a re-sampling method that uses different portions of the data to test and train a model on different iterations. It is mainly used in settings where the goal is prediction, and one wants to estimate how accurately a predictive model will perform in practice ([Cross-validation](https://en.wikipedia.org/wiki/Cross-validation_(statistics))).

For more information on cross-validation:

[Cross-validation under separate sampling: strong bias and how to correct it](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4296143/){.uri}

[Impact of the Choice of Cross-Validation Techniques on the Results of Machine Learning-Based Diagnostic Applications](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8369053/){.uri}

```{r}
pred_length <- nrow(infert2)
fit_glm_error <- c()
fit_glm_sq_error <- c()
for(i in 1:pred_length){
  fit_glm <- glm(percentage_s ~ age_group + induced, family = binomial(link = "logit"), data = infert2[-i,]) #Leave-one-out Cross Validation
  fit_glm_pred <- (predict(fit_glm, infert2[i,]))^2
  fit_glm_error[i] <- infert2$percentage_s[i] - fit_glm_pred
  fit_glm_sq_error[i] = (infert2$percentage_s[i] - fit_glm_pred)^2
}
hist(fit_glm_error, breaks = 50, xlim = range(-50,50), title = "Histogram of Errors", xlab = "Fitted GLM Errors")
```

This histogram of errors shows that there are some errors that are significantly greater than 30. However, the errors are close to zero so we established a good model.

```{r}
fit_glm_sq_error <- na.omit(fit_glm_sq_error) #remove NaN
rmse_fit_glm <- sqrt(mean(fit_glm_sq_error))
rmse_fit_glm #Root Mean Square Error
```

The root mean square error (RMSE) is a metric that tells us how far apart our predicted values are from our observed values in a regression analysis, on average. The larger the RMSE, the larger the difference between the predicted and observed values, which means the worse a regression model fits the data. Conversely, the smaller the RMSE, the better a model is able to fit the data ([How to Calculate RMSE in R](https://www.statology.org/how-to-calculate-rmse-in-r/)).

# Conclusion

In conclusion, we worked with data grappling, exploratory data analysis, and linear modeling, to build and test a predictive model .

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
