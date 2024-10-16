---
title: "Homework"
output:
  word_document: default
  html_document: default
date: "2024-10-12"
---

```{r setup,include=FALSE}
knitr::opts_chunk$set(echo=TRUE)
Sys.setenv(LANG="en")
```

## Part 3:Forward selection algorithm

### a


```{r message=FALSE, warning=FALSE}
library(ISLR2)
library(leaps)
data(Hitters)
attach(Hitters)
Hitters <- na.omit(Hitters)
```

### b


```{r message=FALSE, warning=FALSE}
null <- lm(Salary~1)
summary(null)
```

The estimate for the intercept is 535.93,which means that,on average,the salary is predicted to be 535.93 when using this model.The Std. Error(27.82) indicates the standard error of this estimate.The p-value (Pr<2e-16) show that this intercept is highly statistically significant.

### c


```{r}
r.squared <- numeric()
variables <- colnames(Hitters)[colnames(Hitters)!="Salary"]
for(var in variables){
  model <- lm(Salary~Hitters[[var]],data=Hitters)
  r.squared[var] <- summary(model)$r.squared
}
best <- names(which.max(r.squared))
best
max(r.squared)
```
The variable CRBI is the one that increases the $R^2$ value the most when added to the null model,with an  $R^2$ value of 0.3215.This means that 32.15% of the variance in salary can be explained by the CRBI variable alone.

### d


```{r}
stepwise <- regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
(sum.step <- summary(stepwise))
which.max(sum.step$rsq)
```


The model with 19 predictors is considered the "best" in terms of $R^2$,meaning it explains the highest proportion of variance in the salary.

### e


```{r}
plot(stepwise,scale="r2",main="Forward Stepwise Selection")
```



### f


```{r}
model.list <- list()
bic.values <- numeric()
r.squared.list <- numeric()
selected.vars <- c()
for (step in 1:length(variables)){
  best.rsquared <- 0
  best.var <- NULL
  best.bic <- Inf
  for(var in setdiff(variables,selected.vars)){
    formula <- as.formula(paste("Salary ~",paste(c(selected.vars,var),collapse=" + ")))
    model <- lm(formula,data=Hitters)
    r.squared <- summary(model)$r.squared
    bic.value <- BIC(model)
    if(r.squared>best.rsquared) {
      best.rsquared <- r.squared
      best.var <- var
      best.bic <- bic.value
    }
  }
  selected.vars <- c(selected.vars,best.var)
  model.list[[step]] <- selected.vars
  r.squared.list[step] <- best.rsquared
  bic.values[step] <- best.bic
}
for (i in 1:length(model.list)) {
  cat("Step",i,": Model with variables:",paste(model.list[[i]],collapse=","),"\n")
  cat("R-squared:",r.squared.list[i],"\n\n")
}
```


First variable added:CRBI,which alone explains 32.15% of the variance in salary($R^2$=0.3215).

Incremental improvements:Adding variables like Hits,PutOuts,and Division results in notable increases in ($R^2$),especially in the early steps.

Final model:The full model includes all 19 variables with an $R^2$ of 0.5461,indicating that 54.61% of the variance in salary is explained by the model.


### g


```{r}
bic.values
```
The minimum BIC value is 3817.785,which is in step 6,indicating that the model built in step 6 is the best and balances the model's fit and complexity.After step 6,as the number of variables increases,the BIC value begins to rise,indicating that although the model becomes more complex,the risk of overfitting increases.From the perspective of BIC,the model in step 6 is the best.This model contains the following variables:CRBI,Hits,PutOuts,Division,AtBat,Walks.
