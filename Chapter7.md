---
title: "Chapter7"
author: "Megan Bontrager"
date: "7/10/2019"
output: 
  html_document: 
    keep_md: yes
---



## Video 7 notes:

Review:

Fork: common cause of 2 things (open)

Pipe: middle term mediates relationship between 2 other variables (open)

Collider: Z is a common result of 2 other variables (closed)

Descendant: A descendant of the Z exists and is like a weaker version of Z 



## Exercises

### 7E1. State the three motivating criteria that define information entropy. Try to express each in your own words.

- The measure should be continuous so it can change very incrementally as our information changes. 
- The measure needs to increase as the possible events increases. If there are three possible outcomes, that scenario should have higher uncertainty than if there were just two. [Question: what about situations where it's not A or B that we are trying to predict, but a continuous response?]
- The measure should be additive. If there are multiple responses we're trying to predict, then the summed uncertainty of each response should be equal to the total uncertainty. 


### 7E2. Suppose a coin is weighted such that, when it is tossed and lands on a table, it comes up heads 70% of the time. What is the entropy of this coin?


```r
p = c(0.3, 0.7)
-sum(p*log(p))
```

```
## [1] 0.6108643
```
0.61

### 7E3. Suppose a four-sided die is loaded such that, when tossed onto a table, it shows “1” 20%, “2” 25%, ”3” 25%, and ”4” 30% of the time. What is the entropy of this die?


```r
p = c(0.2, 0.25, 0.25, 0.3)
-sum(p*log(p))
```

```
## [1] 1.376227
```
1.38

### 7E4. Suppose another four-sided die is loaded such that it never shows “4”.  e other three sides show equally often. What is the entropy of this die?

Assume that if pi = 0, pi*log(pi) -> 0, so can ignore.


```r
p = c(1/3, 1/3, 1/3)
-sum(p*log(p))
```

```
## [1] 1.098612
```
1.10


### 7M1. Write down and compare the definitions of AIC, DIC, and WAIC. Which of these criteria is most general? Which assumptions are required to transform a more general criterion into a less general one?

From least to most general:

AIC: an estimate of the average out of sample deviance. Assumes flat priors, gaussian posterior, sample size >> number of parameters.

D_train + 2p = 2lppd + 2p

p = number of free parameters (<- what is free?)

DIC: an estimate of the average out of sample deviance. Assumes gaussian posterior, sample size >> number of parameters, but can accomodate informative piors.

WAIC: an approximation of the out of sample deviance that converges to the LOO CV approximation of a large sample. No assumptions about the shape of the posterior and can accomodate flat priors. 


### 7M2. Explain the difference between model selection and model averaging. What information is lost under model selection? What information is lost under model averaging?

Model selection means trying to pick the "best" model, by choosing the model with the lowest IC value. By selecting just one model and not assessing the others, you lose information about the relative performance of the different models you have fit. If many models perform similarly, that's relevant information because maybe all are valid descriptions of the data. Similarly, if one model is far better than the others, that gives you more confidence that it's the best among those you compared. 

A better approach is to compare models and retain information about their relative performance. You could then weight the models by their performance and average the parameter esitmates across them. 


### 7M3. When comparing models with an information criterion, why must all models be fit to exactly the same observations? What would happen to the information criterion values, if the models were fit to different numbers of observations? Perform some experiments, if you are not sure.



### 7M4. What happens to the effective number of parameters, as measured by DIC or WAIC, as a prior becomes more concentrated? Why? Perform some experiments, if you are not sure.

The "effective number of parameters" is equivalent to the penalty term in these calculations. If the prior is more concentrated

### 7M5.Provide an informal explanation of why informative priors reduce overfitting.

Informative priors reduce overfitting because they prevent the model from learning too much from the data, and dampen the range of parameter values that the model will give. 

### 7M6. Provide an information explanation of why overly informative priors result in underfitting.

If parameters are too informative, they will prevent the sample from learning from the data. 




