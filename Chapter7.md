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
