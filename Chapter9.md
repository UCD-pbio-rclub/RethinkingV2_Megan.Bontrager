---
title: "Chapter9"
author: "Megan Bontrager"
date: "9/27/2019"
output: 
  html_document: 
    keep_md: yes
---



## 8E1

Which of the following is a requirement of the simple Metropolis algorithm?
(1)  The parameters must be discrete. <- No
(2)  The likelihood function must be Gaussian. <- No
(3)  The proposal distribution must be symmetric. <- Yes

## 8E2

Gibbs sampling is more efficient than the Metropolis algorithm. How does it achieve this extra efficiency? Are there any limitations to the Gibbs sampling strategy?

Gibbs sampling uses "smarter" proposals to characterize the posterior distribution with fewer samples than a classic metropolis algorithm. It does this with conjugate prior-likelihood pairs, which are analytically tractable.
