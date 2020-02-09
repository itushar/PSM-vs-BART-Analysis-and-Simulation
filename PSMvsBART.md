---
title: "R Notebook"
author: "Tushar"
output:
  html_document:
    keep_md: true
---

## 1. Data Description

This analysis simulates hypothetical data where the treatment group consists of children who attend private schools. The question of interest here is the effect of attending private schools on their overall performance scores.

The data includes overall income of the family (income), Mother's age (momage), father's age (dadage), number of events attending by parents (events), number of siblings (siblings)

  
## 2. Data Generation Description

The response surface for World A is linear and all assumptions hold. The $Y(0)$ and $Y(1)$ are

$$
Y(0) = income + (0.5*momage) + (0.4*dadage) - (2*events) - (2*siblings) + \epsilon \\ 
Y(1) = 20 + income + (0.5*mo
mage) + (0.4*dadage) - (2*events) - (2*siblings) + \epsilon
$$
Atleast one of the respose surface in World B should be non linear (with R square less that 0.75) and other assumptions hold.

$$
Y(0) = income + (0.5*momage) + (0.4*dadage) - (2*events) - (2*siblings) + \epsilon \\ \\
Y(1) = (0.05*income^2) + (0.5*momage) + (0.4*dadage) - (0.1*events^2) -\\ (2*siblings) - (0.18*income/siblings) + \epsilon
$$

World C has the same response surface as World B but violates one of the assumptions

$$
Y(0) = income + (0.5*momage) + (0.4*dadage) - (2*events) - (2*siblings) + \epsilon \\ \\
Y(1) = (0.05*income^2) + (0.5*momage) + (0.4*dadage) - (0.1*events^2) -\\ (2*siblings) - (0.18*income/siblings) + \epsilon\
$$

## 3. Data Generation Process


```r
set.seed(123)
library(matrixcalc)
library(MASS)
library(arm)
```

```
## Loading required package: Matrix
```

```
## Loading required package: lme4
```

```
## 
## arm (Version 1.10-1, built: 2018-4-12)
```

```
## Working directory is C:/Users/tusha/OneDrive/Documents
```

```r
# generate sample data
mu <- c(30, 30, 32, 3, 2)

#Cov matrix
sigma <- matrix(c(10, 0.6, 0.8, 0.1, 0.5, 0.6, 4,0.3, 0.3, 0.7, 0.8, 0.3, 4, 0.2, 0.8, 0.1, 0.3, 0.2, 1, 0.2, 0.5, 0.7, 0.8, 0.2, 1), 5, 5)

sigmat <- t(sigma)

Sigma <- sigma*sigmat
is.positive.definite(Sigma)
```

```
## [1] TRUE
```

```r
set.seed(1234)
d <- as.data.frame(mvrnorm(1000, mu, Sigma))
colnames(d) <- c("income", "momage", "dadage", "events", "siblings")

#print(min(d$income))
```



```r
income <- d$income
momage <- d$momage
dadage <- d$dadage
events <- d$events
siblings <- d$siblings
```


### 3a. World A


```r
y0_hat <- income + 0.5*momage + 0.4*dadage - 2*events + 2*siblings
y1_hat <- 20 + income + 0.5*momage + 0.4*dadage - 2*events + 2*siblings

#library(arm)
ps <- invlogit(2 + 0.22*income - 0.2*dadage - 0.2*momage + events + siblings)
#cat("Min : ",min(ps))
#cat("\nMax : ",max(ps))

#print(hist(z))
z <- rbinom(1000,1,ps)


y0 <- y0_hat + rnorm(1000,0,1)
y1 <- y1_hat + rnorm(1000,0,1)
y <- (y1*z) + (y0*(1-z))

worldA <- data.frame(income=income, momage=momage, dadage=dadage, events=events, siblings=siblings, y0=y0, y1=y1, y=y, z=z, ps=ps)

#print(max(y0_hat))
a.ate <- mean(worldA$y1 - worldA$y0)
a.ate <- round(a.ate, digits = 2)
cat("ATE for A - ", a.ate)
```

```
## ATE for A -  20.04
```

```r
a.att <- mean(worldA$y1[worldA$z==1] - worldA$y0[worldA$z==1])
a.att <- round(a.att, digits = 2)
cat("\nATT for A - ", a.att)
```

```
## 
## ATT for A -  20.08
```

```r
#print(mean(worldA$income[z==0]))
```

### 3b. World B


```r
#y0_hat <- 0.2*income + momage + 10*momedu - 2*momwork + 2*first
#y1_hat <- 0.0001*income*income*income + momage + 10*momedu - 15*momwork + 15*first

y0_hat <- income + 0.5*momage + 0.4*dadage - 2*events + 2*siblings
y1_hat <- 0.05*income*income + 0.5*momage + 0.4*dadage + 0.1*events*events + 2*siblings - (0.18*income)/siblings

#print(min(y1_hat))
#print(max(y1_hat))

y0 <- y0_hat + rnorm(1000,0,1)
y1 <- y1_hat + rnorm(1000,0,1)
y <- (y1*z) + (y0*(1-z))
#worldB <- data.frame(income=income, momage=momage, momedu=momedu, first=first, momwork=momwork, y0=y0, y1=y1, y=y, z=z)

worldB <- data.frame(income=income, momage=momage, dadage=dadage, events=events, siblings=siblings, y0=y0, y1=y1, y=y, z=z, ps=ps)

b.ate <- mean(worldB$y1 - worldB$y0)
b.ate <- round(b.ate, digits = 2)
cat("ATE for B - ", b.ate)
```

```
## ATE for B -  22.48
```

```r
b.att <- mean(worldB$y1[worldB$z==1] - worldB$y0[worldB$z==1])
b.att <- round(b.att, digits = 2)
cat("\nATT for B - ", b.att)
```

```
## 
## ATT for B -  29.48
```


```r
# To Check R square value (around 0.75 is good)
print(summary(lm(formula = y ~ income + momage + dadage + events + siblings + z, data =worldB)))
```

```
## 
## Call:
## lm(formula = y ~ income + momage + dadage + events + siblings + 
##     z, data = worldB)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -244.935   -5.382   -1.748    4.413   93.289 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -29.13522    6.13951  -4.746 2.38e-06 ***
## income        2.52288    0.06015  41.942  < 2e-16 ***
## momage        0.22470    0.12960   1.734   0.0833 .  
## dadage        0.25854    0.13333   1.939   0.0528 .  
## events       -0.59572    0.52195  -1.141   0.2540    
## siblings      4.18873    0.55263   7.580 7.93e-14 ***
## z            11.08711    1.37371   8.071 2.00e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.85 on 993 degrees of freedom
## Multiple R-squared:  0.7652,	Adjusted R-squared:  0.7638 
## F-statistic: 539.4 on 6 and 993 DF,  p-value: < 2.2e-16
```

## 3c. World C


```r
x <- rnorm(1000,80,20)
psC <- invlogit(0.25*income + events + siblings  - x*0.1)

#psC <- invlogit(0.22*income + events + siblings - 10)
#cat("Min : ",min(psC))
#cat("\nMax : ",max(psC))

#hist(z)
zC <- rbinom(1000,1,psC)

y0_hat <- income + 0.5*momage + 0.4*dadage - 2*events + 2*siblings
y1_hat <- 0.05*income*income + 0.5*momage + 0.4*dadage + 0.1*events*events + 2*siblings - (0.18*income)/siblings

#print(min(y0_hat))

y0 <- y0_hat + rnorm(1000,0,1)
y1 <- y1_hat + rnorm(1000,0,1)
y <- (y1*zC) + (y0*(1-zC))
worldC <- data.frame(income=income, momage=momage, dadage=dadage, events=events, siblings=siblings, y0=y0, y1=y1, y=y, z=zC, ps=psC)

c.ate <- mean(worldC$y1 - worldC$y0)
c.ate <- round(c.ate, digits = 2)
cat("ATE for C - ", c.ate)
```

```
## ATE for C -  22.45
```

```r
c.att <- mean(worldC$y1[worldC$z==1] - worldC$y0[worldC$z==1])
c.att <- round(c.att, digits = 2)
cat("\nATT for C - ", c.att)
```

```
## 
## ATT for C -  24.79
```


```r
#Overlap - world C
hist(worldC$ps[worldC$z==1], border="red", xlim=c(0,1), ylim=c(0,250), xlab="Pscore", main="World C - Propensity Score (Before Matching)")
hist(worldC$ps[worldC$z==0], border="blue", add=T)
legend("topright",legend=c("Treated","Control"), col =c("red","blue"), lty=1, cex= 0.8)
```

![](PSMvsBART_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

For World A, Linearity holds and there is enough overlaps from the plot.


```r
plot(worldA$income[z==0], worldA$y[z==0], col="blue",xlab="Income",ylab="Y",ylim=c(0,120), xlim=c(-10,70),main="WORLD A - Income vs Observed Outcomes")
points(worldA$income[z==1], worldA$y[z==1],col="red")
legend("topleft",legend=c("Control","Treated"), col =c("blue","red"),lty=3, cex = 0.8)
```

![](PSMvsBART_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


```r
#Bias
bias.table <- data.frame()

lr.A = summary(lm(formula = y ~ income + momage + dadage + events + siblings + z, data =worldA))

est.A = lr.A$coefficients["z","Estimate"]
std.A = est.A/sd(worldA$y)
biasA <- a.ate/sd(worldA$y) - std.A

cat("Bias (World A) :", biasA)
```

```
## Bias (World A) : 0.001933687
```

```r
bias.temp <- data.frame(World = "World A", Bias = biasA)
bias.table <- rbind(bias.table, bias.temp)
```

For World B, Linearity dosen't hold, Ignorability holds as all confounders are considered.


```r
plot(worldB$income[worldB$z==0], worldB$y[worldB$z==0], col="blue",xlab="Income",ylab="Y",ylim=c(0,250), xlim=c(-10,60),main="WORLD B - Income vs Observed Outcomes")
points(worldB$income[worldB$z==1], worldB$y[worldB$z==1],col="red")
legend("topleft",legend=c("Control","Treated"), col =c("blue","red"),lty=3, cex = 0.8)
```

![](PSMvsBART_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
lr.B = summary(lm(formula = y ~ income + momage + dadage + events + siblings + z, data =worldB))

est.B = lr.B$coefficients["z","Estimate"]
std.B = est.B/sd(worldB$y)
biasB <- b.ate/sd(worldB$y) - std.B

cat("Bias (World B) :", biasB)
```

```
## Bias (World B) : 0.3494323
```

```r
bias.temp <- data.frame(World = "World B", Bias = biasB)
bias.table <- rbind(bias.table, bias.temp)
```

For World C, Linearity dosen't hold. Ignorability is also not satisfied as all the confoudners are not considered.


```r
plot(worldC$income[worldC$z==0], worldC$y[worldC$z==0], col="blue",xlab="Income",ylab="Y",ylim=c(0,250), xlim=c(-10,70),main="WORLD C - Income vs Observed Outcomes")
points(worldC$income[worldC$z==1], worldC$y[worldC$z==1],col="red")
legend("topleft",legend=c("Control","Treated"), col =c("blue","red"),lty=3, cex = 0.8)
```

![](PSMvsBART_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


```r
lr.C = summary(lm(formula = y ~ income + momage + dadage + events + siblings + z, data =worldC))

est.C = lr.C$coefficients["z","Estimate"]
std.C = est.C/sd(worldC$y)
biasC <- c.ate/sd(worldC$y) - std.C

cat("Bias (World C) :", biasC)
```

```
## Bias (World C) : 0.6343722
```

```r
bias.temp <- data.frame(World = "World C", Bias = biasC)
bias.table <- rbind(bias.table, bias.temp)
```


```r
print(bias.table)
```

```
##     World        Bias
## 1 World A 0.001933687
## 2 World B 0.349432326
## 3 World C 0.634372214
```

## 4. Methods and Estimand

### 4.a Estimand

Here we are trying to estimate the effect of attending private schools on their overall performance scores. Hence we need the effect of the treatment on the treated or ATT.

### Method 1- Propensity score matching

#### World A


```r
library(MatchIt)
library(survey)
```

```
## Loading required package: grid
```

```
## Loading required package: survival
```

```
## 
## Attaching package: 'survey'
```

```
## The following object is masked from 'package:graphics':
## 
##     dotchart
```

```r
# Getting Weights
set.seed(1234)
matches.A <- matchit(z~ income + momage + dadage + events + siblings, data=worldA, method = "nearest", distance = "logit", caliper = 0.3)
```

```
## Warning in matchit2nearest(c(`1` = 0L, `2` = 1L, `3` = 1L, `4` = 0L, `5` =
## 1L, : Fewer control than treated units and matching without replacement. Not all
## treated units will receive a match. Treated units will be matched in the order
## specified by m.order: largest
```

```r
#table(matches$weights)
matched.A <- numeric(nrow(worldA))
a <- 1
for(i in 1:nrow(worldA)){
  if(matches.A$weights[i]==1)
    matched.A[a] <- i
  a = a +1
}

worldA_m <- worldA[matched.A,]
table(worldA_m$z)
```

```
## 
##   0   1 
## 187 187
```

```r
#LR with weights

result_psm <- data.frame()

lr_m_design_A <- svydesign(ids=~1, weights=~matched.A, data=worldA)
lr_m_A <- summary(svyglm(y~ z + income + momage + dadage + events + siblings, design=lr_m_design_A, data=worldA))
```

```
## Warning in summary.glm(g): observations with zero weight not used for
## calculating dispersion
```

```
## Warning in summary.glm(glm.object): observations with zero weight not used for
## calculating dispersion
```

```r
cala.att.psm <- lr_m_A$coefficients["z", "Estimate"]
cala.sd.psm <- lr_m_A$coefficients["z","Std. Error"]

temp.result <- data.frame(World = "World A", SATE=a.ate, SATT=a.att, ATT=cala.att.psm, sd=cala.sd.psm, Bias=biasA )

result_psm <- rbind(result_psm, temp.result)
print(lr_m_A)
```

```
## 
## Call:
## svyglm(formula = y ~ z + income + momage + dadage + events + 
##     siblings, design = lr_m_design_A, data = worldA)
## 
## Survey design:
## svydesign(ids = ~1, weights = ~matched.A, data = worldA)
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.012190   0.808025   0.015    0.988    
## z           20.020063   0.137064 146.064   <2e-16 ***
## income       0.993252   0.008053 123.343   <2e-16 ***
## momage       0.505164   0.017098  29.545   <2e-16 ***
## dadage       0.398453   0.017895  22.266   <2e-16 ***
## events      -1.930225   0.071117 -27.142   <2e-16 ***
## siblings     1.954602   0.065846  29.684   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 1.200802)
## 
## Number of Fisher Scoring iterations: 2
```

#### World B


```r
matches.B <- matchit(z~ income + momage + dadage + events + siblings, data=worldB, method = "nearest", distance = "logit", caliper = 0.3)
```

```
## Warning in matchit2nearest(c(`1` = 0L, `2` = 1L, `3` = 1L, `4` = 0L, `5` =
## 1L, : Fewer control than treated units and matching without replacement. Not all
## treated units will receive a match. Treated units will be matched in the order
## specified by m.order: largest
```

```r
#table(matches$weights)
matched.B <- numeric(nrow(worldB))
a <- 1
for(i in 1:nrow(worldB)){
  if(matches.B$weights[i]==1)
    matched.B[a] <- i
  a = a +1
}

worldB_m <- worldB[matched.B,]
table(worldB_m$z)
```

```
## 
##   0   1 
## 187 187
```

```r
lr_m_design_B <- svydesign(ids=~1, weights=~matched.B, data=worldB)
lr_m_B <- summary(svyglm(y~ z + income + momage + dadage + events + siblings, design=lr_m_design_B, data=worldB))
```

```
## Warning in summary.glm(g): observations with zero weight not used for
## calculating dispersion
```

```
## Warning in summary.glm(glm.object): observations with zero weight not used for
## calculating dispersion
```

```r
calb.att.psm <- lr_m_B$coefficients["z", "Estimate"]
calb.sd.psm <- lr_m_B$coefficients["z","Std. Error"]

temp.result <- data.frame(World = "World B", SATE=b.ate, SATT=b.att, ATT=calb.att.psm, sd=calb.sd.psm, Bias=biasB )

result_psm <- rbind(result_psm, temp.result)
print(lr_m_B)
```

```
## 
## Call:
## svyglm(formula = y ~ z + income + momage + dadage + events + 
##     siblings, design = lr_m_design_B, data = worldB)
## 
## Survey design:
## svydesign(ids = ~1, weights = ~matched.B, data = worldB)
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -3.9163     9.6986  -0.404   0.6866    
## z            15.0184     1.3001  11.552  < 2e-16 ***
## income        2.4568     0.1435  17.116  < 2e-16 ***
## momage       -0.3376     0.1971  -1.713   0.0876 .  
## dadage       -0.3578     0.2037  -1.757   0.0797 .  
## events        1.3484     0.7609   1.772   0.0772 .  
## siblings      5.0865     1.2848   3.959 9.04e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 176.8124)
## 
## Number of Fisher Scoring iterations: 2
```

#### World C


```r
matches.C <- matchit(z~ income + momage + dadage + events + siblings, data=worldB, method = "nearest", distance = "logit", caliper = 0.3)
```

```
## Warning in matchit2nearest(c(`1` = 0L, `2` = 1L, `3` = 1L, `4` = 0L, `5` =
## 1L, : Fewer control than treated units and matching without replacement. Not all
## treated units will receive a match. Treated units will be matched in the order
## specified by m.order: largest
```

```r
#table(matches$weights)
matched.C <- numeric(nrow(worldC))
a <- 1
for(i in 1:nrow(worldB)){
  if(matches.C$weights[i]==1)
    matched.C[a] <- i
  a = a +1
}

worldC_m <- worldC[matched.C,]
table(worldC_m$z)
```

```
## 
##   0   1 
##  36 338
```

```r
lr_m_design_C <- svydesign(ids=~1, weights=~matched.C, data=worldC)
lr_m_C <- summary(svyglm(y~ z + income + momage + dadage + events + siblings, design=lr_m_design_C, data=worldC))
```

```
## Warning in summary.glm(g): observations with zero weight not used for
## calculating dispersion
```

```
## Warning in summary.glm(glm.object): observations with zero weight not used for
## calculating dispersion
```

```r
calc.att.psm <- lr_m_C$coefficients["z", "Estimate"]
calc.sd.psm <- lr_m_C$coefficients["z","Std. Error"]

temp.result <- data.frame(World = "World C", SATE=c.ate, SATT=c.att, ATT=calc.att.psm, sd=calc.sd.psm, Bias=biasC )

result_psm <- rbind(result_psm, temp.result)

print(lr_m_C)
```

```
## 
## Call:
## svyglm(formula = y ~ z + income + momage + dadage + events + 
##     siblings, design = lr_m_design_C, data = worldC)
## 
## Survey design:
## svydesign(ids = ~1, weights = ~matched.C, data = worldC)
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -26.43144    8.78074  -3.010  0.00279 ** 
## z             9.14255    2.07285   4.411 1.36e-05 ***
## income        3.02358    0.08377  36.096  < 2e-16 ***
## momage       -0.09199    0.15400  -0.597  0.55065    
## dadage       -0.26423    0.17171  -1.539  0.12471    
## events        1.36745    0.53359   2.563  0.01078 *  
## siblings      5.98782    1.16605   5.135 4.59e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 146.4663)
## 
## Number of Fisher Scoring iterations: 2
```


### Method 2- BART


```r
library(devtools)
```

```
## Loading required package: usethis
```

```r
#library(devtools)
install_github("vdorie/bartCause")
```

```
## WARNING: Rtools is required to build R packages, but is not currently installed.
## 
## Please download and install Rtools custom from http://cran.r-project.org/bin/windows/Rtools/.
```

```
## Skipping install of 'bartCause' from a github remote, the SHA1 (0a00919e) has not changed since last install.
##   Use `force = TRUE` to force installation
```

```r
library(bartCause)
```

```
## 
## Attaching package: 'bartCause'
```

```
## The following object is masked from 'package:lme4':
## 
##     refit
```

#### World A


```r
fitA <- bartc(y, z, income + momage + dadage + events + siblings, data=worldA, estimand = c("att"), commonSup.rule = c("none", "sd", "chisq"), commonSup.cut  = c(NA_real_, 1, 0.05))
```

```
## fitting treatment model via method 'bart'
## fitting response model via method 'bart'
```

```r
result <- data.frame()
result_bart <- data.frame()

A <- summary(fitA)
print(A)
```

```
## Call: bartc(response = y, treatment = z, confounders = income + momage + 
##             dadage + events + siblings, data = worldA, estimand = c("att"), 
##             commonSup.rule = c("none", "sd", "chisq"), commonSup.cut = c(NA_real_, 
##                 1, 0.05))
## 
## Causal inference model fit by:
##   model.rsp: bart
##   model.trt: bart
## 
## Treatment effect:
##     estimate     sd ci.lower ci.upper
## att    20.06 0.1636    19.74    20.38
## Estimates fit from 1000 total observations
## 95% credible interval calculated by: normal approximation
## Result based on 5000 posterior samples across 10 chains
```

```r
temp <- A$estimates["estimate"]
bart.est.A <- temp$estimate
bart.sd.A = A$estimates["sd"]
#std.A <- est.A/sd(worldA$y)
#biasA <- a.ate/sd(worldA$y) - std.A

temp.result <- data.frame(World = "World A", SATE=a.ate, SATT=a.att, BART_ATT =bart.est.A, sd=bart.sd.A, Bias=biasA )

f.result <- data.frame(World = 'World A', SATT=a.att, PSM_ATT= cala.att.psm, BART_ATT= bart.est.A, Bias=biasA)

result_bart <- rbind(result_bart, temp.result)
result <- rbind(result, f.result)
```


#### World B


```r
fitB <- bartc(y, z, income + momage + dadage + events + siblings, data=worldB, estimand = c("att"), commonSup.rule = c("none", "sd", "chisq"), commonSup.cut  = c(NA_real_, 1, 0.05))
```

```
## fitting treatment model via method 'bart'
## fitting response model via method 'bart'
```

```r
B <- summary(fitB)
print(B)
```

```
## Call: bartc(response = y, treatment = z, confounders = income + momage + 
##             dadage + events + siblings, data = worldB, estimand = c("att"), 
##             commonSup.rule = c("none", "sd", "chisq"), commonSup.cut = c(NA_real_, 
##                 1, 0.05))
## 
## Causal inference model fit by:
##   model.rsp: bart
##   model.trt: bart
## 
## Treatment effect:
##     estimate     sd ci.lower ci.upper
## att    28.24 0.9039    26.47    30.01
## Estimates fit from 1000 total observations
## 95% credible interval calculated by: normal approximation
## Result based on 5000 posterior samples across 10 chains
```

```r
temp <- B$estimates["estimate"]
bart.est.B <- temp$estimate
bart.sd.B <- B$estimates["sd"]
#std.B <- est.B/sd(worldB$y)
#biasB <- b.ate/sd(worldA$y) - std.B

temp.result <- data.frame(World = "World B", SATE=b.ate, SATT=b.att, BART_ATT=bart.est.B, sd=bart.sd.B, Bias=biasB )

f.result <- data.frame(World = 'World B', SATT=b.att, PSM_ATT= calb.att.psm, BART_ATT= bart.est.B, Bias=biasB)

result_bart <- rbind(result_bart, temp.result)
result <- rbind(result, f.result)
```

#### World C


```r
fitC <- bartc(y, z, income + momage + dadage + events + siblings, data=worldB, estimand = c("att"), commonSup.rule = c("none", "sd", "chisq"), commonSup.cut  = c(NA_real_, 1, 0.05))
```

```
## fitting treatment model via method 'bart'
## fitting response model via method 'bart'
```

```r
C <- summary(fitC)
print(C)
```

```
## Call: bartc(response = y, treatment = z, confounders = income + momage + 
##             dadage + events + siblings, data = worldB, estimand = c("att"), 
##             commonSup.rule = c("none", "sd", "chisq"), commonSup.cut = c(NA_real_, 
##                 1, 0.05))
## 
## Causal inference model fit by:
##   model.rsp: bart
##   model.trt: bart
## 
## Treatment effect:
##     estimate    sd ci.lower ci.upper
## att    27.66 2.444    22.87    32.45
## Estimates fit from 1000 total observations
## 95% credible interval calculated by: normal approximation
## Result based on 5000 posterior samples across 10 chains
```

```r
temp <- C$estimates["estimate"]
bart.est.C <- temp$estimate
bart.sd.C <- C$estimates["sd"]
#std.C <- est.C/sd(worldC$y)
#biasC <- c.ate/sd(worldC$y) - std.C

temp.result <- data.frame(World = "World C", SATE=c.ate, SATT=c.att, BART_ATT=bart.est.C, sd=bart.sd.C, Bias=biasC )

f.result <- data.frame(World = 'World C', SATT=c.att, PSM_ATT= calc.att.psm, BART_ATT= bart.est.C, Bias=biasC)

result_bart <- rbind(result_bart, temp.result)
result <- rbind(result, f.result)
```

## 5. Results


```r
print(result_psm)
```

```
##     World  SATE  SATT       ATT        sd        Bias
## 1 World A 20.04 20.08 20.020063 0.1370638 0.001933687
## 2 World B 22.48 29.48 15.018427 1.3000891 0.349432326
## 3 World C 22.45 24.79  9.142552 2.0728452 0.634372214
```


```r
print(result_bart)
```

```
##        World  SATE  SATT BART_ATT        sd        Bias
## att  World A 20.04 20.08 20.06018 0.1635537 0.001933687
## att1 World B 22.48 29.48 28.24113 0.9039210 0.349432326
## att2 World C 22.45 24.79 27.65528 2.4440616 0.634372214
```


```r
print(result)
```

```
##     World  SATT   PSM_ATT BART_ATT        Bias
## 1 World A 20.08 20.020063 20.06018 0.001933687
## 2 World B 29.48 15.018427 28.24113 0.349432326
## 3 World C 24.79  9.142552 27.65528 0.634372214
```
## 6. Assumption Discussion

To yield a valid causal estiamate using *PSM* the following assumptions need to be fullfilled.

  a. Ignorability :- This is neccesary to estimate the effect without Bias. THis assumption holds when all the confounders are measured.
  
  b. Overlap :- Having sufficient overlap is required to make causal inferences. If a model is fitted without complete overlap it extrapolates.
  
  c. Balance :- This assumption requires the distributions of the confoudners in the treatment and the control group to be close. If imbalance exists, bias creeeps in and we have to rely more on the correctness of the model.
  
  d. SUTVA :- No interaction between the individual children.
  
  e. Parametric Assumption :- The parametric form of the model is correct.
  
To yield a valid causal estiamate using *BART* the following assumptions need to be fullfilled.

  a. Structural Assumptions : - Weak Ignorability must hold (BART works when there is lack of overlap). 
  b. Parametric Assumptions must be satisfied.
  
BART constructs a counterfactual for each data point with a certainity interval, so that for those points which have empirical conterfactuals the certainity interval is small and those without the empirical counterfactual the interval gets bigger. Using the common support rule we can discard points which have high uncertainity. Hence even if there is lack of overlap it performs well.

## 7. Conclusion

Propensity Score Matching is usefull to estimate causal effect when the confounding covariates are know (throught research). Here the ignorability assumption must be strong as the inference is drawn using scores dervied from what we think the confounders are (forcing overlap)

Even if ignorability holds, imbalance and overlap could still be issues which let's us evaluate the adequacy of the propensity scores and the model used.

BART is usefull to estimate treatment effects when the confounders have lack of overlap. It incorporates the outcome for understanding what covariates are important common support.

It also provides a flexible fit to varying response surface and can work on weak ignorability assumptions.
