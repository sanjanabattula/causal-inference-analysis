Causal Inference Analysis
================
Sanjana Battula and Lindsay Temple
2024-12-10

## Research Question

Does the treatment (government training program) improve earnings from
1975 to 1978?

## Define Variable

age, education, black , hispanic (1 if Hispanic, 0 otherwise), married
(1 if \# married, 0 otherwise), nodegree (1 if no high school degree, 0
otherwise), \# earnings75 (earnings in 1975), and earnings78 (earnings
in 1978). The last \# variable is the outcome. Other variables are
pre-treatment.

Treatment: The variables are treatment (1 if treated, 0 if not treated)
Age: age in years (continuous) Education: I think this is years of
schooling? we need to double check this in the notes / the study itself.
We also need to think about if we want this to be a continuous or
categorical variable Black: 1 if black, 0 otherwise Hispanic: 1 if
Hispanic, 0 otherwise Married: 1 if married, 0 otherwise Nodegree: 1 if
no high school degree, 0 otherwise Earnings75: earnings in 1975
Earnings78: earnings in 1978 Outcome: Earnings78- Earnings75

``` r
#loading the packages
library(readr)
library(tidyverse)
library(dplyr)
setwd("/Users/sbattula/Desktop/CausalInference")
#loading the data
nswdata <- read.table("nswdata.txt", header = TRUE)
attach(nswdata)

#creating the outcome variable, which is the difference in earnings between 1978 and 1975
nswdata$outcome <- nswdata$earnings78 - nswdata$earnings75
```

``` r
# Balance of covariates
# List of covariates
covariates <- list(
  "Age" = age,
  "Education (Years of Schooling)" = education,
  "Black" = black,
  "Hispanic" = hispanic,
  "Married" = married,
  "No High School Degree" = nodegree,
  "Earnings in 1975" = earnings75
)

# Initialize an empty data frame to store results
results <- data.frame(
  Covariate = character(),
  `Treatment Group` = numeric(),
  `Control Group` = numeric(),
  `p-value fo rnull hypothesis` = numeric(),
  stringsAsFactors = FALSE
)

# Loop through covariates to perform t-tests and extract results
for (var in names(covariates)) {
  treated <- covariates[[var]][treatment == 1]
  control <- covariates[[var]][treatment == 0]
  t_test <- t.test(treated, control)
  
  # Add to results table
  results <- rbind(results, data.frame(
    Covariate = var,
    `Treatment Group` = mean(treated, na.rm = TRUE),
    `Control Group` = mean(control, na.rm = TRUE),
    `p-value` = t_test$p.value
  ))
}

# Round numerical values to three decimal places
results$Treatment.Group <- round(results$Treatment.Group, 4)
results$Control.Group <- round(results$Control.Group, 4)
results$p.value <- round(results$p.value, 4)

# Format and display the results
print(results, row.names = FALSE)
```

    ##                       Covariate Treatment.Group Control.Group p.value
    ##                             Age         24.6263       24.4471  0.7216
    ##  Education (Years of Schooling)         10.3805       10.1882  0.1443
    ##                           Black          0.8013        0.8000  0.9645
    ##                        Hispanic          0.0943        0.1129  0.4155
    ##                         Married          0.1684        0.1576  0.7028
    ##           No High School Degree          0.7306        0.8141  0.0092
    ##                Earnings in 1975       3066.0982     3026.6827  0.9172

*Table 1: Comparing treatment and control groups to show that prior to
treatment both groups have relatively similar values.*

``` r
treated.r.jobtrain=nswdata$earnings78[nswdata$treatment==1]
control.r.jobtrain=nswdata$earnings78[nswdata$treatment==0]
# Normal Superpopulation inference
# Comparison with normal distribution based inference
r=c(treated.r.jobtrain,control.r.jobtrain)
z=c(rep(1,length(treated.r.jobtrain)),rep(0,length(control.r.jobtrain)))
normalmodel=lm(r~z)
summary(normalmodel)
```

    ## 
    ## Call:
    ## lm(formula = r ~ z)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -5976  -5090  -1519   3361  54332 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   5090.0      302.8  16.811   <2e-16 ***
    ## z              886.3      472.1   1.877   0.0609 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6242 on 720 degrees of freedom
    ## Multiple R-squared:  0.004872,   Adjusted R-squared:  0.003489 
    ## F-statistic: 3.525 on 1 and 720 DF,  p-value: 0.06086

``` r
t.test(treated.r.jobtrain,control.r.jobtrain,var.equal=TRUE)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  treated.r.jobtrain and control.r.jobtrain
    ## t = 1.8774, df = 720, p-value = 0.06086
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##   -40.52635 1813.13381
    ## sample estimates:
    ## mean of x mean of y 
    ##  5976.352  5090.048

``` r
# Alternative = “greater” in the Wilcoxon test rejects for large
# values of the Wilcoxon rank sumtest
wilcox.test(treated.r.jobtrain,control.r.jobtrain,alternative="greater")
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  treated.r.jobtrain and control.r.jobtrain
    ## W = 68210, p-value = 0.03096
    ## alternative hypothesis: true location shift is greater than 0

## Data Assumptions

``` r
#missing values
which(is.na(nswdata))
```

    ## integer(0)

``` r
#number treated is 297
which(treatment == 1)
```

    ##   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
    ##  [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
    ##  [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
    ##  [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
    ##  [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
    ##  [91]  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
    ## [109] 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
    ## [127] 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
    ## [145] 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162
    ## [163] 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
    ## [181] 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198
    ## [199] 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216
    ## [217] 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234
    ## [235] 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252
    ## [253] 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270
    ## [271] 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288
    ## [289] 289 290 291 292 293 294 295 296 297

``` r
#number of controls is 425
which(treatment == 0)
```

    ##   [1] 298 299 300 301 302 303 304 305 306 307 308 309 310 311 312 313 314 315
    ##  [19] 316 317 318 319 320 321 322 323 324 325 326 327 328 329 330 331 332 333
    ##  [37] 334 335 336 337 338 339 340 341 342 343 344 345 346 347 348 349 350 351
    ##  [55] 352 353 354 355 356 357 358 359 360 361 362 363 364 365 366 367 368 369
    ##  [73] 370 371 372 373 374 375 376 377 378 379 380 381 382 383 384 385 386 387
    ##  [91] 388 389 390 391 392 393 394 395 396 397 398 399 400 401 402 403 404 405
    ## [109] 406 407 408 409 410 411 412 413 414 415 416 417 418 419 420 421 422 423
    ## [127] 424 425 426 427 428 429 430 431 432 433 434 435 436 437 438 439 440 441
    ## [145] 442 443 444 445 446 447 448 449 450 451 452 453 454 455 456 457 458 459
    ## [163] 460 461 462 463 464 465 466 467 468 469 470 471 472 473 474 475 476 477
    ## [181] 478 479 480 481 482 483 484 485 486 487 488 489 490 491 492 493 494 495
    ## [199] 496 497 498 499 500 501 502 503 504 505 506 507 508 509 510 511 512 513
    ## [217] 514 515 516 517 518 519 520 521 522 523 524 525 526 527 528 529 530 531
    ## [235] 532 533 534 535 536 537 538 539 540 541 542 543 544 545 546 547 548 549
    ## [253] 550 551 552 553 554 555 556 557 558 559 560 561 562 563 564 565 566 567
    ## [271] 568 569 570 571 572 573 574 575 576 577 578 579 580 581 582 583 584 585
    ## [289] 586 587 588 589 590 591 592 593 594 595 596 597 598 599 600 601 602 603
    ## [307] 604 605 606 607 608 609 610 611 612 613 614 615 616 617 618 619 620 621
    ## [325] 622 623 624 625 626 627 628 629 630 631 632 633 634 635 636 637 638 639
    ## [343] 640 641 642 643 644 645 646 647 648 649 650 651 652 653 654 655 656 657
    ## [361] 658 659 660 661 662 663 664 665 666 667 668 669 670 671 672 673 674 675
    ## [379] 676 677 678 679 680 681 682 683 684 685 686 687 688 689 690 691 692 693
    ## [397] 694 695 696 697 698 699 700 701 702 703 704 705 706 707 708 709 710 711
    ## [415] 712 713 714 715 716 717 718 719 720 721 722

## Matching

### Propensity Score

The two distributions have similar medians, and there is a good amount
of overlap indicating matching is possible

``` r
#creating the propensity score
p <- glm(treatment ~ age + education + black + hispanic + married + nodegree, family = binomial)$fitted.values
#adding propensity score to the dataframe
nswdata$propensity <- p
#making a boxplot to visualize propensity score between the two treatment groups
boxplot(nswdata$propensity ~ treatment)
```

![](CausalInferenceAnalysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Distance Matrix

The length of the distance matrix will be the number of controls, double
check this matches

``` r
#install.packages("DOS2")
library(DOS2)
#creating separate tables for treatment and covariates
treat <- nswdata$treatment
covariates <-  nswdata %>% dplyr::select(age, education, black, hispanic, married, nodegree)
dmat <- smahal(treat, covariates)
dim(dmat)
```

    ## [1] 297 425

``` r
#adding the caliper on the propensity score
dmat <- addcaliper(dmat, treat, p)
dim(dmat)
```

    ## [1] 297 425

### Full Match

We don’t have enough controls for anything other than a 1:1 match. We
will have to omit 425 - 297 = 128 controls

``` r
#install.packages("optmatch")
library(optmatch)

m <- fullmatch(dmat, min.controls = 1, max.controls = 1, omit.fraction = 128/425)
length(m)
```

    ## [1] 722

``` r
matched(m)[1:10]
```

    ##    1    2    3    4    5    6    7    8    9   10 
    ## TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

## Outcome Analysis

``` r
#regression, or whatever the appropriate analysis ends up being
linearmodel <- lm(outcome ~ treatment + age + education + black + hispanic + married + nodegree, data = nswdata)
summary(linearmodel)
```

    ## 
    ## Call:
    ## lm(formula = outcome ~ treatment + age + education + black + 
    ##     hispanic + married + nodegree, data = nswdata)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -38153  -3383  -1076   3503  55863 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  1986.822   3123.319   0.636  0.52490   
    ## treatment     869.294    561.559   1.548  0.12207   
    ## age             4.194     43.424   0.097  0.92308   
    ## education      26.633    215.306   0.124  0.90159   
    ## black         112.085    956.157   0.117  0.90672   
    ## hispanic     1229.152   1253.250   0.981  0.32704   
    ## married     -2210.949    767.530  -2.881  0.00409 **
    ## nodegree     -217.758    891.527  -0.244  0.80710   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7384 on 714 degrees of freedom
    ## Multiple R-squared:  0.01729,    Adjusted R-squared:  0.007654 
    ## F-statistic: 1.794 on 7 and 714 DF,  p-value: 0.08539

Job training is estimated to increase earnings by \$869 with a 95%
confidence interval of 869 + (1.96\*562) = (-232.52, 1970.52).

``` r
library(MatchIt)
# Full matching using MatchIt

# Full matching on a probit PS
matched_data <- matchit(treatment ~ age + education + black + hispanic + married + nodegree, data = nswdata, method = "full", distance = "glm", link = "probit")
matched_data
```

    ## A `matchit` object
    ##  - method: Optimal full matching
    ##  - distance: Propensity score             - estimated with probit regression
    ##  - number of obs.: 722 (original), 722 (matched)
    ##  - target estimand: ATT
    ##  - covariates: age, education, black, hispanic, married, nodegree

``` r
# Checking balance after full matching
summary(matched_data, un = FALSE)
```

    ## 
    ## Call:
    ## matchit(formula = treatment ~ age + education + black + hispanic + 
    ##     married + nodegree, data = nswdata, method = "full", distance = "glm", 
    ##     link = "probit")
    ## 
    ## Summary of Balance for Matched Data:
    ##           Means Treated Means Control Std. Mean Diff. Var. Ratio eCDF Mean
    ## distance         0.4178        0.4178         -0.0012     0.9955    0.0029
    ## age             24.6263       24.1207          0.0756     1.1865    0.0171
    ## education       10.3805       10.3455          0.0192     1.1312    0.0065
    ## black            0.8013        0.7811          0.0506          .    0.0202
    ## hispanic         0.0943        0.1076         -0.0457          .    0.0134
    ## married          0.1684        0.1684         -0.0003          .    0.0001
    ## nodegree         0.7306        0.7306         -0.0000          .    0.0000
    ##           eCDF Max Std. Pair Dist.
    ## distance    0.0202          0.0085
    ## age         0.0400          0.6393
    ## education   0.0228          0.3942
    ## black       0.0202          0.3357
    ## hispanic    0.0134          0.1986
    ## married     0.0001          0.5011
    ## nodegree    0.0000          0.0000
    ## 
    ## Sample Sizes:
    ##               Control Treated
    ## All             425.      297
    ## Matched (ESS)   248.8     297
    ## Matched         425.      297
    ## Unmatched         0.        0
    ## Discarded         0.        0

``` r
# Plots
plot(matched_data, type = "jitter", interactive = FALSE)
```

![](CausalInferenceAnalysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
plot(matched_data, type = "density", interactive = FALSE)
```

![](CausalInferenceAnalysis_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->![](CausalInferenceAnalysis_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
plot(summary(matched_data))
```

![](CausalInferenceAnalysis_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->

``` r
library(MatchIt)

# Pair matching using MatchIt

# Pair matching on a probit PS
matched_data <- matchit(treatment ~ age + education + black + hispanic + married + nodegree, 
                        data = nswdata, 
                        method = "nearest", 
                        distance = "glm", 
                        link = "probit")
matched_data
```

    ## A `matchit` object
    ##  - method: 1:1 nearest neighbor matching without replacement
    ##  - distance: Propensity score             - estimated with probit regression
    ##  - number of obs.: 722 (original), 594 (matched)
    ##  - target estimand: ATT
    ##  - covariates: age, education, black, hispanic, married, nodegree

``` r
# Checking balance after pair matching
summary(matched_data, un = FALSE)
```

    ## 
    ## Call:
    ## matchit(formula = treatment ~ age + education + black + hispanic + 
    ##     married + nodegree, data = nswdata, method = "nearest", distance = "glm", 
    ##     link = "probit")
    ## 
    ## Summary of Balance for Matched Data:
    ##           Means Treated Means Control Std. Mean Diff. Var. Ratio eCDF Mean
    ## distance         0.4178        0.4167          0.0204     1.0360    0.0044
    ## age             24.6263       24.3232          0.0453     1.1281    0.0160
    ## education       10.3805       10.3401          0.0222     1.1105    0.0077
    ## black            0.8013        0.7845          0.0422          .    0.0168
    ## hispanic         0.0943        0.1044         -0.0346          .    0.0101
    ## married          0.1684        0.1717         -0.0090          .    0.0034
    ## nodegree         0.7306        0.7407         -0.0228          .    0.0101
    ##           eCDF Max Std. Pair Dist.
    ## distance    0.0370          0.0339
    ## age         0.0370          0.6133
    ## education   0.0303          0.3260
    ## black       0.0168          0.2447
    ## hispanic    0.0101          0.1267
    ## married     0.0034          0.3869
    ## nodegree    0.0101          0.0228
    ## 
    ## Sample Sizes:
    ##           Control Treated
    ## All           425     297
    ## Matched       297     297
    ## Unmatched     128       0
    ## Discarded       0       0

``` r
# Plots
plot(matched_data, type = "jitter", interactive = FALSE)
```

![](CausalInferenceAnalysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
plot(matched_data, type = "density", interactive = FALSE)
```

![](CausalInferenceAnalysis_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->![](CausalInferenceAnalysis_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

``` r
plot(summary(matched_data))
```

![](CausalInferenceAnalysis_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

# Estimating the treatment effect

``` r
m.data <- match.data(matched_data)

head(m.data)
```

    ##   treatment age education black hispanic married nodegree earnings75 earnings78
    ## 1         1  37        11     1        0       1        1          0  9930.0460
    ## 2         1  22         9     0        1       0        1          0  3595.8940
    ## 3         1  30        12     1        0       0        0          0 24909.4500
    ## 4         1  27        11     1        0       0        1          0  7506.1460
    ## 5         1  33         8     1        0       0        1          0   289.7899
    ## 6         1  22         9     1        0       0        1          0  4056.4940
    ##      outcome propensity  distance weights subclass
    ## 1  9930.0460  0.3922231 0.3922978       1        1
    ## 2  3595.8940  0.3531369 0.3532935       1        2
    ## 3 24909.4500  0.4983386 0.4982584       1        3
    ## 4  7506.1460  0.3762343 0.3762443       1        4
    ## 5   289.7899  0.3911213 0.3909173       1        5
    ## 6  4056.4940  0.3897082 0.3896207       1        6

``` r
# Fit a propensity score using logistic regression with each covariate entering 
# linearly into the logistic link function
# Put x=TRUE in order to have model object include design matrix
propscore.model <- glm(treatment ~ age + education + black + hispanic + married + nodegree, family = binomial, x = TRUE, y = TRUE, data = nswdata)

# treated = Variable denoting which units are treated
datatemp=nswdata
datatemp$treated=propscore.model$y
treated=datatemp$treated
datatemp$logit.ps=predict(propscore.model)
datatemp$outcome=nswdata$outcome
rownames(datatemp)=seq(1,nrow(datatemp),1) # Make the rownames in datatemp be 1:number of rows

library(optmatch)
diff.propensity.score.mat=outer(datatemp$logit.ps[datatemp$treated==1],datatemp$logit.ps[datatemp$treated==0],"-")
distmat.propensity=abs(diff.propensity.score.mat)

# Label the rows and columns of the distance matrix by the rownames in datatemp
rownames(distmat.propensity)=rownames(datatemp)[datatemp$treated==1]
colnames(distmat.propensity)=rownames(datatemp)[datatemp$treated==0]

# Full matching
matchvec=fullmatch(distmat.propensity,data=datatemp)
datatemp$matchvec=matchvec
stratumStructure(matchvec) # structure of the match
```

    ## 6:1 5:1 4:1 3:1 2:1 1:1 1:2 1:3 1:4 1:5 1:6 1:7 1:9 
    ##   1   3   3   3   8 173  23  14  14   7   3   4   1

``` r
effectiveSampleSize(matchvec) # effective sample size 
```

    ## [1] 299.3571

``` r
# Number the strata
matchedset.index=substr(matchvec,start=3,stop=10)
matchedset.index.numeric=as.numeric(matchedset.index)

# Calculate standardized difference before and after a full match
# Calculate standardized difference before and after a full match
# Drop observations with missing values from the calculations
# stratum.myindex should contain strata for each subject, 0 means a unit was not 
# matched
# Use harmonic mean weights
standardized.diff.harmonic.func=function(x,treatment,stratum.myindex,missing=rep(0,length(x))){
  xtreated=x[treatment==1 & missing==0];
  xcontrol=x[treatment==0 & missing==0];
  var.xtreated=var(xtreated);
  var.xcontrol=var(xcontrol);
  combinedsd=sqrt(.5*(var.xtreated+var.xcontrol));
  std.diff.before.matching=(mean(xtreated)-mean(xcontrol))/combinedsd;
  nostratum=length(unique(stratum.myindex))-1*max(stratum.myindex==0);
  if(max(stratum.myindex==0)==0){
    stratumlist=sort(unique(stratum.myindex))
  }
  if(max(stratum.myindex==0)==1){
    templist=sort(unique(stratum.myindex))
    stratumlist=templist[-1]
  }
  diff.in.stratum=rep(0,nostratum);
  number.in.stratum=rep(0,nostratum);
  harmonic.weight=rep(0,nostratum)
  for(i in 1:nostratum){
    if(sum(stratum.myindex==stratumlist[i] & treatment==1 & missing==0)==0 | sum(stratum.myindex==stratumlist[i] & treatment==0 & missing==0)==0){
      number.in.stratum[i]=0
    }
    if(sum(stratum.myindex==stratumlist[i] & treatment==1 & missing==0)>0 & sum(stratum.myindex==stratumlist[i] & treatment==0 & missing==0)>0){
      diff.in.stratum[i]=mean(x[stratum.myindex==stratumlist[i] & treatment==1 & missing==0])-mean(x[stratum.myindex==stratumlist[i] & treatment==0 & missing==0]);
      number.in.stratum[i]=sum(stratum.myindex==stratumlist[i])
      harmonic.weight[i]=1/(.5/sum(stratum.myindex==stratumlist[i] & treatment==1)+.5/sum(stratum.myindex==stratumlist[i] & treatment==0))
    }
  }
  std.diff.after.matching=(sum(harmonic.weight*diff.in.stratum)/sum(harmonic.weight))/combinedsd;
  list(std.diff.before.matching=std.diff.before.matching,std.diff.after.matching=std.diff.after.matching);
}

# Covariates used in propensity score model
Xmat=propscore.model$x;

# Which variables are missing
missing.mat=matrix(rep(0,ncol(Xmat)*nrow(Xmat)),ncol=ncol(Xmat))

# Put in NAs for all X variables which are missing and for which mean value has 
# been imputed
Xmat.without.missing=Xmat
for(i in 1:ncol(Xmat)){
  Xmat.without.missing[missing.mat[,i]==1,i]=NA
}

# Calculate the standardized differences
std.diff.before=rep(0,ncol(Xmat.without.missing));
std.diff.after=rep(0,ncol(Xmat.without.missing));
names(std.diff.before)=names(Xmat[1,]);
names(std.diff.after)=names(Xmat[1,]);
for(i in 1:ncol(Xmat.without.missing)){
missing.temp=is.na(Xmat.without.missing[,i])
temp.stand.diff=standardized.diff.harmonic.func(Xmat.without.missing[,i],datatemp$treated,matchedset.index.numeric,missing.temp);
std.diff.before[i]=temp.stand.diff$std.diff.before.matching;
std.diff.after[i]=temp.stand.diff$std.diff.after.matching;
}

# Rename std.diff.before and std.diff.after to shorter names sd.bf and sd.af
# and use digits option to be able to columns of std.diff.before and 
# std.diff.after in one row
sd.bf=std.diff.before
sd.af=std.diff.after
options(digits=2)
cbind(sd.bf,sd.af)
```

    ##               sd.bf  sd.af
    ## (Intercept)     NaN    NaN
    ## age          0.0270  0.012
    ## education    0.1117  0.016
    ## black        0.0034  0.027
    ## hispanic    -0.0612 -0.032
    ## married      0.0289 -0.025
    ## nodegree    -0.1998  0.000

``` r
# Love plot 
library(ggplot2)  
abs.stand.diff.before=abs(sd.bf[-1])
abs.stand.diff.after=abs(sd.af[-1])
covariates=names(sd.bf[-1])
plot.dataframe=data.frame(abs.stand.diff=c(abs.stand.diff.before,abs.stand.diff.after),covariates=rep(covariates,2),type=c(rep("Before",length(covariates)),rep("After",length(covariates))))
ggplot(plot.dataframe,aes(x=abs.stand.diff,y=covariates))+geom_point(size=5,aes(shape=factor(type)))+scale_shape_manual(values=c(4,1))+geom_vline(xintercept=c(.1,.2),lty=2)
```

![](CausalInferenceAnalysis_files/figure-gfm/Love%20plot-1.png)<!-- -->

``` r
# Put data into format for senfmCI function
library(sensitivityfull)
stratum.myindex=matchedset.index.numeric
nostratum=length(unique(stratum.myindex))-1*max(stratum.myindex==0);
if(max(stratum.myindex==0)==0){
  stratumlist=sort(unique(stratum.myindex))
}
if(max(stratum.myindex==0)==1){
  templist=sort(unique(stratum.myindex))
  stratumlist=templist[-1]
}
treated1=rep(0,nostratum)
stratumsize=rep(0,nostratum)
for(i in 1:nostratum){
  stratumsize[i]=sum(stratum.myindex==stratumlist[i])
}

y=matrix(rep(NA,nostratum*max(stratumsize)),nrow=nostratum)
for(i in 1:nostratum){
  no.treated.in.stratum=sum(stratum.myindex==stratumlist[i] & datatemp$treated==1)
  no.control.in.stratum=sum(stratum.myindex==stratumlist[i] & datatemp$treated==0)
  treated.in.stratum=which(stratum.myindex==stratumlist[i] & datatemp$treated==1)
  control.in.stratum=which(stratum.myindex==stratumlist[i] & datatemp$treated==0)  
  if(no.treated.in.stratum==1){
    y[i,1]=datatemp$outcome[treated.in.stratum]
    y[i,2:(no.control.in.stratum+1)]=datatemp$outcome[control.in.stratum]
    treated1[i]=1
  }
  if(no.treated.in.stratum>1){
    y[i,1]=datatemp$outcome[control.in.stratum]
    y[i,2:(no.treated.in.stratum+1)]=datatemp$outcome[treated.in.stratum]
    treated1[i]=0
  }
}

treated1=as.logical(treated1)
senfmCI(y,treated1, gamma = 1) # tau = 1 (CI contains 0)
```

    ## $PointEstimates
    ## [1] 965 965
    ## 
    ## $ConfidenceInterval
    ## [1] -127 2086
    ## 
    ## $description
    ##            Coverage               Gamma Confidence Interval 
    ##              "0.95"                 "1"         "Two-sided"

## Regression After Matching

``` r
reg.formula=update(propscore.model$formula,outcome~treated+matchvec+.)
matched.reg.model=lm(reg.formula,data=datatemp)# Point estimate of treatment effect
coef(matched.reg.model)[2]# Confidence interval
```

    ## treated 
    ##    1170

``` r
confint(matched.reg.model)[2,]
```

    ##  2.5 % 97.5 % 
    ##    -44   2383

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
