---
title: "Rate of Change of Variables Over Time"
output: html_notebook
editor_options: 
  chunk_output_type: inline
---

Job Reallocation Rate as defined and collected by Business Employment Dynamics is based on the formula:

$$Job \; Reallocation \; Rate = \frac{gain + loss}{total\;employment}$$

Using the formula $rate = \frac{final-initial}{initial}$, the rate of change of the variables are calculated to indentify if there are discernable relationships in them. 

# Rate of Change of Training Indices Over Time
## Rate of Change for Required Level of Education

```r
lmod <- lm(dRL ~ period.num, t)
summary(lmod)
```

```
## 
## Call:
## lm(formula = dRL ~ period.num, data = t)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.085630 -0.001297  0.000442  0.004438  0.031503 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept) 4.890e-04  1.441e-03   0.339    0.735
## period.num  3.552e-05  1.532e-04   0.232    0.817
## 
## Residual standard error: 0.01007 on 264 degrees of freedom
##   (19 observations deleted due to missingness)
## Multiple R-squared:  0.0002037,	Adjusted R-squared:  -0.003583 
## F-statistic: 0.0538 on 1 and 264 DF,  p-value: 0.8168
```
## Rate of Change for On-the-job Training

```r
lmod <- lm(dOJ ~ period.num, t)
summary(lmod)
```

```
## 
## Call:
## lm(formula = dOJ ~ period.num, data = t)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.012413 -0.001669 -0.000439  0.001319  0.035685 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept) 7.940e-04  5.454e-04   1.456    0.147
## period.num  4.180e-05  5.798e-05   0.721    0.472
## 
## Residual standard error: 0.003812 on 264 degrees of freedom
##   (19 observations deleted due to missingness)
## Multiple R-squared:  0.001965,	Adjusted R-squared:  -0.001815 
## F-statistic: 0.5199 on 1 and 264 DF,  p-value: 0.4715
```
## Rate of Change for In-Plant Training

```r
lmod <- lm(dPT ~ period.num, t)
summary(lmod)
```

```
## 
## Call:
## lm(formula = dPT ~ period.num, data = t)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.035269 -0.001658 -0.000314  0.001297  0.052261 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept) -5.880e-05  8.262e-04  -0.071    0.943
## period.num   9.423e-05  8.782e-05   1.073    0.284
## 
## Residual standard error: 0.005774 on 264 degrees of freedom
##   (19 observations deleted due to missingness)
## Multiple R-squared:  0.004342,	Adjusted R-squared:  0.0005702 
## F-statistic: 1.151 on 1 and 264 DF,  p-value: 0.2843
```
## Rate of Change for Relevant Work Experience

```r
lmod <- lm(dRW ~ period.num, t)
summary(lmod)
```

```
## 
## Call:
## lm(formula = dRW ~ period.num, data = t)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.023028 -0.002019 -0.000493  0.001662  0.037693 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept) 8.783e-04  7.053e-04   1.245    0.214
## period.num  8.853e-05  7.498e-05   1.181    0.239
## 
## Residual standard error: 0.004929 on 264 degrees of freedom
##   (19 observations deleted due to missingness)
## Multiple R-squared:  0.005254,	Adjusted R-squared:  0.001486 
## F-statistic: 1.394 on 1 and 264 DF,  p-value: 0.2387
```

According to the results, there is little to no statistical relationship between the variables over time. The changes are also minimal, at less than |1%|. 


```r
ggplot(data = t) + 
  geom_point(mapping = aes(x = period.num, y = dRW, colour = factor(NAICS))) + 
  ggtitle("Rate of Change of RW Over Time")
```

```
## Warning: Removed 19 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

Visualizing the data allows us to see that the fluctuations are relatively small across industry. 

# Rate of Change of Job Reallocation Rates Over Time
## Rate of Change for Job Reallocation Rate

```r
lmod <- lm(dadj_realloc ~ period.num, t)
summary(lmod)
```

```
## 
## Call:
## lm(formula = dadj_realloc ~ period.num, data = t)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.186400 -0.016924  0.000432  0.014377  0.306010 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -0.0186994  0.0058341  -3.205  0.00152 **
## period.num   0.0010391  0.0006202   1.675  0.09503 . 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.04077 on 264 degrees of freedom
##   (19 observations deleted due to missingness)
## Multiple R-squared:  0.01052,	Adjusted R-squared:  0.006773 
## F-statistic: 2.807 on 1 and 264 DF,  p-value: 0.09503
```
## Rate of Change for Job Gains Rate

```r
lmod <- lm(dgain ~ period.num, t)
summary(lmod)
```

```
## 
## Call:
## lm(formula = dgain ~ period.num, data = t)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.35654 -0.02717 -0.00512  0.01922  0.48424 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -0.025514   0.010218  -2.497   0.0131 *
## period.num   0.002089   0.001086   1.923   0.0555 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.07141 on 264 degrees of freedom
##   (19 observations deleted due to missingness)
## Multiple R-squared:  0.01382,	Adjusted R-squared:  0.01008 
## F-statistic:   3.7 on 1 and 264 DF,  p-value: 0.0555
```
## Rate of Change for Job Loss Rate

```r
lmod <- lm(dloss ~ period.num, t)
summary(lmod)
```

```
## 
## Call:
## lm(formula = dloss ~ period.num, data = t)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.48168 -0.03039 -0.00417  0.02145  1.14843 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept) -0.0079733  0.0166535  -0.479    0.632
## period.num   0.0003047  0.0017702   0.172    0.863
## 
## Residual standard error: 0.1164 on 264 degrees of freedom
##   (19 observations deleted due to missingness)
## Multiple R-squared:  0.0001122,	Adjusted R-squared:  -0.003675 
## F-statistic: 0.02962 on 1 and 264 DF,  p-value: 0.8635
```

For job reallocation rate and its components, it has relatively weak relationship. Nevertheless, it is interesting that the rate is mostly positive even though the rates as a whole has a negative relationship with the variables over time. 

Visualizing the data, the fluctuations in general are also small except for Mining (NAICS 21), whose values are large relative to other industries. 


```r
ggplot(data = t) + 
  geom_point(mapping = aes(x = period.num, y = dadj_realloc, colour = factor(NAICS))) + 
  ggtitle("Rate of Change of Job Reallocation Rate Over Time")
```

```
## Warning: Removed 19 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

