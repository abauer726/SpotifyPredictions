---
title: "Homework 1"
name: "Anna Bauer, Grant Cai, Alexis Navarra"
date: "Winter 2022"
output: pdf_document
---






```r
algae <- read_table2("algaeBloom.txt", col_names=
                       c('season','size','speed','mxPH','mn02','C1','NO3','NH4','cPO4','PO4','CHla','a1','a2','a3','a4','a5','a6','a7'), na = "XXXXXXX")

glimpse(algae)
```

```
## Rows: 200
## Columns: 18
## $ season <chr> "winter", "spring", "autumn", "spring", "autumn", "winter", "su~
## $ size   <chr> "small", "small", "small", "small", "small", "small", "small", ~
## $ speed  <chr> "medium", "medium", "medium", "medium", "medium", "high", "high~
## $ mxPH   <dbl> 8.00, 8.35, 8.10, 8.07, 8.06, 8.25, 8.15, 8.05, 8.70, 7.93, 7.7~
## $ mn02   <dbl> 9.8, 8.0, 11.4, 4.8, 9.0, 13.1, 10.3, 10.6, 3.4, 9.9, 10.2, 11.~
## $ C1     <dbl> 60.80, 57.75, 40.02, 77.36, 55.35, 65.75, 73.25, 59.07, 21.95, ~
## $ NO3    <dbl> 6.238, 1.288, 5.330, 2.302, 10.416, 9.248, 1.535, 4.990, 0.886,~
## $ NH4    <dbl> 578.00, 370.00, 346.67, 98.18, 233.70, 430.00, 110.00, 205.67, ~
## $ cPO4   <dbl> 105.00, 428.75, 125.67, 61.18, 58.22, 18.25, 61.25, 44.67, 36.3~
## $ PO4    <dbl> 170.00, 558.75, 187.06, 138.70, 97.58, 56.67, 111.75, 77.43, 71~
## $ CHla   <dbl> 50.000, 1.300, 15.600, 1.400, 10.500, 28.400, 3.200, 6.900, 5.5~
## $ a1     <dbl> 0.0, 1.4, 3.3, 3.1, 9.2, 15.1, 2.4, 18.2, 25.4, 17.0, 16.6, 32.~
## $ a2     <dbl> 0.0, 7.6, 53.6, 41.0, 2.9, 14.6, 1.2, 1.6, 5.4, 0.0, 0.0, 0.0, ~
## $ a3     <dbl> 0.0, 4.8, 1.9, 18.9, 7.5, 1.4, 3.2, 0.0, 2.5, 0.0, 0.0, 0.0, 2.~
## $ a4     <dbl> 0.0, 1.9, 0.0, 0.0, 0.0, 0.0, 3.9, 0.0, 0.0, 2.9, 0.0, 0.0, 0.0~
## $ a5     <dbl> 34.2, 6.7, 0.0, 1.4, 7.5, 22.5, 5.8, 5.5, 0.0, 0.0, 1.2, 0.0, 1~
## $ a6     <dbl> 8.3, 0.0, 0.0, 0.0, 4.1, 12.6, 6.8, 8.7, 0.0, 0.0, 0.0, 0.0, 0.~
## $ a7     <dbl> 0.0, 2.1, 9.7, 1.4, 1.0, 2.9, 0.0, 0.0, 0.0, 1.7, 6.0, 1.5, 2.1~
```

```r
dim(algae)
```

```
## [1] 200  18
```

1. Descriptive summary statistics (10 pts in total) Given the lack of further information on the problem domain, it is wise to investigate some of the statistical properties of the data, so as to get a better grasp of the problem. It is always a good idea to start our analysis with some kind of exploratory data analysis. A first idea of the statistical properties of the data can be obtained through a summary of its descriptive statistics.

a. Count the number of observations in each size using summarise() in dplyr.

```r
algae %>%
  group_by(size) %>%
    summarise(n = n())
```

```
## # A tibble: 3 x 2
##   size       n
##   <chr>  <int>
## 1 large     45
## 2 medium    84
## 3 small     71
```

b. Are there missing values? (2 pts) Calculate the mean and variance of each chemical (Ignore
a1 through a7). (1 pts) What do you notice about the magnitude of the two quantities for different
chemicals?


```r
summary(algae)
```

```
##     season              size              speed                mxPH     
##  Length:200         Length:200         Length:200         Min.   :5.60  
##  Class :character   Class :character   Class :character   1st Qu.:7.70  
##  Mode  :character   Mode  :character   Mode  :character   Median :8.06  
##                                                           Mean   :8.01  
##                                                           3rd Qu.:8.40  
##                                                           Max.   :9.70  
##                                                           NA's   :1     
##       mn02             C1             NO3             NH4       
##  Min.   : 1.50   Min.   :  0.2   Min.   : 0.05   Min.   :    5  
##  1st Qu.: 7.72   1st Qu.: 11.0   1st Qu.: 1.30   1st Qu.:   38  
##  Median : 9.80   Median : 32.7   Median : 2.67   Median :  103  
##  Mean   : 9.12   Mean   : 43.6   Mean   : 3.28   Mean   :  501  
##  3rd Qu.:10.80   3rd Qu.: 57.8   3rd Qu.: 4.45   3rd Qu.:  227  
##  Max.   :13.40   Max.   :391.5   Max.   :45.65   Max.   :24064  
##  NA's   :2       NA's   :10      NA's   :2       NA's   :2      
##       cPO4            PO4             CHla              a1       
##  Min.   :  1.0   Min.   :  1.0   Min.   :  0.20   Min.   : 0.00  
##  1st Qu.: 15.7   1st Qu.: 41.4   1st Qu.:  2.00   1st Qu.: 1.50  
##  Median : 40.1   Median :103.3   Median :  5.47   Median : 6.95  
##  Mean   : 73.6   Mean   :137.9   Mean   : 13.97   Mean   :16.92  
##  3rd Qu.: 99.3   3rd Qu.:213.8   3rd Qu.: 18.31   3rd Qu.:24.80  
##  Max.   :564.6   Max.   :771.6   Max.   :110.46   Max.   :89.80  
##  NA's   :2       NA's   :2       NA's   :12                      
##        a2              a3              a4              a5       
##  Min.   : 0.00   Min.   : 0.00   Min.   : 0.00   Min.   : 0.00  
##  1st Qu.: 0.00   1st Qu.: 0.00   1st Qu.: 0.00   1st Qu.: 0.00  
##  Median : 3.00   Median : 1.55   Median : 0.00   Median : 1.90  
##  Mean   : 7.46   Mean   : 4.31   Mean   : 1.99   Mean   : 5.06  
##  3rd Qu.:11.38   3rd Qu.: 4.92   3rd Qu.: 2.40   3rd Qu.: 7.50  
##  Max.   :72.60   Max.   :42.80   Max.   :44.60   Max.   :44.40  
##                                                                 
##        a6              a7      
##  Min.   : 0.00   Min.   : 0.0  
##  1st Qu.: 0.00   1st Qu.: 0.0  
##  Median : 0.00   Median : 1.0  
##  Mean   : 5.96   Mean   : 2.5  
##  3rd Qu.: 6.92   3rd Qu.: 2.4  
##  Max.   :77.60   Max.   :31.6  
## 
```
From the summary table above, we can see the number of missing values in each column. The below code chunk calculates the mean and the variance while accounting for the missing values in each of the chemical columns.

```r
mean_col <- cbind(
  lapply(algae[4:11], FUN = mean, na.rm = T)
)
var_col <- cbind(
  lapply(algae[4:11], FUN = var, na.rm = T)
)

knitr::kable(list(mean_col))
```

\begin{table}

\centering
\begin{tabular}[t]{l|l}
\hline
mxPH & 8.01173366834171\\
\hline
mn02 & 9.11777777777778\\
\hline
C1 & 43.6362788421053\\
\hline
NO3 & 3.28238888888889\\
\hline
NH4 & 501.295828383838\\
\hline
cPO4 & 73.590595959596\\
\hline
PO4 & 137.882100959596\\
\hline
CHla & 13.9711968085106\\
\hline
\end{tabular}
\end{table}

```r
knitr::kable(list(var_col))
```

\begin{table}

\centering
\begin{tabular}[t]{l|l}
\hline
mxPH & 0.357969327699102\\
\hline
mn02 & 5.71808945290468\\
\hline
C1 & 2193.17172527517\\
\hline
NO3 & 14.2617564622109\\
\hline
NH4 & 3851584.68486516\\
\hline
cPO4 & 8305.84993021472\\
\hline
PO4 & 16639.3845452802\\
\hline
CHla & 420.082734939669\\
\hline
\end{tabular}
\end{table}
We notice when making this calculation that some of the variance values are abnormally large (in particular, for chemicals C1, NH4, cPO4, and PO4). This indicates a large spread in the data points for these chemcials.

c. Mean and Variance is one measure of central tendency and spread of data. Median and Median
Absolute Deviation are alternative measures of central tendency and spread.
For a univariate data set X1,X2, ...,Xn, the Median Absolute Deviation (MAD) is defined as the
median of the absolute deviations from the data’s median:
$$MAD = median(|X_i ≠ median(X)|)$$
(3 pts) Compute median and MAD of each chemical and compare the two sets of quantities (i.e.,
mean & variance vs. median & MAD). (1 pts) What do you notice?

```r
median_col <- cbind(
  lapply(algae[4:11], FUN = median, na.rm = T)
)
mad_col <- cbind(
  lapply(algae[4:11], FUN = mad, na.rm = T)
)

knitr::kable(list(median_col))
```

\begin{table}

\centering
\begin{tabular}[t]{l|l}
\hline
mxPH & 8.06\\
\hline
mn02 & 9.8\\
\hline
C1 & 32.73\\
\hline
NO3 & 2.675\\
\hline
NH4 & 103.1665\\
\hline
cPO4 & 40.15\\
\hline
PO4 & 103.2855\\
\hline
CHla & 5.475\\
\hline
\end{tabular}
\end{table}

```r
knitr::kable(list(mad_col))
```

\begin{table}

\centering
\begin{tabular}[t]{l|l}
\hline
mxPH & 0.504084\\
\hline
mn02 & 2.053401\\
\hline
C1 & 33.2495289\\
\hline
NO3 & 2.172009\\
\hline
NH4 & 111.617548413\\
\hline
cPO4 & 44.0458221\\
\hline
PO4 & 122.3211717\\
\hline
CHla & 6.6717\\
\hline
\end{tabular}
\end{table}
The above code chunk calculates the median and MAD values of each chemical. We notice from these 

2. Data visualization (8 pts in total) Most of the time, the information in the data set is also well
captured graphically. Histogram, scatter plot, boxplot, Q-Q plot are frequently used tools for data
visualization. Use ggplot for all of these visualizations.

a. (2 pts) Produce a histogram of mnO2 with the title ‘Histogram of mnO2’ based on algae data
set. (1 pts) Use an appropriate argument to show the probability instead of the frequency as the
vertical axis. (Hint: look at the examples in the help file for function geom_histogram()). (1 pts)
Is the distribution skewed?

```r
algae %>%
  ggplot(aes(mn02)) + ggtitle("Histogram of mn02") +
  geom_histogram(aes(mn02))
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 2 rows containing non-finite values (stat_bin).
```

![](homework-1-131_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 

b. (1 pts) Add a density curve using geom_density() and (1 pts) rug plots using geom_rug() to
above histogram.


c. (1 pts) Create a boxplot with the title ‘A conditioned Boxplot of Algal a3’ for a3 grouped by speed.
(Refer to help page for geom_boxplot()). (1 pts) What do you notice?



3. Dealing with missing values (8 pts in total)

a. (2 pts) How many observations contain missing values? (2 pts) How many missing values are there
in each variable?

```r
# Counting how many rows contain missing values
sum(apply(algae, MARGIN = 1, anyNA))
```

```
## [1] 16
```

```r
#Counting NA values for each variable/column
apply(X = is.na(algae), MARGIN = 2, FUN = sum)
```

```
## season   size  speed   mxPH   mn02     C1    NO3    NH4   cPO4    PO4   CHla 
##      0      0      0      1      2     10      2      2      2      2     12 
##     a1     a2     a3     a4     a5     a6     a7 
##      0      0      0      0      0      0      0
```

b. (3 pts) Removing observations with missing values: use filter() function in dplyr package
to observations with any missing value, and save the resulting dataset (without missing values) as
algae.del. (1 pts) Report how many observations are in algae.del.

```r
algae.del <- algae %>% filter(complete.cases(.))
nrow(algae.del)
```

```
## [1] 184
```


4. In lecture we present the bias-variance tradeoff that takes the form 
$$\mathbb{E}(y_0-\hat{f}(x_0))^2 = \text{Var}(\hat{f}(x_0))+[\text{Bias}(\hat{f}(x_0))]^2+\text{Var}(\epsilon)$$
where the underlying model $$Y = f(X) + \epsilon$$ satisifes: (1) $$\epsilon$$ is a zero-mean random noise, and X is
non-random (all randomness in Y comes from $$\epsilon$$); (2) $$(x_0, y_0)$$ is a test observation, independent of the
training set, and drawn from the same model; (3) $$\hat{f(\cdot)}$$ is the estimate of f obtained on a training set.
>>>>>>> 5e24e526849e38a9d2ad33efe50da32d603c78ae

a. (2 pts) Which of the term(s) in the bias-variance tradeoff above represent the reducible error? (2
pts) Which term(s) represent the irreducible error?

$$\text{Var}(\hat{f}(x_0))+[\text{Bias}(\hat{f}(x_0))]^2$$ is the reducible error. $$Var(\epsilon)$$ represents the irreducible error.

b. (4 pts) Use the bias-variance tradeoff above to show that the expected test error is always at least
as large as the irreducible error.

$$\text{Var}(\hat{f}(x_0))$$ is always non-negative, and so is the square of $$text{Bias}(\hat{f}(x_0))$$. As a result, the expected test MSE is always at least as large as the irreducible error, $$\text{Var}(\epsilon)$$. (Maybe needs more explanation about why Var is non-negative).
