crop
================
Kar
2021

-   [1 R PACKAGES LOADED](#1-r-packages-loaded)
-   [2 INTRODUCTION](#2-introduction)
-   [3 DATA IMPORT AND CLEANING](#3-data-import-and-cleaning)
    -   [3.1 Data import](#31-data-import)
    -   [3.2 Data exploration](#32-data-exploration)
    -   [3.3 Data manipulation](#33-data-manipulation)
-   [4 Exploratory Data Analysis](#4-exploratory-data-analysis)
-   [5 Model Building](#5-model-building)
-   [6 Model for Production](#6-model-for-production)
-   [Conclusion](#conclusion)

# 1 R PACKAGES LOADED

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(skimr)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

# 2 INTRODUCTION

# 3 DATA IMPORT AND CLEANING

This project uses a public crop dataset in kaggle.com
[Link](https://www.kaggle.com/atharvaingle/crop-recommendation-dataset),
named “Crop Recommendation Dataset” by Artharva Ingle.

## 3.1 Data import

The data set is uploaded into R using following code, and the table
indicates a successful import.

``` r
crop <- read.csv("Crop_recom.csv",
                 fileEncoding = "UTF-8-BOM")
```

The table shows soil nutrients, N, P, and K, as well as temperature,
humidity, pH, rainfall, and label. There are no unit for N, P, and K and
the values are in term of N-P-K ratio in the soil.

The dataset has following descriptions, adapted from the Kaggle website.

``` r
variables <- c("N", "P", "K", "temperature", "humidity", "ph", "rainfall", "label")

description <- c("N-P-K Ratio of Nitrogen (N) portion in soil",
                 "N-P-K Ratio of Phosphorus (P) portion in soil",
                 "N-P-K Ratio of Potassium (K) portion in soil",
                 "temperature in degree Celsius, oC",
                 "relative humidity in %",
                 "Soil pH value",
                 "rainfall in mm",
                 "Different crops")

data.frame(variables, description) %>% 
  kbl() %>% 
  kable_styling(bootstrap_options = c("hover", "stripped", "bordered"))
```

<table class="table table-hover table-bordered" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
variables
</th>
<th style="text-align:left;">
description
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
N
</td>
<td style="text-align:left;">
N-P-K Ratio of Nitrogen (N) portion in soil
</td>
</tr>
<tr>
<td style="text-align:left;">
P
</td>
<td style="text-align:left;">
N-P-K Ratio of Phosphorus (P) portion in soil
</td>
</tr>
<tr>
<td style="text-align:left;">
K
</td>
<td style="text-align:left;">
N-P-K Ratio of Potassium (K) portion in soil
</td>
</tr>
<tr>
<td style="text-align:left;">
temperature
</td>
<td style="text-align:left;">
temperature in degree Celsius, oC
</td>
</tr>
<tr>
<td style="text-align:left;">
humidity
</td>
<td style="text-align:left;">
relative humidity in %
</td>
</tr>
<tr>
<td style="text-align:left;">
ph
</td>
<td style="text-align:left;">
Soil pH value
</td>
</tr>
<tr>
<td style="text-align:left;">
rainfall
</td>
<td style="text-align:left;">
rainfall in mm
</td>
</tr>
<tr>
<td style="text-align:left;">
label
</td>
<td style="text-align:left;">
Different crops
</td>
</tr>
</tbody>
</table>

## 3.2 Data exploration

This data has 2200 rows of observations and 8 columns of variables.

-   The “label” is labelled as a character variable.

-   And the rest are numerical variables.

-   The data set has obviously been cleaned and having no missing values
    that needs to be managed by looking at the **complete\_rate** and
    the associated column **n\_missing** that used to detect missing
    values in the data frame.

``` r
skim_without_charts(crop)
```

<table style="width: auto;" class="table table-condensed">
<caption>
Data summary
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Name
</td>
<td style="text-align:left;">
crop
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
2200
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Column type frequency:
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Group variables
</td>
<td style="text-align:left;">
None
</td>
</tr>
</tbody>
</table>

**Variable type: character**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim\_variable
</th>
<th style="text-align:right;">
n\_missing
</th>
<th style="text-align:right;">
complete\_rate
</th>
<th style="text-align:right;">
min
</th>
<th style="text-align:right;">
max
</th>
<th style="text-align:right;">
empty
</th>
<th style="text-align:right;">
n\_unique
</th>
<th style="text-align:right;">
whitespace
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
label
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

**Variable type: numeric**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim\_variable
</th>
<th style="text-align:right;">
n\_missing
</th>
<th style="text-align:right;">
complete\_rate
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:right;">
sd
</th>
<th style="text-align:right;">
p0
</th>
<th style="text-align:right;">
p25
</th>
<th style="text-align:right;">
p50
</th>
<th style="text-align:right;">
p75
</th>
<th style="text-align:right;">
p100
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
N
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
50.55
</td>
<td style="text-align:right;">
36.92
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
21.00
</td>
<td style="text-align:right;">
37.00
</td>
<td style="text-align:right;">
84.25
</td>
<td style="text-align:right;">
140.00
</td>
</tr>
<tr>
<td style="text-align:left;">
P
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
53.36
</td>
<td style="text-align:right;">
32.99
</td>
<td style="text-align:right;">
5.00
</td>
<td style="text-align:right;">
28.00
</td>
<td style="text-align:right;">
51.00
</td>
<td style="text-align:right;">
68.00
</td>
<td style="text-align:right;">
145.00
</td>
</tr>
<tr>
<td style="text-align:left;">
K
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
48.15
</td>
<td style="text-align:right;">
50.65
</td>
<td style="text-align:right;">
5.00
</td>
<td style="text-align:right;">
20.00
</td>
<td style="text-align:right;">
32.00
</td>
<td style="text-align:right;">
49.00
</td>
<td style="text-align:right;">
205.00
</td>
</tr>
<tr>
<td style="text-align:left;">
temperature
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
25.62
</td>
<td style="text-align:right;">
5.06
</td>
<td style="text-align:right;">
8.83
</td>
<td style="text-align:right;">
22.77
</td>
<td style="text-align:right;">
25.60
</td>
<td style="text-align:right;">
28.56
</td>
<td style="text-align:right;">
43.68
</td>
</tr>
<tr>
<td style="text-align:left;">
humidity
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
71.48
</td>
<td style="text-align:right;">
22.26
</td>
<td style="text-align:right;">
14.26
</td>
<td style="text-align:right;">
60.26
</td>
<td style="text-align:right;">
80.47
</td>
<td style="text-align:right;">
89.95
</td>
<td style="text-align:right;">
99.98
</td>
</tr>
<tr>
<td style="text-align:left;">
ph
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
6.47
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
3.50
</td>
<td style="text-align:right;">
5.97
</td>
<td style="text-align:right;">
6.43
</td>
<td style="text-align:right;">
6.92
</td>
<td style="text-align:right;">
9.94
</td>
</tr>
<tr>
<td style="text-align:left;">
rainfall
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
103.46
</td>
<td style="text-align:right;">
54.96
</td>
<td style="text-align:right;">
20.21
</td>
<td style="text-align:right;">
64.55
</td>
<td style="text-align:right;">
94.87
</td>
<td style="text-align:right;">
124.27
</td>
<td style="text-align:right;">
298.56
</td>
</tr>
</tbody>
</table>

Numerical variables either labelled as “int”, “num”, or “dbl” in R,
which the last “dbl” is usually used to labeled floating numbers.

``` r
str(crop)
```

    ## 'data.frame':    2200 obs. of  8 variables:
    ##  $ N          : int  90 85 60 74 78 69 69 94 89 68 ...
    ##  $ P          : int  42 58 55 35 42 37 55 53 54 58 ...
    ##  $ K          : int  43 41 44 40 42 42 38 40 38 38 ...
    ##  $ temperature: num  20.9 21.8 23 26.5 20.1 ...
    ##  $ humidity   : num  82 80.3 82.3 80.2 81.6 ...
    ##  $ ph         : num  6.5 7.04 7.84 6.98 7.63 ...
    ##  $ rainfall   : num  203 227 264 243 263 ...
    ##  $ label      : chr  "rice" "rice" "rice" "rice" ...

## 3.3 Data manipulation

**Convert ”label“ to factor**

To quickly examine what are the levels within the variable “label”,
which is important as it tells the type of crops within this dataset, I
will convert the variable “label” from character into factor. It will
help examination using R functions and later analysis.

``` r
# To protect the original dataset I will hereby create a new object named "crop2" to carry the cleaned data set from this project.

crop2 <- crop %>% 
  mutate(label = as.factor(label))
```

Following result shows that there are 22 crops in this data set. Each
crop has 100 samples, which is excellent as sample sizes are equal and
will make statistical comparison fairer.

``` r
summary(crop2$label)
```

    ##       apple      banana   blackgram    chickpea     coconut      coffee 
    ##         100         100         100         100         100         100 
    ##      cotton      grapes        jute kidneybeans      lentil       maize 
    ##         100         100         100         100         100         100 
    ##       mango   mothbeans    mungbean   muskmelon      orange      papaya 
    ##         100         100         100         100         100         100 
    ##  pigeonpeas pomegranate        rice  watermelon 
    ##         100         100         100         100

**Reduce decimal places**

This step is optional but I decided to make it happened. I will restrict
the decimal places of “temperature”, “humidity”, “pH”, and “rainfall” to
only one, as this length of decimal places is sufficient.

``` r
crop2 <- crop2 %>% 
  mutate(temperature = round(temperature, 1),
         humidity = round(humidity, 1),
         ph = round(ph, 1),
         rainfall = round(rainfall, 1))
```

Following shows statistics of all the columns within the dataset such as
minimum, maximum, median, and mean. It also shows the number of samples
if the column is in factor format, such as the “label” that I have
converted it from character format.

``` r
summary(crop2)
```

    ##        N                P                K           temperature   
    ##  Min.   :  0.00   Min.   :  5.00   Min.   :  5.00   Min.   : 8.80  
    ##  1st Qu.: 21.00   1st Qu.: 28.00   1st Qu.: 20.00   1st Qu.:22.80  
    ##  Median : 37.00   Median : 51.00   Median : 32.00   Median :25.60  
    ##  Mean   : 50.55   Mean   : 53.36   Mean   : 48.15   Mean   :25.62  
    ##  3rd Qu.: 84.25   3rd Qu.: 68.00   3rd Qu.: 49.00   3rd Qu.:28.60  
    ##  Max.   :140.00   Max.   :145.00   Max.   :205.00   Max.   :43.70  
    ##                                                                    
    ##     humidity            ph          rainfall            label     
    ##  Min.   : 14.30   Min.   :3.50   Min.   : 20.20   apple    : 100  
    ##  1st Qu.: 60.27   1st Qu.:6.00   1st Qu.: 64.58   banana   : 100  
    ##  Median : 80.50   Median :6.40   Median : 94.90   blackgram: 100  
    ##  Mean   : 71.48   Mean   :6.47   Mean   :103.46   chickpea : 100  
    ##  3rd Qu.: 89.92   3rd Qu.:6.90   3rd Qu.:124.25   coconut  : 100  
    ##  Max.   :100.00   Max.   :9.90   Max.   :298.60   coffee   : 100  
    ##                                                   (Other)  :1600

# 4 Exploratory Data Analysis

# 5 Model Building

# 6 Model for Production

# Conclusion
