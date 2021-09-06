Agriculture Machine Learning (Classification Prediction)
================
Kar
2021

-   [1 R PACKAGES](#1-r-packages)
-   [2 INTRODUCTION](#2-introduction)
-   [3 DATA IMPORT AND CLEANING](#3-data-import-and-cleaning)
    -   [3.1 Data import](#31-data-import)
    -   [3.2 Data exploration](#32-data-exploration)
    -   [3.3 Data manipulation](#33-data-manipulation)
-   [4 Exploratory Data Analysis](#4-exploratory-data-analysis)
    -   [4.1 Histograms](#41-histograms)
    -   [4.2 Correlogram](#42-correlogram)
    -   [4.3 Scatter plot](#43-scatter-plot)
    -   [4.4 Boxplots](#44-boxplots)
-   [5 Model Building](#5-model-building)
    -   [5.1 Variables Selection](#51-variables-selection)
    -   [5.2 Train-Test Split](#52-train-test-split)
    -   [5.3 Model building](#53-model-building)
-   [6 Model for Production](#6-model-for-production)
-   [Conclusion](#conclusion)

# 1 R PACKAGES

``` r
library(tidyverse)
library(skimr)
library(kableExtra)
library(corrplot)
library(caret)
```

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

## 4.1 Histograms

It will be interesting to visualise the distribution of each numerical
variables in the data set as a initial examination.

``` r
# set up data frame for this section

df4.1 <- crop2 %>% 
  pivot_longer(c(1:7),
               values_to = "result",
               names_to = "variables") %>% 
  mutate(variables = factor(variables, levels = c("N",
                                                     "P",
                                                     "K",
                                                     "temperature",
                                                     "humidity",
                                                     "ph",
                                                     "rainfall"))) 



# plot

ggplot(df4.1, aes(x = result, fill = variables)) +
  geom_histogram(colour = "white") +
  facet_wrap(~variables, scales = "free_x") +
  labs(x = "Variables",
       title = "Distribution of Numerical Variables") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")) 
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](crop_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> Insights:

-   Distributions of N, P, and K are quite wide spread which might be
    due to the presence of different soil types contributing to this
    dataset. It indicating that they can be counding variables in this
    dataset.

-   The majority of temperatures are spread between 20 - 30oC in this
    dataset.

-   Humidity is quite wide spread with majority fall between 75 - 100%.

-   pH and in soil are most slightly acidic with a value around 6.

-   Rainfalls in this entire dataset are less than 300mm, with majority
    fall between 50 - 120mm.

## 4.2 Correlogram

Following correlogram (a plot for correlation) shows that variables are
independent from each other except P and K.

There is a strong relationship between P and K with a correlation of
0.74. It might be an issue during model building however VIF will be
checked again in that section to avoid multicollinearity. Though a
correlation less than 0.8 between two variables should be indicating
that multicollinearity might not exist, but VIF will be computed.

``` r
# convert into matrix, remove the factor "label" 

cor_c <- cor(crop2[, 1:7])

# correlogram

corrplot(cor_c, method = "number", type = "upper")
```

![](crop_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## 4.3 Scatter plot

Plotting the relationship between P and K.

``` r
ggplot(crop2, aes(x = P, y = K, colour = label)) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  labs(title = "Positive Relationship betweem P and K") 
```

![](crop_files/figure-gfm/unnamed-chunk-12-1.png)<!-- --> From the
graph, there might be a positive relationship between K and P, however
the high value of correlation 0.74 might be due to the type to crops and
especially the crop groups near 150.

## 4.4 Boxplots

This section uses boxplot to simultaneously compare the 7 numerical
variables among different crops stored within the vector, “label”.
Recall, the 7 numerical variables are:

``` r
names(crop2[1:7])
```

    ## [1] "N"           "P"           "K"           "temperature" "humidity"   
    ## [6] "ph"          "rainfall"

``` r
# set up data frame

df4.3 <- crop2 %>% 
  pivot_longer(c(1:7),
               values_to = "result",
               names_to = "variables") %>% 
  mutate(variables = factor(variables, levels = c("N",
                                                     "P",
                                                     "K",
                                                     "temperature",
                                                     "humidity",
                                                     "ph",
                                                     "rainfall"))) 

# plot boxplots

ggplot(df4.3, aes(x = label, y = result, colour = label)) +
  geom_boxplot() +
  facet_wrap(~variables, scale = "free", ncol =1) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2))
```

![](crop_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

# 5 Model Building

The purpose of modeling of this project is to build an effective
predictive model to suggest the most appropriate crops to grow in a
particular farm based on various parameter.

## 5.1 Variables Selection

This section aims to select relevant and confounding variables during
model building. According to the purpose of this project all of these
variables are agricultural-wise important and relevant in determining
the type of crops to grow in a particular farm.

``` r
crop2
```

    ##        N   P   K temperature humidity  ph rainfall       label
    ## 1     90  42  43        20.9     82.0 6.5    202.9        rice
    ## 2     85  58  41        21.8     80.3 7.0    226.7        rice
    ## 3     60  55  44        23.0     82.3 7.8    264.0        rice
    ## 4     74  35  40        26.5     80.2 7.0    242.9        rice
    ## 5     78  42  42        20.1     81.6 7.6    262.7        rice
    ## 6     69  37  42        23.1     83.4 7.1    251.1        rice
    ## 7     69  55  38        22.7     82.6 5.7    271.3        rice
    ## 8     94  53  40        20.3     82.9 5.7    242.0        rice
    ## 9     89  54  38        24.5     83.5 6.7    230.4        rice
    ## 10    68  58  38        23.2     83.0 6.3    221.2        rice
    ## 11    91  53  40        26.5     81.4 5.4    264.6        rice
    ## 12    90  46  42        24.0     81.5 7.5    250.1        rice
    ## 13    78  58  44        26.8     80.9 5.1    284.4        rice
    ## 14    93  56  36        24.0     82.1 7.0    185.3        rice
    ## 15    94  50  37        25.7     80.7 6.9    209.6        rice
    ## 16    60  48  39        24.3     80.3 7.0    231.1        rice
    ## 17    85  38  41        21.6     82.8 6.2    276.7        rice
    ## 18    91  35  39        23.8     80.4 7.0    206.3        rice
    ## 19    77  38  36        21.9     80.2 6.0    224.6        rice
    ## 20    88  35  40        23.6     83.6 5.9    291.3        rice
    ## 21    89  45  36        21.3     80.5 6.4    185.5        rice
    ## 22    76  40  43        25.2     83.1 5.1    231.4        rice
    ## 23    67  59  41        21.9     81.0 6.0    213.4        rice
    ## 24    83  41  43        21.1     82.7 6.3    233.1        rice
    ## 25    98  47  37        23.5     81.3 7.4    224.1        rice
    ## 26    66  53  41        25.1     80.5 7.8    257.0        rice
    ## 27    97  59  43        26.4     84.0 6.3    271.4        rice
    ## 28    97  50  41        24.5     80.5 7.1    260.3        rice
    ## 29    60  49  44        20.8     84.5 6.2    240.1        rice
    ## 30    84  51  35        22.3     80.6 6.0    198.0        rice
    ## 31    73  57  41        21.4     84.9 5.8    272.2        rice
    ## 32    92  35  40        22.2     80.3 6.4    200.1        rice
    ## 33    85  37  39        24.5     82.7 6.4    224.7        rice
    ## 34    98  53  38        20.3     81.6 5.0    270.4        rice
    ## 35    88  54  44        25.7     83.9 6.1    233.1        rice
    ## 36    95  55  42        26.8     82.1 6.0    193.3        rice
    ## 37    99  57  35        26.8     81.2 6.0    272.3        rice
    ## 38    95  39  36        23.9     83.2 5.6    285.2        rice
    ## 39    60  43  44        21.0     83.0 7.4    298.4        rice
    ## 40    63  44  41        24.2     83.7 5.6    257.0        rice
    ## 41    62  42  36        22.8     82.1 6.4    248.7        rice
    ## 42    64  45  43        25.6     83.5 5.5    209.9        rice
    ## 43    83  60  36        25.6     80.1 6.9    200.8        rice
    ## 44    82  40  40        23.8     84.8 6.3    298.6        rice
    ## 45    85  52  45        26.3     82.4 7.2    265.5        rice
    ## 46    91  35  38        24.9     80.5 6.1    183.7        rice
    ## 47    76  49  42        25.0     84.5 5.2    197.0        rice
    ## 48    74  39  38        23.2     84.6 7.8    233.0        rice
    ## 49    79  43  39        21.7     80.7 7.1    210.8        rice
    ## 50    88  55  45        24.6     80.4 7.7    253.7        rice
    ## 51    60  36  43        23.4     83.1 5.3    219.9        rice
    ## 52    76  60  39        20.0     80.3 6.8    208.6        rice
    ## 53    93  56  42        23.9     82.2 7.4    195.1        rice
    ## 54    65  60  43        22.0     81.9 5.7    227.4        rice
    ## 55    95  52  36        26.2     83.8 5.5    286.5        rice
    ## 56    75  38  39        23.4     84.8 6.2    283.9        rice
    ## 57    74  54  38        25.7     83.5 7.1    217.4        rice
    ## 58    91  36  45        24.4     82.5 6.0    268.0        rice
    ## 59    71  46  40        20.3     82.1 7.2    192.0        rice
    ## 60    99  55  35        21.7     80.2 6.5    278.0        rice
    ## 61    72  40  38        20.4     82.2 7.6    245.2        rice
    ## 62    83  58  45        25.8     83.5 5.9    245.7        rice
    ## 63    93  58  38        20.6     83.8 6.9    279.5        rice
    ## 64    70  36  42        21.8     80.7 6.9    202.4        rice
    ## 65    76  47  42        20.1     83.3 5.7    263.6        rice
    ## 66    99  41  36        24.5     82.7 6.7    182.6        rice
    ## 67    99  54  37        21.1     80.3 5.6    198.7        rice
    ## 68    86  59  35        25.8     82.1 6.9    243.5        rice
    ## 69    69  46  41        23.6     80.3 5.0    263.1        rice
    ## 70    91  56  37        23.4     80.6 6.4    269.5        rice
    ## 71    61  52  41        25.0     83.9 6.9    204.8        rice
    ## 72    67  45  38        22.7     82.2 7.3    260.9        rice
    ## 73    79  42  37        24.9     82.8 6.6    295.6        rice
    ## 74    78  43  42        21.3     83.0 7.3    192.3        rice
    ## 75    75  54  36        26.3     84.6 7.0    257.5        rice
    ## 76    97  36  45        22.2     81.9 6.9    278.1        rice
    ## 77    67  47  44        26.7     81.8 7.9    280.4        rice
    ## 78    73  35  38        24.9     82.0 5.0    185.9        rice
    ## 79    77  36  37        26.9     81.5 6.1    194.6        rice
    ## 80    81  41  38        22.7     83.7 7.5    200.9        rice
    ## 81    68  57  43        26.1     80.4 5.7    182.9        rice
    ## 82    72  45  35        25.4     82.9 5.8    195.4        rice
    ## 83    61  53  43        26.4     81.1 6.3    223.4        rice
    ## 84    67  43  39        26.0     85.0 6.0    186.8        rice
    ## 85    67  58  39        25.3     80.5 5.5    220.1        rice
    ## 86    66  60  38        22.1     83.5 6.4    231.7        rice
    ## 87    82  43  38        23.3     81.4 5.1    242.3        rice
    ## 88    84  50  44        25.5     81.4 5.9    182.7        rice
    ## 89    81  53  42        23.7     81.0 5.2    233.7        rice
    ## 90    91  50  40        20.8     84.1 6.5    230.2        rice
    ## 91    93  53  38        26.9     81.9 7.1    290.7        rice
    ## 92    90  44  38        23.8     83.9 7.5    241.2        rice
    ## 93    81  45  35        26.5     80.1 6.2    218.9        rice
    ## 94    78  40  38        26.5     83.9 7.5    248.2        rice
    ## 95    60  51  36        22.7     82.8 6.0    257.0        rice
    ## 96    88  46  42        22.7     83.5 6.6    194.3        rice
    ## 97    93  47  37        21.5     82.1 6.5    295.9        rice
    ## 98    60  55  45        21.4     83.3 5.9    287.6        rice
    ## 99    78  35  44        26.5     84.7 7.1    183.6        rice
    ## 100   65  37  40        23.4     83.6 5.3    188.4        rice
    ## 101   71  54  16        22.6     63.7 5.7     87.8       maize
    ## 102   61  44  17        26.1     71.6 6.9    102.3       maize
    ## 103   80  43  16        23.6     71.6 6.7     66.7       maize
    ## 104   73  58  21        20.0     57.7 6.6     60.7       maize
    ## 105   61  38  20        18.5     62.7 6.0     65.4       maize
    ## 106   68  41  16        21.8     57.8 6.2    102.1       maize
    ## 107   93  41  17        25.6     66.5 6.0    105.5       maize
    ## 108   89  60  19        25.2     66.7 5.9     78.1       maize
    ## 109   76  44  17        20.4     62.6 5.9     65.3       maize
    ## 110   67  60  25        24.9     66.8 5.8    109.2       maize
    ## 111   70  44  19        23.3     73.5 5.9     94.3       maize
    ## 112   90  49  21        24.8     68.4 6.5     74.1       maize
    ## 113   62  52  16        22.3     58.8 7.0     63.9       maize
    ## 114   92  44  16        18.9     65.8 6.1     94.8       maize
    ## 115   66  54  21        25.2     60.2 5.9     72.1       maize
    ## 116   63  58  22        18.3     55.3 6.2     63.7       maize
    ## 117   70  47  17        24.6     70.4 6.6    104.2       maize
    ## 118   61  41  17        25.1     65.3 6.0     76.7       maize
    ## 119   66  53  19        23.1     60.1 6.0     65.5       maize
    ## 120   74  55  19        18.1     62.9 6.3     84.2       maize
    ## 121   77  57  21        24.9     73.8 6.6     79.7       maize
    ## 122   99  50  15        18.1     71.1 5.6     88.1       maize
    ## 123   74  56  22        18.3     66.7 6.8     81.0       maize
    ## 124   83  45  21        18.8     58.8 5.7     79.8       maize
    ## 125  100  48  16        25.7     67.2 5.5     74.5       maize
    ## 126   79  51  16        25.3     68.5 6.6     96.5       maize
    ## 127   94  39  18        23.9     57.5 5.9    102.8       maize
    ## 128   75  49  15        21.5     71.5 5.9    102.5       maize
    ## 129   78  48  22        23.1     63.1 5.6     70.4       maize
    ## 130   87  54  20        25.6     63.5 6.6    108.8       maize
    ## 131   87  35  25        21.4     63.2 6.2     65.9       maize
    ## 132   63  43  19        18.5     55.5 6.6     91.0       maize
    ## 133   84  57  25        22.5     68.0 6.5     64.4       maize
    ## 134   64  35  23        23.0     61.9 5.7     63.0       maize
    ## 135   60  46  22        24.9     65.6 6.6     87.9       maize
    ## 136   98  44  21        25.8     74.1 6.5    107.5       maize
    ## 137   75  56  18        19.4     62.4 5.7     61.0       maize
    ## 138   86  55  21        21.5     59.6 6.8    109.8       maize
    ## 139   98  35  18        23.8     74.8 6.3     91.8       maize
    ## 140   76  57  18        19.0     74.5 6.1     94.3       maize
    ## 141   99  56  17        24.1     73.1 6.2     71.1       maize
    ## 142   60  44  23        24.8     70.0 5.7     76.7       maize
    ## 143   74  48  17        21.6     60.3 6.4     69.2       maize
    ## 144   89  60  17        25.4     57.2 6.0    101.7       maize
    ## 145   69  51  23        22.2     72.9 6.8    106.6       maize
    ## 146   96  46  22        20.6     69.0 6.5     66.3       maize
    ## 147   61  60  15        24.9     68.7 6.3     91.3       maize
    ## 148   74  58  18        20.0     56.4 6.7    109.0       maize
    ## 149   74  43  23        26.0     61.9 6.3     99.6       maize
    ## 150   63  43  17        19.3     65.5 6.8     71.3       maize
    ## 151   99  36  20        20.6     65.3 6.7     78.3       maize
    ## 152   77  36  23        24.7     56.7 6.6     88.5       maize
    ## 153   87  60  23        20.3     63.9 6.4     62.5       maize
    ## 154   60  38  17        18.4     64.2 6.5     76.4       maize
    ## 155   94  54  17        23.4     61.7 5.9    107.3       maize
    ## 156   95  38  22        19.8     61.2 5.7    100.8       maize
    ## 157   84  44  21        21.9     61.9 5.9    107.3       maize
    ## 158   77  58  19        22.8     56.5 5.8    101.6       maize
    ## 159   66  44  20        19.1     69.0 6.7     80.7       maize
    ## 160   63  35  16        22.0     65.4 6.3     83.7       maize
    ## 161   79  45  20        23.8     59.2 5.7     90.0       maize
    ## 162   72  60  25        18.5     69.0 5.8     88.1       maize
    ## 163   67  51  24        23.5     61.3 5.6     64.8       maize
    ## 164   86  36  24        26.5     72.9 5.8     73.3       maize
    ## 165   76  48  18        19.3     69.6 5.8     83.2       maize
    ## 166   75  53  18        20.7     59.4 6.9    103.7       maize
    ## 167   81  45  23        19.3     68.0 6.2     84.2       maize
    ## 168   73  45  21        24.6     73.6 6.6     96.6       maize
    ## 169   71  35  24        22.3     59.5 5.8     68.0       maize
    ## 170   96  54  22        25.7     61.3 7.0     83.2       maize
    ## 171   99  39  18        19.2     68.3 6.1     87.9       maize
    ## 172   62  48  20        21.7     60.5 6.7     95.7       maize
    ## 173   86  37  16        20.5     59.2 5.6     67.6       maize
    ## 174   94  50  19        23.3     73.6 5.9     97.6       maize
    ## 175   76  39  24        24.3     55.6 7.0     64.2       maize
    ## 176   77  52  17        24.9     65.7 5.7     75.8       maize
    ## 177   74  39  23        22.6     65.8 6.8     88.2       maize
    ## 178   81  49  20        18.0     60.6 5.5    104.2       maize
    ## 179   63  42  21        23.3     72.3 5.8     67.1       maize
    ## 180   99  38  21        22.9     71.6 6.4     67.7       maize
    ## 181   90  52  25        26.0     69.4 6.8    103.2       maize
    ## 182   68  40  19        26.1     66.2 6.7    107.2       maize
    ## 183   60  57  24        18.7     61.6 6.1     75.0       maize
    ## 184   71  52  18        25.1     56.0 5.8     78.2       maize
    ## 185   61  59  17        23.3     59.2 6.5    105.0       maize
    ## 186   88  38  15        25.1     65.9 6.5     62.5       maize
    ## 187   65  60  22        25.4     72.5 6.6    107.9       maize
    ## 188   78  37  22        25.3     63.3 6.3     74.5       maize
    ## 189   78  58  15        25.0     67.8 6.5     62.9       maize
    ## 190   92  60  23        18.7     71.5 5.7     69.9       maize
    ## 191   79  59  17        20.4     63.7 6.6    108.5       maize
    ## 192   91  55  15        18.1     72.6 6.4     79.0       maize
    ## 193   76  51  18        26.2     72.0 6.2     79.8       maize
    ## 194   87  48  25        18.7     61.4 6.7     93.6       maize
    ## 195   71  60  22        26.1     59.4 6.2     85.8       maize
    ## 196   90  57  24        18.9     72.8 6.2     82.3       maize
    ## 197   67  35  22        23.3     63.2 6.4    108.8       maize
    ## 198   60  54  19        18.7     62.5 6.4     70.2       maize
    ## 199   83  58  23        19.7     59.7 6.4     65.5       maize
    ## 200   83  57  19        25.7     70.7 6.9     98.7       maize
    ## 201   40  72  77        17.0     17.0 7.5     88.6    chickpea
    ## 202   23  72  84        19.0     17.1 6.9     79.9    chickpea
    ## 203   39  58  85        17.9     15.4 6.0     68.5    chickpea
    ## 204   22  72  85        18.9     15.7 6.4     88.5    chickpea
    ## 205   36  67  77        18.4     19.6 7.2     79.3    chickpea
    ## 206   32  73  81        20.5     15.4 6.0     92.7    chickpea
    ## 207   58  70  84        20.7     16.6 6.2     74.7    chickpea
    ## 208   59  70  84        17.3     18.7 7.6     82.6    chickpea
    ## 209   42  62  75        18.2     18.9 7.0     81.8    chickpea
    ## 210   28  74  81        18.0     18.3 8.8     82.0    chickpea
    ## 211   58  66  79        21.0     19.3 8.7     93.6    chickpea
    ## 212   43  66  79        19.5     15.2 8.0     74.6    chickpea
    ## 213   58  63  81        19.8     14.7 6.5     79.0    chickpea
    ## 214   23  62  85        19.0     19.5 8.5     80.7    chickpea
    ## 215   27  62  77        18.2     14.7 6.6     70.2    chickpea
    ## 216   28  72  84        18.7     19.2 6.5     71.6    chickpea
    ## 217   50  56  76        21.0     19.9 8.0     73.5    chickpea
    ## 218   39  71  84        20.3     16.4 8.1     82.5    chickpea
    ## 219   25  78  76        17.5     15.8 7.2     67.0    chickpea
    ## 220   31  70  77        20.9     14.3 6.5     90.5    chickpea
    ## 221   26  80  83        17.1     16.1 7.5     71.3    chickpea
    ## 222   25  68  77        20.1     15.1 7.7     85.7    chickpea
    ## 223   31  78  76        17.6     15.0 8.5     89.3    chickpea
    ## 224   60  68  83        19.1     18.4 6.6     85.5    chickpea
    ## 225   59  62  83        18.6     19.2 8.1     72.9    chickpea
    ## 226   22  67  78        17.2     14.4 6.2     72.3    chickpea
    ## 227   36  65  80        18.3     16.7 6.1     74.9    chickpea
    ## 228   59  60  84        19.0     18.7 7.7     94.7    chickpea
    ## 229   54  77  85        17.1     17.1 7.8     83.7    chickpea
    ## 230   43  68  81        17.5     17.9 6.8     78.9    chickpea
    ## 231   28  76  82        20.6     14.3 6.7     83.8    chickpea
    ## 232   42  79  85        17.2     15.8 6.1     76.6    chickpea
    ## 233   32  60  83        19.7     19.4 8.8     91.8    chickpea
    ## 234   22  78  76        17.8     19.1 8.6     76.3    chickpea
    ## 235   31  79  75        18.8     16.1 8.2     89.7    chickpea
    ## 236   28  58  81        17.5     16.5 6.2     93.4    chickpea
    ## 237   57  58  77        18.7     17.6 8.0     81.2    chickpea
    ## 238   49  55  78        18.7     16.2 7.9     81.7    chickpea
    ## 239   46  76  77        18.2     19.7 7.0     83.7    chickpea
    ## 240   54  61  77        18.8     15.2 6.2     77.5    chickpea
    ## 241   38  60  76        18.7     17.8 8.9     77.9    chickpea
    ## 242   59  55  79        20.4     16.9 8.8     82.3    chickpea
    ## 243   36  76  75        18.4     16.6 8.7     70.5    chickpea
    ## 244   57  68  81        17.2     17.3 8.1     72.8    chickpea
    ## 245   35  66  81        19.4     15.8 6.1     85.2    chickpea
    ## 246   35  64  78        17.9     14.3 7.5     85.4    chickpea
    ## 247   52  60  79        19.5     18.2 8.4     75.6    chickpea
    ## 248   27  76  83        19.1     14.9 6.3     89.6    chickpea
    ## 249   57  60  84        19.1     17.3 6.6     75.5    chickpea
    ## 250   52  68  78        17.5     17.0 6.9     86.1    chickpea
    ## 251   43  79  79        19.4     19.0 7.8     80.3    chickpea
    ## 252   44  74  85        20.2     19.6 7.2     78.3    chickpea
    ## 253   24  55  78        17.3     15.2 6.6     75.6    chickpea
    ## 254   29  77  75        17.5     15.5 7.8     72.9    chickpea
    ## 255   20  60  78        18.2     14.7 6.4     90.8    chickpea
    ## 256   56  67  78        17.6     16.7 8.3     77.8    chickpea
    ## 257   37  66  85        20.9     18.9 6.5     78.1    chickpea
    ## 258   49  71  76        19.7     17.6 6.6     85.6    chickpea
    ## 259   59  69  80        19.1     17.9 8.2     69.4    chickpea
    ## 260   20  79  77        18.5     16.0 7.6     76.3    chickpea
    ## 261   24  56  85        18.2     17.4 6.5     80.6    chickpea
    ## 262   51  72  75        18.9     15.0 7.1     80.1    chickpea
    ## 263   57  73  85        18.5     14.7 7.4     91.9    chickpea
    ## 264   22  64  82        19.5     17.2 6.5     87.5    chickpea
    ## 265   52  73  79        17.3     18.7 7.8     94.0    chickpea
    ## 266   29  75  75        19.6     18.7 7.1     88.5    chickpea
    ## 267   44  59  78        20.7     19.9 7.6     84.8    chickpea
    ## 268   41  69  82        20.0     16.6 6.7     69.0    chickpea
    ## 269   52  56  85        20.1     14.4 6.8     88.7    chickpea
    ## 270   34  76  80        20.7     15.8 8.0     65.2    chickpea
    ## 271   42  74  83        19.3     14.3 7.5     65.8    chickpea
    ## 272   34  71  79        17.9     15.9 7.7     74.6    chickpea
    ## 273   27  73  79        19.2     15.8 7.4     82.7    chickpea
    ## 274   30  70  79        20.3     20.0 7.3     69.6    chickpea
    ## 275   57  57  75        17.1     18.3 7.8     87.3    chickpea
    ## 276   27  79  82        17.1     17.5 6.3     70.9    chickpea
    ## 277   32  71  85        20.6     14.4 6.4     92.1    chickpea
    ## 278   31  76  82        20.8     17.9 7.6     79.2    chickpea
    ## 279   33  75  84        19.5     18.7 7.2     68.8    chickpea
    ## 280   47  80  77        17.2     16.4 7.6     72.9    chickpea
    ## 281   54  62  80        17.5     16.4 7.5     79.5    chickpea
    ## 282   47  79  78        17.5     14.8 6.6     65.1    chickpea
    ## 283   35  57  83        19.5     17.4 7.5     80.5    chickpea
    ## 284   53  73  77        19.7     18.1 7.3     73.6    chickpea
    ## 285   45  61  78        19.5     16.1 6.5     81.5    chickpea
    ## 286   37  78  79        20.0     14.8 7.8     88.7    chickpea
    ## 287   30  75  81        19.4     16.8 6.4     68.5    chickpea
    ## 288   37  55  82        19.5     18.0 8.4     78.4    chickpea
    ## 289   53  65  76        20.2     16.4 8.7     77.3    chickpea
    ## 290   22  60  85        18.8     14.7 7.8     94.8    chickpea
    ## 291   60  61  78        20.7     19.8 6.3     94.0    chickpea
    ## 292   42  67  77        19.0     15.9 7.1     78.7    chickpea
    ## 293   39  76  76        20.0     15.6 8.1     69.2    chickpea
    ## 294   35  63  76        17.8     17.6 7.7     90.8    chickpea
    ## 295   30  65  82        20.7     15.3 7.1     76.8    chickpea
    ## 296   57  56  78        17.3     18.8 8.9     68.0    chickpea
    ## 297   48  65  78        17.4     14.3 7.9     73.1    chickpea
    ## 298   36  56  83        18.9     19.8 7.5     69.1    chickpea
    ## 299   40  58  75        18.6     14.8 7.2     89.6    chickpea
    ## 300   49  69  82        18.3     15.4 7.3     81.8    chickpea
    ## 301   13  60  25        17.1     20.6 5.7    128.3 kidneybeans
    ## 302   25  70  16        19.6     18.9 5.8    106.4 kidneybeans
    ## 303   31  55  22        22.9     21.3 5.9    109.2 kidneybeans
    ## 304   40  64  16        16.4     24.2 5.9    140.4 kidneybeans
    ## 305    2  61  20        22.1     23.0 6.0     76.6 kidneybeans
    ## 306   26  65  22        17.8     18.8 5.9    143.1 kidneybeans
    ## 307   17  57  21        19.9     20.3 5.8     60.9 kidneybeans
    ## 308   26  80  18        19.3     23.3 5.6    104.8 kidneybeans
    ## 309   17  59  17        18.4     23.4 5.7    133.0 kidneybeans
    ## 310   27  59  22        21.8     23.2 5.8    130.1 kidneybeans
    ## 311   28  58  24        19.7     18.3 5.7    143.8 kidneybeans
    ## 312   25  57  19        17.2     19.9 5.6     88.0 kidneybeans
    ## 313   28  80  17        19.6     18.7 5.8    144.2 kidneybeans
    ## 314   25  60  22        21.6     21.2 5.9    134.4 kidneybeans
    ## 315   12  78  23        16.1     18.7 6.0     88.1 kidneybeans
    ## 316    6  77  25        20.6     24.4 5.8     69.6 kidneybeans
    ## 317   22  79  17        21.4     20.4 5.9    116.5 kidneybeans
    ## 318   27  80  15        19.1     21.2 5.8     86.2 kidneybeans
    ## 319   10  55  23        21.2     19.6 5.7    137.2 kidneybeans
    ## 320   23  65  20        23.0     22.4 5.8    108.4 kidneybeans
    ## 321   19  78  16        20.7     23.1 6.0     67.7 kidneybeans
    ## 322   19  65  25        18.1     18.3 5.6    144.8 kidneybeans
    ## 323   22  70  19        18.2     21.1 5.5     69.4 kidneybeans
    ## 324   37  64  22        17.5     18.8 6.0    121.9 kidneybeans
    ## 325   11  71  17        19.9     21.5 5.7     82.7 kidneybeans
    ## 326   18  79  20        20.3     23.2 5.9    139.8 kidneybeans
    ## 327   21  63  17        15.8     19.2 6.0    108.3 kidneybeans
    ## 328   24  80  22        16.7     19.2 5.6     96.8 kidneybeans
    ## 329   34  60  22        17.7     18.2 5.6    100.7 kidneybeans
    ## 330   16  75  21        18.5     23.6 5.7     87.1 kidneybeans
    ## 331   17  77  23        24.5     20.8 5.7     64.2 kidneybeans
    ## 332   37  72  18        18.9     24.5 5.7    105.4 kidneybeans
    ## 333   40  73  20        21.6     20.3 5.8     61.1 kidneybeans
    ## 334    9  77  17        20.1     24.5 5.8    106.2 kidneybeans
    ## 335    1  62  23        15.4     18.4 5.6    139.0 kidneybeans
    ## 336   33  59  22        22.6     21.6 5.9    122.4 kidneybeans
    ## 337   23  59  19        22.0     24.9 5.9    129.6 kidneybeans
    ## 338    6  62  22        20.5     18.1 5.8    120.5 kidneybeans
    ## 339   25  63  20        15.8     21.1 5.5     95.2 kidneybeans
    ## 340    7  79  23        19.6     19.7 5.8     96.7 kidneybeans
    ## 341    8  72  17        20.6     19.8 5.7     87.9 kidneybeans
    ## 342   27  64  15        20.2     24.8 5.5    138.2 kidneybeans
    ## 343   28  66  23        21.5     24.3 6.0    120.7 kidneybeans
    ## 344   32  57  18        15.5     23.8 5.7    107.4 kidneybeans
    ## 345   27  56  22        19.9     20.7 5.8    108.6 kidneybeans
    ## 346   17  77  24        20.8     18.9 5.6    109.0 kidneybeans
    ## 347    0  65  15        23.5     23.2 5.6     95.8 kidneybeans
    ## 348   13  72  21        24.3     21.0 5.8     60.3 kidneybeans
    ## 349   34  60  23        20.1     25.0 5.7    100.0 kidneybeans
    ## 350    9  80  19        21.8     18.6 5.9    125.1 kidneybeans
    ## 351   11  72  20        19.5     24.9 6.0    113.3 kidneybeans
    ## 352    3  67  24        17.0     19.9 5.5    103.3 kidneybeans
    ## 353   35  69  23        16.8     25.0 5.6     75.5 kidneybeans
    ## 354    3  77  25        24.8     22.9 5.6     62.2 kidneybeans
    ## 355   23  62  19        16.5     20.5 5.6     98.8 kidneybeans
    ## 356   22  71  17        18.2     19.4 5.5    107.7 kidneybeans
    ## 357   31  79  25        23.2     22.3 5.9     63.4 kidneybeans
    ## 358   34  59  18        23.4     22.0 5.7     87.7 kidneybeans
    ## 359   12  63  17        18.4     19.4 5.7    138.4 kidneybeans
    ## 360   27  56  20        19.3     20.5 5.5     95.0 kidneybeans
    ## 361    7  63  24        23.0     24.0 5.9    107.7 kidneybeans
    ## 362   24  67  22        20.1     22.9 5.6    104.6 kidneybeans
    ## 363   11  71  24        21.1     22.7 5.6    141.6 kidneybeans
    ## 364   37  74  15        24.9     18.2 5.6     62.7 kidneybeans
    ## 365   25  76  24        15.3     24.9 5.6    135.3 kidneybeans
    ## 366   34  66  17        18.8     21.3 5.9    125.1 kidneybeans
    ## 367   20  69  15        23.4     22.8 5.9    107.4 kidneybeans
    ## 368   37  65  16        22.8     19.0 5.7     63.6 kidneybeans
    ## 369   18  74  15        24.9     22.3 5.7    146.5 kidneybeans
    ## 370    4  67  25        23.8     24.4 5.9    119.6 kidneybeans
    ## 371   37  56  25        22.1     19.6 5.8    126.7 kidneybeans
    ## 372    5  59  15        18.9     20.2 6.0    134.2 kidneybeans
    ## 373   11  61  21        18.6     23.0 5.5    135.3 kidneybeans
    ## 374   22  80  20        23.0     18.9 5.7    100.1 kidneybeans
    ## 375   12  61  19        19.3     24.1 5.7     68.5 kidneybeans
    ## 376    5  74  21        16.2     21.4 5.6     67.0 kidneybeans
    ## 377   27  69  22        17.9     24.9 5.9     69.1 kidneybeans
    ## 378   31  75  18        15.5     21.4 5.8     88.9 kidneybeans
    ## 379   36  68  20        17.1     23.8 5.9     81.8 kidneybeans
    ## 380    5  65  16        21.3     18.5 5.9    109.1 kidneybeans
    ## 381   32  79  15        23.9     20.7 5.7     81.6 kidneybeans
    ## 382   11  78  22        23.9     22.7 5.9    112.7 kidneybeans
    ## 383    0  55  22        23.0     20.6 5.9    143.9 kidneybeans
    ## 384   14  59  15        21.4     22.9 5.8    146.5 kidneybeans
    ## 385   29  68  23        24.2     19.3 5.8    116.7 kidneybeans
    ## 386   32  68  19        24.6     18.2 5.5    149.7 kidneybeans
    ## 387   17  64  17        21.0     24.9 5.7    124.6 kidneybeans
    ## 388   13  69  19        17.3     20.0 5.9    115.2 kidneybeans
    ## 389   14  67  22        23.8     24.8 5.6     84.6 kidneybeans
    ## 390    9  69  20        19.3     24.0 5.6    129.3 kidneybeans
    ## 391   20  73  22        16.0     22.3 6.0    130.4 kidneybeans
    ## 392   40  78  20        19.2     20.8 5.7     80.2 kidneybeans
    ## 393   27  72  23        19.9     21.8 6.0     64.0 kidneybeans
    ## 394   14  67  15        19.6     24.7 5.7    139.3 kidneybeans
    ## 395    7  56  18        18.3     24.3 5.7     76.1 kidneybeans
    ## 396   27  65  18        20.1     23.2 5.6     73.4 kidneybeans
    ## 397   30  63  16        23.6     21.9 5.5    100.6 kidneybeans
    ## 398   37  70  25        19.7     24.9 5.8     84.1 kidneybeans
    ## 399   27  63  19        20.9     21.2 5.6    133.2 kidneybeans
    ## 400   22  60  24        18.8     20.2 5.6    104.3 kidneybeans
    ## 401    3  72  24        36.5     57.9 6.0    122.7  pigeonpeas
    ## 402   40  59  23        36.9     62.7 5.3    163.7  pigeonpeas
    ## 403   33  73  23        29.2     59.4 6.0    103.3  pigeonpeas
    ## 404   27  57  24        27.3     43.4 6.1    142.3  pigeonpeas
    ## 405   10  79  18        21.1     55.5 5.6    184.6  pigeonpeas
    ## 406   30  75  25        30.3     42.4 6.4    149.3  pigeonpeas
    ## 407   40  70  20        31.8     45.0 5.6    147.0  pigeonpeas
    ## 408   38  55  19        33.2     38.2 5.9    198.8  pigeonpeas
    ## 409   35  58  20        29.4     63.5 5.8     90.1  pigeonpeas
    ## 410   38  61  21        30.3     67.4 4.7    127.8  pigeonpeas
    ## 411   33  58  24        35.5     68.8 5.3    108.6  pigeonpeas
    ## 412   16  56  17        33.8     40.0 7.4    176.6  pigeonpeas
    ## 413   31  72  17        28.7     49.5 5.8     96.4  pigeonpeas
    ## 414   16  80  20        31.2     56.7 7.3    122.0  pigeonpeas
    ## 415   27  72  17        29.0     57.2 6.3    120.7  pigeonpeas
    ## 416   40  62  19        27.3     34.1 4.7     96.5  pigeonpeas
    ## 417   18  58  16        21.5     38.8 5.0    180.4  pigeonpeas
    ## 418    3  68  16        18.3     34.7 5.0    107.5  pigeonpeas
    ## 419   26  67  24        37.0     37.7 5.6    161.5  pigeonpeas
    ## 420   16  70  20        24.8     40.1 5.6    121.6  pigeonpeas
    ## 421   24  63  19        19.3     56.0 4.7    194.6  pigeonpeas
    ## 422    9  76  25        28.9     50.1 5.7    179.2  pigeonpeas
    ## 423   16  55  19        19.5     47.2 6.4    192.4  pigeonpeas
    ## 424   28  75  21        24.8     50.5 6.0    114.3  pigeonpeas
    ## 425   16  71  24        18.3     38.4 4.9    139.6  pigeonpeas
    ## 426   24  70  21        19.1     45.4 5.5    132.8  pigeonpeas
    ## 427   38  72  21        28.2     49.4 5.9    186.5  pigeonpeas
    ## 428    9  66  21        30.1     34.1 5.7    157.1  pigeonpeas
    ## 429   34  56  17        33.4     35.4 4.5    139.7  pigeonpeas
    ## 430    1  76  19        24.2     46.7 6.7    177.3  pigeonpeas
    ## 431    6  69  19        26.9     41.7 4.8     94.5  pigeonpeas
    ## 432   26  73  21        31.3     58.0 4.9    161.8  pigeonpeas
    ## 433   27  61  18        33.3     67.1 5.3    108.5  pigeonpeas
    ## 434   27  71  23        23.5     46.5 7.1    150.9  pigeonpeas
    ## 435   36  61  21        34.5     39.0 5.6    168.6  pigeonpeas
    ## 436   17  73  18        19.5     34.5 5.6    197.4  pigeonpeas
    ## 437   26  72  22        28.8     37.6 4.7     91.7  pigeonpeas
    ## 438   17  64  16        31.0     32.2 7.2    180.7  pigeonpeas
    ## 439   14  74  19        18.4     36.8 6.6     93.1  pigeonpeas
    ## 440   39  60  15        35.1     31.0 5.0    116.9  pigeonpeas
    ## 441    6  66  15        34.9     30.4 6.3    159.3  pigeonpeas
    ## 442    8  59  18        29.5     35.7 6.2    187.9  pigeonpeas
    ## 443    2  67  18        34.5     47.5 5.9    129.0  pigeonpeas
    ## 444    1  76  17        28.4     52.1 6.0    147.0  pigeonpeas
    ## 445   16  73  19        18.4     34.8 4.7    163.3  pigeonpeas
    ## 446   23  75  25        31.1     47.2 7.1     91.3  pigeonpeas
    ## 447   32  70  20        20.9     46.2 6.2    195.6  pigeonpeas
    ## 448   28  59  22        30.9     52.8 7.1    171.0  pigeonpeas
    ## 449    5  62  23        27.9     66.5 4.7    145.4  pigeonpeas
    ## 450   36  67  25        36.0     36.5 6.4    136.0  pigeonpeas
    ## 451    1  66  23        19.5     56.9 4.8    173.2  pigeonpeas
    ## 452   24  73  20        19.6     32.3 4.6    176.4  pigeonpeas
    ## 453   17  67  18        31.2     56.5 5.6    129.2  pigeonpeas
    ## 454    5  55  18        33.5     45.7 7.3    126.7  pigeonpeas
    ## 455    5  56  24        24.8     45.0 5.0    188.5  pigeonpeas
    ## 456   37  77  17        36.2     31.9 5.6    191.1  pigeonpeas
    ## 457   13  73  20        30.5     35.5 5.4    162.6  pigeonpeas
    ## 458    6  63  23        26.0     49.9 5.9    160.3  pigeonpeas
    ## 459   16  77  22        31.5     35.6 6.6    100.5  pigeonpeas
    ## 460   25  64  20        33.2     32.5 4.8    105.0  pigeonpeas
    ## 461   34  75  24        23.5     51.3 4.8    192.3  pigeonpeas
    ## 462   20  77  23        34.9     38.8 5.2    148.3  pigeonpeas
    ## 463   35  80  25        28.1     44.9 4.9    197.1  pigeonpeas
    ## 464   14  75  24        24.5     57.3 6.4    118.4  pigeonpeas
    ## 465   36  80  21        33.6     48.4 7.1    100.5  pigeonpeas
    ## 466    7  77  18        20.6     60.5 6.7    191.1  pigeonpeas
    ## 467   29  78  25        20.0     59.3 6.0    195.8  pigeonpeas
    ## 468   30  60  21        28.9     62.5 5.5    182.3  pigeonpeas
    ## 469   20  74  16        36.0     43.6 4.8    159.9  pigeonpeas
    ## 470   19  57  23        23.7     47.3 7.3    141.1  pigeonpeas
    ## 471    3  60  19        25.7     40.7 4.8    100.8  pigeonpeas
    ## 472    5  77  19        31.1     66.7 6.2    175.9  pigeonpeas
    ## 473    5  68  20        18.7     61.3 5.0    139.9  pigeonpeas
    ## 474   37  73  21        29.5     63.5 5.6    189.5  pigeonpeas
    ## 475    9  59  24        20.4     39.4 4.7    137.2  pigeonpeas
    ## 476   20  72  15        36.0     56.0 7.3    134.9  pigeonpeas
    ## 477   31  56  23        31.5     35.4 5.7    174.6  pigeonpeas
    ## 478    0  70  21        36.3     56.0 4.7    101.6  pigeonpeas
    ## 479   21  74  15        29.5     67.1 6.5    153.3  pigeonpeas
    ## 480   13  67  18        30.6     34.8 5.4    177.6  pigeonpeas
    ## 481   27  74  20        24.7     60.0 5.9     92.0  pigeonpeas
    ## 482   29  72  24        23.2     36.7 7.0    162.6  pigeonpeas
    ## 483    5  68  20        19.0     33.1 6.1    155.4  pigeonpeas
    ## 484   39  57  19        29.3     45.9 6.4    165.4  pigeonpeas
    ## 485   22  62  16        34.6     54.3 4.8    180.9  pigeonpeas
    ## 486   18  55  23        22.0     56.3 7.0    136.8  pigeonpeas
    ## 487   39  77  21        23.0     60.2 4.6    159.7  pigeonpeas
    ## 488   13  75  20        30.6     35.3 7.0    178.9  pigeonpeas
    ## 489   27  71  24        31.5     48.2 7.1    165.4  pigeonpeas
    ## 490   26  64  22        26.0     40.6 5.2    109.2  pigeonpeas
    ## 491   23  55  16        21.0     69.7 5.1    185.2  pigeonpeas
    ## 492    4  69  19        19.3     47.7 5.4    149.1  pigeonpeas
    ## 493   20  67  19        19.2     50.5 5.7    180.6  pigeonpeas
    ## 494    7  74  17        22.5     62.6 5.7     96.7  pigeonpeas
    ## 495   17  64  18        36.8     58.3 6.1    124.6  pigeonpeas
    ## 496   35  71  17        29.9     66.4 6.9    198.1  pigeonpeas
    ## 497   11  72  22        29.4     44.8 6.8    172.4  pigeonpeas
    ## 498   20  60  22        29.7     42.9 6.9    186.9  pigeonpeas
    ## 499   10  71  18        19.5     66.3 6.2    173.1  pigeonpeas
    ## 500   33  61  24        20.0     48.9 4.6    122.5  pigeonpeas
    ## 501    3  49  18        27.9     64.7 3.7     32.7   mothbeans
    ## 502   22  59  23        27.3     51.3 4.4     36.5   mothbeans
    ## 503   36  58  25        28.7     59.3 8.4     36.9   mothbeans
    ## 504    4  43  18        29.0     61.1 8.8     73.0   mothbeans
    ## 505   29  54  16        27.8     54.7 8.2     32.1   mothbeans
    ## 506   32  43  22        32.0     54.1 5.3     71.6   mothbeans
    ## 507   14  55  15        27.3     55.3 8.1     73.4   mothbeans
    ## 508    5  35  20        28.9     53.6 9.7     66.4   mothbeans
    ## 509   25  57  24        27.7     58.6 7.0     36.9   mothbeans
    ## 510   11  53  24        28.5     55.8 7.4     61.3   mothbeans
    ## 511   40  49  17        31.0     45.9 6.7     53.6   mothbeans
    ## 512   38  56  25        25.7     45.4 7.9     67.4   mothbeans
    ## 513   27  43  23        31.7     56.9 5.9     44.9   mothbeans
    ## 514   24  38  22        24.5     58.5 8.2     35.0   mothbeans
    ## 515   23  45  21        31.5     51.8 9.0     74.4   mothbeans
    ## 516   29  57  20        25.6     50.7 5.9     53.4   mothbeans
    ## 517   31  35  23        30.3     47.2 7.7     68.0   mothbeans
    ## 518    0  55  25        28.2     43.7 4.5     45.8   mothbeans
    ## 519    7  45  22        25.5     44.8 9.9     74.3   mothbeans
    ## 520   17  58  25        31.1     43.6 6.5     32.8   mothbeans
    ## 521   11  44  17        26.3     55.6 8.0     35.1   mothbeans
    ## 522   22  49  22        28.2     61.6 3.7     72.7   mothbeans
    ## 523    9  51  19        27.0     49.3 5.5     48.3   mothbeans
    ## 524   28  48  15        25.2     55.3 9.3     40.9   mothbeans
    ## 525   26  50  19        27.3     51.7 6.0     32.6   mothbeans
    ## 526   36  56  20        25.4     49.7 7.4     31.9   mothbeans
    ## 527    8  60  18        31.2     46.0 3.8     53.1   mothbeans
    ## 528   24  37  21        30.6     58.2 5.8     62.7   mothbeans
    ## 529   22  43  24        25.4     53.2 4.5     46.2   mothbeans
    ## 530   36  43  24        27.1     43.7 3.5     41.5   mothbeans
    ## 531   22  44  24        24.3     56.3 6.0     59.0   mothbeans
    ## 532   17  43  22        30.1     45.9 5.5     41.1   mothbeans
    ## 533    8  45  15        28.1     61.0 4.6     33.8   mothbeans
    ## 534    7  56  23        26.3     40.0 5.5     55.5   mothbeans
    ## 535   36  57  16        28.6     57.1 8.3     57.0   mothbeans
    ## 536   11  45  19        28.7     44.4 3.8     44.1   mothbeans
    ## 537    6  36  22        24.2     59.8 8.9     42.2   mothbeans
    ## 538   17  57  20        28.5     45.2 3.8     66.2   mothbeans
    ## 539    4  47  20        26.0     65.0 4.2     72.2   mothbeans
    ## 540    9  49  16        30.9     41.4 7.7     55.1   mothbeans
    ## 541   25  51  24        25.5     61.7 9.4     65.1   mothbeans
    ## 542   36  44  21        25.1     51.3 4.5     38.5   mothbeans
    ## 543   21  38  20        27.1     63.6 5.8     62.2   mothbeans
    ## 544   37  57  20        31.1     44.8 7.4     70.8   mothbeans
    ## 545   32  48  18        26.5     56.4 6.0     64.2   mothbeans
    ## 546   29  44  20        30.0     63.6 8.6     31.8   mothbeans
    ## 547   25  51  18        27.8     54.8 9.5     50.3   mothbeans
    ## 548   10  44  24        31.0     43.0 8.0     58.3   mothbeans
    ## 549   23  35  18        26.5     47.4 5.4     37.0   mothbeans
    ## 550    9  60  23        32.0     57.2 6.3     64.3   mothbeans
    ## 551    3  58  21        25.4     46.8 9.2     55.6   mothbeans
    ## 552   22  42  22        25.5     57.0 7.9     48.5   mothbeans
    ## 553   12  39  21        29.0     62.9 8.2     70.5   mothbeans
    ## 554   39  36  22        29.3     60.5 9.1     34.0   mothbeans
    ## 555   32  41  16        28.6     61.4 7.7     68.5   mothbeans
    ## 556   30  41  15        24.8     44.2 5.9     52.1   mothbeans
    ## 557   19  36  22        25.4     58.6 6.2     57.0   mothbeans
    ## 558    4  46  15        31.0     62.4 3.5     63.8   mothbeans
    ## 559   21  39  20        27.1     52.3 7.4     60.7   mothbeans
    ## 560   35  57  25        27.1     42.3 8.3     71.1   mothbeans
    ## 561   22  55  24        28.6     57.3 8.7     64.5   mothbeans
    ## 562   35  51  17        28.8     49.8 3.6     40.9   mothbeans
    ## 563   17  56  17        27.9     45.4 6.0     69.7   mothbeans
    ## 564   28  57  17        30.5     61.6 9.4     61.9   mothbeans
    ## 565   22  36  16        30.6     50.8 8.2     64.6   mothbeans
    ## 566   11  41  19        26.9     41.8 5.1     44.1   mothbeans
    ## 567   38  38  18        26.3     61.2 6.3     35.7   mothbeans
    ## 568   23  37  24        28.8     44.2 8.0     34.0   mothbeans
    ## 569   25  35  20        28.9     43.4 8.9     71.9   mothbeans
    ## 570   40  45  20        29.4     57.7 6.9     38.3   mothbeans
    ## 571   23  58  19        24.2     58.3 5.2     59.2   mothbeans
    ## 572    2  56  23        26.7     59.8 7.6     36.9   mothbeans
    ## 573    3  56  17        28.2     53.5 8.7     52.1   mothbeans
    ## 574   26  51  25        28.8     52.6 7.8     55.2   mothbeans
    ## 575   39  42  20        29.3     61.3 8.1     40.8   mothbeans
    ## 576   27  59  20        28.0     52.6 4.4     36.0   mothbeans
    ## 577   24  45  19        26.9     48.8 6.0     34.7   mothbeans
    ## 578    7  40  17        31.2     40.9 8.5     53.8   mothbeans
    ## 579   15  45  23        24.2     61.4 7.2     46.0   mothbeans
    ## 580   26  52  23        30.0     49.6 4.9     52.9   mothbeans
    ## 581   20  45  16        29.9     54.6 4.6     45.4   mothbeans
    ## 582   34  54  24        31.2     41.6 5.0     68.8   mothbeans
    ## 583   19  51  25        26.8     48.2 3.5     43.9   mothbeans
    ## 584   29  41  21        31.5     62.8 8.9     64.6   mothbeans
    ## 585   20  50  22        31.0     46.4 9.4     38.3   mothbeans
    ## 586   11  40  23        29.6     63.0 5.8     50.2   mothbeans
    ## 587   15  54  15        30.0     57.0 8.4     44.9   mothbeans
    ## 588   35  55  22        30.9     52.6 8.6     55.5   mothbeans
    ## 589    9  59  25        30.4     60.2 7.7     35.4   mothbeans
    ## 590   40  45  18        30.4     55.2 5.3     30.9   mothbeans
    ## 591   35  38  19        25.3     63.2 9.1     32.7   mothbeans
    ## 592   14  58  17        30.5     60.0 4.6     33.5   mothbeans
    ## 593   40  55  18        30.4     40.6 7.1     48.0   mothbeans
    ## 594   18  36  23        24.0     53.8 7.2     35.0   mothbeans
    ## 595   35  52  15        28.7     61.1 9.9     65.7   mothbeans
    ## 596    4  59  22        29.3     49.0 8.9     42.4   mothbeans
    ## 597   22  51  16        28.0     61.3 8.6     70.1   mothbeans
    ## 598   33  47  17        24.9     48.3 8.6     63.9   mothbeans
    ## 599    2  51  17        25.9     46.0 5.8     38.5   mothbeans
    ## 600   16  51  21        31.0     50.0 3.5     32.8   mothbeans
    ## 601   19  55  20        27.4     87.8 7.2     54.7    mungbean
    ## 602    8  54  20        28.3     80.8 7.0     38.8    mungbean
    ## 603   36  55  20        27.0     84.3 6.6     55.3    mungbean
    ## 604   10  56  16        28.2     81.0 6.8     36.4    mungbean
    ## 605   22  56  17        29.9     87.3 6.9     44.8    mungbean
    ## 606    9  57  24        29.9     89.7 7.2     43.0    mungbean
    ## 607   34  59  23        28.6     83.2 6.9     56.5    mungbean
    ## 608   31  51  25        27.5     85.6 7.2     53.0    mungbean
    ## 609    0  49  18        29.7     87.9 7.0     41.8    mungbean
    ## 610   21  39  20        28.1     82.1 7.1     46.8    mungbean
    ## 611   28  35  22        29.5     86.7 7.2     59.9    mungbean
    ## 612   17  52  17        27.9     86.5 6.4     44.6    mungbean
    ## 613   24  42  23        28.2     82.4 6.4     44.0    mungbean
    ## 614   28  46  16        29.0     85.0 6.7     45.9    mungbean
    ## 615   21  38  21        29.8     86.5 6.6     37.5    mungbean
    ## 616   34  60  25        29.8     85.2 6.8     40.8    mungbean
    ## 617   19  53  22        27.9     80.5 6.9     42.8    mungbean
    ## 618   31  58  15        27.1     85.0 7.1     51.5    mungbean
    ## 619   19  35  24        27.1     83.6 6.9     49.1    mungbean
    ## 620   24  53  17        29.0     89.1 6.4     57.7    mungbean
    ## 621   13  47  20        29.2     87.9 6.5     43.1    mungbean
    ## 622   31  53  16        28.7     85.8 6.5     48.5    mungbean
    ## 623   28  45  23        29.7     80.3 6.5     56.8    mungbean
    ## 624   31  37  21        27.2     86.4 6.7     37.3    mungbean
    ## 625   33  60  15        29.0     81.7 6.5     56.5    mungbean
    ## 626   34  45  21        28.2     82.6 6.3     37.0    mungbean
    ## 627   13  57  25        28.3     86.2 6.9     50.5    mungbean
    ## 628   33  57  17        27.9     88.7 6.8     57.8    mungbean
    ## 629   32  57  22        28.7     87.5 6.8     44.6    mungbean
    ## 630   23  59  25        27.8     88.7 6.3     56.7    mungbean
    ## 631   35  41  18        28.7     81.6 6.7     59.9    mungbean
    ## 632    6  48  24        28.6     84.6 6.8     48.5    mungbean
    ## 633   29  36  25        28.3     88.4 7.1     48.6    mungbean
    ## 634    4  36  22        27.6     86.1 7.0     43.8    mungbean
    ## 635   10  59  22        28.6     87.0 7.2     36.9    mungbean
    ## 636   14  48  21        29.2     84.8 7.0     53.4    mungbean
    ## 637    8  50  21        28.6     89.1 6.2     50.5    mungbean
    ## 638   20  40  15        29.6     88.1 7.2     45.0    mungbean
    ## 639   36  43  22        27.8     87.2 6.4     58.4    mungbean
    ## 640   14  57  15        29.9     83.1 6.6     40.1    mungbean
    ## 641   11  60  23        27.3     88.5 7.0     51.1    mungbean
    ## 642   10  59  15        29.8     89.3 6.3     58.9    mungbean
    ## 643    7  60  25        28.3     82.8 6.4     56.0    mungbean
    ## 644    2  47  15        29.9     86.0 6.4     58.4    mungbean
    ## 645   20  45  22        29.6     90.0 6.9     55.0    mungbean
    ## 646    2  39  15        28.1     82.9 6.5     49.6    mungbean
    ## 647   27  40  24        27.8     90.0 7.1     52.8    mungbean
    ## 648   35  48  15        27.1     87.5 7.0     55.0    mungbean
    ## 649    4  59  25        27.7     81.9 6.2     54.6    mungbean
    ## 650    1  48  24        29.3     85.6 6.2     59.0    mungbean
    ## 651   36  43  21        28.4     84.9 7.1     52.9    mungbean
    ## 652   11  46  24        27.7     89.8 6.5     56.5    mungbean
    ## 653   34  47  19        27.3     85.4 6.6     53.2    mungbean
    ## 654   21  44  18        27.1     86.9 7.1     50.5    mungbean
    ## 655   17  58  20        28.1     85.9 6.4     39.2    mungbean
    ## 656   25  40  21        27.7     81.1 6.2     44.2    mungbean
    ## 657    2  38  18        27.5     89.9 6.6     45.5    mungbean
    ## 658    9  48  20        29.7     84.3 6.4     56.1    mungbean
    ## 659   37  49  25        29.9     85.9 6.4     41.4    mungbean
    ## 660   36  38  15        28.4     87.6 6.3     58.0    mungbean
    ## 661   40  58  15        29.5     87.6 7.0     43.2    mungbean
    ## 662   30  44  16        29.7     82.9 6.4     50.9    mungbean
    ## 663    1  59  23        27.5     87.2 7.2     43.8    mungbean
    ## 664    9  48  22        27.8     87.1 6.4     49.5    mungbean
    ## 665   14  41  17        29.1     88.5 7.1     36.5    mungbean
    ## 666   35  52  19        27.1     89.9 6.7     37.5    mungbean
    ## 667   31  48  17        28.9     86.9 6.6     53.8    mungbean
    ## 668    4  41  20        28.1     83.8 6.6     37.4    mungbean
    ## 669   30  37  25        29.9     80.1 7.1     54.8    mungbean
    ## 670    9  35  20        27.4     81.0 6.9     40.5    mungbean
    ## 671   20  41  20        29.3     89.5 7.1     50.9    mungbean
    ## 672   37  50  23        29.7     88.5 6.5     56.0    mungbean
    ## 673   34  35  21        28.4     82.7 6.7     58.2    mungbean
    ## 674   14  37  15        28.0     84.0 6.6     48.9    mungbean
    ## 675   23  39  22        29.3     82.0 6.9     42.0    mungbean
    ## 676    5  45  21        28.4     88.0 6.5     43.1    mungbean
    ## 677   22  37  20        27.6     86.5 6.6     39.3    mungbean
    ## 678   40  51  17        28.7     86.1 6.9     50.0    mungbean
    ## 679   27  56  20        29.2     87.1 6.4     51.5    mungbean
    ## 680   31  40  22        29.4     86.2 6.4     53.4    mungbean
    ## 681   38  36  21        28.0     84.9 6.6     36.1    mungbean
    ## 682    6  37  17        28.1     80.4 6.8     38.1    mungbean
    ## 683    6  47  18        29.2     80.3 6.7     40.2    mungbean
    ## 684   24  44  17        29.9     80.0 6.7     50.7    mungbean
    ## 685   25  59  19        29.1     83.7 6.6     44.0    mungbean
    ## 686   32  56  21        27.4     88.7 6.7     58.3    mungbean
    ## 687    8  45  18        27.9     85.4 7.0     43.3    mungbean
    ## 688   19  39  17        29.3     81.8 6.9     44.5    mungbean
    ## 689   39  37  15        29.0     83.8 6.8     59.8    mungbean
    ## 690   33  37  19        27.9     86.6 7.2     43.5    mungbean
    ## 691   26  54  17        28.5     89.0 6.3     49.5    mungbean
    ## 692   21  51  15        29.4     89.2 6.7     48.3    mungbean
    ## 693   22  54  20        28.6     83.6 6.7     41.0    mungbean
    ## 694   29  45  16        28.4     87.9 6.6     43.1    mungbean
    ## 695    4  40  21        28.8     80.5 6.7     44.3    mungbean
    ## 696   10  37  22        28.7     89.1 7.1     58.5    mungbean
    ## 697    4  44  19        28.0     83.5 6.9     43.3    mungbean
    ## 698   20  45  17        28.2     83.7 6.8     37.2    mungbean
    ## 699   23  45  23        28.8     86.7 7.0     56.1    mungbean
    ## 700   25  48  21        28.4     83.5 6.3     52.6    mungbean
    ## 701   56  79  15        29.5     63.2 7.5     71.9   blackgram
    ## 702   25  62  21        26.7     68.1 7.0     67.2   blackgram
    ## 703   42  61  22        26.3     62.3 7.4     70.2   blackgram
    ## 704   42  73  25        34.0     67.2 6.5     73.2   blackgram
    ## 705   44  58  18        28.0     65.1 6.8     72.5   blackgram
    ## 706   50  55  16        28.8     65.3 7.6     62.3   blackgram
    ## 707   35  72  21        34.0     64.3 7.7     66.9   blackgram
    ## 708   30  64  20        33.9     61.6 6.6     68.0   blackgram
    ## 709   27  64  21        32.8     68.7 7.5     73.7   blackgram
    ## 710   50  74  17        27.1     63.4 6.5     73.8   blackgram
    ## 711   39  73  24        25.7     61.2 7.2     69.3   blackgram
    ## 712   57  67  25        32.3     66.6 7.6     64.6   blackgram
    ## 713   52  63  19        29.6     68.3 6.9     67.5   blackgram
    ## 714   55  66  22        30.9     68.8 7.7     66.6   blackgram
    ## 715   51  56  18        28.1     64.2 6.7     70.9   blackgram
    ## 716   36  66  15        30.1     69.3 6.7     67.1   blackgram
    ## 717   59  55  19        31.7     62.5 7.3     69.0   blackgram
    ## 718   50  58  23        27.8     62.5 7.6     69.8   blackgram
    ## 719   30  65  25        32.9     64.6 7.7     71.5   blackgram
    ## 720   20  62  18        29.4     65.0 7.4     61.9   blackgram
    ## 721   58  71  15        27.8     67.6 6.9     74.0   blackgram
    ## 722   25  71  24        28.5     60.4 7.2     74.9   blackgram
    ## 723   52  71  16        27.7     68.5 7.1     71.8   blackgram
    ## 724   40  63  18        30.4     67.7 6.7     63.0   blackgram
    ## 725   20  60  25        27.3     69.1 6.7     61.2   blackgram
    ## 726   48  61  21        30.3     61.7 6.6     65.6   blackgram
    ## 727   49  68  22        28.6     61.5 7.1     63.5   blackgram
    ## 728   48  62  15        25.4     66.6 7.5     65.8   blackgram
    ## 729   32  66  17        34.9     65.3 7.2     70.1   blackgram
    ## 730   21  63  22        25.1     67.7 6.9     74.6   blackgram
    ## 731   20  72  19        32.5     64.3 7.4     65.8   blackgram
    ## 732   25  65  21        33.9     68.6 6.9     69.2   blackgram
    ## 733   41  78  21        25.2     60.4 6.6     70.9   blackgram
    ## 734   53  67  17        31.8     69.0 7.3     61.5   blackgram
    ## 735   39  60  21        34.9     63.6 7.0     64.7   blackgram
    ## 736   25  76  17        31.7     68.6 7.2     62.3   blackgram
    ## 737   21  78  19        27.2     66.8 6.9     69.9   blackgram
    ## 738   57  60  17        26.2     67.9 7.5     73.6   blackgram
    ## 739   56  75  15        30.2     60.1 7.2     66.4   blackgram
    ## 740   49  72  15        31.6     67.8 7.1     74.9   blackgram
    ## 741   24  80  19        29.7     69.1 6.8     65.7   blackgram
    ## 742   49  76  18        27.1     67.7 7.4     60.5   blackgram
    ## 743   28  68  19        34.6     61.4 7.7     72.4   blackgram
    ## 744   55  78  21        33.4     62.9 6.6     63.6   blackgram
    ## 745   50  64  25        28.8     63.4 6.7     70.3   blackgram
    ## 746   34  80  19        31.5     63.1 6.5     71.5   blackgram
    ## 747   20  68  23        25.5     64.0 7.7     63.2   blackgram
    ## 748   55  67  16        34.4     69.7 6.6     70.3   blackgram
    ## 749   23  70  15        34.6     63.1 7.4     60.4   blackgram
    ## 750   53  74  15        29.4     64.9 7.5     72.2   blackgram
    ## 751   26  67  16        29.1     67.9 7.2     67.8   blackgram
    ## 752   33  80  22        28.6     65.7 6.6     70.1   blackgram
    ## 753   37  79  19        27.5     69.3 7.1     69.4   blackgram
    ## 754   33  75  21        33.0     68.9 6.7     62.3   blackgram
    ## 755   22  55  20        34.0     70.0 7.4     61.2   blackgram
    ## 756   20  68  17        30.1     60.1 6.6     71.7   blackgram
    ## 757   43  68  20        29.6     66.2 7.5     69.4   blackgram
    ## 758   44  76  22        27.3     68.0 7.8     68.9   blackgram
    ## 759   34  60  16        31.4     64.2 7.3     63.9   blackgram
    ## 760   21  72  17        31.5     66.6 7.6     61.7   blackgram
    ## 761   25  68  19        29.4     64.3 7.1     67.5   blackgram
    ## 762   41  62  15        29.4     64.1 7.4     65.2   blackgram
    ## 763   28  65  23        28.4     61.9 7.4     74.2   blackgram
    ## 764   35  64  15        28.5     63.5 6.5     69.5   blackgram
    ## 765   52  58  16        30.6     61.1 7.2     71.4   blackgram
    ## 766   58  75  25        25.3     61.4 7.3     68.6   blackgram
    ## 767   34  66  19        33.0     60.2 7.6     73.4   blackgram
    ## 768   52  70  16        33.7     66.6 7.5     67.3   blackgram
    ## 769   23  57  19        32.8     68.0 7.3     73.4   blackgram
    ## 770   42  58  25        27.5     62.9 6.5     69.5   blackgram
    ## 771   37  62  17        25.7     69.8 7.1     74.6   blackgram
    ## 772   44  75  22        30.0     64.1 7.6     71.2   blackgram
    ## 773   21  80  20        28.2     68.3 7.4     64.3   blackgram
    ## 774   56  76  16        28.3     61.2 7.5     63.3   blackgram
    ## 775   29  76  15        28.5     64.2 7.0     69.7   blackgram
    ## 776   43  61  20        26.9     61.6 6.8     63.5   blackgram
    ## 777   55  60  15        32.8     68.8 7.2     64.1   blackgram
    ## 778   44  63  15        26.4     64.5 7.3     63.5   blackgram
    ## 779   29  67  21        29.8     63.4 6.6     63.0   blackgram
    ## 780   47  63  16        27.4     67.1 6.7     72.5   blackgram
    ## 781   40  68  17        34.1     65.1 7.7     70.4   blackgram
    ## 782   58  61  15        30.9     64.2 7.4     62.8   blackgram
    ## 783   41  74  18        28.8     61.0 6.6     73.4   blackgram
    ## 784   58  79  17        27.2     66.1 7.0     62.3   blackgram
    ## 785   27  62  24        28.6     66.8 7.4     62.3   blackgram
    ## 786   27  60  17        26.4     63.6 7.0     64.4   blackgram
    ## 787   52  65  20        32.8     66.2 6.8     68.8   blackgram
    ## 788   44  55  25        29.6     65.9 7.4     71.2   blackgram
    ## 789   21  62  24        33.5     62.7 6.8     65.5   blackgram
    ## 790   60  59  22        31.9     66.7 7.2     74.2   blackgram
    ## 791   33  77  21        30.3     65.6 7.0     71.6   blackgram
    ## 792   59  58  17        28.5     66.3 7.4     62.8   blackgram
    ## 793   29  63  17        30.0     67.9 7.3     66.5   blackgram
    ## 794   59  63  18        31.7     60.1 6.5     66.7   blackgram
    ## 795   29  70  15        30.3     63.5 6.9     74.2   blackgram
    ## 796   58  73  16        33.4     65.7 6.9     64.9   blackgram
    ## 797   55  77  22        31.4     63.0 7.8     64.8   blackgram
    ## 798   42  79  23        27.7     63.3 6.8     68.6   blackgram
    ## 799   44  77  21        32.6     61.3 7.3     61.8   blackgram
    ## 800   38  62  25        32.7     67.8 7.5     63.4   blackgram
    ## 801   32  76  15        28.1     63.5 7.6     43.4      lentil
    ## 802   13  61  22        19.4     63.3 7.7     46.8      lentil
    ## 803   38  60  20        29.8     60.6 7.5     46.8      lentil
    ## 804   11  74  17        21.4     69.9 6.6     46.6      lentil
    ## 805   37  71  16        26.3     68.5 7.3     46.1      lentil
    ## 806   29  71  18        22.2     62.1 6.4     53.5      lentil
    ## 807    2  72  18        26.6     61.0 7.8     50.9      lentil
    ## 808    6  59  21        26.6     66.1 6.1     50.9      lentil
    ## 809   13  64  20        19.1     62.6 6.6     36.5      lentil
    ## 810    8  58  17        28.8     69.2 7.3     35.2      lentil
    ## 811    6  77  20        25.8     60.3 6.1     49.1      lentil
    ## 812    2  75  22        23.9     61.8 6.7     52.6      lentil
    ## 813    3  69  23        28.7     63.2 7.3     43.0      lentil
    ## 814   27  80  24        28.4     61.8 7.8     49.0      lentil
    ## 815   39  78  15        21.4     62.6 5.9     41.8      lentil
    ## 816   40  79  17        21.1     63.2 6.4     38.7      lentil
    ## 817   37  62  22        24.0     61.6 7.4     49.8      lentil
    ## 818   31  60  24        25.4     65.9 7.7     51.9      lentil
    ## 819   22  67  22        29.0     64.5 7.5     54.9      lentil
    ## 820    3  78  18        20.2     68.7 6.9     50.9      lentil
    ## 821    4  80  16        29.2     68.0 7.4     44.9      lentil
    ## 822   13  61  24        18.3     69.7 7.6     49.4      lentil
    ## 823   12  66  20        27.4     63.4 7.3     44.4      lentil
    ## 824    4  61  21        24.8     60.1 6.8     48.8      lentil
    ## 825    9  60  21        29.9     67.3 7.5     40.4      lentil
    ## 826   18  66  22        25.9     67.6 6.3     47.9      lentil
    ## 827   32  56  18        20.0     65.8 7.1     46.1      lentil
    ## 828    6  72  15        23.0     66.7 7.7     54.5      lentil
    ## 829   15  77  20        25.1     66.9 7.4     49.0      lentil
    ## 830    0  65  24        28.5     62.4 7.8     53.1      lentil
    ## 831   30  79  22        18.3     69.5 6.3     48.6      lentil
    ## 832    3  63  16        24.4     61.2 6.9     53.1      lentil
    ## 833    2  78  23        21.3     66.4 7.3     45.4      lentil
    ## 834   10  78  18        18.5     62.7 6.3     44.1      lentil
    ## 835   14  67  25        25.3     60.9 7.2     49.4      lentil
    ## 836   39  65  23        25.4     69.1 7.7     41.0      lentil
    ## 837   19  72  15        28.8     69.8 6.9     44.1      lentil
    ## 838   18  57  21        27.4     63.9 6.2     49.5      lentil
    ## 839   31  58  15        28.3     60.2 6.2     45.4      lentil
    ## 840   28  58  25        27.5     62.0 6.9     37.8      lentil
    ## 841    5  65  19        18.3     68.1 7.0     48.8      lentil
    ## 842   16  65  19        27.6     69.3 7.0     42.7      lentil
    ## 843   34  65  19        23.4     63.2 5.9     45.4      lentil
    ## 844   14  69  19        21.0     63.7 7.2     52.4      lentil
    ## 845   22  55  16        23.8     68.0 6.5     49.7      lentil
    ## 846   24  61  17        22.6     65.4 6.2     38.3      lentil
    ## 847    2  79  15        21.5     65.5 7.5     35.8      lentil
    ## 848   26  63  17        29.9     65.7 7.0     45.0      lentil
    ## 849   27  61  15        25.3     67.1 7.0     48.3      lentil
    ## 850   24  70  16        25.2     68.9 6.5     35.0      lentil
    ## 851   13  74  25        24.1     61.1 6.5     44.2      lentil
    ## 852    6  64  23        23.3     67.4 7.1     36.2      lentil
    ## 853   12  58  23        21.7     63.4 6.8     50.4      lentil
    ## 854   32  79  22        27.6     63.5 5.9     54.4      lentil
    ## 855    6  68  18        24.4     62.5 6.7     47.3      lentil
    ## 856   10  79  20        25.0     66.9 6.4     38.2      lentil
    ## 857   38  77  22        28.2     69.3 6.3     35.4      lentil
    ## 858   17  74  17        26.0     69.6 7.4     37.1      lentil
    ## 859   26  68  24        28.0     64.1 7.5     37.2      lentil
    ## 860   23  75  17        24.9     64.0 7.2     48.3      lentil
    ## 861   32  78  22        24.0     62.4 7.0     53.4      lentil
    ## 862   19  79  19        20.1     67.8 6.7     42.9      lentil
    ## 863   22  60  18        19.6     61.3 6.7     41.8      lentil
    ## 864   28  69  16        29.8     66.3 6.5     35.7      lentil
    ## 865    1  67  21        27.5     60.5 6.6     48.1      lentil
    ## 866   12  67  23        25.6     63.1 6.6     45.5      lentil
    ## 867   36  67  20        20.4     60.5 6.9     53.3      lentil
    ## 868   28  70  21        25.4     60.5 7.4     39.2      lentil
    ## 869   12  71  19        24.9     60.7 7.1     42.2      lentil
    ## 870   22  68  16        27.7     63.2 7.7     37.5      lentil
    ## 871   26  66  22        18.1     65.1 6.3     51.5      lentil
    ## 872   16  65  16        18.1     62.5 6.1     50.6      lentil
    ## 873   14  59  22        23.8     67.9 6.8     46.9      lentil
    ## 874   33  59  19        23.2     62.7 7.6     49.6      lentil
    ## 875   21  63  17        25.1     68.2 6.6     41.5      lentil
    ## 876    0  69  21        25.9     61.9 7.1     36.7      lentil
    ## 877   10  75  17        18.4     68.1 7.7     39.0      lentil
    ## 878   30  61  18        27.1     67.0 6.2     52.5      lentil
    ## 879    0  74  17        23.3     64.5 7.2     47.0      lentil
    ## 880   35  74  22        26.7     63.0 6.9     42.9      lentil
    ## 881    7  63  24        19.6     64.5 6.8     53.0      lentil
    ## 882    9  56  17        26.1     66.8 6.3     46.5      lentil
    ## 883   14  74  15        28.0     65.6 6.5     49.9      lentil
    ## 884   14  76  20        29.1     62.1 7.0     36.5      lentil
    ## 885   36  65  16        25.7     64.1 7.7     50.2      lentil
    ## 886   28  67  21        21.8     63.7 6.3     46.6      lentil
    ## 887   28  79  16        24.7     60.3 6.1     53.1      lentil
    ## 888   40  61  22        20.9     65.8 7.0     44.2      lentil
    ## 889   10  70  19        24.8     69.0 7.3     41.6      lentil
    ## 890   12  80  19        21.9     65.2 6.0     36.1      lentil
    ## 891   37  77  20        25.9     68.7 7.1     51.0      lentil
    ## 892    0  67  22        29.8     69.4 6.6     51.6      lentil
    ## 893    7  73  25        27.5     63.1 7.3     45.2      lentil
    ## 894   10  56  18        28.0     68.6 7.3     46.1      lentil
    ## 895   39  70  15        20.8     63.9 6.4     47.9      lentil
    ## 896   26  56  22        23.1     60.4 7.0     52.6      lentil
    ## 897    9  77  17        21.7     63.6 6.3     38.1      lentil
    ## 898    4  59  19        26.3     67.6 7.6     40.8      lentil
    ## 899   34  73  15        21.0     63.8 7.6     53.1      lentil
    ## 900   33  77  15        23.9     66.3 7.8     40.7      lentil
    ## 901    2  24  38        24.6     91.6 5.9    112.0 pomegranate
    ## 902    6  18  37        19.7     89.9 5.9    108.0 pomegranate
    ## 903    8  26  36        18.8     87.4 6.8    102.5 pomegranate
    ## 904   37  18  39        24.1     94.5 6.4    110.2 pomegranate
    ## 905    0  27  38        22.4     89.9 6.7    109.4 pomegranate
    ## 906   31  25  38        25.0     92.4 6.5    109.4 pomegranate
    ## 907   21  21  38        22.6     89.3 6.3    104.9 pomegranate
    ## 908    6  30  40        22.8     91.5 6.4    107.0 pomegranate
    ## 909   25  27  41        19.2     94.3 6.9    108.0 pomegranate
    ## 910   15  11  38        23.1     92.7 6.6    109.4 pomegranate
    ## 911   14   5  36        24.9     85.2 5.8    104.8 pomegranate
    ## 912   16  10  41        24.8     85.6 6.7    105.8 pomegranate
    ## 913   36   7  37        19.9     86.4 5.8    108.3 pomegranate
    ## 914    4  20  41        24.3     93.8 6.5    104.5 pomegranate
    ## 915   29  22  40        23.6     89.7 6.1    107.7 pomegranate
    ## 916   16  15  42        19.7     89.1 6.9    108.5 pomegranate
    ## 917   18  27  41        22.4     92.3 7.2    104.8 pomegranate
    ## 918   11  18  42        21.6     94.9 5.9    102.9 pomegranate
    ## 919    5  15  38        18.3     88.2 5.7    108.1 pomegranate
    ## 920   18  23  44        23.7     89.6 6.2    105.6 pomegranate
    ## 921    9   8  40        22.5     89.9 6.6    111.7 pomegranate
    ## 922   40  27  45        21.7     94.8 5.9    112.4 pomegranate
    ## 923   22  23  44        20.1     89.3 6.1    107.3 pomegranate
    ## 924    9  16  39        18.4     91.1 6.1    105.2 pomegranate
    ## 925   12  29  40        19.7     89.8 6.6    111.3 pomegranate
    ## 926    0  17  42        23.2     91.2 6.9    109.1 pomegranate
    ## 927    2  21  44        18.9     87.3 6.6    102.8 pomegranate
    ## 928   28   6  40        22.1     91.3 6.8    106.9 pomegranate
    ## 929    8  23  44        18.5     89.7 7.1    108.5 pomegranate
    ## 930   29  16  36        19.8     88.9 5.7    102.9 pomegranate
    ## 931   17  18  43        24.5     90.8 5.8    103.2 pomegranate
    ## 932   34  21  42        18.8     89.9 6.6    111.0 pomegranate
    ## 933   21  23  42        19.5     90.3 6.9    104.4 pomegranate
    ## 934   25  17  40        18.9     87.7 6.6    111.3 pomegranate
    ## 935    8  25  36        19.9     95.0 6.8    104.0 pomegranate
    ## 936   26  18  42        19.7     89.6 6.9    108.2 pomegranate
    ## 937    4  19  42        23.8     87.8 6.3    111.2 pomegranate
    ## 938   36  24  41        24.9     94.3 7.0    103.9 pomegranate
    ## 939    5  24  40        24.7     93.9 6.3    104.7 pomegranate
    ## 940   19  17  39        24.7     85.6 6.7    111.3 pomegranate
    ## 941   39  30  38        20.1     87.6 7.0    108.1 pomegranate
    ## 942    5  29  44        21.0     93.1 5.6    104.8 pomegranate
    ## 943    4  24  43        22.4     88.2 7.2    109.9 pomegranate
    ## 944   38  21  35        20.3     89.4 5.8    111.0 pomegranate
    ## 945   37  11  36        24.2     85.6 6.7    106.9 pomegranate
    ## 946    9  25  41        24.8     91.9 6.0    109.3 pomegranate
    ## 947   29  22  43        19.7     88.0 5.6    106.0 pomegranate
    ## 948    5  21  38        22.4     90.3 6.1    112.5 pomegranate
    ## 949   22  26  38        22.9     85.1 7.0    110.2 pomegranate
    ## 950    4  18  37        22.9     85.4 7.1    106.3 pomegranate
    ## 951   21   6  41        24.9     89.4 7.1    107.2 pomegranate
    ## 952   29  21  45        23.4     93.1 6.7    105.2 pomegranate
    ## 953   23   5  44        21.2     94.3 7.2    107.6 pomegranate
    ## 954   13   7  43        18.2     91.1 7.0    109.7 pomegranate
    ## 955    5  13  37        22.3     89.8 5.6    103.3 pomegranate
    ## 956   27  24  41        24.3     90.9 6.6    110.5 pomegranate
    ## 957    7  23  35        19.8     88.7 7.1    102.6 pomegranate
    ## 958   12  20  39        19.9     86.2 6.0    111.0 pomegranate
    ## 959    4  19  43        18.1     93.1 5.8    106.4 pomegranate
    ## 960    3   9  45        23.9     89.6 6.5    104.6 pomegranate
    ## 961    1  27  36        24.0     93.3 5.7    105.0 pomegranate
    ## 962   23  30  44        20.9     85.4 6.1    103.0 pomegranate
    ## 963   24  21  42        20.8     87.2 7.0    109.4 pomegranate
    ## 964   13  30  37        20.9     91.6 6.3    106.9 pomegranate
    ## 965   40  11  44        24.5     86.1 6.3    111.4 pomegranate
    ## 966   21   9  40        24.5     90.6 6.0    105.6 pomegranate
    ## 967    3  27  44        24.6     92.0 6.6    111.0 pomegranate
    ## 968   40  29  42        24.6     89.0 7.1    110.7 pomegranate
    ## 969   14  25  40        20.1     91.0 6.4    103.7 pomegranate
    ## 970   38  14  37        21.8     94.6 6.7    102.6 pomegranate
    ## 971   34   9  36        22.8     86.3 6.3    110.4 pomegranate
    ## 972   32  14  37        22.7     88.5 6.8    104.7 pomegranate
    ## 973   18  21  35        23.3     94.9 6.4    111.1 pomegranate
    ## 974    8  23  38        19.3     87.2 7.0    105.5 pomegranate
    ## 975   15   6  41        19.0     88.8 6.9    108.7 pomegranate
    ## 976    0   5  36        24.4     90.9 6.2    105.5 pomegranate
    ## 977   22   9  44        24.7     88.9 5.7    112.2 pomegranate
    ## 978   14   8  43        21.9     94.5 7.1    111.7 pomegranate
    ## 979   31  11  45        24.8     86.9 6.0    107.6 pomegranate
    ## 980   39  17  45        18.1     90.4 6.9    104.9 pomegranate
    ## 981   10   5  42        20.2     91.1 6.9    109.3 pomegranate
    ## 982    8  28  38        23.2     94.4 6.8    105.7 pomegranate
    ## 983   32  13  42        23.5     93.0 5.8    106.6 pomegranate
    ## 984   18   9  40        19.4     89.0 5.6    106.2 pomegranate
    ## 985   20  27  41        20.5     92.5 5.7    110.6 pomegranate
    ## 986   39  25  36        18.9     95.0 5.6    107.6 pomegranate
    ## 987   20   7  45        18.9     89.2 6.1    112.5 pomegranate
    ## 988   11  10  45        22.6     88.5 6.4    109.0 pomegranate
    ## 989   40  18  43        19.4     86.8 5.8    109.9 pomegranate
    ## 990    3  26  39        24.4     91.2 7.1    103.6 pomegranate
    ## 991    9  16  36        23.8     92.9 5.9    107.0 pomegranate
    ## 992   30  20  38        22.6     93.2 7.1    110.1 pomegranate
    ## 993   40   9  41        24.4     85.4 5.8    106.1 pomegranate
    ## 994   40  30  35        20.9     91.1 6.3    104.4 pomegranate
    ## 995   32  25  35        18.1     85.7 5.9    107.0 pomegranate
    ## 996   33  23  45        20.0     85.8 7.1    112.3 pomegranate
    ## 997    4  14  41        19.9     89.8 6.4    102.8 pomegranate
    ## 998   13  17  45        21.3     92.7 7.2    106.3 pomegranate
    ## 999   39  24  39        23.7     93.3 6.4    109.8 pomegranate
    ## 1000   8  28  37        23.9     86.2 6.1    108.3 pomegranate
    ## 1001  91  94  46        29.4     76.2 6.1     92.8      banana
    ## 1002 105  95  50        27.3     83.7 5.8    101.0      banana
    ## 1003 108  92  53        27.4     83.0 6.3    104.9      banana
    ## 1004  86  76  54        29.3     80.1 5.9     90.1      banana
    ## 1005  80  77  49        26.1     79.4 5.5    113.2      banana
    ## 1006  93  94  53        25.9     84.4 6.1    114.5      banana
    ## 1007  90  92  55        27.0     80.2 6.1     97.3      banana
    ## 1008 108  89  53        29.6     78.1 5.8     99.3      banana
    ## 1009 108  88  55        26.3     83.4 5.9    113.9      banana
    ## 1010 105  77  52        29.2     76.2 5.8    100.0      banana
    ## 1011 118  88  52        28.7     82.7 5.8     98.8      banana
    ## 1012 101  87  54        29.1     76.5 6.4    100.2      banana
    ## 1013  95  75  50        28.1     75.3 5.6    118.3      banana
    ## 1014 106  85  53        27.2     78.8 5.9     99.7      banana
    ## 1015  86  95  49        28.1     78.0 6.5    108.4      banana
    ## 1016  83  79  55        25.1     83.3 5.6     98.7      banana
    ## 1017  85  95  47        25.9     78.3 6.2    119.8      banana
    ## 1018 109  79  45        27.7     79.7 6.5    108.7      banana
    ## 1019 100  76  45        25.6     75.9 5.6    102.8      banana
    ## 1020 117  86  48        28.7     82.5 6.2    116.2      banana
    ## 1021 114  94  53        26.3     76.9 6.2    118.7      banana
    ## 1022 110  78  50        25.9     78.9 5.9     98.2      banana
    ## 1023  94  70  48        25.1     84.9 6.2     91.5      banana
    ## 1024  80  71  47        27.5     80.8 6.2    105.1      banana
    ## 1025 114  79  51        26.2     82.3 6.3    112.1      banana
    ## 1026  88  78  45        29.1     79.2 6.3     92.1      banana
    ## 1027 112  73  48        29.2     77.3 5.7     90.7      banana
    ## 1028 117  76  47        25.6     77.4 6.1     93.1      banana
    ## 1029 111  87  48        26.4     81.4 5.6     98.2      banana
    ## 1030  89  83  47        28.1     77.8 5.6    109.5      banana
    ## 1031  93  91  47        27.8     83.3 6.1    117.3      banana
    ## 1032  92  81  52        27.4     81.5 6.4     94.3      banana
    ## 1033 105  74  45        25.1     81.4 6.1    119.2      banana
    ## 1034 102  71  48        28.7     79.3 5.7    102.5      banana
    ## 1035  94  91  51        29.2     76.7 5.6    109.6      banana
    ## 1036 116  71  47        27.6     82.1 6.4     91.3      banana
    ## 1037 117  79  49        25.4     82.4 6.2    113.0      banana
    ## 1038 119  72  55        26.0     83.3 6.2    112.1      banana
    ## 1039  99  73  53        26.3     81.1 5.9    118.7      banana
    ## 1040  91  84  52        29.1     78.7 6.4    117.5      banana
    ## 1041  80  90  47        26.6     79.4 6.2    107.4      banana
    ## 1042 101  70  48        25.4     75.0 6.0    116.6      banana
    ## 1043 108  89  53        29.1     80.2 5.9    112.4      banana
    ## 1044 100  80  52        27.5     77.3 6.0    110.3      banana
    ## 1045 109  91  53        29.7     83.5 6.0    110.3      banana
    ## 1046  82  78  46        25.1     85.0 5.7    110.4      banana
    ## 1047 106  70  55        25.9     78.5 5.7    116.3      banana
    ## 1048  90  86  52        25.9     82.0 5.8    119.1      banana
    ## 1049  83  95  50        26.5     77.8 5.5    108.9      banana
    ## 1050 119  90  48        28.7     79.6 6.0    118.3      banana
    ## 1051 107  72  45        28.1     81.5 5.8     91.4      banana
    ## 1052 116  81  55        26.4     83.7 5.9     95.1      banana
    ## 1053 101  75  50        26.6     81.4 6.2    110.0      banana
    ## 1054  93  81  50        27.7     76.6 6.0    102.2      banana
    ## 1055  95  75  45        29.0     83.0 5.8    109.0      banana
    ## 1056 107  71  55        29.4     84.0 6.1    117.2      banana
    ## 1057  83  94  47        27.4     81.1 6.5    112.1      banana
    ## 1058 102  73  54        26.4     84.4 5.7    111.0      banana
    ## 1059  86  79  45        27.8     82.7 5.8     99.2      banana
    ## 1060 117  86  53        25.2     83.6 5.7    115.9      banana
    ## 1061 111  79  53        28.3     75.8 6.2    119.7      banana
    ## 1062  95  74  50        25.9     80.5 6.0    110.1      banana
    ## 1063  91  75  55        27.5     76.1 6.2    109.3      banana
    ## 1064  93  83  46        29.4     83.5 5.8    109.2      banana
    ## 1065  92  85  51        29.2     81.1 5.7    108.9      banana
    ## 1066 104  80  54        27.1     81.3 5.9    110.1      banana
    ## 1067 103  72  51        26.1     81.8 6.1    104.5      banana
    ## 1068  92  75  45        29.0     78.0 5.7     90.4      banana
    ## 1069  93  85  49        28.0     79.3 5.7    119.5      banana
    ## 1070 120  87  52        28.1     76.1 5.9    119.0      banana
    ## 1071 108  72  46        25.2     85.0 6.1     90.9      banana
    ## 1072 105  88  54        25.8     84.5 6.0    114.2      banana
    ## 1073  98  79  50        25.3     84.5 6.4     91.1      banana
    ## 1074 111  88  55        29.4     78.3 5.5     96.5      banana
    ## 1075  97  74  45        26.5     78.5 5.7    113.1      banana
    ## 1076  95  82  48        27.4     83.3 5.7     92.8      banana
    ## 1077  89  91  55        25.1     80.3 6.3     94.3      banana
    ## 1078  89  85  55        26.7     76.5 6.3     91.7      banana
    ## 1079 118  88  51        25.4     79.5 6.2    100.7      banana
    ## 1080 101  92  45        28.2     80.6 5.8     98.0      banana
    ## 1081  99  92  47        28.1     77.5 6.3    103.5      banana
    ## 1082  82  77  46        28.9     82.2 5.9     95.8      banana
    ## 1083  90  86  55        28.0     84.2 5.6     97.6      banana
    ## 1084  95  88  52        28.0     78.9 6.2     94.7      banana
    ## 1085 104  73  46        29.1     80.1 6.3     90.5      banana
    ## 1086 102  73  52        27.9     83.4 6.4     90.2      banana
    ## 1087 100  74  52        25.4     81.5 5.8     96.5      banana
    ## 1088  94  89  48        28.6     84.5 5.7    111.1      banana
    ## 1089  99  70  46        26.6     83.0 5.7    100.5      banana
    ## 1090 112  87  48        27.2     77.4 6.2     99.5      banana
    ## 1091 117  82  45        25.3     79.3 5.6    105.4      banana
    ## 1092  96  86  51        29.9     77.0 6.3     92.0      banana
    ## 1093 113  85  45        27.9     76.6 6.0    109.1      banana
    ## 1094 105  93  46        25.0     78.8 5.8    108.4      banana
    ## 1095  85  89  51        29.2     84.7 6.2    108.6      banana
    ## 1096 108  94  47        27.4     84.5 6.4     90.8      banana
    ## 1097  92  81  52        28.0     76.5 5.9    103.7      banana
    ## 1098 110  71  54        28.7     82.2 5.7     94.4      banana
    ## 1099  82  75  55        27.3     78.5 6.3     92.2      banana
    ## 1100 117  81  53        29.5     78.2 5.5     98.1      banana
    ## 1101   2  40  27        29.7     47.5 6.0     90.1       mango
    ## 1102  39  24  31        33.6     53.7 4.8     98.7       mango
    ## 1103  21  26  27        27.0     47.7 5.7     95.9       mango
    ## 1104  25  22  25        33.6     45.5 6.0     95.7       mango
    ## 1105   0  21  32        35.9     54.3 6.4     92.2       mango
    ## 1106  20  19  35        34.2     50.6 6.1     98.0       mango
    ## 1107  19  21  34        30.0     53.2 5.1     97.7       mango
    ## 1108  18  17  31        31.7     45.2 5.7     93.8       mango
    ## 1109  11  36  33        36.0     52.2 6.0     95.4       mango
    ## 1110  30  28  30        31.9     52.2 5.1     98.5       mango
    ## 1111  18  19  27        27.8     52.3 4.8     94.1       mango
    ## 1112  23  23  27        34.7     51.4 5.2     97.3       mango
    ## 1113  37  30  34        27.5     53.6 6.8     99.4       mango
    ## 1114  11  27  30        27.7     48.6 6.4     89.9       mango
    ## 1115  12  19  31        27.3     52.7 5.6     91.9       mango
    ## 1116   3  28  33        30.3     48.9 5.8     94.4       mango
    ## 1117  37  38  32        31.9     45.5 5.4     91.6       mango
    ## 1118  26  37  30        35.4     49.5 6.2     97.4       mango
    ## 1119  14  18  30        29.8     52.1 5.2     95.7       mango
    ## 1120  40  16  35        34.2     54.2 5.0     98.3       mango
    ## 1121   4  20  25        28.9     47.9 5.7    100.0       mango
    ## 1122  36  25  33        28.0     53.3 5.5     99.6       mango
    ## 1123  30  17  31        31.2     54.5 6.8     94.6       mango
    ## 1124  28  37  28        32.1     50.5 6.1     98.6       mango
    ## 1125  38  15  30        28.9     48.1 5.1     97.0       mango
    ## 1126  12  37  30        31.1     47.4 4.5     90.3       mango
    ## 1127  38  19  31        34.7     49.1 5.9     90.7       mango
    ## 1128   8  33  29        30.0     49.5 6.4     91.8       mango
    ## 1129  15  27  28        33.8     46.1 4.5     90.8       mango
    ## 1130  34  16  25        30.1     51.0 6.1     92.1       mango
    ## 1131  11  36  31        27.9     51.8 6.5    100.3       mango
    ## 1132  33  29  34        31.4     49.2 6.8     93.0       mango
    ## 1133  12  31  26        35.8     51.9 5.4    100.2       mango
    ## 1134  12  34  28        33.4     45.0 6.1     98.8       mango
    ## 1135   5  16  31        36.0     48.7 4.6     98.0       mango
    ## 1136   1  30  29        28.3     51.4 6.4     91.7       mango
    ## 1137  16  35  31        32.3     50.2 5.3     96.0       mango
    ## 1138  35  18  26        32.0     50.8 5.3     97.4       mango
    ## 1139   4  40  26        27.6     48.6 6.7     95.8       mango
    ## 1140   9  29  34        29.4     45.9 5.7    100.8       mango
    ## 1141   2  38  33        32.4     53.2 4.7     90.2       mango
    ## 1142  26  32  32        30.9     49.9 6.8     90.1       mango
    ## 1143  34  38  31        35.4     45.6 6.5     97.4       mango
    ## 1144   5  32  33        32.3     52.6 5.8     93.4       mango
    ## 1145  31  29  26        28.2     47.4 5.0     97.8       mango
    ## 1146  34  34  35        27.3     47.2 6.4     95.3       mango
    ## 1147  36  19  32        27.1     50.7 4.9     92.4       mango
    ## 1148   7  17  26        34.9     48.8 6.4     91.6       mango
    ## 1149  38  15  27        33.7     48.5 6.8     92.3       mango
    ## 1150   5  19  25        27.4     54.4 6.4     96.3       mango
    ## 1151  37  36  26        32.9     52.6 4.7     94.5       mango
    ## 1152  21  31  32        35.4     51.4 5.3     90.3       mango
    ## 1153  37  36  27        27.6     47.9 5.9     90.4       mango
    ## 1154  23  23  30        32.8     47.5 4.8     90.9       mango
    ## 1155  36  26  26        30.2     51.1 6.8     95.2       mango
    ## 1156  24  33  35        29.3     54.8 5.3    100.8       mango
    ## 1157  26  18  30        32.1     51.1 6.3     96.6       mango
    ## 1158  22  17  26        28.7     47.7 4.8     99.6       mango
    ## 1159  11  34  32        29.1     49.4 6.8     97.6       mango
    ## 1160  29  35  28        28.3     53.5 7.0     90.4       mango
    ## 1161  22  28  26        27.7     45.4 4.9     92.8       mango
    ## 1162  23  24  32        28.1     46.2 5.6     93.3       mango
    ## 1163   1  35  34        30.8     46.7 6.3     92.2       mango
    ## 1164   2  24  34        28.9     54.8 6.5     94.8       mango
    ## 1165  39  37  25        33.3     45.6 7.0     98.3       mango
    ## 1166  15  36  27        27.8     54.0 5.6     91.0       mango
    ## 1167   3  18  31        31.7     48.2 6.4     91.1       mango
    ## 1168   8  38  32        29.8     46.7 5.0     91.4       mango
    ## 1169  33  31  34        31.3     50.2 5.4     89.8       mango
    ## 1170  14  29  32        35.6     49.0 6.9     97.5       mango
    ## 1171  18  20  26        31.7     52.0 5.4     90.0       mango
    ## 1172   9  21  32        32.3     53.6 5.9     95.9       mango
    ## 1173  20  30  27        27.8     51.6 4.7     95.9       mango
    ## 1174   9  38  25        34.6     50.3 5.5    100.3       mango
    ## 1175  26  24  34        31.3     52.2 6.8     89.7       mango
    ## 1176  31  36  29        33.9     52.7 6.5     97.5       mango
    ## 1177  14  18  35        31.1     47.0 4.8     91.5       mango
    ## 1178  40  16  35        31.9     49.0 6.5     89.6       mango
    ## 1179  28  27  34        32.5     50.7 6.5     95.0       mango
    ## 1180   0  17  30        35.5     48.0 6.3     97.8       mango
    ## 1181   1  29  29        27.3     49.3 6.1     93.5       mango
    ## 1182   2  36  31        30.9     50.0 5.7     91.8       mango
    ## 1183  12  27  26        29.1     45.6 5.3     96.2       mango
    ## 1184   7  28  35        30.0     46.8 4.7     96.6       mango
    ## 1185   0  36  26        34.1     51.3 5.1     96.4       mango
    ## 1186  26  35  31        33.4     53.1 5.3     98.1       mango
    ## 1187  27  21  30        35.4     52.5 5.1     91.2       mango
    ## 1188  22  38  31        31.5     53.1 5.8     98.6       mango
    ## 1189  22  18  31        30.8     47.9 6.0     90.4       mango
    ## 1190  28  23  28        30.0     50.1 5.7     96.1       mango
    ## 1191   7  31  27        31.3     47.6 6.5     94.7       mango
    ## 1192  29  34  26        33.9     54.4 6.3     89.3       mango
    ## 1193   8  37  33        28.1     55.0 6.1     97.5       mango
    ## 1194  39  16  27        35.5     52.9 4.9     91.5       mango
    ## 1195  40  24  25        28.7     50.4 5.4     95.9       mango
    ## 1196  19  38  26        31.5     48.8 4.5     93.2       mango
    ## 1197  21  21  30        27.7     51.4 5.4    100.8       mango
    ## 1198  22  18  33        30.4     52.5 6.6     93.9       mango
    ## 1199  31  20  30        32.2     54.0 6.2     91.9       mango
    ## 1200  18  26  31        32.6     47.7 5.4     91.1       mango
    ## 1201  24 130 195        30.0     81.5 6.1     67.1      grapes
    ## 1202  13 144 204        30.7     82.4 6.1     68.4      grapes
    ## 1203  22 123 205        32.4     83.9 5.9     68.7      grapes
    ## 1204  36 125 196        37.5     80.7 6.2     66.8      grapes
    ## 1205  24 131 196        22.0     83.7 5.7     65.3      grapes
    ## 1206   2 123 198        39.6     82.2 6.3     70.4      grapes
    ## 1207  35 140 197        16.8     82.8 6.1     66.8      grapes
    ## 1208  11 122 195        12.1     83.6 5.6     69.6      grapes
    ## 1209   6 123 203        12.8     81.6 6.1     66.8      grapes
    ## 1210  17 134 204        39.0     80.2 6.5     73.9      grapes
    ## 1211  25 130 197        39.7     82.7 5.6     74.9      grapes
    ## 1212  27 145 205         9.5     82.3 5.8     66.0      grapes
    ## 1213   9 122 201        29.6     80.9 5.6     68.1      grapes
    ## 1214  16 139 203        17.8     81.0 6.3     65.8      grapes
    ## 1215  32 141 204         8.8     82.9 5.5     67.2      grapes
    ## 1216  22 138 195        27.8     83.5 6.2     73.0      grapes
    ## 1217  31 144 202        11.0     80.6 5.9     68.2      grapes
    ## 1218   3 136 205        17.6     80.8 6.3     71.4      grapes
    ## 1219  28 122 197        19.9     82.7 5.9     69.7      grapes
    ## 1220   4 136 204        29.9     81.8 5.9     65.5      grapes
    ## 1221  39 145 201        36.7     80.6 5.8     72.2      grapes
    ## 1222  38 132 197        20.4     81.5 5.9     66.9      grapes
    ## 1223  36 133 198        25.5     84.0 6.2     69.2      grapes
    ## 1224  25 121 201        30.5     82.7 5.6     70.1      grapes
    ## 1225  15 125 199        18.4     80.6 5.6     69.8      grapes
    ## 1226  24 140 205        12.1     83.6 5.9     68.7      grapes
    ## 1227  13 132 203        23.6     82.5 6.4     73.2      grapes
    ## 1228   5 126 197        12.8     81.2 6.4     67.1      grapes
    ## 1229  30 120 200        38.1     82.2 6.2     65.7      grapes
    ## 1230  23 142 197        39.1     82.0 6.0     69.3      grapes
    ## 1231  26 135 203        33.8     81.2 5.7     74.5      grapes
    ## 1232   7 126 203        16.8     82.0 5.7     73.3      grapes
    ## 1233  32 139 198        35.9     82.7 6.4     66.5      grapes
    ## 1234   9 141 202        21.0     81.2 6.1     66.4      grapes
    ## 1235  20 142 196        10.9     80.0 6.2     68.7      grapes
    ## 1236  32 129 201        16.4     83.0 6.5     71.6      grapes
    ## 1237   3 134 199        20.3     81.3 5.8     71.1      grapes
    ## 1238  38 138 204        25.1     83.3 6.3     73.0      grapes
    ## 1239  14 131 198        33.5     83.9 5.6     67.9      grapes
    ## 1240  20 122 204        11.8     80.9 6.5     65.1      grapes
    ## 1241  40 126 201        11.4     80.0 6.1     71.2      grapes
    ## 1242  36 128 204        25.2     80.7 5.7     67.0      grapes
    ## 1243  11 132 197        16.0     81.2 5.7     74.4      grapes
    ## 1244   0 137 195        22.4     80.2 6.3     65.4      grapes
    ## 1245  19 123 200        34.8     81.0 6.2     65.7      grapes
    ## 1246  31 136 197        31.1     83.3 5.7     71.4      grapes
    ## 1247   4 134 200        28.6     81.0 5.8     73.3      grapes
    ## 1248  39 139 201        41.2     81.0 5.5     68.7      grapes
    ## 1249   8 127 196        27.0     83.2 5.8     71.0      grapes
    ## 1250  39 138 203        21.2     82.3 6.4     74.6      grapes
    ## 1251  32 120 204        10.4     83.4 6.1     67.4      grapes
    ## 1252  12 142 203        31.3     82.6 6.0     65.0      grapes
    ## 1253   8 133 195        20.5     81.0 6.5     71.3      grapes
    ## 1254   8 139 199        29.4     81.5 6.3     66.1      grapes
    ## 1255  21 134 202        10.7     80.0 6.4     65.3      grapes
    ## 1256  40 140 195        15.0     80.5 6.3     71.6      grapes
    ## 1257  39 127 202        15.3     81.7 6.5     71.6      grapes
    ## 1258  19 120 195        18.7     81.1 5.9     73.6      grapes
    ## 1259  21 139 201        19.4     83.4 6.0     67.2      grapes
    ## 1260  17 136 195        41.2     81.6 6.4     65.9      grapes
    ## 1261  33 139 203        33.3     82.5 5.7     70.7      grapes
    ## 1262  22 133 201        23.8     80.1 6.0     67.3      grapes
    ## 1263  32 130 196        40.7     81.2 6.4     74.0      grapes
    ## 1264  37 135 205        11.8     80.3 5.5     74.1      grapes
    ## 1265  15 140 195        13.3     83.5 5.7     65.8      grapes
    ## 1266  39 132 196        35.8     83.3 5.8     73.7      grapes
    ## 1267  40 121 199        26.2     81.0 6.3     66.1      grapes
    ## 1268  40 132 202        24.6     80.7 6.0     69.7      grapes
    ## 1269  29 142 203        29.7     83.7 5.9     66.5      grapes
    ## 1270  32 121 199        39.4     81.3 6.1     74.1      grapes
    ## 1271   6 140 205        17.7     82.9 6.3     69.9      grapes
    ## 1272   8 120 196        24.1     82.7 6.1     69.8      grapes
    ## 1273  34 133 202        15.3     80.1 5.8     74.8      grapes
    ## 1274  35 135 199        21.8     80.5 6.4     69.4      grapes
    ## 1275  16 145 199        26.9     80.8 6.0     69.3      grapes
    ## 1276   8 136 201        41.7     82.2 5.6     74.2      grapes
    ## 1277  25 129 195        18.0     81.2 5.8     72.4      grapes
    ## 1278  16 130 201        29.1     82.8 5.7     68.9      grapes
    ## 1279  39 129 203        34.4     83.2 5.9     71.0      grapes
    ## 1280  38 135 203        41.4     82.8 6.4     69.9      grapes
    ## 1281  33 120 205        35.1     82.3 5.6     69.7      grapes
    ## 1282  35 125 204        19.6     80.2 6.1     73.7      grapes
    ## 1283   1 132 200        16.3     82.9 5.6     66.6      grapes
    ## 1284  39 140 203        21.1     80.6 6.3     69.3      grapes
    ## 1285  28 145 202        19.2     82.9 6.5     66.8      grapes
    ## 1286   6 128 200        26.0     82.6 5.8     70.3      grapes
    ## 1287   6 139 199        25.7     81.6 6.3     74.1      grapes
    ## 1288  29 122 196        41.9     81.2 5.6     73.1      grapes
    ## 1289  37 144 197        11.2     80.8 6.4     66.3      grapes
    ## 1290  38 120 197        17.5     82.9 6.3     73.8      grapes
    ## 1291  38 141 198        13.1     80.3 5.8     70.8      grapes
    ## 1292  14 121 203         9.7     83.7 6.2     74.5      grapes
    ## 1293   6 125 204        27.9     82.9 5.7     69.9      grapes
    ## 1294  32 138 197         9.5     80.7 5.9     69.4      grapes
    ## 1295  11 124 204        13.4     80.1 6.4     71.4      grapes
    ## 1296  23 138 200         9.9     80.2 6.0     68.4      grapes
    ## 1297  40 143 201        25.0     82.7 6.5     66.7      grapes
    ## 1298   6 142 202        27.2     82.9 6.2     70.4      grapes
    ## 1299  37 124 195        18.7     83.5 6.2     66.6      grapes
    ## 1300  35 134 204         9.9     82.6 5.8     66.0      grapes
    ## 1301 119  25  51        26.5     80.9 6.3     53.7  watermelon
    ## 1302 119  19  55        25.2     83.4 6.8     46.9  watermelon
    ## 1303 105  30  50        25.3     81.8 6.4     57.0  watermelon
    ## 1304 114   8  50        24.7     88.3 6.6     58.0  watermelon
    ## 1305  93  22  52        26.6     81.3 6.9     41.9  watermelon
    ## 1306  80  26  55        24.5     89.0 6.1     49.1  watermelon
    ## 1307  85  27  45        26.1     88.7 6.5     57.8  watermelon
    ## 1308  85  22  53        26.0     89.8 6.8     59.5  watermelon
    ## 1309  82  22  45        26.2     85.3 6.5     54.6  watermelon
    ## 1310 118  13  54        24.4     89.8 6.0     44.1  watermelon
    ## 1311  83  25  53        26.5     80.0 6.1     57.7  watermelon
    ## 1312  86  15  47        24.0     84.2 6.4     53.8  watermelon
    ## 1313 101  10  47        25.5     83.3 6.9     57.6  watermelon
    ## 1314 119   9  50        26.7     83.9 6.3     40.8  watermelon
    ## 1315 104  17  46        25.7     80.2 6.2     43.1  watermelon
    ## 1316  95  12  51        25.8     84.2 6.7     44.2  watermelon
    ## 1317 102  14  52        26.8     89.6 6.5     57.7  watermelon
    ## 1318 109  21  55        24.9     89.7 6.8     57.4  watermelon
    ## 1319  81  18  50        26.8     88.2 6.4     58.8  watermelon
    ## 1320 103  17  51        25.1     80.0 6.2     44.2  watermelon
    ## 1321 105  14  50        26.2     87.7 6.4     59.7  watermelon
    ## 1322  97   8  52        24.9     87.0 6.2     49.5  watermelon
    ## 1323 120  19  49        25.8     84.3 6.8     56.5  watermelon
    ## 1324  95  16  55        25.3     87.6 6.6     40.1  watermelon
    ## 1325  83  29  52        25.8     87.6 6.7     46.1  watermelon
    ## 1326  83   9  45        25.9     89.1 6.0     46.9  watermelon
    ## 1327  91  21  50        24.3     81.4 6.8     48.3  watermelon
    ## 1328 116   5  54        25.4     81.0 6.7     57.2  watermelon
    ## 1329 112  28  54        24.9     85.1 6.7     55.3  watermelon
    ## 1330  88  29  51        24.7     88.9 6.1     48.5  watermelon
    ## 1331 118  15  45        24.2     84.2 6.5     48.0  watermelon
    ## 1332  92  21  48        25.8     82.0 6.4     54.8  watermelon
    ## 1333 106  14  45        24.5     84.2 6.4     57.3  watermelon
    ## 1334  99   5  47        24.1     84.8 6.6     51.2  watermelon
    ## 1335  98   8  51        26.2     86.5 6.3     49.4  watermelon
    ## 1336 108  22  46        26.2     86.7 6.1     53.3  watermelon
    ## 1337 119   7  55        26.0     84.6 6.0     44.4  watermelon
    ## 1338 117  27  48        26.5     82.4 6.8     54.3  watermelon
    ## 1339 109  10  53        26.8     87.8 6.6     46.1  watermelon
    ## 1340  80  16  46        25.5     81.4 6.9     48.5  watermelon
    ## 1341 100  18  52        26.2     80.4 6.9     56.5  watermelon
    ## 1342  91   7  53        25.1     89.3 6.5     43.5  watermelon
    ## 1343  86   6  53        25.9     83.5 6.9     42.1  watermelon
    ## 1344 107   5  52        26.7     90.0 6.9     57.4  watermelon
    ## 1345 103  16  49        24.1     81.6 6.9     51.8  watermelon
    ## 1346 101  20  48        24.7     82.8 6.2     57.1  watermelon
    ## 1347  85  25  47        26.1     87.6 6.3     58.5  watermelon
    ## 1348  84   7  51        26.8     87.7 6.4     55.7  watermelon
    ## 1349 102  28  54        25.2     80.3 6.9     55.5  watermelon
    ## 1350  98  25  52        25.3     83.2 6.2     49.3  watermelon
    ## 1351  97  25  50        26.2     80.9 6.1     49.1  watermelon
    ## 1352  90  16  45        24.9     80.6 6.3     50.6  watermelon
    ## 1353  95  12  46        26.2     81.0 6.3     54.7  watermelon
    ## 1354  82  23  49        26.8     87.2 6.9     51.7  watermelon
    ## 1355  82  25  51        24.3     87.5 6.1     48.1  watermelon
    ## 1356 110  28  46        24.3     88.0 6.5     51.3  watermelon
    ## 1357 118  21  51        24.4     86.3 6.7     48.6  watermelon
    ## 1358 120  20  45        25.7     88.7 6.1     54.2  watermelon
    ## 1359  91   7  52        25.1     83.5 6.4     56.4  watermelon
    ## 1360  81   6  55        24.9     85.9 6.1     51.7  watermelon
    ## 1361 101  13  54        25.4     82.9 6.8     56.3  watermelon
    ## 1362 101  17  55        24.4     87.1 6.5     44.6  watermelon
    ## 1363 111   6  53        26.5     88.6 6.3     46.1  watermelon
    ## 1364 107  10  49        25.8     89.0 6.8     45.2  watermelon
    ## 1365 115  11  46        24.4     89.4 6.6     40.3  watermelon
    ## 1366  84  25  52        24.4     81.3 6.1     44.2  watermelon
    ## 1367 120   7  47        24.2     83.0 6.7     54.8  watermelon
    ## 1368  91  12  46        24.6     85.5 6.3     48.3  watermelon
    ## 1369  89  22  52        24.9     86.1 6.2     53.1  watermelon
    ## 1370 113  19  46        25.4     81.1 6.3     49.5  watermelon
    ## 1371  97  22  50        26.3     86.1 6.8     59.0  watermelon
    ## 1372 117  30  50        24.9     87.2 6.7     46.6  watermelon
    ## 1373  90  14  52        24.8     89.2 6.4     59.7  watermelon
    ## 1374 104  23  47        27.0     86.7 6.8     42.9  watermelon
    ## 1375  81  16  45        26.9     86.3 6.7     59.8  watermelon
    ## 1376  88   5  47        25.9     86.7 6.7     41.2  watermelon
    ## 1377  92   7  45        26.7     81.1 6.9     51.5  watermelon
    ## 1378  81  18  50        26.4     80.9 6.5     47.8  watermelon
    ## 1379 111   5  55        26.3     84.4 6.5     50.8  watermelon
    ## 1380 108  23  51        26.8     83.9 6.1     40.2  watermelon
    ## 1381 113  30  50        26.0     84.0 6.3     43.9  watermelon
    ## 1382  83  10  53        24.9     85.0 6.2     48.8  watermelon
    ## 1383 101  11  51        25.5     84.2 6.8     44.2  watermelon
    ## 1384 114  21  55        25.4     87.9 6.5     57.5  watermelon
    ## 1385  99   6  45        26.1     86.6 6.0     40.7  watermelon
    ## 1386  92  20  55        25.1     87.5 6.6     59.3  watermelon
    ## 1387  92   7  48        26.3     86.6 7.0     54.4  watermelon
    ## 1388  91  24  55        26.3     83.1 6.3     46.8  watermelon
    ## 1389 110  21  54        26.7     87.8 6.7     47.5  watermelon
    ## 1390 112  25  51        25.0     85.6 6.9     56.7  watermelon
    ## 1391  89  25  54        24.7     85.6 6.4     49.0  watermelon
    ## 1392 100  10  53        24.5     84.6 6.2     42.0  watermelon
    ## 1393  83  22  54        25.9     82.0 6.3     54.5  watermelon
    ## 1394  95  14  50        26.6     84.3 6.6     56.3  watermelon
    ## 1395 119  30  49        25.4     80.5 6.9     47.7  watermelon
    ## 1396  97  12  47        25.3     89.6 6.8     58.3  watermelon
    ## 1397 110   7  45        26.6     84.7 6.2     48.3  watermelon
    ## 1398  96  18  50        25.3     84.3 6.9     41.5  watermelon
    ## 1399  83  23  55        26.9     83.9 6.5     44.0  watermelon
    ## 1400 120  24  47        27.0     89.4 6.3     58.5  watermelon
    ## 1401 115  17  55        27.6     94.1 6.8     28.1   muskmelon
    ## 1402 114  27  48        27.8     93.0 6.5     26.3   muskmelon
    ## 1403 101  25  52        29.1     94.2 6.8     22.5   muskmelon
    ## 1404 118  18  52        28.0     90.8 6.6     20.8   muskmelon
    ## 1405  95  26  45        29.9     94.6 6.1     28.2   muskmelon
    ## 1406  81  25  49        29.9     93.3 6.1     26.3   muskmelon
    ## 1407 117  24  53        29.2     92.2 6.3     21.3   muskmelon
    ## 1408 114  30  51        29.2     90.1 6.1     25.9   muskmelon
    ## 1409 113   6  52        27.8     90.4 6.7     25.2   muskmelon
    ## 1410 108  26  52        28.8     94.3 6.2     26.2   muskmelon
    ## 1411  81  30  48        28.5     92.1 6.0     29.9   muskmelon
    ## 1412 115   9  52        29.1     91.0 6.0     29.1   muskmelon
    ## 1413  83   7  45        29.1     90.7 6.7     25.3   muskmelon
    ## 1414  84  21  55        28.5     94.8 6.5     21.1   muskmelon
    ## 1415 109  26  45        28.3     90.4 6.2     21.6   muskmelon
    ## 1416  95  27  55        28.5     91.2 6.2     20.9   muskmelon
    ## 1417 119   5  55        29.7     94.3 6.2     26.8   muskmelon
    ## 1418 110  14  51        27.0     91.7 6.1     21.3   muskmelon
    ## 1419  82  18  48        29.1     94.2 6.2     26.7   muskmelon
    ## 1420  87  14  48        29.7     92.6 6.6     29.1   muskmelon
    ## 1421  85   9  53        28.2     92.9 6.4     28.8   muskmelon
    ## 1422 100   6  53        29.1     93.9 6.1     23.7   muskmelon
    ## 1423 107  12  46        29.6     93.6 6.6     27.6   muskmelon
    ## 1424  91  13  47        29.1     92.4 6.1     28.0   muskmelon
    ## 1425 102  25  50        28.2     92.9 6.1     20.4   muskmelon
    ## 1426 117  25  53        29.1     92.1 6.4     24.5   muskmelon
    ## 1427  85  21  52        29.6     90.1 6.1     23.7   muskmelon
    ## 1428 104  25  55        29.8     90.4 6.1     22.7   muskmelon
    ## 1429 102  24  54        27.7     90.9 6.7     22.8   muskmelon
    ## 1430 116  25  50        29.3     92.9 6.1     28.7   muskmelon
    ## 1431 100  17  48        29.7     94.3 6.4     26.5   muskmelon
    ## 1432 110  25  54        28.9     90.8 6.4     23.4   muskmelon
    ## 1433 104  25  51        29.0     93.9 6.5     23.6   muskmelon
    ## 1434 107  11  54        28.6     91.3 6.1     29.4   muskmelon
    ## 1435  98  26  52        27.3     90.7 6.2     28.7   muskmelon
    ## 1436  88  17  52        29.9     90.8 6.6     25.4   muskmelon
    ## 1437  87  25  46        27.4     90.0 6.4     21.8   muskmelon
    ## 1438 120   8  46        29.6     90.7 6.7     28.4   muskmelon
    ## 1439  95  13  46        29.8     93.8 6.1     23.3   muskmelon
    ## 1440 108  22  47        28.5     91.7 6.2     25.1   muskmelon
    ## 1441  82  13  52        27.1     94.9 6.4     26.5   muskmelon
    ## 1442 120  23  55        27.8     91.6 6.7     26.5   muskmelon
    ## 1443 110  22  47        29.0     91.8 6.2     24.9   muskmelon
    ## 1444  95  23  45        27.8     90.6 6.3     21.2   muskmelon
    ## 1445 106  10  49        27.7     92.0 6.4     20.2   muskmelon
    ## 1446  99  12  52        28.7     94.3 6.0     22.2   muskmelon
    ## 1447 106  20  51        29.7     91.0 6.3     20.5   muskmelon
    ## 1448  83  11  53        29.5     92.9 6.2     22.0   muskmelon
    ## 1449 117  19  55        28.8     91.8 6.1     25.2   muskmelon
    ## 1450  98  26  49        27.3     90.5 6.1     23.5   muskmelon
    ## 1451 113  20  48        27.5     94.9 6.4     27.3   muskmelon
    ## 1452 101  17  47        29.5     94.7 6.2     26.3   muskmelon
    ## 1453  98   7  45        27.8     92.5 6.2     26.9   muskmelon
    ## 1454  93  22  48        29.1     91.5 6.8     21.9   muskmelon
    ## 1455  95  21  47        27.9     93.6 6.4     20.7   muskmelon
    ## 1456 109  12  48        29.5     92.1 6.7     20.8   muskmelon
    ## 1457 118  12  47        28.0     92.2 6.0     28.9   muskmelon
    ## 1458 100  14  49        29.5     91.1 6.4     26.0   muskmelon
    ## 1459  89   9  47        29.5     90.8 6.7     28.8   muskmelon
    ## 1460  95  16  46        27.1     90.1 6.7     24.5   muskmelon
    ## 1461  95   7  45        27.3     90.8 6.0     25.1   muskmelon
    ## 1462  87   6  45        29.8     90.8 6.4     22.8   muskmelon
    ## 1463  93  20  50        29.9     93.2 6.4     24.3   muskmelon
    ## 1464  84  29  49        29.9     93.9 6.3     20.4   muskmelon
    ## 1465 111   5  47        28.0     91.5 6.3     21.2   muskmelon
    ## 1466 111   5  52        29.9     94.0 6.1     21.0   muskmelon
    ## 1467 111  15  54        27.7     92.9 6.2     22.1   muskmelon
    ## 1468  89  11  47        29.8     94.7 6.3     27.9   muskmelon
    ## 1469 110  15  48        28.6     92.9 6.2     27.6   muskmelon
    ## 1470  95  30  52        29.5     90.3 6.6     26.0   muskmelon
    ## 1471 115  12  52        27.5     95.0 6.7     21.0   muskmelon
    ## 1472 120  25  50        28.1     94.8 6.3     21.8   muskmelon
    ## 1473 102  11  45        29.0     93.1 6.4     24.2   muskmelon
    ## 1474  94   5  55        28.6     91.9 6.1     26.9   muskmelon
    ## 1475  84  18  46        27.1     93.4 6.8     25.3   muskmelon
    ## 1476 107  22  54        28.0     90.8 6.6     21.6   muskmelon
    ## 1477  80  18  52        27.9     91.1 6.5     24.1   muskmelon
    ## 1478  86  18  45        29.0     90.7 6.6     22.3   muskmelon
    ## 1479 113  28  48        28.9     92.5 6.2     24.4   muskmelon
    ## 1480 115  18  53        29.2     94.2 6.0     22.1   muskmelon
    ## 1481  82  20  54        29.3     90.0 6.5     21.4   muskmelon
    ## 1482  98  22  47        29.1     91.9 6.3     28.8   muskmelon
    ## 1483 117  25  54        28.7     92.5 6.2     29.1   muskmelon
    ## 1484  83  15  49        28.9     91.4 6.4     23.2   muskmelon
    ## 1485 120  16  51        28.0     91.6 6.5     23.3   muskmelon
    ## 1486 111   5  50        27.6     91.8 6.4     24.8   muskmelon
    ## 1487  85  21  47        29.9     90.6 6.2     24.7   muskmelon
    ## 1488  90  23  54        28.6     90.5 6.2     27.3   muskmelon
    ## 1489  99  29  55        29.2     91.5 6.7     26.5   muskmelon
    ## 1490 102  11  47        28.0     92.8 6.5     27.1   muskmelon
    ## 1491  80  18  51        28.1     91.8 6.7     20.8   muskmelon
    ## 1492  87  21  52        27.4     94.3 6.1     27.2   muskmelon
    ## 1493 114   8  52        29.3     94.6 6.4     28.2   muskmelon
    ## 1494  99   6  46        28.6     94.2 6.4     29.0   muskmelon
    ## 1495  89  25  50        27.0     91.3 6.4     25.1   muskmelon
    ## 1496  96  13  55        29.5     94.6 6.7     21.1   muskmelon
    ## 1497  82  26  47        28.5     93.5 6.6     24.2   muskmelon
    ## 1498 106  21  52        28.9     94.8 6.3     23.0   muskmelon
    ## 1499  90  15  52        27.0     91.4 6.4     23.7   muskmelon
    ## 1500 106  16  54        29.0     91.7 6.6     24.7   muskmelon
    ## 1501  24 128 196        22.8     90.7 5.5    110.4       apple
    ## 1502   7 144 197        23.8     94.3 6.1    114.1       apple
    ## 1503  14 128 205        22.6     94.6 6.2    116.0       apple
    ## 1504   8 120 201        21.2     91.1 6.3    122.2       apple
    ## 1505  20 129 201        23.4     91.7 5.6    116.1       apple
    ## 1506  32 137 204        22.9     93.1 5.8    117.7       apple
    ## 1507  27 139 205        22.5     93.4 5.8    105.5       apple
    ## 1508   0 123 205        22.0     93.0 5.8    121.1       apple
    ## 1509  22 144 196        21.9     91.7 6.5    117.1       apple
    ## 1510   1 124 199        23.7     93.3 5.7    112.7       apple
    ## 1511  30 122 197        21.4     92.7 5.6    106.1       apple
    ## 1512  29 121 196        22.8     94.3 6.1    123.6       apple
    ## 1513  13 126 204        23.1     92.8 6.4    108.2       apple
    ## 1514   9 139 199        23.3     94.5 5.9    105.4       apple
    ## 1515   0 133 200        23.7     90.5 5.7    104.2       apple
    ## 1516  30 143 199        23.8     90.6 5.8    102.3       apple
    ## 1517  36 140 198        23.3     91.5 6.3    104.4       apple
    ## 1518  37 137 199        22.6     90.2 5.7    108.3       apple
    ## 1519  33 121 203        22.5     94.8 5.6    114.8       apple
    ## 1520   7 144 195        23.0     93.6 5.9    104.6       apple
    ## 1521  35 128 205        21.1     93.6 6.0    107.9       apple
    ## 1522  29 128 198        22.4     92.7 5.7    121.5       apple
    ## 1523   2 143 196        22.7     90.5 5.7    109.9       apple
    ## 1524  34 140 198        21.7     93.4 5.8    115.2       apple
    ## 1525  29 144 204        22.4     92.5 5.8    119.1       apple
    ## 1526  32 141 203        21.3     92.8 5.8    109.1       apple
    ## 1527  13 144 197        22.9     94.9 6.3    105.7       apple
    ## 1528  25 143 198        22.8     91.5 6.0    107.9       apple
    ## 1529   9 137 200        21.1     90.7 5.6    102.8       apple
    ## 1530   6 144 198        21.1     90.3 5.6    104.5       apple
    ## 1531  37 126 196        23.6     91.0 5.6    107.2       apple
    ## 1532   2 120 203        23.1     94.7 5.9    108.6       apple
    ## 1533  11 143 197        23.0     93.3 5.9    122.2       apple
    ## 1534  10 141 201        22.1     91.0 6.4    104.5       apple
    ## 1535  24 142 202        22.5     91.5 5.7    101.8       apple
    ## 1536  23 138 195        22.5     91.7 5.8    124.4       apple
    ## 1537  18 125 204        22.4     94.5 6.0    116.7       apple
    ## 1538  13 121 196        22.2     93.5 6.4    120.2       apple
    ## 1539  26 122 202        22.4     94.7 5.6    107.2       apple
    ## 1540  28 123 202        22.8     92.1 6.4    120.4       apple
    ## 1541  26 121 201        22.2     90.0 6.2    112.3       apple
    ## 1542  21 137 196        23.6     91.7 5.8    123.6       apple
    ## 1543  21 135 198        23.9     94.9 5.8    105.0       apple
    ## 1544   5 144 205        21.4     92.6 6.2    102.8       apple
    ## 1545   2 123 205        22.4     90.8 5.7    125.0       apple
    ## 1546  15 133 199        24.0     91.6 5.8    117.6       apple
    ## 1547  31 130 198        21.8     92.7 5.6    120.1       apple
    ## 1548  25 143 200        23.8     92.8 6.0    100.6       apple
    ## 1549  16 143 204        23.7     91.5 5.6    121.9       apple
    ## 1550  19 122 202        23.3     90.4 5.8    112.9       apple
    ## 1551  10 125 196        22.3     90.0 5.7    113.1       apple
    ## 1552  20 139 202        23.5     92.2 5.7    108.0       apple
    ## 1553  28 123 198        23.5     91.5 5.7    111.8       apple
    ## 1554  28 136 200        23.1     92.4 6.2    114.7       apple
    ## 1555   2 131 199        22.5     91.2 6.0    124.2       apple
    ## 1556   2 140 197        22.7     92.8 5.5    105.1       apple
    ## 1557  27 138 201        23.7     93.9 6.0    105.4       apple
    ## 1558  30 127 204        22.5     92.5 6.1    100.9       apple
    ## 1559  32 145 203        23.8     90.8 6.4    109.6       apple
    ## 1560  29 139 205        23.6     93.7 6.2    116.7       apple
    ## 1561  26 126 195        21.4     93.0 5.9    118.4       apple
    ## 1562  40 136 202        22.9     94.6 5.9    117.5       apple
    ## 1563   6 124 200        23.0     93.8 6.0    109.6       apple
    ## 1564  35 138 200        21.2     90.8 5.7    103.7       apple
    ## 1565  17 136 196        23.9     90.5 5.9    103.1       apple
    ## 1566  33 134 205        21.0     94.3 6.1    114.7       apple
    ## 1567  16 143 197        22.6     93.5 5.9    116.9       apple
    ## 1568  27 120 200        21.5     90.7 6.1    116.7       apple
    ## 1569  29 145 205        22.8     92.1 6.2    109.3       apple
    ## 1570   3 141 197        22.0     91.1 6.1    115.5       apple
    ## 1571  15 123 204        22.5     92.5 6.4    115.4       apple
    ## 1572   5 136 195        22.4     91.9 6.3    107.8       apple
    ## 1573  10 136 204        21.2     92.2 6.3    105.9       apple
    ## 1574   7 141 195        23.9     93.5 5.5    104.9       apple
    ## 1575   2 129 201        22.8     94.4 5.7    122.1       apple
    ## 1576  29 138 197        22.2     92.4 5.8    121.7       apple
    ## 1577  30 137 200        22.9     90.7 5.6    118.6       apple
    ## 1578  29 132 204        23.1     90.2 6.1    108.2       apple
    ## 1579  14 139 197        21.7     92.8 6.1    121.7       apple
    ## 1580  18 125 203        22.4     91.6 6.2    102.6       apple
    ## 1581  33 143 204        21.1     92.0 5.8    122.5       apple
    ## 1582  40 144 196        22.7     92.3 6.0    107.0       apple
    ## 1583   9 143 197        23.8     92.9 5.6    117.7       apple
    ## 1584  38 135 203        23.8     93.7 6.0    100.8       apple
    ## 1585  28 130 196        22.1     94.7 6.1    112.9       apple
    ## 1586  35 142 203        21.2     90.2 5.9    123.6       apple
    ## 1587  12 129 205        22.4     91.2 6.1    118.7       apple
    ## 1588   1 135 203        22.8     92.7 5.6    113.8       apple
    ## 1589   0 145 205        21.2     90.1 5.5    114.0       apple
    ## 1590  31 121 201        23.2     90.3 5.7    110.7       apple
    ## 1591  35 131 203        22.4     93.9 5.9    102.7       apple
    ## 1592  29 140 195        23.6     91.0 5.6    116.7       apple
    ## 1593  33 138 198        22.3     90.7 6.2    122.7       apple
    ## 1594  14 140 197        23.4     90.9 6.1    113.0       apple
    ## 1595  35 145 195        22.0     94.6 6.2    111.0       apple
    ## 1596  40 120 197        23.8     92.5 5.9    119.6       apple
    ## 1597  25 132 198        22.3     90.9 5.7    100.1       apple
    ## 1598  31 137 196        22.1     93.8 6.4    120.6       apple
    ## 1599  36 144 196        23.7     94.5 6.5    115.4       apple
    ## 1600  10 140 197        22.2     90.3 6.2    124.5       apple
    ## 1601  22  30  12        15.8     92.5 6.4    119.0      orange
    ## 1602  37   6  13        26.0     91.5 7.5    101.3      orange
    ## 1603  27  13   6        13.4     91.4 7.3    111.2      orange
    ## 1604   7  16   9        18.9     92.0 7.8    114.7      orange
    ## 1605  20   7   9        29.5     91.6 7.1    111.2      orange
    ## 1606  26  27  10        28.1     92.9 6.1    114.1      orange
    ## 1607   5  23  15        25.7     92.0 7.4    112.5      orange
    ## 1608   0  18  14        29.8     92.0 7.2    114.4      orange
    ## 1609  39  24  14        30.6     90.9 7.2    106.1      orange
    ## 1610  13  23   6        24.0     90.3 7.4    102.7      orange
    ## 1611  21  17  15        24.0     91.5 7.5    118.5      orange
    ## 1612  33  12   8        25.3     90.3 6.8    117.4      orange
    ## 1613   6   9  12        31.1     90.1 7.0    109.7      orange
    ## 1614  19   7  10        14.8     91.2 6.1    100.2      orange
    ## 1615  24  18   6        26.6     94.5 6.3    116.4      orange
    ## 1616   9  11   8        24.9     94.4 6.6    111.8      orange
    ## 1617  31   8   7        34.5     93.6 7.2    103.6      orange
    ## 1618  22  17   5        24.1     90.7 6.9    102.8      orange
    ## 1619  13   5   8        23.9     90.1 7.5    103.9      orange
    ## 1620  16   8   9        24.6     91.3 7.6    111.3      orange
    ## 1621   4  13   6        15.6     94.3 7.6    101.5      orange
    ## 1622   0  25  14        19.3     92.0 6.4    116.5      orange
    ## 1623   8   7  10        28.3     92.0 6.9    105.2      orange
    ## 1624   4  23   5        22.7     93.4 7.5    110.3      orange
    ## 1625  33  14   8        21.0     93.0 7.7    110.7      orange
    ## 1626  30   7  15        33.2     91.1 7.8    115.8      orange
    ## 1627  21  29  12        22.3     92.2 6.4    117.4      orange
    ## 1628  11  14   5        11.5     94.9 6.9    115.6      orange
    ## 1629   9   8  15        14.3     94.4 8.0    110.2      orange
    ## 1630   5  18  14        33.1     93.5 7.4    119.2      orange
    ## 1631  29  25  14        30.5     90.5 7.8    113.3      orange
    ## 1632  33  12  15        30.3     92.0 6.1    116.7      orange
    ## 1633   8  16   6        12.2     90.3 7.1    108.4      orange
    ## 1634  15  14   8        10.0     90.2 6.2    119.4      orange
    ## 1635  16   7   8        22.8     90.6 6.4    116.5      orange
    ## 1636   0  12   7        20.2     90.7 7.0    116.8      orange
    ## 1637   5  25   6        30.7     94.0 6.0    106.8      orange
    ## 1638   6   8  11        24.4     92.4 6.6    119.7      orange
    ## 1639  10   5   5        21.2     91.4 7.8    113.0      orange
    ## 1640   1  17   6        10.8     91.4 6.8    117.5      orange
    ## 1641   1  30  10        11.9     91.3 7.3    103.6      orange
    ## 1642   0  23  15        22.6     93.4 7.6    109.9      orange
    ## 1643  24  27   9        18.9     93.2 6.2    119.4      orange
    ## 1644  36  11  13        17.3     93.0 7.2    112.7      orange
    ## 1645  40  21   8        34.9     92.9 7.4    102.2      orange
    ## 1646  40  22   6        24.5     91.9 6.5    116.0      orange
    ## 1647  32  18  13        13.8     91.7 6.0    108.0      orange
    ## 1648   9  10  10        22.4     93.5 6.0    101.5      orange
    ## 1649  13  16   8        34.7     93.1 6.9    100.2      orange
    ## 1650  15   9  11        11.5     94.1 7.9    108.8      orange
    ## 1651  29  11   5        23.1     91.9 7.6    104.4      orange
    ## 1652   1  15   9        30.0     94.6 7.5    115.4      orange
    ## 1653  18   5  11        20.9     90.9 6.3    102.5      orange
    ## 1654  14  22   9        17.2     91.1 6.5    112.5      orange
    ## 1655  33  15   7        15.8     91.7 7.7    109.8      orange
    ## 1656   4   6   7        23.0     91.1 6.7    112.7      orange
    ## 1657  17  16  14        16.4     92.2 6.6    102.9      orange
    ## 1658  12  20  10        24.5     93.1 6.5    109.5      orange
    ## 1659  34  29   8        31.9     91.2 6.5    105.3      orange
    ## 1660  39  28  10        31.3     91.5 7.2    109.2      orange
    ## 1661  31  25  12        18.1     90.0 7.0    111.8      orange
    ## 1662  12   6   8        30.8     92.9 6.4    107.4      orange
    ## 1663  12  29  13        22.5     91.5 7.6    118.0      orange
    ## 1664  26  11  11        13.7     91.0 7.6    106.3      orange
    ## 1665  19  24  15        20.5     93.7 7.1    111.8      orange
    ## 1666  39  21   9        13.2     94.0 6.4    106.3      orange
    ## 1667  16  29  13        32.3     93.7 6.2    117.6      orange
    ## 1668  36  29  13        20.7     90.9 7.8    109.8      orange
    ## 1669  37  23  12        31.5     90.5 6.4    113.1      orange
    ## 1670  39   9  15        25.4     91.8 8.0    116.8      orange
    ## 1671  31   5  14        17.7     91.7 6.6    110.7      orange
    ## 1672  18  12   8        12.6     91.8 6.2    119.4      orange
    ## 1673  20  20  10        11.9     93.7 7.0    106.1      orange
    ## 1674   5   8   5        11.0     92.2 6.6    112.8      orange
    ## 1675  20   8  12        25.3     95.0 7.3    118.0      orange
    ## 1676  25  21  11        32.2     90.2 6.5    104.7      orange
    ## 1677  14  19  14        17.7     94.4 6.7    108.1      orange
    ## 1678  37  18  12        10.3     90.2 7.4    106.7      orange
    ## 1679  26  15   6        17.2     94.8 6.9    108.0      orange
    ## 1680  13  22   5        19.7     90.5 7.8    100.2      orange
    ## 1681  32  25   9        10.4     93.8 7.8    101.1      orange
    ## 1682  19   7   9        27.3     91.7 7.0    101.1      orange
    ## 1683  28   7   9        34.6     92.1 6.7    115.6      orange
    ## 1684  24  30  11        32.4     94.5 6.6    113.3      orange
    ## 1685   7  17  10        10.2     91.2 6.5    106.4      orange
    ## 1686  18  23   8        21.5     93.4 6.4    101.5      orange
    ## 1687   7  20  12        16.5     94.8 6.5    110.0      orange
    ## 1688  20  23  11        31.9     90.1 6.4    109.9      orange
    ## 1689  18  14  11        28.0     90.0 6.6    117.1      orange
    ## 1690  34  11  10        31.8     94.6 7.4    115.2      orange
    ## 1691  20  29  10        29.1     93.3 7.4    100.8      orange
    ## 1692  37  24  13        19.1     90.7 7.9    108.0      orange
    ## 1693  12   8  10        16.1     91.4 8.0    107.4      orange
    ## 1694  34  10  14        34.1     92.1 6.7    116.8      orange
    ## 1695   6  13   9        34.5     90.6 7.8    118.3      orange
    ## 1696  27  30   5        32.7     90.5 7.7    113.3      orange
    ## 1697  13   8  12        25.2     92.5 7.1    114.3      orange
    ## 1698   6   7   7        27.7     94.5 7.2    114.0      orange
    ## 1699  40  17  15        21.4     90.9 7.9    107.1      orange
    ## 1700  31  26   9        11.7     93.3 7.6    103.2      orange
    ## 1701  61  68  50        35.2     91.5 6.8    243.1      papaya
    ## 1702  58  46  45        42.4     90.8 6.6     88.5      papaya
    ## 1703  45  47  55        38.4     91.1 6.8    119.3      papaya
    ## 1704  39  65  53        35.3     92.1 6.6    235.6      papaya
    ## 1705  31  68  45        42.9     90.1 6.9    196.2      papaya
    ## 1706  70  68  45        33.8     92.9 7.0    203.4      papaya
    ## 1707  68  62  50        33.2     92.8 7.0    197.5      papaya
    ## 1708  34  65  47        23.5     93.7 6.8    191.8      papaya
    ## 1709  38  68  54        29.3     90.8 6.7    202.1      papaya
    ## 1710  69  64  47        40.2     94.5 7.0    186.7      papaya
    ## 1711  58  51  47        42.1     91.7 6.8    197.4      papaya
    ## 1712  59  47  53        32.9     91.5 6.9     47.3      papaya
    ## 1713  44  64  54        29.8     91.4 6.7    232.7      papaya
    ## 1714  56  57  48        31.6     93.0 6.5     63.6      papaya
    ## 1715  69  60  54        36.3     93.1 7.0    141.2      papaya
    ## 1716  56  58  49        37.1     94.6 6.7    172.5      papaya
    ## 1717  49  55  53        38.4     93.6 6.5     77.7      papaya
    ## 1718  38  51  52        32.7     90.8 6.9     78.9      papaya
    ## 1719  54  65  47        27.9     91.6 6.7    149.9      papaya
    ## 1720  57  57  51        39.0     91.5 7.0    105.9      papaya
    ## 1721  39  52  53        32.5     94.7 6.7     51.1      papaya
    ## 1722  58  67  45        38.7     91.7 6.7     62.6      papaya
    ## 1723  61  64  52        43.3     92.8 6.6    110.6      papaya
    ## 1724  34  62  55        27.6     90.7 6.6    238.5      papaya
    ## 1725  31  48  45        40.8     92.9 6.6    132.8      papaya
    ## 1726  47  46  52        23.2     91.4 6.5    206.4      papaya
    ## 1727  32  68  52        32.7     92.6 6.8    248.9      papaya
    ## 1728  36  59  46        34.3     93.6 6.7    127.3      papaya
    ## 1729  61  51  51        39.3     94.2 6.6    121.0      papaya
    ## 1730  70  54  46        39.7     91.1 6.9    122.8      papaya
    ## 1731  44  56  49        39.2     91.3 6.5     64.4      papaya
    ## 1732  34  68  51        27.3     94.2 6.7     40.4      papaya
    ## 1733  50  59  47        40.8     92.1 6.7    209.9      papaya
    ## 1734  39  70  52        26.3     90.8 6.7     59.5      papaya
    ## 1735  34  61  49        28.1     93.3 6.5    117.8      papaya
    ## 1736  44  60  55        34.3     90.6 6.8     98.5      papaya
    ## 1737  31  62  52        33.8     93.0 7.0    182.0      papaya
    ## 1738  65  62  51        31.5     90.9 6.5    207.1      papaya
    ## 1739  44  57  53        42.3     90.5 6.9     74.9      papaya
    ## 1740  50  47  48        24.6     90.6 6.7    218.2      papaya
    ## 1741  43  50  48        28.3     91.4 6.6    179.3      papaya
    ## 1742  60  46  53        24.5     93.0 6.8    183.5      papaya
    ## 1743  70  68  55        42.8     94.6 6.7     78.8      papaya
    ## 1744  59  62  52        43.7     93.1 6.6    103.8      papaya
    ## 1745  60  58  51        42.1     92.9 6.8    165.7      papaya
    ## 1746  42  60  47        33.5     92.1 6.8    136.8      papaya
    ## 1747  35  66  47        31.7     91.7 7.0     48.8      papaya
    ## 1748  34  65  48        41.4     90.0 6.7    199.3      papaya
    ## 1749  36  54  46        42.5     94.9 6.7    214.4      papaya
    ## 1750  39  64  52        28.9     94.6 6.7     63.7      papaya
    ## 1751  37  52  47        43.1     93.9 6.5    211.9      papaya
    ## 1752  33  47  46        29.2     94.0 6.8    209.4      papaya
    ## 1753  34  48  48        41.0     91.4 6.8    181.5      papaya
    ## 1754  49  54  50        25.6     93.2 6.8     97.3      papaya
    ## 1755  40  65  49        35.3     91.1 6.7    163.9      papaya
    ## 1756  68  52  49        24.4     92.3 6.6     63.4      papaya
    ## 1757  50  46  52        31.2     90.2 6.7     54.0      papaya
    ## 1758  65  63  50        31.9     91.3 6.5     79.3      papaya
    ## 1759  40  49  47        42.9     91.2 6.5    246.4      papaya
    ## 1760  42  53  48        23.1     94.3 6.8    231.5      papaya
    ## 1761  49  55  51        24.9     93.9 6.7    135.2      papaya
    ## 1762  59  62  49        43.4     93.4 6.9    114.8      papaya
    ## 1763  63  58  47        26.8     90.8 6.9    144.7      papaya
    ## 1764  70  65  52        30.4     93.1 6.6     76.0      papaya
    ## 1765  63  50  52        28.6     93.2 6.8    115.8      papaya
    ## 1766  40  64  47        32.5     93.5 6.9     71.7      papaya
    ## 1767  63  58  50        43.0     94.6 6.7     41.6      papaya
    ## 1768  45  58  49        30.1     90.3 6.8     75.2      papaya
    ## 1769  66  69  47        23.7     93.6 6.9     87.5      papaya
    ## 1770  54  67  52        35.7     93.3 6.6    141.3      papaya
    ## 1771  69  67  52        27.7     94.4 6.8     82.8      papaya
    ## 1772  67  68  49        35.3     92.4 6.8    149.8      papaya
    ## 1773  45  57  47        23.2     90.8 6.7    161.7      papaya
    ## 1774  56  50  52        33.1     92.3 6.8     88.1      papaya
    ## 1775  70  50  53        37.5     90.4 6.9    172.3      papaya
    ## 1776  44  47  45        38.7     94.7 6.6    218.1      papaya
    ## 1777  50  60  47        32.6     92.7 6.9     93.8      papaya
    ## 1778  52  51  53        38.4     93.1 7.0    210.3      papaya
    ## 1779  35  68  45        42.9     90.1 6.6    234.8      papaya
    ## 1780  68  69  52        25.7     92.7 6.8     53.0      papaya
    ## 1781  32  55  52        37.6     92.0 7.0    159.7      papaya
    ## 1782  32  55  51        29.6     93.2 6.6     62.7      papaya
    ## 1783  48  62  47        25.3     93.0 6.8    174.4      papaya
    ## 1784  39  69  53        25.9     93.0 7.0    241.8      papaya
    ## 1785  49  61  45        32.8     94.6 6.8    240.5      papaya
    ## 1786  48  57  54        29.0     90.2 6.6    126.8      papaya
    ## 1787  69  66  49        40.0     90.2 6.5     92.1      papaya
    ## 1788  53  55  55        33.3     91.3 6.7    234.5      papaya
    ## 1789  38  61  52        31.2     94.9 6.6     46.4      papaya
    ## 1790  57  64  55        26.7     93.0 6.6     62.5      papaya
    ## 1791  51  57  55        24.7     90.1 6.7    108.4      papaya
    ## 1792  56  65  45        38.2     94.0 6.8    218.1      papaya
    ## 1793  54  66  52        36.6     93.8 6.9    104.4      papaya
    ## 1794  58  55  47        26.1     93.7 6.7    240.7      papaya
    ## 1795  68  70  54        31.3     92.8 7.0     54.8      papaya
    ## 1796  42  59  55        40.1     94.4 7.0    149.1      papaya
    ## 1797  43  64  47        38.6     91.6 6.8    102.3      papaya
    ## 1798  35  67  49        41.3     91.2 6.6    239.7      papaya
    ## 1799  56  59  55        37.0     91.8 6.6    188.5      papaya
    ## 1800  39  64  53        23.0     91.1 6.6    208.3      papaya
    ## 1801  18  30  29        26.8     92.9 6.4    224.6     coconut
    ## 1802  37  23  28        25.6     94.3 5.7    224.3     coconut
    ## 1803  13  28  33        28.1     95.6 5.7    151.1     coconut
    ## 1804   2  21  35        25.0     91.5 6.3    179.8     coconut
    ## 1805  10  18  35        27.8     99.6 6.4    181.7     coconut
    ## 1806   7  11  32        29.3     95.1 5.5    184.8     coconut
    ## 1807  39   5  31        27.1     93.7 5.6    151.0     coconut
    ## 1808  34   6  27        25.8     90.9 5.9    147.9     coconut
    ## 1809  31  30  29        26.6     91.0 5.6    178.8     coconut
    ## 1810  25   7  35        28.4     99.2 5.6    189.7     coconut
    ## 1811  16  18  26        28.4     91.8 5.6    145.5     coconut
    ## 1812  26  10  33        28.3     96.9 6.1    198.8     coconut
    ## 1813  27   8  32        27.0     96.5 5.6    144.3     coconut
    ## 1814  37  18  30        27.6     99.3 6.4    157.9     coconut
    ## 1815  19  15  34        26.3     99.7 5.7    215.9     coconut
    ## 1816   0  19  33        27.1     95.2 6.2    204.7     coconut
    ## 1817  31  20  26        25.6     97.6 6.4    199.8     coconut
    ## 1818   9  17  32        25.9     93.4 5.8    172.1     coconut
    ## 1819  22  11  29        28.0     95.0 6.0    218.0     coconut
    ## 1820  31   6  26        29.1     91.3 5.7    157.2     coconut
    ## 1821  34   6  30        27.1     97.0 5.9    171.8     coconut
    ## 1822  24   6  32        28.1     90.0 6.4    172.5     coconut
    ## 1823   1   8  26        27.5     94.2 5.6    156.7     coconut
    ## 1824  31  13  33        27.6     95.5 5.9    205.5     coconut
    ## 1825  10   9  28        29.0     94.0 6.3    150.1     coconut
    ## 1826  36  27  26        26.6     95.8 6.3    171.6     coconut
    ## 1827  38  24  33        28.3     97.0 6.0    142.9     coconut
    ## 1828  11   6  25        28.7     96.7 6.1    179.0     coconut
    ## 1829  16  14  30        29.7     96.3 6.4    209.8     coconut
    ## 1830  33  14  35        27.1     96.7 6.0    149.2     coconut
    ## 1831  16   6  29        29.3     92.0 5.9    132.1     coconut
    ## 1832  32  11  31        25.1     93.3 6.2    134.8     coconut
    ## 1833  38  14  30        26.9     91.2 5.6    194.9     coconut
    ## 1834   8   6  33        28.3     93.6 6.1    171.9     coconut
    ## 1835  23   6  33        29.2     92.7 6.0    205.0     coconut
    ## 1836  29  25  35        28.4     91.6 5.5    160.7     coconut
    ## 1837  24  14  33        29.4     93.3 6.4    218.5     coconut
    ## 1838  32  12  30        25.4     98.1 5.6    218.1     coconut
    ## 1839  30  25  31        26.3     98.6 5.8    208.1     coconut
    ## 1840  14  21  35        29.5     91.9 6.1    194.3     coconut
    ## 1841  27  22  29        28.8     92.2 6.0    145.4     coconut
    ## 1842  40   5  29        28.5     97.8 5.8    160.4     coconut
    ## 1843  17  11  32        28.7     93.4 5.6    156.8     coconut
    ## 1844  30  30  35        25.0     95.6 6.0    165.8     coconut
    ## 1845  28  10  30        29.9     91.1 6.3    192.8     coconut
    ## 1846  39   7  29        27.5     94.6 6.4    150.2     coconut
    ## 1847  32  20  35        26.5     98.4 5.6    144.6     coconut
    ## 1848   7  15  32        25.0     95.9 6.2    174.8     coconut
    ## 1849  29  17  29        29.2     95.7 6.0    211.3     coconut
    ## 1850  34  15  34        27.1     91.1 5.7    224.7     coconut
    ## 1851  14  23  25        26.2     97.0 5.6    135.4     coconut
    ## 1852  18  19  29        27.6     92.5 6.2    162.8     coconut
    ## 1853   7  21  35        25.8     94.7 5.8    131.2     coconut
    ## 1854  24  27  34        28.9     95.1 6.2    145.1     coconut
    ## 1855  39  29  29        26.5     94.5 6.1    199.9     coconut
    ## 1856  29   8  28        26.9     91.7 6.1    214.4     coconut
    ## 1857  10  24  27        27.6     94.9 5.7    145.9     coconut
    ## 1858   0  29  32        28.1     98.4 5.9    171.7     coconut
    ## 1859  32  11  31        29.5     92.6 6.5    131.2     coconut
    ## 1860  37  10  32        29.0     95.2 6.2    222.8     coconut
    ## 1861  20  29  27        25.1     92.4 6.0    157.8     coconut
    ## 1862  31  29  35        27.2     92.2 6.1    141.3     coconut
    ## 1863  17  30  27        29.0     90.8 5.9    205.6     coconut
    ## 1864   1  12  30        27.8     95.9 5.6    131.1     coconut
    ## 1865   6  13  29        27.3    100.0 5.8    201.8     coconut
    ## 1866  15  28  32        28.8     99.6 6.2    224.4     coconut
    ## 1867  27  24  29        26.6     97.0 6.1    191.0     coconut
    ## 1868   3  23  30        29.7     95.7 6.1    215.2     coconut
    ## 1869   8  26  26        25.5     91.6 5.7    212.9     coconut
    ## 1870  20  28  26        26.4     91.5 5.5    167.0     coconut
    ## 1871  26  18  27        27.5     92.9 5.8    142.1     coconut
    ## 1872   1   6  35        27.0     95.7 6.2    147.2     coconut
    ## 1873  27  30  31        29.0     90.7 5.7    148.8     coconut
    ## 1874  23   7  34        26.1     91.5 5.9    134.1     coconut
    ## 1875   0  26  31        25.1     95.0 5.5    192.9     coconut
    ## 1876  38   6  25        25.5     96.9 6.2    191.3     coconut
    ## 1877  25  12  26        28.6     95.7 6.4    134.8     coconut
    ## 1878  40   5  32        26.1     96.7 6.0    143.5     coconut
    ## 1879   0  19  31        25.5     94.4 6.3    178.7     coconut
    ## 1880  26   9  32        25.9     94.7 6.5    144.2     coconut
    ## 1881  35  30  34        28.3     95.4 6.1    182.4     coconut
    ## 1882  19  30  30        29.6     91.4 5.8    224.8     coconut
    ## 1883  31  13  33        29.7     95.2 6.3    148.3     coconut
    ## 1884  17  29  26        26.1     93.3 6.1    195.4     coconut
    ## 1885   2  30  30        26.0     94.8 6.3    209.5     coconut
    ## 1886  30  13  25        27.2     91.5 6.4    164.9     coconut
    ## 1887   8  15  33        29.0     98.1 5.5    213.9     coconut
    ## 1888  18  12  35        26.1     96.4 6.3    131.3     coconut
    ## 1889   8  28  30        25.5     94.3 6.0    135.1     coconut
    ## 1890  40  22  29        27.6    100.0 5.7    174.6     coconut
    ## 1891  27  10  33        27.8     97.5 6.5    154.1     coconut
    ## 1892  21  20  31        25.6     99.7 5.9    165.8     coconut
    ## 1893   3   9  35        26.9     99.8 6.3    225.6     coconut
    ## 1894  22  16  27        29.2     90.3 6.0    188.9     coconut
    ## 1895  27   8  30        26.4     98.3 6.0    221.2     coconut
    ## 1896  22   8  33        28.4     95.9 5.7    203.9     coconut
    ## 1897  28  27  32        28.9     93.0 5.8    191.8     coconut
    ## 1898  23  21  26        26.5     93.5 5.9    149.2     coconut
    ## 1899  37   5  34        25.8     93.8 5.8    152.4     coconut
    ## 1900  19  26  29        26.9     98.8 5.7    166.6     coconut
    ## 1901 133  47  24        24.4     79.2 7.2     90.8      cotton
    ## 1902 136  36  20        23.1     84.9 6.9     71.3      cotton
    ## 1903 104  47  18        24.0     77.0 7.6     90.8      cotton
    ## 1904 133  47  23        24.9     75.6 6.8     89.8      cotton
    ## 1905 126  38  23        25.4     83.6 6.2     88.4      cotton
    ## 1906 126  50  19        24.7     81.7 6.6     78.6      cotton
    ## 1907 113  41  20        25.0     80.5 7.3     96.3      cotton
    ## 1908 121  45  22        22.5     81.3 6.4     64.2      cotton
    ## 1909 121  47  16        23.6     79.3 7.7     72.5      cotton
    ## 1910 129  60  22        24.6     79.1 5.9     71.9      cotton
    ## 1911 107  45  25        23.1     83.6 7.2     71.8      cotton
    ## 1912 122  59  18        23.5     83.6 6.2     79.8      cotton
    ## 1913 140  38  15        24.1     75.9 6.0     69.9      cotton
    ## 1914 102  49  21        24.7     84.8 6.3     89.8      cotton
    ## 1915 111  40  25        24.5     84.4 6.2     90.9      cotton
    ## 1916 131  35  18        24.5     82.2 7.1     64.0      cotton
    ## 1917 135  43  16        23.5     81.7 6.7     86.8      cotton
    ## 1918 100  46  18        24.2     76.0 6.4     69.1      cotton
    ## 1919 123  39  24        25.0     78.2 7.5     86.1      cotton
    ## 1920 117  56  15        26.0     77.1 7.4     89.1      cotton
    ## 1921 121  36  24        23.7     81.7 7.4     99.4      cotton
    ## 1922 101  58  18        25.7     81.4 6.7     78.6      cotton
    ## 1923 107  42  24        22.0     84.6 6.1     86.0      cotton
    ## 1924 100  41  22        22.4     84.6 7.3     93.5      cotton
    ## 1925 125  39  21        25.0     82.2 8.0     95.0      cotton
    ## 1926 105  60  23        23.5     77.2 6.2     87.5      cotton
    ## 1927 102  46  19        22.8     82.6 6.6     81.5      cotton
    ## 1928 131  49  22        25.5     80.0 7.3     67.1      cotton
    ## 1929 139  35  15        25.2     83.5 5.9     86.6      cotton
    ## 1930 108  36  19        22.8     77.5 7.2     64.6      cotton
    ## 1931 118  45  23        23.4     77.4 8.0     71.7      cotton
    ## 1932 107  51  22        24.9     78.2 6.0     79.6      cotton
    ## 1933 125  60  17        24.1     84.5 6.8     80.4      cotton
    ## 1934 113  37  20        25.0     79.0 7.4     97.1      cotton
    ## 1935 131  52  16        23.7     84.5 6.5     88.5      cotton
    ## 1936 115  48  16        25.5     84.1 7.2     88.9      cotton
    ## 1937 113  38  25        22.0     79.5 7.4     90.4      cotton
    ## 1938 111  41  18        23.6     78.1 6.1     81.0      cotton
    ## 1939 111  53  19        24.0     78.0 6.4     84.6      cotton
    ## 1940 122  48  16        24.7     75.6 6.3     61.8      cotton
    ## 1941 108  46  17        24.3     84.9 6.9     65.0      cotton
    ## 1942 132  41  22        24.3     81.0 7.8     90.4      cotton
    ## 1943 103  42  17        24.3     84.6 6.5     81.1      cotton
    ## 1944 133  50  25        25.7     81.2 7.6     99.9      cotton
    ## 1945 127  37  18        24.9     76.3 7.0     91.9      cotton
    ## 1946 110  39  25        22.6     77.3 7.2     75.1      cotton
    ## 1947 131  38  19        23.9     75.7 6.8     90.5      cotton
    ## 1948 108  38  24        23.4     76.4 7.4     78.8      cotton
    ## 1949 122  40  17        25.0     81.3 6.9     80.0      cotton
    ## 1950 111  50  15        25.2     80.3 7.9     84.6      cotton
    ## 1951 140  40  17        22.7     77.1 6.0     77.6      cotton
    ## 1952 100  40  20        22.5     76.3 7.4     86.8      cotton
    ## 1953 123  50  16        23.0     75.5 6.5     70.7      cotton
    ## 1954 107  36  21        25.3     75.7 6.2     62.6      cotton
    ## 1955 118  50  19        23.0     82.3 6.4     66.5      cotton
    ## 1956 103  51  20        22.8     84.1 7.0     91.6      cotton
    ## 1957 133  57  19        23.5     76.0 7.9     84.1      cotton
    ## 1958 129  47  20        24.4     80.8 6.3     98.6      cotton
    ## 1959 116  52  19        22.9     75.4 6.1     67.1      cotton
    ## 1960 114  40  23        25.5     81.1 6.8     95.4      cotton
    ## 1961 131  60  17        25.3     81.8 7.4     83.5      cotton
    ## 1962 107  43  18        22.4     81.5 6.7     65.5      cotton
    ## 1963 123  44  21        25.8     75.0 7.6     91.4      cotton
    ## 1964 112  49  25        25.7     77.9 6.5     66.2      cotton
    ## 1965 119  44  15        22.1     82.9 7.1     60.7      cotton
    ## 1966 130  59  19        25.1     82.5 6.5     93.5      cotton
    ## 1967 127  53  24        22.2     76.2 6.1     70.4      cotton
    ## 1968 134  52  18        24.0     76.6 8.0     76.1      cotton
    ## 1969 109  36  18        25.4     76.5 7.5     62.5      cotton
    ## 1970 100  48  17        23.8     83.0 7.8     66.3      cotton
    ## 1971 132  52  19        24.2     76.7 6.4     61.9      cotton
    ## 1972 102  37  25        25.3     77.9 5.9     72.8      cotton
    ## 1973 111  39  22        22.6     80.4 6.1     88.6      cotton
    ## 1974 117  51  15        23.0     78.7 6.0     99.8      cotton
    ## 1975 136  36  24        22.7     80.4 7.6     90.1      cotton
    ## 1976 134  56  18        23.8     83.9 6.7     71.0      cotton
    ## 1977 112  54  15        25.5     81.6 6.2     76.9      cotton
    ## 1978 105  56  15        26.0     82.0 7.3     74.1      cotton
    ## 1979 140  45  15        25.5     80.0 5.8     99.4      cotton
    ## 1980 126  46  25        24.4     81.7 6.8     60.8      cotton
    ## 1981 106  49  24        23.0     76.5 7.0     90.6      cotton
    ## 1982 121  53  19        23.5     76.7 8.0     80.1      cotton
    ## 1983 108  60  17        22.8     76.8 6.6     97.8      cotton
    ## 1984 116  56  17        24.7     77.7 8.0     85.2      cotton
    ## 1985 100  52  19        23.5     82.4 7.9     93.5      cotton
    ## 1986 129  43  16        25.6     77.9 6.7     78.6      cotton
    ## 1987 118  44  23        22.1     82.8 6.7     67.1      cotton
    ## 1988 117  43  25        24.7     78.5 7.8     69.3      cotton
    ## 1989 126  37  21        25.8     84.2 6.6     77.0      cotton
    ## 1990 120  48  16        22.5     75.4 7.5     71.9      cotton
    ## 1991 102  45  16        23.7     77.5 7.3     74.9      cotton
    ## 1992 131  56  20        22.0     81.8 7.8     92.2      cotton
    ## 1993 114  40  17        24.3     80.1 6.4     69.5      cotton
    ## 1994 101  37  18        22.9     82.7 7.6     92.9      cotton
    ## 1995 106  46  20        23.4     78.6 6.2     81.2      cotton
    ## 1996 113  38  20        22.1     78.6 6.4     74.9      cotton
    ## 1997 102  53  21        23.0     76.1 6.9     91.5      cotton
    ## 1998 110  39  18        24.5     75.4 7.8     63.9      cotton
    ## 1999 107  58  15        23.7     75.8 7.6     76.6      cotton
    ## 2000 120  60  15        22.3     83.9 7.3     65.4      cotton
    ## 2001  89  47  38        25.5     72.2 6.0    151.9        jute
    ## 2002  60  37  39        26.6     82.9 6.0    161.2        jute
    ## 2003  63  41  45        25.3     86.9 7.1    196.6        jute
    ## 2004  86  40  39        25.7     88.2 6.2    175.6        jute
    ## 2005  96  41  40        23.6     72.0 6.1    190.4        jute
    ## 2006 100  35  36        25.3     72.0 6.3    190.6        jute
    ## 2007  63  37  43        23.4     85.1 6.7    185.7        jute
    ## 2008  70  43  40        24.4     88.8 6.2    169.1        jute
    ## 2009  67  55  44        26.3     75.1 7.3    182.3        jute
    ## 2010  74  40  40        25.1     83.1 6.4    169.3        jute
    ## 2011  89  53  44        24.9     71.9 7.3    150.2        jute
    ## 2012  74  46  45        25.8     88.4 6.0    189.4        jute
    ## 2013  89  41  38        23.1     74.7 6.3    199.8        jute
    ## 2014  60  55  40        25.0     89.0 7.0    151.5        jute
    ## 2015  67  43  38        25.2     70.9 7.3    195.9        jute
    ## 2016  70  38  35        24.4     79.3 7.0    164.3        jute
    ## 2017  74  49  38        23.3     71.5 7.5    164.5        jute
    ## 2018  90  40  39        25.7     81.9 6.6    192.0        jute
    ## 2019  82  35  44        27.0     78.2 6.2    169.8        jute
    ## 2020  73  45  37        23.7     74.6 6.7    181.3        jute
    ## 2021  85  53  38        24.9     73.8 6.6    153.9        jute
    ## 2022  81  56  36        23.4     72.6 7.1    174.8        jute
    ## 2023  84  55  38        26.9     79.8 7.0    173.1        jute
    ## 2024  80  45  42        23.1     75.0 7.4    151.9        jute
    ## 2025  76  54  45        24.3     77.6 6.2    185.0        jute
    ## 2026  76  56  39        24.4     89.9 6.6    197.1        jute
    ## 2027  81  40  45        25.8     80.8 6.4    174.5        jute
    ## 2028  76  44  45        25.5     84.5 6.7    168.8        jute
    ## 2029  69  47  40        25.4     76.2 6.1    183.8        jute
    ## 2030  82  40  45        26.2     81.7 6.7    180.1        jute
    ## 2031  69  57  35        24.3     78.5 6.2    186.2        jute
    ## 2032  81  36  38        23.8     88.0 6.3    150.3        jute
    ## 2033  67  60  38        24.8     78.5 7.2    162.3        jute
    ## 2034  72  51  40        23.2     74.1 7.4    199.5        jute
    ## 2035  65  39  45        23.7     70.9 6.8    184.5        jute
    ## 2036  78  50  43        25.1     85.7 6.3    159.6        jute
    ## 2037  77  52  41        23.9     83.5 6.1    167.7        jute
    ## 2038  89  52  42        23.1     81.5 6.1    196.7        jute
    ## 2039  62  49  37        24.2     82.9 7.5    166.1        jute
    ## 2040  90  48  45        24.1     71.3 6.5    153.6        jute
    ## 2041  66  47  36        24.9     74.4 6.6    175.6        jute
    ## 2042  80  52  39        26.4     76.9 7.2    197.2        jute
    ## 2043  89  52  45        24.9     77.0 7.2    196.5        jute
    ## 2044  77  51  44        23.3     82.7 7.1    166.2        jute
    ## 2045  94  37  41        24.8     87.1 6.5    179.2        jute
    ## 2046  75  41  35        25.0     78.6 6.9    166.6        jute
    ## 2047  60  55  36        26.1     80.5 7.1    150.6        jute
    ## 2048  62  56  35        26.0     81.7 6.2    163.3        jute
    ## 2049  84  40  42        26.3     73.4 6.7    186.7        jute
    ## 2050 100  56  40        26.4     83.3 7.4    176.2        jute
    ## 2051  75  56  44        25.3     73.7 6.1    168.0        jute
    ## 2052  78  46  42        23.1     78.5 7.1    155.4        jute
    ## 2053  82  48  36        25.8     81.8 6.4    193.2        jute
    ## 2054 100  58  41        23.2     87.9 6.7    160.6        jute
    ## 2055  88  50  40        25.6     80.0 7.1    182.3        jute
    ## 2056  67  41  40        25.8     87.8 7.3    152.6        jute
    ## 2057  72  42  43        26.6     80.9 6.4    181.3        jute
    ## 2058  89  40  43        26.2     73.0 7.1    190.0        jute
    ## 2059  89  57  43        26.9     73.2 7.0    177.2        jute
    ## 2060  61  41  44        24.4     82.1 6.5    159.9        jute
    ## 2061  79  45  43        25.7     79.2 7.2    187.2        jute
    ## 2062  84  40  43        25.0     88.3 7.2    169.4        jute
    ## 2063  98  43  35        25.4     76.4 7.3    188.6        jute
    ## 2064  75  36  44        23.3     74.3 6.6    153.7        jute
    ## 2065  89  58  35        24.0     82.1 6.1    167.1        jute
    ## 2066  91  41  37        24.5     83.2 6.1    192.2        jute
    ## 2067  77  48  36        25.9     84.1 7.4    154.8        jute
    ## 2068  66  58  35        23.6     79.5 7.3    185.3        jute
    ## 2069  62  59  41        24.2     74.9 7.2    192.5        jute
    ## 2070  82  35  35        25.5     87.0 7.3    176.5        jute
    ## 2071  61  41  35        25.0     79.5 6.8    195.8        jute
    ## 2072  99  57  38        24.8     82.1 6.4    156.4        jute
    ## 2073  70  42  43        23.2     76.7 6.5    157.1        jute
    ## 2074  90  59  35        24.3     89.9 7.1    175.2        jute
    ## 2075  73  43  42        26.6     78.0 6.3    154.8        jute
    ## 2076  67  46  44        26.8     78.2 7.1    153.9        jute
    ## 2077  84  37  42        25.5     81.1 6.7    169.9        jute
    ## 2078  72  41  36        24.1     80.6 6.2    176.9        jute
    ## 2079  71  56  37        23.2     86.2 6.5    176.1        jute
    ## 2080  64  53  38        26.2     78.5 6.9    183.4        jute
    ## 2081  65  54  39        23.8     71.1 7.1    160.1        jute
    ## 2082  60  58  37        26.1     79.1 6.1    171.5        jute
    ## 2083  86  39  43        26.1     71.2 6.4    193.1        jute
    ## 2084  90  50  44        26.9     73.5 6.3    171.5        jute
    ## 2085  91  38  36        26.5     77.2 7.3    157.9        jute
    ## 2086  87  48  38        23.8     80.9 7.2    190.3        jute
    ## 2087  72  41  36        26.5     86.8 6.1    153.0        jute
    ## 2088  71  54  35        26.6     71.0 7.3    199.3        jute
    ## 2089  82  46  41        23.3     79.8 6.6    187.3        jute
    ## 2090  71  52  43        26.5     74.0 6.7    180.3        jute
    ## 2091  80  43  43        23.8     74.4 6.0    172.6        jute
    ## 2092  77  55  43        25.5     76.0 6.7    193.7        jute
    ## 2093  95  57  41        23.2     73.7 6.4    184.8        jute
    ## 2094  63  47  35        27.0     89.1 7.4    193.9        jute
    ## 2095  93  43  38        23.6     86.1 7.0    150.2        jute
    ## 2096  87  44  43        23.9     86.8 6.7    177.5        jute
    ## 2097  88  52  39        23.9     88.1 6.9    154.7        jute
    ## 2098  90  39  37        24.8     81.7 6.9    190.8        jute
    ## 2099  90  39  43        24.4     82.3 6.8    191.0        jute
    ## 2100  84  38  43        26.6     73.8 7.3    159.3        jute
    ## 2101  91  21  26        26.3     57.4 7.3    191.7      coffee
    ## 2102 107  21  26        26.5     55.3 7.2    144.7      coffee
    ## 2103  83  38  35        25.7     52.9 7.2    136.7      coffee
    ## 2104 108  24  31        24.1     56.2 6.4    147.3      coffee
    ## 2105 116  28  34        23.4     60.4 6.4    122.2      coffee
    ## 2106 116  23  25        23.4     52.3 6.9    139.4      coffee
    ## 2107 109  31  27        23.1     50.4 7.0    164.5      coffee
    ## 2108  89  25  34        23.1     63.7 7.2    129.9      coffee
    ## 2109 118  18  32        27.6     51.1 6.4    122.8      coffee
    ## 2110 111  32  34        25.5     69.4 6.4    171.4      coffee
    ## 2111  84  36  28        26.7     55.6 6.1    140.6      coffee
    ## 2112  85  33  25        26.2     52.5 6.9    189.1      coffee
    ## 2113  99  15  27        27.0     57.3 6.5    165.7      coffee
    ## 2114  81  30  31        24.7     51.9 7.0    135.1      coffee
    ## 2115  95  39  29        27.4     56.0 7.1    149.0      coffee
    ## 2116  81  34  30        25.2     62.3 6.6    135.0      coffee
    ## 2117  80  15  28        23.1     68.0 6.7    161.9      coffee
    ## 2118 104  20  26        27.2     53.0 7.5    175.7      coffee
    ## 2119 109  29  28        23.3     60.5 6.7    194.2      coffee
    ## 2120 100  32  26        25.2     57.5 6.0    124.2      coffee
    ## 2121 100  24  28        25.6     57.7 7.1    195.8      coffee
    ## 2122  83  21  28        25.6     60.5 7.5    190.2      coffee
    ## 2123 120  23  28        25.7     51.3 6.9    196.3      coffee
    ## 2124 104  26  30        24.4     62.7 6.4    148.7      coffee
    ## 2125 108  33  31        23.7     66.8 7.4    144.7      coffee
    ## 2126  91  25  26        24.5     67.0 7.5    180.5      coffee
    ## 2127  86  26  27        27.1     52.9 6.1    192.4      coffee
    ## 2128  98  18  27        27.6     68.5 6.5    167.4      coffee
    ## 2129 111  27  31        23.6     55.3 6.0    191.4      coffee
    ## 2130  84  39  35        23.2     52.1 7.0    117.3      coffee
    ## 2131  98  27  27        24.7     51.3 7.2    197.6      coffee
    ## 2132 118  21  34        24.4     64.7 7.2    119.6      coffee
    ## 2133 103  27  31        27.2     51.6 6.7    126.2      coffee
    ## 2134  82  24  33        26.5     67.1 6.8    120.6      coffee
    ## 2135  86  31  35        27.0     60.8 6.5    191.5      coffee
    ## 2136  88  35  35        27.6     58.5 6.8    117.9      coffee
    ## 2137  84  27  29        23.3     53.0 7.2    168.3      coffee
    ## 2138 120  40  33        24.2     54.3 6.7    115.2      coffee
    ## 2139 106  40  30        23.4     64.1 6.8    122.7      coffee
    ## 2140 113  21  33        26.0     55.8 7.3    176.9      coffee
    ## 2141 117  34  25        24.8     56.8 7.2    124.4      coffee
    ## 2142  80  30  25        26.2     65.6 7.5    148.4      coffee
    ## 2143  88  21  27        24.4     66.0 7.2    181.6      coffee
    ## 2144 113  33  34        26.0     62.1 6.6    153.5      coffee
    ## 2145  87  23  28        26.2     62.3 7.0    193.7      coffee
    ## 2146 113  15  29        27.1     63.6 6.8    190.2      coffee
    ## 2147  98  29  30        25.6     61.0 6.2    199.5      coffee
    ## 2148  97  29  27        27.7     54.4 7.2    139.9      coffee
    ## 2149  85  35  32        26.2     54.3 6.9    133.1      coffee
    ## 2150  82  29  35        26.7     52.2 6.2    156.2      coffee
    ## 2151 103  33  25        27.1     55.7 6.9    139.5      coffee
    ## 2152 112  17  28        27.6     61.3 6.8    196.6      coffee
    ## 2153  99  19  33        27.5     55.5 6.3    130.6      coffee
    ## 2154 120  20  34        23.6     50.6 6.9    130.4      coffee
    ## 2155 114  27  28        25.0     57.9 7.2    192.9      coffee
    ## 2156 100  40  35        27.6     54.4 7.0    177.8      coffee
    ## 2157 108  35  25        24.0     61.1 7.0    161.5      coffee
    ## 2158 115  31  30        24.2     67.4 6.8    122.4      coffee
    ## 2159  87  28  30        25.6     68.7 6.5    168.8      coffee
    ## 2160  82  24  26        24.3     53.6 6.1    184.4      coffee
    ## 2161  94  26  27        26.4     52.3 7.5    177.3      coffee
    ## 2162  87  28  35        26.6     57.2 6.8    152.1      coffee
    ## 2163 118  40  35        26.4     58.5 7.5    121.6      coffee
    ## 2164  87  38  29        25.2     57.9 6.7    156.1      coffee
    ## 2165  92  40  30        23.4     55.2 6.0    171.7      coffee
    ## 2166  97  22  26        23.6     59.7 6.1    185.2      coffee
    ## 2167  99  40  32        24.2     69.9 7.0    163.3      coffee
    ## 2168  89  28  33        26.4     53.8 7.0    175.4      coffee
    ## 2169 112  39  29        26.1     63.4 6.7    147.8      coffee
    ## 2170 111  28  26        27.8     64.5 6.9    192.7      coffee
    ## 2171 114  20  26        25.6     62.7 7.3    193.6      coffee
    ## 2172 117  26  30        27.9     68.0 7.1    115.2      coffee
    ## 2173 111  29  31        26.1     52.3 6.1    161.3      coffee
    ## 2174 119  30  28        26.4     64.6 6.5    163.6      coffee
    ## 2175 116  40  33        24.9     54.2 7.0    129.5      coffee
    ## 2176  95  37  35        27.3     68.4 6.3    192.4      coffee
    ## 2177  86  40  33        26.1     52.3 7.4    136.3      coffee
    ## 2178 117  37  32        23.1     67.1 6.8    162.6      coffee
    ## 2179 105  18  35        23.5     68.4 6.7    171.9      coffee
    ## 2180 109  23  25        25.1     68.5 7.0    194.9      coffee
    ## 2181  80  18  31        24.0     58.8 7.3    134.7      coffee
    ## 2182 101  31  26        26.7     69.7 6.9    158.9      coffee
    ## 2183 103  33  33        26.7     50.5 7.1    126.8      coffee
    ## 2184  93  26  27        24.6     56.5 7.3    137.7      coffee
    ## 2185 104  35  28        27.5     50.7 7.0    144.0      coffee
    ## 2186 116  36  25        27.6     58.5 6.2    156.7      coffee
    ## 2187 107  38  29        26.7     57.6 6.4    145.1      coffee
    ## 2188 101  33  33        27.0     62.0 6.9    142.9      coffee
    ## 2189 107  31  31        23.2     53.0 6.8    153.1      coffee
    ## 2190  99  16  30        23.5     65.4 6.4    186.2      coffee
    ## 2191 103  40  30        27.3     55.2 6.3    141.5      coffee
    ## 2192 118  31  34        27.5     62.9 6.1    181.4      coffee
    ## 2193 106  21  35        25.6     57.0 7.4    188.6      coffee
    ## 2194 116  38  34        23.3     50.0 6.0    183.5      coffee
    ## 2195  97  35  26        24.9     53.7 6.3    166.3      coffee
    ## 2196 107  34  32        26.8     66.4 6.8    177.8      coffee
    ## 2197  99  15  27        27.4     56.6 6.1    127.9      coffee
    ## 2198 118  33  30        24.1     67.2 6.4    173.3      coffee
    ## 2199 117  32  34        26.3     52.1 6.8    127.2      coffee
    ## 2200 104  18  30        23.6     60.4 6.8    140.9      coffee

Therefore, I will keep all of these variables.

## 5.2 Train-Test Split

The column “label” is what we are going to predict.

``` r
set.seed(123)

# Create Data Partition

training.set <- crop2$label %>% createDataPartition(p = 0.8, list = F)

# split 80% for train set and 20% for test set

train.data <- crop2[training.set, ]

test.data <- crop2[-training.set, ]
```

## 5.3 Model building

build linear regression

``` r
model_lm <- lm(label ~., data = train.data)
```

    ## Warning in model.response(mf, "numeric"): using type = "numeric" with a factor
    ## response will be ignored

    ## Warning in Ops.factor(y, z$residuals): '-' not meaningful for factors

-   train validate test split (train and validate, then use test test to
    makesure effective in real world)
-   multiple linear regression
-   Lasso regression (because this dataset is going to be so sparse with
    all these dummy variable, lasso help to normalise that)
-   Random forest (so we have a tree-based model to compare to our LM)
    -   Can use gradient boosted tree
    -   Support vector regression (classification)
-   tune models GridsearchCV
-   test ensembles

# 6 Model for Production

# Conclusion
