Recommendation of Crop Classes by Predictive Model
================
Kar Ng
2021

-   [1. SUMMARY](#1-summary)
-   [2 R PACKAGES](#2-r-packages)
-   [4 DATA IMPORT AND CLEANING](#4-data-import-and-cleaning)
    -   [4.1 Data import](#41-data-import)
    -   [4.2 Data exploration](#42-data-exploration)
    -   [4.3 Data manipulation](#43-data-manipulation)
-   [5 EXPLORATORY DATA ANALYSIS](#5-exploratory-data-analysis)
    -   [5.1 Histograms](#51-histograms)
    -   [5.2 Correlogram](#52-correlogram)
    -   [5.3 Scatter plot](#53-scatter-plot)
    -   [5.4 Boxplots](#54-boxplots)
-   [6 MODEL BUILDING](#6-model-building)
    -   [6.1 Variables Selection](#61-variables-selection)
    -   [6.2 Train-Test Split](#62-train-test-split)
    -   [6.3 Model](#63-model)
        -   [6.3.1 Discriminant Analysis](#631-discriminant-analysis)
        -   [6.3.2 Naive Bayes Classifier](#632-naive-bayes-classifier)
        -   [6.3.3 Suppor Vector Method
            (SVM)](#633-suppor-vector-method-svm)
        -   [6.3.4 KNN](#634-knn)
        -   [6.3.5 Decision Tree](#635-decision-tree)
        -   [6.3.6 Random Forest](#636-random-forest)
        -   [6.3.7 Gradient Boosted Random
            Forest](#637-gradient-boosted-random-forest)
    -   [6.4 Accuracy Comparison](#64-accuracy-comparison)
    -   [6.5 Confusion Matrix](#65-confusion-matrix)
-   [8 MODEL FOR PRODUCTION](#8-model-for-production)
    -   [8.1 Direct Model Prediction](#81-direct-model-prediction)
    -   [8.2 Functionise the model](#82-functionise-the-model)
    -   [8.3 API App with Plumber](#83-api-app-with-plumber)
-   [9 CONCLUSION](#9-conclusion)
-   [10 CREDIT AND ACKNOWLEDGEMENT](#10-credit-and-acknowledgement)

------------------------------------------------------------------------

![](C:\Users\karho\Desktop\R\github\crop\pic6_thumbnail.jpg)

------------------------------------------------------------------------

Reading time: 13 minutes

# 1. SUMMARY

This project uses a dataset from *Kaggle.com* that shared publicly with
a machine learning task. This dataset was collected in India. We were
asked to build a predictive model to help farmers to make informed
decision.

We need to build a model that is able to recommend the most suitable
crop to grow in a particular farm based on various parameters specified
in the dataset. These parameters include the levels of nitrogen,
phosphorus, and potassium, as well as temperature, humidity, pH, and
rainfall level. There are 22 agricultural crops recommendable in the
dataset.

Machine learning techniques were applied in this project and 13 models
were built and compared. Naive Bayes classifier and boosted random
forest were the two best models in predicting the test dataset, both at
the same accuracy of 99.55%. Confusion matrix was applied, sensitivity
and specificity were excellent at a level of above 90% in all crop data
points. The model was put into production with development of a API app
to ease application of these models.

*Highlight*

![](C:\Users\karho\Desktop\R\github\crop\pic5_combine.JPG)

# 2 R PACKAGES

``` r
library(tidyverse)
library(skimr)
library(kableExtra)
library(corrplot)
library(caret)
library(MASS)
library(randomForest)
library(xgboost)
library(rpart)
```

# 4 DATA IMPORT AND CLEANING

This project uses a public dataset in kaggle.com
[Link](https://www.kaggle.com/atharvaingle/crop-recommendation-dataset),
called “Crop Recommendation Dataset” by *Artharva Ingle*.

## 4.1 Data import

``` r
crop <- read.csv("Crop_recom.csv",
                 fileEncoding = "UTF-8-BOM")
```

The dataset has following descriptions, adapted from the *Kaggle*
website.

``` r
Variables <- c("N", "P", "K", "temperature", "humidity", "ph", "rainfall", "label")

Description <- c("N-P-K Ratio of Nitrogen (N) portion in soil",
                 "N-P-K Ratio of Phosphorus (P) portion in soil",
                 "N-P-K Ratio of Potassium (K) portion in soil",
                 "temperature in degree Celsius, oC",
                 "relative humidity in %",
                 "Soil pH value",
                 "rainfall in mm",
                 "Different crops")

data.frame(Variables, Description) %>% 
  kbl() %>% 
  kable_styling(bootstrap_options = c("hover", "stripped", "bordered"))
```

<table class="table table-hover table-bordered" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Variables
</th>
<th style="text-align:left;">
Description
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

## 4.2 Data exploration

This dataset has 2200 rows of observations and 8 columns of variables.

-   The “label” is categorised as a character variable by R.

-   And the rest are numerical variables.

-   The dataset is clean and having no missing values. It can be assess
    by examining the **complete\_rate** and the associated column
    **n\_missing** that used to detect missing values in the dataset.

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

Numerical variables are either labelled as “int”, “num”, or “dbl” by R.
The “dbl” stands for “double”, which is usually used to label numbers
with decimal places.

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

## 4.3 Data manipulation

**Convert the variable ‘label’ from character into factor**

It will help to quickly examine what are the levels within the variable
“label” using R function.

``` r
# To protect the original dataset, I create a new object named "crop2" to carry the cleaned data set 

crop2 <- crop %>% 
  mutate(label = as.factor(label))
```

Following result shows that there are 22 crops in this data set. Each
crop has 100 samples, which is excellent as sample sizes are equal and
will make statistical comparison fairer。

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

This step is optional but I will make it happen. I will restrict the
decimal places of “temperature”, “humidity”, “pH”, and “rainfall” to
only one, as this length of decimal places is sufficient in the case of
the project.

``` r
crop2 <- crop2 %>% 
  mutate(temperature = round(temperature, 1),
         humidity = round(humidity, 1),
         ph = round(ph, 1),
         rainfall = round(rainfall, 1))
```

Following summary shows the general statistics of all the variables,
such as minimum, maximum, median, and mean. It also shows the number of
samples if the variables are categorical and is in factor format, such
as the “label” that I converted in the previous section from character
into factor.

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

# 5 EXPLORATORY DATA ANALYSIS

## 5.1 Histograms

It will be interesting to visualise the distribution of each numerical
variables in the data set as an initial visual examination.

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

![](crop_files/figure-gfm/unnamed-chunk-11-1.png)<!-- --> Insights:

-   Distributions of N, P, and K are quite wide spread。

-   The temperatures are mostly between 20 - 30oC。

-   Humidity is quite wide spread with majority fall between 75 - 100%.

-   pH and in soil are most slightly acidic with a value around 6.

-   Rainfalls in this entire dataset are less than 300mm, with majority
    fall between 50 - 120mm.

## 5.2 Correlogram

Following correlogram (a plot for correlation) shows that variables are
independent from each other except the correlation between P and K. here
is a strong relationship between P and K with a correlation of 0.74.

``` r
# convert into matrix, remove the factor "label" 

cor_c <- cor(crop2[, 1:7])

# correlogram

corrplot(cor_c, method = "number", type = "upper")
```

![](crop_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

However, the rule of thumb is that if correlation is greater than 0.8
between two independent variables, then multicollinearity would exist.
Therefore, it is safe to use P and K together during modeling.

## 5.3 Scatter plot

It might be intersting to see how are P and K relate to each other.

``` r
ggplot(crop2, aes(x = P, y = K, colour = label)) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  labs(title = "Positive Relationship betweem P and K") 
```

![](crop_files/figure-gfm/unnamed-chunk-13-1.png)<!-- --> From the
graph, there might be a positive relationship between K and P, however
the high value of correlation 0.74 is due to the type of crops,
especially the crop types near 150 units of P.

## 5.4 Boxplots

This section uses boxplot to compare the type of crops and other
predictor variables.

``` r
# set up data frame

library(tidytext)


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
                                                     "rainfall")),
         label = reorder_within(x = label, by = result, within = variables)) 
  
# plot boxplots

ggplot(df4.3, aes(x = label, y = result, colour = label)) +
  geom_boxplot() +
  facet_wrap(~variables, scale = "free", ncol =1) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)) +
  scale_x_reordered()
```

![](crop_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

# 6 MODEL BUILDING

## 6.1 Variables Selection

This section aimed to select relevant and confounding variables for
model building.

According to the purpose of this project, all of these variables are
agricultural-wise important and are critical in determining the type of
crops to grow in a particular farm.

``` r
head(crop2)
```

    ##    N  P  K temperature humidity  ph rainfall label
    ## 1 90 42 43        20.9     82.0 6.5    202.9  rice
    ## 2 85 58 41        21.8     80.3 7.0    226.7  rice
    ## 3 60 55 44        23.0     82.3 7.8    264.0  rice
    ## 4 74 35 40        26.5     80.2 7.0    242.9  rice
    ## 5 78 42 42        20.1     81.6 7.6    262.7  rice
    ## 6 69 37 42        23.1     83.4 7.1    251.1  rice

Therefore, I will keep all variables.

## 6.2 Train-Test Split

The column “label” is what we are going to predict, which is the
types/classes of crops.

``` r
set.seed(123)

# Create Data Partition

training.set <- crop2$label %>% createDataPartition(p = 0.8, list = F)

# split 80% for train set and 20% for test set

train.data <- crop2[training.set, ]

test.data <- crop2[-training.set, ]
```

## 6.3 Model

In this section, I will build and explore the appropriate machine
learning models for farmers to make prediction.

### 6.3.1 Discriminant Analysis

5 methods of Discriminant Analysis are carried out, they are linear,
quadratic, mixture, flexible and regularised discriminant analysis.

``` r
# Create data parameter 

preprocess_parameter <- train.data %>% preProcess(method = c("center", "scale"))

# Data standardisation

train.transformed <- preprocess_parameter %>% predict(train.data)
test.transformed <- preprocess_parameter %>% predict(test.data)

# build discriminant models

model_lda <- lda(label ~., data = train.transformed)
model_qda <- qda(label ~., data = train.transformed)
model_mda <- mda::mda(label ~., data = train.transformed)
model_fda <- mda::fda(label ~., data = train.transformed)
model_rda <- klaR::rda(label ~., data = train.transformed)

# predictions based on test data

predict_lda <- model_lda %>% predict(test.transformed)
predict_qda <- model_qda %>% predict(test.transformed)
predict_mda <- model_mda %>% predict(test.transformed)
predict_fda <- model_fda %>% predict(test.transformed)
predict_rda <- model_rda %>% predict(test.transformed)

# putting the results together

data.frame(
lda_accuracy = mean(predict_lda$class == test.transformed$label),
qda_accuracy = mean(predict_qda$class == test.transformed$label),
mda_accuracy = mean(predict_mda == test.transformed$label),
fda_accuracy = mean(predict_fda == test.transformed$label),
rda_accuracy = mean(predict_rda$class == test.transformed$label)
)
```

    ##   lda_accuracy qda_accuracy mda_accuracy fda_accuracy rda_accuracy
    ## 1    0.9727273    0.9931818    0.9818182    0.9727273    0.9931818

All 5 methods of discriminant models produce great accuracy results.

To be conservative, flexible discriminant analysis (FDA) would be
selected as the best model, if it outcompete other models that I build
later section. It is because FDA does not assume normality of data and
equal variances among classes, and it has also an exellent level of
accuracy at 97%.

QDA is suitable to large data set and RDA is commonly used for large
multivariate dataset (especially when predictor &gt; sample size) with
the present of multicollinearity. The LDA is known for its strict
restriction with assumptions of data normality and equal variances among
classes, whereas MDA is slightly less restrictive model compared to LDA.

### 6.3.2 Naive Bayes Classifier

``` r
model_nb <- train(label ~., data = train.data,
                   method = "nb",
                   trControl = trainControl("cv", number = 10))

# predictions

prediction_nb <- model_nb %>% predict(test.data)

# model accuracy

mean(prediction_nb == test.data$label)
```

    ## [1] 0.9954545

### 6.3.3 Suppor Vector Method (SVM)

This model is sometimes referred as support vector regression (SVR).
This method identify the optimal decision boundary when it separates the
points from different classes and then use the final drawn boundary for
prediction.

I will perform 3 types of SVR here and examine which SVR model produces
the best predictions based on test dataset. The 3 SVR models are linear,
non-linear (Radial Kernal), and non-linear (polynomial kernal).

#### 6.3.3.1 Linear SVM

Applying following codes from caret packages, it helps to determine the
best Cost (C) which is the tuning parameter of SVM. The higher the c,
the better the accuracy result of particular model.

``` r
set.seed(123)

model_svml <- train(label ~., data = train.data,
                    method = "svmLinear",
                    trControl = trainControl("cv", number = 10),
                    tuneGrid = expand.grid(C = seq(0, 2, length = 20)),
                    preProcess = c("center", "scale"))


# predictions

predictions_svml <- model_svml %>% predict(test.data)

# accuracy

mean(predictions_svml == test.data$label)
```

    ## [1] 0.9818182

``` r
plot(model_svml)
```

![](crop_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
model_svml$bestTune
```

    ##    C
    ## 20 2

#### 6.3.3.2 Non-linear SVM (Radial)

For non-linear SVM, I will use either radial kernal or polynomial
kernal. The R package will find the best values for modelt

``` r
# build the model

set.seed(123)

model_svmR <- train(label ~., data = train.data,
                    method = "svmRadial",
                    trControl = trainControl("cv", number = 10),
                    tuneLength = 10,
                    preProcess = c("center", "scale"))

# predictions

prediction_svmR <- model_svmR %>% predict(test.data)

# accuracy

mean(prediction_svmR == test.data$label)
```

    ## [1] 0.9863636

``` r
model_svmR$bestTune
```

    ##       sigma  C
    ## 8 0.1441001 32

#### 6.3.3.3 Non-linear (Polynomial)

``` r
# build the model

set.seed(123)

# This model takes quite some time to run
model_poly <- train(label ~., data = train.data,
                    method = "svmPoly",
                    trControl = trainControl("cv", number = 10),
                    tuneLength = 4,
                    preProcess = c("center", "scale")
                    )

# predictions 

prediction_poly <- model_poly %>% predict(test.data)

# accuracy

mean(prediction_poly == test.data$label)
```

    ## [1] 0.9840909

### 6.3.4 KNN

Applying the K-Nearest Neighbor in this section. I will use caret
package to help to search for the optimal k number by argument
“tuneLength”.

``` r
set.seed(123)

model_knn <- train(label ~., data = train.data,
                   method = "knn",
                   trControl = trainControl("cv", number = 10),
                   preProcess = c("center", "scale"),
                   tuneLength = 10)

# plot model

plot(model_knn)
```

![](crop_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

K value (Neighbors) has a negative relationship with Accuracy. A k-value
of 5 has the highest accuracy during cross-validation prediction.

``` r
#predictions

prediction_knn <- model_knn %>% predict(test.data)

# accuracy

mean(prediction_knn == test.data$label)
```

    ## [1] 0.9727273

### 6.3.5 Decision Tree

``` r
# packages：rpart
# build the model

set.seed(123)

model_tree <- train(label ~., data = train.data,
                    method = "rpart",
                    trControl = trainControl("cv", number = 10),
                    tuneLength = 5)     
                    # tuneLength to search for the best complexity parameter to prune the tree

plot(model_tree)
```

![](crop_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

Complexity parameter (cp) at lower value produce the highest accuracy. R
recommends a cp of 0.0077 to generate the highest accuracy rate.

``` r
model_tree$bestTune
```

    ##            cp
    ## 1 0.007738095

``` r
par(xpd = NA)
plot(model_tree$finalModel, main = "Decision Tree")
text(model_tree$finalModel, srt = 14, cex = 0.8, col = "blue")
```

![](crop_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
# predictions

prediction_tree <- model_tree %>% predict(test.data)

# accuracy

mean(prediction_tree == test.data$label)
```

    ## [1] 0.9545455

### 6.3.6 Random Forest

I tune a couple of hyperparameters in the following codes. These tuning
may help to avoid overfitting on noisy data set (P. Bruce and Bruce
2017).

``` r
# build the model

model_rf <- list()
for (nodesize in c(1,2,4,8)){   
  set.seed(123)
  model <- train(label ~., data = train.data,
               method = "rf",
               trControl = trainControl("cv", number = 10),
               metric = "Accuracy",
               nodesize = nodesize)
  model.name <- toString(nodesize)
  model_rf[[model.name]] <- model
}

# Results comparison

resamples(model_rf) %>% summary(metric = "Accuracy")
```

    ## 
    ## Call:
    ## summary.resamples(object = ., metric = "Accuracy")
    ## 
    ## Models: 1, 2, 4, 8 
    ## Number of resamples: 10 
    ## 
    ## Accuracy 
    ##        Min.   1st Qu.    Median      Mean   3rd Qu. Max. NA's
    ## 1 0.9829545 0.9900568 0.9943182 0.9943182 1.0000000    1    0
    ## 2 0.9886364 0.9900568 0.9943182 0.9943182 0.9985795    1    0
    ## 4 0.9886364 0.9900568 0.9943182 0.9948864 1.0000000    1    0
    ## 8 0.9829545 0.9900568 0.9971591 0.9948864 1.0000000    1    0

4 different nodesize (1, 2, 4, 8) produced 99% of accuracy. Nodesize is
the minimum of terminal nodes. The differences in term of accuracy
between nodesize were really small, node 8 has the highest median
accuracy of 99.7% base on cross validations.

I will pick nodesize of 8 to be the most appropriate nodesize for this
random forest model.

Following statistics of the final model of this random forest
computation indicating that:

-   500 trees are trained (default).  
-   The optimal number of variables randomly sampled at each split is 2,
    known as mtry and bootstrap sampling. The optimal value is selected
    by *caret* automation.  
-   OBB rate is extremely small at only 0.51%.

**Variable importance**

Across all of the trees within the random forest algorithm, rainfall,
humidity, and K are the three most important variables.

``` r
Gini.table <- randomForest::importance(model_rf$`8`$finalModel)
Gini.table <- Gini.table %>% as.data.frame()
Gini.table %>% 
  arrange(desc(MeanDecreaseGini))
```

    ##             MeanDecreaseGini
    ## rainfall           366.08567
    ## humidity           354.14878
    ## K                  298.64230
    ## P                  253.62531
    ## N                  174.39842
    ## temperature        119.00671
    ## ph                  82.96643

``` r
varImpPlot(model_rf$`8`$finalModel)
```

![](crop_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

“MeanDecreaseGini” represents the average decrease in node impurity by
the variable. If rainfall and humidity are excluded from the model,
large impurity will occur and affect the prediction accuracy.

``` r
# prediction

prediction_rf <- model_rf %>% predict(test.data)

# accuracy

mean(prediction_rf$`8` == test.data$label)
```

    ## [1] 0.9909091

This tuned random forest model has 99.1% accuracy.

### 6.3.7 Gradient Boosted Random Forest

``` r
model_brf <- train(label ~., data = train.data,
                   method = "xgbTree",
                   trControl = trainControl("cv", number = 10))

# prediction

prediction_brf <- model_brf %>% predict(test.data)

# accuracy

mean(prediction_brf == test.data$label)
```

    ## [1] 0.9954545

## 6.4 Accuracy Comparison

``` r
# df

p.summary <- data.frame(
  LDA = mean(predict_lda$class == test.transformed$label),
  QDA = mean(predict_qda$class == test.transformed$label),
  MDA = mean(predict_mda == test.transformed$label),
  FDA = mean(predict_fda == test.transformed$label),
  RDA = mean(predict_rda$class == test.transformed$label),
  NaiveBayer = mean(prediction_nb == test.data$label),
  SVM_linear = mean(predictions_svml == test.data$label),
  SVM_svmR = mean(prediction_svmR == test.data$label),
  SVM_Poly = mean(prediction_poly == test.data$label),
  knn = mean(prediction_knn == test.data$label),
  decision_tree = mean(prediction_tree == test.data$label),
  RandomForest = mean(prediction_rf$`8` == test.data$label),
  RandomForest_Boosted = mean(prediction_brf == test.data$label)
)

# pivot longer

models_accuracy <- p.summary %>% 
  pivot_longer(c(1:13),
               names_to = "Model",
               values_to = "Accuracy") %>% 
  mutate(Accuracy = round(Accuracy*100, 2)) %>% 
  arrange(desc(Accuracy)) 


# plot

ggplot(models_accuracy, aes(x = reorder(Model, -Accuracy), 
                            y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", width = 0.8) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 20, vjust = 0.85)) +
  geom_text(aes(label = Accuracy), vjust = 1.5) +
  labs(x = "Model",
       y = "Accuracy, %",
       title = "Model Accuracy in Predicting the Test Dataset") 
```

![](crop_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

NaiveBayer is ranked the best model in predicting the type of crops.

## 6.5 Confusion Matrix

Picking the best model, Naive Bayers classifier, to be evaluated in
terms of sensitivity and specificity. This model will be used for
production in the next section if these matrices are good.

*Naive Bayers Classifier*

This model has both sensitivity and specificity above 0.9 for all crops,
which is excellent.

``` r
# Confusion Matrix (predicted classes, observed classes)

nb_cm <- confusionMatrix(prediction_nb, test.data$label)
nb_cm_metrics <- nb_cm$byClass

# cleaning

df6.5 <- nb_cm_metrics %>% 
  data.frame(nb_cm_metrics) %>% 
  mutate(class = row.names(nb_cm_metrics),
         class = str_replace_all(class, "Class: ", " ")) %>% 
  relocate(class, .before = Sensitivity) %>% 
  mutate(class = factor(class))

row.names(df6.5) <- NULL

# manipulation

df6.5 <- df6.5 %>% 
  pivot_longer(c("Sensitivity", "Specificity"),
               names_to = "metrics",
               values_to = "Results")

# plot

ggplot(df6.5, aes(y = Results, x = metrics, colour = class)) +
  geom_jitter(size = 4, alpha = 0.5) +
  scale_y_continuous(lim = c(0, 1), breaks = seq(0, 1, 0.2)) +
  facet_wrap(~metrics, scale = "free_x") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 12),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = " ",
       title = "ML Model: Naive Bayers Classifier")
```

    ## Warning: Removed 22 rows containing missing values (geom_point).

![](crop_files/figure-gfm/unnamed-chunk-36-1.png)<!-- --> Therefore, in
terms of sensitivity and specificity, this model is considered safe to
be used for production.

# 8 MODEL FOR PRODUCTION

This section will put the best models, **Naive Bayers classifier**, into
production by creating a user API app to aid easy prediction of any
given values.

Suppose that I received two fictional requests from 2 clients in two
different farms, they are both asking what best to crop on their land,
and I have been given their environmental conditions:

``` r
# new data

Variables <- c("N", "P", "K", "temperature", "humidity", "pH", "rainfall")
Client_Jenny <- c(100, 50, 50, 30, 80, 6, 100)
Client_Mike <- c(90, 20, 40, 20, 80, 5, 80)

# df

data.frame(Variables, Client_Jenny, Client_Mike) %>% 
  kbl(align = "c",
      table.attr = "style = 'width:40%;'"
      ) %>% 
  kable_styling(bootstrap_options = c("hover", "stripped", "bordered"))
```

<table style="width:40%; margin-left: auto; margin-right: auto;" class="table table-hover table-bordered">
<thead>
<tr>
<th style="text-align:center;">
Variables
</th>
<th style="text-align:center;">
Client\_Jenny
</th>
<th style="text-align:center;">
Client\_Mike
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
N
</td>
<td style="text-align:center;">
100
</td>
<td style="text-align:center;">
90
</td>
</tr>
<tr>
<td style="text-align:center;">
P
</td>
<td style="text-align:center;">
50
</td>
<td style="text-align:center;">
20
</td>
</tr>
<tr>
<td style="text-align:center;">
K
</td>
<td style="text-align:center;">
50
</td>
<td style="text-align:center;">
40
</td>
</tr>
<tr>
<td style="text-align:center;">
temperature
</td>
<td style="text-align:center;">
30
</td>
<td style="text-align:center;">
20
</td>
</tr>
<tr>
<td style="text-align:center;">
humidity
</td>
<td style="text-align:center;">
80
</td>
<td style="text-align:center;">
80
</td>
</tr>
<tr>
<td style="text-align:center;">
pH
</td>
<td style="text-align:center;">
6
</td>
<td style="text-align:center;">
5
</td>
</tr>
<tr>
<td style="text-align:center;">
rainfall
</td>
<td style="text-align:center;">
100
</td>
<td style="text-align:center;">
80
</td>
</tr>
</tbody>
</table>

There are 3 different ways to put this prediction into work.

## 8.1 Direct Model Prediction

It is a wonderful traditional way, though it is a bit tedious. This
method works best for predicting large request (large dataset).

Following codes convert clients request into data frames.

``` r
# convert client data into data frame

Jenny <- data.frame(N = 100, 
                           P = 50,
                           K = 50,
                           temperature = 30,
                           humidity = 80,
                           ph = 6, 
                           rainfall = 100)


Mike <- data.frame(N = 90, 
                          P = 20,
                          K = 40,
                          temperature = 20,
                          humidity = 80,
                          ph = 5, 
                          rainfall = 80)
```

-   Naive Bayes Classifier recommends **Banana** to be the best crop to
    grow for Jenny.

``` r
# Prediction by Naive Bayes Classifier (nb) 

model_nb %>% predict(Jenny)
```

    ## [1] banana
    ## 22 Levels: apple banana blackgram chickpea coconut coffee cotton ... watermelon

-   Naive Bayes Classifier recommends **rice** to be the best crop to
    grow for Mike.

``` r
# Prediction by Naive Bayes Classifier (nb) and Boosted Random Forest (brf)

model_nb %>% predict(Mike) 
```

    ## [1] rice
    ## 22 Levels: apple banana blackgram chickpea coconut coffee cotton ... watermelon

## 8.2 Functionise the model

Alternatively I can build a Naive Bayes Classifier function for this
crop classification project.

``` r
# Create the function of naive bayes

crop_nb <- function(N, P, K, temperature, humidity, ph, rainfall){
  
  to_predict <- data_frame(N = as.numeric(N),
                           P = as.numeric(P),
                           K = as.numeric(K),
                           temperature = as.numeric(temperature),
                           humidity = as.numeric(humidity),
                           ph = as.numeric(ph),
                           rainfall = as.numeric(rainfall))
  
  model_nb %>% predict(to_predict)
  
}
```

-   Making the prediction for Jenny using the *crop\_nb* function.

``` r
Jenny
```

    ##     N  P  K temperature humidity ph rainfall
    ## 1 100 50 50          30       80  6      100

``` r
crop_nb(100, 50, 50, 30, 80, 6, 100)
```

    ## [1] banana
    ## 22 Levels: apple banana blackgram chickpea coconut coffee cotton ... watermelon

-   Making the prediction for Mike using the *crop\_nb* function.

``` r
Mike
```

    ##    N  P  K temperature humidity ph rainfall
    ## 1 90 20 40          20       80  5       80

``` r
crop_nb(90, 20, 40, 20, 80, 5, 80)
```

    ## [1] rice
    ## 22 Levels: apple banana blackgram chickpea coconut coffee cotton ... watermelon

## 8.3 API App with Plumber

This app allows me to freely make crop recommendation for any request
without much coding works. Find the codes of this app in my github repo
with the R file name - **plumber.R**

**Prediction for Jenny**

``` r
Jenny
```

    ##     N  P  K temperature humidity ph rainfall
    ## 1 100 50 50          30       80  6      100

![](C:\Users\karho\Desktop\R\github\crop\pic1_api1.JPG)

![](C:\Users\karho\Desktop\R\github\crop\pic1_api2.JPG)

-   Banana is recommended the best crop for Jenny.

**Prediction for Mike**

``` r
Mike
```

    ##    N  P  K temperature humidity ph rainfall
    ## 1 90 20 40          20       80  5       80

![](C:\Users\karho\Desktop\R\github\crop\pic3_api3.JPG)

![](C:\Users\karho\Desktop\R\github\crop\pic3_api4.JPG)

``` r
test.data
```

    ##        N   P   K temperature humidity  ph rainfall       label
    ## 1     90  42  43        20.9     82.0 6.5    202.9        rice
    ## 21    89  45  36        21.3     80.5 6.4    185.5        rice
    ## 22    76  40  43        25.2     83.1 5.1    231.4        rice
    ## 27    97  59  43        26.4     84.0 6.3    271.4        rice
    ## 31    73  57  41        21.4     84.9 5.8    272.2        rice
    ## 34    98  53  38        20.3     81.6 5.0    270.4        rice
    ## 38    95  39  36        23.9     83.2 5.6    285.2        rice
    ## 39    60  43  44        21.0     83.0 7.4    298.4        rice
    ## 52    76  60  39        20.0     80.3 6.8    208.6        rice
    ## 53    93  56  42        23.9     82.2 7.4    195.1        rice
    ## 58    91  36  45        24.4     82.5 6.0    268.0        rice
    ## 59    71  46  40        20.3     82.1 7.2    192.0        rice
    ## 62    83  58  45        25.8     83.5 5.9    245.7        rice
    ## 69    69  46  41        23.6     80.3 5.0    263.1        rice
    ## 77    67  47  44        26.7     81.8 7.9    280.4        rice
    ## 83    61  53  43        26.4     81.1 6.3    223.4        rice
    ## 86    66  60  38        22.1     83.5 6.4    231.7        rice
    ## 88    84  50  44        25.5     81.4 5.9    182.7        rice
    ## 94    78  40  38        26.5     83.9 7.5    248.2        rice
    ## 99    78  35  44        26.5     84.7 7.1    183.6        rice
    ## 105   61  38  20        18.5     62.7 6.0     65.4       maize
    ## 108   89  60  19        25.2     66.7 5.9     78.1       maize
    ## 109   76  44  17        20.4     62.6 5.9     65.3       maize
    ## 117   70  47  17        24.6     70.4 6.6    104.2       maize
    ## 120   74  55  19        18.1     62.9 6.3     84.2       maize
    ## 122   99  50  15        18.1     71.1 5.6     88.1       maize
    ## 131   87  35  25        21.4     63.2 6.2     65.9       maize
    ## 136   98  44  21        25.8     74.1 6.5    107.5       maize
    ## 140   76  57  18        19.0     74.5 6.1     94.3       maize
    ## 153   87  60  23        20.3     63.9 6.4     62.5       maize
    ## 155   94  54  17        23.4     61.7 5.9    107.3       maize
    ## 159   66  44  20        19.1     69.0 6.7     80.7       maize
    ## 162   72  60  25        18.5     69.0 5.8     88.1       maize
    ## 166   75  53  18        20.7     59.4 6.9    103.7       maize
    ## 177   74  39  23        22.6     65.8 6.8     88.2       maize
    ## 178   81  49  20        18.0     60.6 5.5    104.2       maize
    ## 182   68  40  19        26.1     66.2 6.7    107.2       maize
    ## 183   60  57  24        18.7     61.6 6.1     75.0       maize
    ## 190   92  60  23        18.7     71.5 5.7     69.9       maize
    ## 199   83  58  23        19.7     59.7 6.4     65.5       maize
    ## 201   40  72  77        17.0     17.0 7.5     88.6    chickpea
    ## 204   22  72  85        18.9     15.7 6.4     88.5    chickpea
    ## 207   58  70  84        20.7     16.6 6.2     74.7    chickpea
    ## 210   28  74  81        18.0     18.3 8.8     82.0    chickpea
    ## 211   58  66  79        21.0     19.3 8.7     93.6    chickpea
    ## 214   23  62  85        19.0     19.5 8.5     80.7    chickpea
    ## 220   31  70  77        20.9     14.3 6.5     90.5    chickpea
    ## 226   22  67  78        17.2     14.4 6.2     72.3    chickpea
    ## 243   36  76  75        18.4     16.6 8.7     70.5    chickpea
    ## 244   57  68  81        17.2     17.3 8.1     72.8    chickpea
    ## 250   52  68  78        17.5     17.0 6.9     86.1    chickpea
    ## 251   43  79  79        19.4     19.0 7.8     80.3    chickpea
    ## 252   44  74  85        20.2     19.6 7.2     78.3    chickpea
    ## 262   51  72  75        18.9     15.0 7.1     80.1    chickpea
    ## 268   41  69  82        20.0     16.6 6.7     69.0    chickpea
    ## 272   34  71  79        17.9     15.9 7.7     74.6    chickpea
    ## 274   30  70  79        20.3     20.0 7.3     69.6    chickpea
    ## 290   22  60  85        18.8     14.7 7.8     94.8    chickpea
    ## 298   36  56  83        18.9     19.8 7.5     69.1    chickpea
    ## 300   49  69  82        18.3     15.4 7.3     81.8    chickpea
    ## 304   40  64  16        16.4     24.2 5.9    140.4 kidneybeans
    ## 311   28  58  24        19.7     18.3 5.7    143.8 kidneybeans
    ## 316    6  77  25        20.6     24.4 5.8     69.6 kidneybeans
    ## 324   37  64  22        17.5     18.8 6.0    121.9 kidneybeans
    ## 325   11  71  17        19.9     21.5 5.7     82.7 kidneybeans
    ## 326   18  79  20        20.3     23.2 5.9    139.8 kidneybeans
    ## 328   24  80  22        16.7     19.2 5.6     96.8 kidneybeans
    ## 330   16  75  21        18.5     23.6 5.7     87.1 kidneybeans
    ## 337   23  59  19        22.0     24.9 5.9    129.6 kidneybeans
    ## 341    8  72  17        20.6     19.8 5.7     87.9 kidneybeans
    ## 347    0  65  15        23.5     23.2 5.6     95.8 kidneybeans
    ## 348   13  72  21        24.3     21.0 5.8     60.3 kidneybeans
    ## 352    3  67  24        17.0     19.9 5.5    103.3 kidneybeans
    ## 356   22  71  17        18.2     19.4 5.5    107.7 kidneybeans
    ## 357   31  79  25        23.2     22.3 5.9     63.4 kidneybeans
    ## 358   34  59  18        23.4     22.0 5.7     87.7 kidneybeans
    ## 362   24  67  22        20.1     22.9 5.6    104.6 kidneybeans
    ## 364   37  74  15        24.9     18.2 5.6     62.7 kidneybeans
    ## 368   37  65  16        22.8     19.0 5.7     63.6 kidneybeans
    ## 396   27  65  18        20.1     23.2 5.6     73.4 kidneybeans
    ## 404   27  57  24        27.3     43.4 6.1    142.3  pigeonpeas
    ## 406   30  75  25        30.3     42.4 6.4    149.3  pigeonpeas
    ## 408   38  55  19        33.2     38.2 5.9    198.8  pigeonpeas
    ## 414   16  80  20        31.2     56.7 7.3    122.0  pigeonpeas
    ## 415   27  72  17        29.0     57.2 6.3    120.7  pigeonpeas
    ## 416   40  62  19        27.3     34.1 4.7     96.5  pigeonpeas
    ## 419   26  67  24        37.0     37.7 5.6    161.5  pigeonpeas
    ## 420   16  70  20        24.8     40.1 5.6    121.6  pigeonpeas
    ## 424   28  75  21        24.8     50.5 6.0    114.3  pigeonpeas
    ## 426   24  70  21        19.1     45.4 5.5    132.8  pigeonpeas
    ## 441    6  66  15        34.9     30.4 6.3    159.3  pigeonpeas
    ## 443    2  67  18        34.5     47.5 5.9    129.0  pigeonpeas
    ## 445   16  73  19        18.4     34.8 4.7    163.3  pigeonpeas
    ## 447   32  70  20        20.9     46.2 6.2    195.6  pigeonpeas
    ## 448   28  59  22        30.9     52.8 7.1    171.0  pigeonpeas
    ## 483    5  68  20        19.0     33.1 6.1    155.4  pigeonpeas
    ## 491   23  55  16        21.0     69.7 5.1    185.2  pigeonpeas
    ## 492    4  69  19        19.3     47.7 5.4    149.1  pigeonpeas
    ## 496   35  71  17        29.9     66.4 6.9    198.1  pigeonpeas
    ## 499   10  71  18        19.5     66.3 6.2    173.1  pigeonpeas
    ## 511   40  49  17        31.0     45.9 6.7     53.6   mothbeans
    ## 520   17  58  25        31.1     43.6 6.5     32.8   mothbeans
    ## 521   11  44  17        26.3     55.6 8.0     35.1   mothbeans
    ## 525   26  50  19        27.3     51.7 6.0     32.6   mothbeans
    ## 527    8  60  18        31.2     46.0 3.8     53.1   mothbeans
    ## 534    7  56  23        26.3     40.0 5.5     55.5   mothbeans
    ## 547   25  51  18        27.8     54.8 9.5     50.3   mothbeans
    ## 550    9  60  23        32.0     57.2 6.3     64.3   mothbeans
    ## 553   12  39  21        29.0     62.9 8.2     70.5   mothbeans
    ## 555   32  41  16        28.6     61.4 7.7     68.5   mothbeans
    ## 562   35  51  17        28.8     49.8 3.6     40.9   mothbeans
    ## 566   11  41  19        26.9     41.8 5.1     44.1   mothbeans
    ## 570   40  45  20        29.4     57.7 6.9     38.3   mothbeans
    ## 571   23  58  19        24.2     58.3 5.2     59.2   mothbeans
    ## 575   39  42  20        29.3     61.3 8.1     40.8   mothbeans
    ## 582   34  54  24        31.2     41.6 5.0     68.8   mothbeans
    ## 584   29  41  21        31.5     62.8 8.9     64.6   mothbeans
    ## 594   18  36  23        24.0     53.8 7.2     35.0   mothbeans
    ## 597   22  51  16        28.0     61.3 8.6     70.1   mothbeans
    ## 598   33  47  17        24.9     48.3 8.6     63.9   mothbeans
    ## 601   19  55  20        27.4     87.8 7.2     54.7    mungbean
    ## 607   34  59  23        28.6     83.2 6.9     56.5    mungbean
    ## 608   31  51  25        27.5     85.6 7.2     53.0    mungbean
    ## 610   21  39  20        28.1     82.1 7.1     46.8    mungbean
    ## 616   34  60  25        29.8     85.2 6.8     40.8    mungbean
    ## 621   13  47  20        29.2     87.9 6.5     43.1    mungbean
    ## 623   28  45  23        29.7     80.3 6.5     56.8    mungbean
    ## 626   34  45  21        28.2     82.6 6.3     37.0    mungbean
    ## 646    2  39  15        28.1     82.9 6.5     49.6    mungbean
    ## 657    2  38  18        27.5     89.9 6.6     45.5    mungbean
    ## 659   37  49  25        29.9     85.9 6.4     41.4    mungbean
    ## 661   40  58  15        29.5     87.6 7.0     43.2    mungbean
    ## 668    4  41  20        28.1     83.8 6.6     37.4    mungbean
    ## 673   34  35  21        28.4     82.7 6.7     58.2    mungbean
    ## 676    5  45  21        28.4     88.0 6.5     43.1    mungbean
    ## 678   40  51  17        28.7     86.1 6.9     50.0    mungbean
    ## 683    6  47  18        29.2     80.3 6.7     40.2    mungbean
    ## 691   26  54  17        28.5     89.0 6.3     49.5    mungbean
    ## 698   20  45  17        28.2     83.7 6.8     37.2    mungbean
    ## 700   25  48  21        28.4     83.5 6.3     52.6    mungbean
    ## 708   30  64  20        33.9     61.6 6.6     68.0   blackgram
    ## 712   57  67  25        32.3     66.6 7.6     64.6   blackgram
    ## 713   52  63  19        29.6     68.3 6.9     67.5   blackgram
    ## 715   51  56  18        28.1     64.2 6.7     70.9   blackgram
    ## 718   50  58  23        27.8     62.5 7.6     69.8   blackgram
    ## 719   30  65  25        32.9     64.6 7.7     71.5   blackgram
    ## 722   25  71  24        28.5     60.4 7.2     74.9   blackgram
    ## 728   48  62  15        25.4     66.6 7.5     65.8   blackgram
    ## 734   53  67  17        31.8     69.0 7.3     61.5   blackgram
    ## 738   57  60  17        26.2     67.9 7.5     73.6   blackgram
    ## 739   56  75  15        30.2     60.1 7.2     66.4   blackgram
    ## 755   22  55  20        34.0     70.0 7.4     61.2   blackgram
    ## 756   20  68  17        30.1     60.1 6.6     71.7   blackgram
    ## 764   35  64  15        28.5     63.5 6.5     69.5   blackgram
    ## 766   58  75  25        25.3     61.4 7.3     68.6   blackgram
    ## 780   47  63  16        27.4     67.1 6.7     72.5   blackgram
    ## 785   27  62  24        28.6     66.8 7.4     62.3   blackgram
    ## 790   60  59  22        31.9     66.7 7.2     74.2   blackgram
    ## 791   33  77  21        30.3     65.6 7.0     71.6   blackgram
    ## 794   59  63  18        31.7     60.1 6.5     66.7   blackgram
    ## 801   32  76  15        28.1     63.5 7.6     43.4      lentil
    ## 804   11  74  17        21.4     69.9 6.6     46.6      lentil
    ## 806   29  71  18        22.2     62.1 6.4     53.5      lentil
    ## 807    2  72  18        26.6     61.0 7.8     50.9      lentil
    ## 815   39  78  15        21.4     62.6 5.9     41.8      lentil
    ## 819   22  67  22        29.0     64.5 7.5     54.9      lentil
    ## 827   32  56  18        20.0     65.8 7.1     46.1      lentil
    ## 834   10  78  18        18.5     62.7 6.3     44.1      lentil
    ## 841    5  65  19        18.3     68.1 7.0     48.8      lentil
    ## 845   22  55  16        23.8     68.0 6.5     49.7      lentil
    ## 849   27  61  15        25.3     67.1 7.0     48.3      lentil
    ## 851   13  74  25        24.1     61.1 6.5     44.2      lentil
    ## 852    6  64  23        23.3     67.4 7.1     36.2      lentil
    ## 853   12  58  23        21.7     63.4 6.8     50.4      lentil
    ## 857   38  77  22        28.2     69.3 6.3     35.4      lentil
    ## 861   32  78  22        24.0     62.4 7.0     53.4      lentil
    ## 883   14  74  15        28.0     65.6 6.5     49.9      lentil
    ## 892    0  67  22        29.8     69.4 6.6     51.6      lentil
    ## 893    7  73  25        27.5     63.1 7.3     45.2      lentil
    ## 898    4  59  19        26.3     67.6 7.6     40.8      lentil
    ## 904   37  18  39        24.1     94.5 6.4    110.2 pomegranate
    ## 909   25  27  41        19.2     94.3 6.9    108.0 pomegranate
    ## 911   14   5  36        24.9     85.2 5.8    104.8 pomegranate
    ## 915   29  22  40        23.6     89.7 6.1    107.7 pomegranate
    ## 918   11  18  42        21.6     94.9 5.9    102.9 pomegranate
    ## 925   12  29  40        19.7     89.8 6.6    111.3 pomegranate
    ## 932   34  21  42        18.8     89.9 6.6    111.0 pomegranate
    ## 934   25  17  40        18.9     87.7 6.6    111.3 pomegranate
    ## 935    8  25  36        19.9     95.0 6.8    104.0 pomegranate
    ## 945   37  11  36        24.2     85.6 6.7    106.9 pomegranate
    ## 952   29  21  45        23.4     93.1 6.7    105.2 pomegranate
    ## 958   12  20  39        19.9     86.2 6.0    111.0 pomegranate
    ## 965   40  11  44        24.5     86.1 6.3    111.4 pomegranate
    ## 966   21   9  40        24.5     90.6 6.0    105.6 pomegranate
    ## 972   32  14  37        22.7     88.5 6.8    104.7 pomegranate
    ## 984   18   9  40        19.4     89.0 5.6    106.2 pomegranate
    ## 988   11  10  45        22.6     88.5 6.4    109.0 pomegranate
    ## 990    3  26  39        24.4     91.2 7.1    103.6 pomegranate
    ## 994   40  30  35        20.9     91.1 6.3    104.4 pomegranate
    ## 997    4  14  41        19.9     89.8 6.4    102.8 pomegranate
    ## 1006  93  94  53        25.9     84.4 6.1    114.5      banana
    ## 1015  86  95  49        28.1     78.0 6.5    108.4      banana
    ## 1018 109  79  45        27.7     79.7 6.5    108.7      banana
    ## 1028 117  76  47        25.6     77.4 6.1     93.1      banana
    ## 1033 105  74  45        25.1     81.4 6.1    119.2      banana
    ## 1038 119  72  55        26.0     83.3 6.2    112.1      banana
    ## 1041  80  90  47        26.6     79.4 6.2    107.4      banana
    ## 1045 109  91  53        29.7     83.5 6.0    110.3      banana
    ## 1047 106  70  55        25.9     78.5 5.7    116.3      banana
    ## 1052 116  81  55        26.4     83.7 5.9     95.1      banana
    ## 1053 101  75  50        26.6     81.4 6.2    110.0      banana
    ## 1058 102  73  54        26.4     84.4 5.7    111.0      banana
    ## 1059  86  79  45        27.8     82.7 5.8     99.2      banana
    ## 1082  82  77  46        28.9     82.2 5.9     95.8      banana
    ## 1084  95  88  52        28.0     78.9 6.2     94.7      banana
    ## 1086 102  73  52        27.9     83.4 6.4     90.2      banana
    ## 1092  96  86  51        29.9     77.0 6.3     92.0      banana
    ## 1096 108  94  47        27.4     84.5 6.4     90.8      banana
    ## 1098 110  71  54        28.7     82.2 5.7     94.4      banana
    ## 1099  82  75  55        27.3     78.5 6.3     92.2      banana
    ## 1103  21  26  27        27.0     47.7 5.7     95.9       mango
    ## 1106  20  19  35        34.2     50.6 6.1     98.0       mango
    ## 1110  30  28  30        31.9     52.2 5.1     98.5       mango
    ## 1118  26  37  30        35.4     49.5 6.2     97.4       mango
    ## 1127  38  19  31        34.7     49.1 5.9     90.7       mango
    ## 1133  12  31  26        35.8     51.9 5.4    100.2       mango
    ## 1138  35  18  26        32.0     50.8 5.3     97.4       mango
    ## 1139   4  40  26        27.6     48.6 6.7     95.8       mango
    ## 1140   9  29  34        29.4     45.9 5.7    100.8       mango
    ## 1142  26  32  32        30.9     49.9 6.8     90.1       mango
    ## 1147  36  19  32        27.1     50.7 4.9     92.4       mango
    ## 1150   5  19  25        27.4     54.4 6.4     96.3       mango
    ## 1151  37  36  26        32.9     52.6 4.7     94.5       mango
    ## 1152  21  31  32        35.4     51.4 5.3     90.3       mango
    ## 1153  37  36  27        27.6     47.9 5.9     90.4       mango
    ## 1159  11  34  32        29.1     49.4 6.8     97.6       mango
    ## 1167   3  18  31        31.7     48.2 6.4     91.1       mango
    ## 1191   7  31  27        31.3     47.6 6.5     94.7       mango
    ## 1192  29  34  26        33.9     54.4 6.3     89.3       mango
    ## 1198  22  18  33        30.4     52.5 6.6     93.9       mango
    ## 1203  22 123 205        32.4     83.9 5.9     68.7      grapes
    ## 1216  22 138 195        27.8     83.5 6.2     73.0      grapes
    ## 1217  31 144 202        11.0     80.6 5.9     68.2      grapes
    ## 1232   7 126 203        16.8     82.0 5.7     73.3      grapes
    ## 1238  38 138 204        25.1     83.3 6.3     73.0      grapes
    ## 1247   4 134 200        28.6     81.0 5.8     73.3      grapes
    ## 1256  40 140 195        15.0     80.5 6.3     71.6      grapes
    ## 1258  19 120 195        18.7     81.1 5.9     73.6      grapes
    ## 1259  21 139 201        19.4     83.4 6.0     67.2      grapes
    ## 1261  33 139 203        33.3     82.5 5.7     70.7      grapes
    ## 1264  37 135 205        11.8     80.3 5.5     74.1      grapes
    ## 1269  29 142 203        29.7     83.7 5.9     66.5      grapes
    ## 1272   8 120 196        24.1     82.7 6.1     69.8      grapes
    ## 1286   6 128 200        26.0     82.6 5.8     70.3      grapes
    ## 1290  38 120 197        17.5     82.9 6.3     73.8      grapes
    ## 1292  14 121 203         9.7     83.7 6.2     74.5      grapes
    ## 1293   6 125 204        27.9     82.9 5.7     69.9      grapes
    ## 1294  32 138 197         9.5     80.7 5.9     69.4      grapes
    ## 1298   6 142 202        27.2     82.9 6.2     70.4      grapes
    ## 1300  35 134 204         9.9     82.6 5.8     66.0      grapes
    ## 1303 105  30  50        25.3     81.8 6.4     57.0  watermelon
    ## 1309  82  22  45        26.2     85.3 6.5     54.6  watermelon
    ## 1320 103  17  51        25.1     80.0 6.2     44.2  watermelon
    ## 1327  91  21  50        24.3     81.4 6.8     48.3  watermelon
    ## 1330  88  29  51        24.7     88.9 6.1     48.5  watermelon
    ## 1331 118  15  45        24.2     84.2 6.5     48.0  watermelon
    ## 1340  80  16  46        25.5     81.4 6.9     48.5  watermelon
    ## 1343  86   6  53        25.9     83.5 6.9     42.1  watermelon
    ## 1345 103  16  49        24.1     81.6 6.9     51.8  watermelon
    ## 1347  85  25  47        26.1     87.6 6.3     58.5  watermelon
    ## 1354  82  23  49        26.8     87.2 6.9     51.7  watermelon
    ## 1356 110  28  46        24.3     88.0 6.5     51.3  watermelon
    ## 1361 101  13  54        25.4     82.9 6.8     56.3  watermelon
    ## 1368  91  12  46        24.6     85.5 6.3     48.3  watermelon
    ## 1370 113  19  46        25.4     81.1 6.3     49.5  watermelon
    ## 1378  81  18  50        26.4     80.9 6.5     47.8  watermelon
    ## 1380 108  23  51        26.8     83.9 6.1     40.2  watermelon
    ## 1385  99   6  45        26.1     86.6 6.0     40.7  watermelon
    ## 1389 110  21  54        26.7     87.8 6.7     47.5  watermelon
    ## 1392 100  10  53        24.5     84.6 6.2     42.0  watermelon
    ## 1406  81  25  49        29.9     93.3 6.1     26.3   muskmelon
    ## 1422 100   6  53        29.1     93.9 6.1     23.7   muskmelon
    ## 1430 116  25  50        29.3     92.9 6.1     28.7   muskmelon
    ## 1438 120   8  46        29.6     90.7 6.7     28.4   muskmelon
    ## 1440 108  22  47        28.5     91.7 6.2     25.1   muskmelon
    ## 1441  82  13  52        27.1     94.9 6.4     26.5   muskmelon
    ## 1445 106  10  49        27.7     92.0 6.4     20.2   muskmelon
    ## 1446  99  12  52        28.7     94.3 6.0     22.2   muskmelon
    ## 1448  83  11  53        29.5     92.9 6.2     22.0   muskmelon
    ## 1451 113  20  48        27.5     94.9 6.4     27.3   muskmelon
    ## 1452 101  17  47        29.5     94.7 6.2     26.3   muskmelon
    ## 1453  98   7  45        27.8     92.5 6.2     26.9   muskmelon
    ## 1456 109  12  48        29.5     92.1 6.7     20.8   muskmelon
    ## 1459  89   9  47        29.5     90.8 6.7     28.8   muskmelon
    ## 1463  93  20  50        29.9     93.2 6.4     24.3   muskmelon
    ## 1466 111   5  52        29.9     94.0 6.1     21.0   muskmelon
    ## 1474  94   5  55        28.6     91.9 6.1     26.9   muskmelon
    ## 1484  83  15  49        28.9     91.4 6.4     23.2   muskmelon
    ## 1487  85  21  47        29.9     90.6 6.2     24.7   muskmelon
    ## 1498 106  21  52        28.9     94.8 6.3     23.0   muskmelon
    ## 1501  24 128 196        22.8     90.7 5.5    110.4       apple
    ## 1502   7 144 197        23.8     94.3 6.1    114.1       apple
    ## 1510   1 124 199        23.7     93.3 5.7    112.7       apple
    ## 1511  30 122 197        21.4     92.7 5.6    106.1       apple
    ## 1524  34 140 198        21.7     93.4 5.8    115.2       apple
    ## 1528  25 143 198        22.8     91.5 6.0    107.9       apple
    ## 1535  24 142 202        22.5     91.5 5.7    101.8       apple
    ## 1537  18 125 204        22.4     94.5 6.0    116.7       apple
    ## 1544   5 144 205        21.4     92.6 6.2    102.8       apple
    ## 1545   2 123 205        22.4     90.8 5.7    125.0       apple
    ## 1548  25 143 200        23.8     92.8 6.0    100.6       apple
    ## 1552  20 139 202        23.5     92.2 5.7    108.0       apple
    ## 1555   2 131 199        22.5     91.2 6.0    124.2       apple
    ## 1556   2 140 197        22.7     92.8 5.5    105.1       apple
    ## 1559  32 145 203        23.8     90.8 6.4    109.6       apple
    ## 1568  27 120 200        21.5     90.7 6.1    116.7       apple
    ## 1574   7 141 195        23.9     93.5 5.5    104.9       apple
    ## 1581  33 143 204        21.1     92.0 5.8    122.5       apple
    ## 1588   1 135 203        22.8     92.7 5.6    113.8       apple
    ## 1598  31 137 196        22.1     93.8 6.4    120.6       apple
    ## 1602  37   6  13        26.0     91.5 7.5    101.3      orange
    ## 1607   5  23  15        25.7     92.0 7.4    112.5      orange
    ## 1608   0  18  14        29.8     92.0 7.2    114.4      orange
    ## 1622   0  25  14        19.3     92.0 6.4    116.5      orange
    ## 1623   8   7  10        28.3     92.0 6.9    105.2      orange
    ## 1624   4  23   5        22.7     93.4 7.5    110.3      orange
    ## 1639  10   5   5        21.2     91.4 7.8    113.0      orange
    ## 1641   1  30  10        11.9     91.3 7.3    103.6      orange
    ## 1644  36  11  13        17.3     93.0 7.2    112.7      orange
    ## 1653  18   5  11        20.9     90.9 6.3    102.5      orange
    ## 1654  14  22   9        17.2     91.1 6.5    112.5      orange
    ## 1661  31  25  12        18.1     90.0 7.0    111.8      orange
    ## 1663  12  29  13        22.5     91.5 7.6    118.0      orange
    ## 1671  31   5  14        17.7     91.7 6.6    110.7      orange
    ## 1674   5   8   5        11.0     92.2 6.6    112.8      orange
    ## 1685   7  17  10        10.2     91.2 6.5    106.4      orange
    ## 1688  20  23  11        31.9     90.1 6.4    109.9      orange
    ## 1694  34  10  14        34.1     92.1 6.7    116.8      orange
    ## 1696  27  30   5        32.7     90.5 7.7    113.3      orange
    ## 1698   6   7   7        27.7     94.5 7.2    114.0      orange
    ## 1702  58  46  45        42.4     90.8 6.6     88.5      papaya
    ## 1706  70  68  45        33.8     92.9 7.0    203.4      papaya
    ## 1728  36  59  46        34.3     93.6 6.7    127.3      papaya
    ## 1735  34  61  49        28.1     93.3 6.5    117.8      papaya
    ## 1740  50  47  48        24.6     90.6 6.7    218.2      papaya
    ## 1744  59  62  52        43.7     93.1 6.6    103.8      papaya
    ## 1745  60  58  51        42.1     92.9 6.8    165.7      papaya
    ## 1747  35  66  47        31.7     91.7 7.0     48.8      papaya
    ## 1750  39  64  52        28.9     94.6 6.7     63.7      papaya
    ## 1754  49  54  50        25.6     93.2 6.8     97.3      papaya
    ## 1755  40  65  49        35.3     91.1 6.7    163.9      papaya
    ## 1756  68  52  49        24.4     92.3 6.6     63.4      papaya
    ## 1761  49  55  51        24.9     93.9 6.7    135.2      papaya
    ## 1764  70  65  52        30.4     93.1 6.6     76.0      papaya
    ## 1771  69  67  52        27.7     94.4 6.8     82.8      papaya
    ## 1774  56  50  52        33.1     92.3 6.8     88.1      papaya
    ## 1775  70  50  53        37.5     90.4 6.9    172.3      papaya
    ## 1780  68  69  52        25.7     92.7 6.8     53.0      papaya
    ## 1783  48  62  47        25.3     93.0 6.8    174.4      papaya
    ## 1792  56  65  45        38.2     94.0 6.8    218.1      papaya
    ## 1801  18  30  29        26.8     92.9 6.4    224.6     coconut
    ## 1807  39   5  31        27.1     93.7 5.6    151.0     coconut
    ## 1814  37  18  30        27.6     99.3 6.4    157.9     coconut
    ## 1821  34   6  30        27.1     97.0 5.9    171.8     coconut
    ## 1825  10   9  28        29.0     94.0 6.3    150.1     coconut
    ## 1827  38  24  33        28.3     97.0 6.0    142.9     coconut
    ## 1832  32  11  31        25.1     93.3 6.2    134.8     coconut
    ## 1834   8   6  33        28.3     93.6 6.1    171.9     coconut
    ## 1835  23   6  33        29.2     92.7 6.0    205.0     coconut
    ## 1857  10  24  27        27.6     94.9 5.7    145.9     coconut
    ## 1859  32  11  31        29.5     92.6 6.5    131.2     coconut
    ## 1876  38   6  25        25.5     96.9 6.2    191.3     coconut
    ## 1878  40   5  32        26.1     96.7 6.0    143.5     coconut
    ## 1879   0  19  31        25.5     94.4 6.3    178.7     coconut
    ## 1880  26   9  32        25.9     94.7 6.5    144.2     coconut
    ## 1883  31  13  33        29.7     95.2 6.3    148.3     coconut
    ## 1886  30  13  25        27.2     91.5 6.4    164.9     coconut
    ## 1887   8  15  33        29.0     98.1 5.5    213.9     coconut
    ## 1891  27  10  33        27.8     97.5 6.5    154.1     coconut
    ## 1899  37   5  34        25.8     93.8 5.8    152.4     coconut
    ## 1905 126  38  23        25.4     83.6 6.2     88.4      cotton
    ## 1906 126  50  19        24.7     81.7 6.6     78.6      cotton
    ## 1907 113  41  20        25.0     80.5 7.3     96.3      cotton
    ## 1911 107  45  25        23.1     83.6 7.2     71.8      cotton
    ## 1913 140  38  15        24.1     75.9 6.0     69.9      cotton
    ## 1918 100  46  18        24.2     76.0 6.4     69.1      cotton
    ## 1919 123  39  24        25.0     78.2 7.5     86.1      cotton
    ## 1921 121  36  24        23.7     81.7 7.4     99.4      cotton
    ## 1962 107  43  18        22.4     81.5 6.7     65.5      cotton
    ## 1967 127  53  24        22.2     76.2 6.1     70.4      cotton
    ## 1968 134  52  18        24.0     76.6 8.0     76.1      cotton
    ## 1970 100  48  17        23.8     83.0 7.8     66.3      cotton
    ## 1973 111  39  22        22.6     80.4 6.1     88.6      cotton
    ## 1983 108  60  17        22.8     76.8 6.6     97.8      cotton
    ## 1984 116  56  17        24.7     77.7 8.0     85.2      cotton
    ## 1991 102  45  16        23.7     77.5 7.3     74.9      cotton
    ## 1992 131  56  20        22.0     81.8 7.8     92.2      cotton
    ## 1994 101  37  18        22.9     82.7 7.6     92.9      cotton
    ## 1996 113  38  20        22.1     78.6 6.4     74.9      cotton
    ## 1997 102  53  21        23.0     76.1 6.9     91.5      cotton
    ## 2001  89  47  38        25.5     72.2 6.0    151.9        jute
    ## 2012  74  46  45        25.8     88.4 6.0    189.4        jute
    ## 2013  89  41  38        23.1     74.7 6.3    199.8        jute
    ## 2014  60  55  40        25.0     89.0 7.0    151.5        jute
    ## 2016  70  38  35        24.4     79.3 7.0    164.3        jute
    ## 2019  82  35  44        27.0     78.2 6.2    169.8        jute
    ## 2027  81  40  45        25.8     80.8 6.4    174.5        jute
    ## 2030  82  40  45        26.2     81.7 6.7    180.1        jute
    ## 2049  84  40  42        26.3     73.4 6.7    186.7        jute
    ## 2065  89  58  35        24.0     82.1 6.1    167.1        jute
    ## 2069  62  59  41        24.2     74.9 7.2    192.5        jute
    ## 2070  82  35  35        25.5     87.0 7.3    176.5        jute
    ## 2071  61  41  35        25.0     79.5 6.8    195.8        jute
    ## 2073  70  42  43        23.2     76.7 6.5    157.1        jute
    ## 2080  64  53  38        26.2     78.5 6.9    183.4        jute
    ## 2081  65  54  39        23.8     71.1 7.1    160.1        jute
    ## 2086  87  48  38        23.8     80.9 7.2    190.3        jute
    ## 2091  80  43  43        23.8     74.4 6.0    172.6        jute
    ## 2093  95  57  41        23.2     73.7 6.4    184.8        jute
    ## 2097  88  52  39        23.9     88.1 6.9    154.7        jute
    ## 2103  83  38  35        25.7     52.9 7.2    136.7      coffee
    ## 2113  99  15  27        27.0     57.3 6.5    165.7      coffee
    ## 2126  91  25  26        24.5     67.0 7.5    180.5      coffee
    ## 2129 111  27  31        23.6     55.3 6.0    191.4      coffee
    ## 2133 103  27  31        27.2     51.6 6.7    126.2      coffee
    ## 2145  87  23  28        26.2     62.3 7.0    193.7      coffee
    ## 2150  82  29  35        26.7     52.2 6.2    156.2      coffee
    ## 2156 100  40  35        27.6     54.4 7.0    177.8      coffee
    ## 2159  87  28  30        25.6     68.7 6.5    168.8      coffee
    ## 2166  97  22  26        23.6     59.7 6.1    185.2      coffee
    ## 2169 112  39  29        26.1     63.4 6.7    147.8      coffee
    ## 2170 111  28  26        27.8     64.5 6.9    192.7      coffee
    ## 2174 119  30  28        26.4     64.6 6.5    163.6      coffee
    ## 2178 117  37  32        23.1     67.1 6.8    162.6      coffee
    ## 2185 104  35  28        27.5     50.7 7.0    144.0      coffee
    ## 2186 116  36  25        27.6     58.5 6.2    156.7      coffee
    ## 2187 107  38  29        26.7     57.6 6.4    145.1      coffee
    ## 2189 107  31  31        23.2     53.0 6.8    153.1      coffee
    ## 2190  99  16  30        23.5     65.4 6.4    186.2      coffee
    ## 2191 103  40  30        27.3     55.2 6.3    141.5      coffee

-   Rice is recommended the best crop for Mike.

# 9 CONCLUSION

In conclusion, 13 machine learning models were built, trained, and
compared to each other in term of accurcy. The best model was Naive
Bayers classifier in predicting the test dataset with an accuracy level
of 99.55%. Confusion matrix was used to examin the sensitivity and
specificity of this model, and both metrics were at an excellent levels
of above 90%.

This model was then used for recommending the suitable crop to grow for
Jenny and Mike. The Naive Bayes model recommends Banana for Jenny and
Rice for Mike.

# 10 CREDIT AND ACKNOWLEDGEMENT

Specially thanks for *Atharva Ingle* for providing this dataset for
Kaggle users to carry out machine learning analysis. The dataset website
is again at
[Kaggle.com](https://www.kaggle.com/atharvaingle/crop-recommendation-dataset).
