# Load packages

library(plumber)
library(tidyverse)
library(caret)

# Load model 

crop_model <- read_rds("model_nb.rds")

#---------------API-----------------#

#* @apiTitle Crop Recommendation By Machine Learning Model 

#* @apiDescription This API serves up predictions of the most appropriate type of crop based on your decision in a range of environmental parameters. The recommendation was achieved by Naive Bayes Model with a 99.5% accuracy. Recommendable crops: apple, banana, blackgram, chickpea, coconut, coffee, cotton, grapes, jute, kidneybeans, lentil, maize, mango, mothbeans，mungbean, muskmelon, orange, papaya, pigeonpeas, pomegranate, rice, watermelon

#--------------Body1-----------------#

#* @get /health-check
#* Health Check - is the API running

status <- function(){
  list(
    status = "All Good",
    time = Sys.time()
  )
}

#--------------Body2-----------------#

#* @get /Crop_prediction_NB

#* @param N Specify N (Nitrogen) in the ratio of N-P-K. e.g. 0, 20, 50, 100, 150
#* @param P Specify P (Phosphorus) in the ratio of N-P-K. e.g. 0, 20, 50, 100, 150
#* @param K Specify K (Potassium) in the ratio of N-P-K. e.g. 0, 20, 50, 100, 150
#* @param temperature degree Celsius
#* @param humidity limited between 0 - 100
#* @param ph pH  
#* @param rainfall Specify the rainfall in the unit of mm

function(N, P, K, temperature, humidity, ph, rainfall){
  
  to_predict <- data_frame(N = as.numeric(N),
                           P = as.numeric(P),
                           K = as.numeric(K),
                           temperature = as.numeric(temperature),
                           humidity = as.numeric(humidity),
                           ph = as.numeric(ph),
                           rainfall = as.numeric(rainfall))
  
  crop_model %>% predict(to_predict)
  
}


 

