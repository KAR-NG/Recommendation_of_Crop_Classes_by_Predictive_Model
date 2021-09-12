library(httr)

base_url <- "http://127.0.0.1:5529/Crop_prediction"

param <- list(N = 50,
              P = 50,
              K = 50,
              temperature = 30,
              humidity = 60,
              ph = 7,
              rainfall = 300)


# create the query string

query_url <- modify_url(url = base_url,
                        query = param)

# Make request

respond <- GET(query_url)  # Status 200 meaning is good, >400 meaning error

respond_raw <- content(respond, as = "text", encoding = "utf-8")

# Extract it

jsonlite::fromJSON(respond_raw)

----

# Create a function
  
get_crop_class <- function(N, P, K, temperature, humidity, ph, rainfall){
  
  base_url <- "http://127.0.0.1:5529/Crop_prediction"
  
  param <- list(N = N,
                P = P,
                K = K,
                temperature = temperature,
                humidity = humidity,
                ph = ph,
                rainfall = rainfall)
  
  
  # create the query string
  
  query_url <- modify_url(url = base_url,
                          query = param)
  
  # Make request
  
  respond <- GET(query_url)  # Status 200 meaning is good, >400 meaning error
  
  respond_raw <- content(respond, as = "text", encoding = "utf-8")
  
  # Extract it
  
  jsonlite::fromJSON(respond_raw)
  
}

get_crop_class(5, 6, 3, 5, 6, 4, 6)


# Now we created an api wrapper. We can take this api and host it on something like RStudio connect, or your own docker image, or something like that. 

# Since it is using http requests, people from python, ruby, scala, java and whatever can call that api.

# Now you have built this infrastructure that you can use to take model and put into production environment.
