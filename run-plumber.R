library(plumber)

pr <- plumb("plumber.R")

pr$run(host = "127.0.0.1", port = 5529)

# Run the api in "Jobs". We can now start to use the URL browser (copy the link in console)




