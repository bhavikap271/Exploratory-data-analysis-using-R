# Please set the directory path below where json file is stored.

#setwd("C:/Users/Bhavika/Courses/CSE587/Project1/Problem1")

library(jsonlite)

json_file <- "election.json"
read_tweets <- jsonlite::fromJSON(paste(readLines(json_file), collapse=""))
head(read_tweets)
