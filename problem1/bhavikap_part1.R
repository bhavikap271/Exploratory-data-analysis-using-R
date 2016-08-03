#Please set the working directory to save the json file in the directory you want
setwd("C:/Users/Bhavika/Courses/CSE587/Project1/Problem1")

library(twitteR)
library(jsonlite)
library(ROAuth)

api_key <- "LnqMq9ZKYIkxuFXintSrHIY8S"
api_secret <- "LrfZrP6wUSO454teTAsjy0LCwkbPJeslvnZva08K5fmLSdslUO"
token <- "700700424404271104-fDwkpRUgdvIkX2vJBW3lTaeKgRwBMBt"
token_secret <- "7jSqkGuUaP60uSq2fNXY1G3Rc1vziT55x0POjCUyVF5LY"

setup_twitter_oauth(api_key, api_secret, token, token_secret)

#search for a kewyword using the searchString variable.
tweets <- searchTwitter(searchString="election",n=500,lang="en")
s_tweets <- strip_retweets(tweets,strip_manual=TRUE,strip_mt=TRUE)

#converting the tweetList to dataframe.
df <- twListToDF(s_tweets)

#converting the data frame to Json object.
exportJson <- toJSON(df,pretty=TRUE)

# writing the json to a file.
write(exportJson,"election.json")


