install.packages("ggmap")
install.packages("mapproj")
library(ggmap)
library(mapproj)
library(bitops)
library(twitteR)
library(RCurl)
library(stringr)
library(jsonlite)

api_key <- "4NqNSbw6EGR8gSO6jqXaTSTlf" # From dev.twitter.com
api_secret <- "l7xhWwIgmMR6S0dp2fK3Ix6f4upbyem3GHjjwDqRTr3iBK2aub" # From dev.twitter.com
token <- "71017487-Rt9MD6hyGrSyc55vum0ZgKM7fzBVkMSyMyUqL5SXa" # From dev.twitter.com
token_secret <- "whiXLH14ILleUNRKBd3V10iRoXLsr293znZZ4UpWheN8P" # From dev.twitter.com

setup_twitter_oauth(api_key,api_secret,token,token_secret)
tweets41 <- searchTwitter("apartment+rent", n=5000, lang="en")
tweets42 <- searchTwitter("apartment+buy", n=5000, lang="en")
tweets43 <- searchTwitter("apartment+sell", n=5000, lang="en")
tweets44 <- searchTwitter("apartment+sublease", n=5000, lang="en")
tweets45 <- searchTwitter("apartment+lease", n=5000, lang="en")
tweets46 <- searchTwitter("house+lease", n=5000, lang="en")
tweets47 <- searchTwitter("house+rent", n=5000, lang="en")
tweets48 <- searchTwitter("house+buy", n=5000, lang="en")
tweets49 <- searchTwitter("house+sell", n=5000, lang="en")
tweets410 <- searchTwitter("house+sublease", n=5000, lang="en")

df41 <-twListToDF(tweets41)
df42 <-twListToDF(tweets42)
df43 <-twListToDF(tweets43)
df44 <-twListToDF(tweets44)
df45 <-twListToDF(tweets45)
df46 <-twListToDF(tweets46)
df47 <-twListToDF(tweets47)
df49 <-twListToDF(tweets49)
frames <- rbind(df41,df42,df43,df44,df45,df46,df47)
t1 <- userTimeline('@RealtorsSD', n=500)
t2 <- userTimeline("@valeriegarcia1", n=500)
t3 <- userTimeline("@MIrealestate",n=500)
t4 <- userTimeline("@sellahouse21",n=500)
t5 <- userTimeline("@MsREAgentLA",n=500)
t6 <- userTimeline("@Agentopolis",n=500)
t8 <- userTimeline("@ludarosenbaum",n=500)
t9 <- userTimeline("@kwri",n=500)
t10 <- userTimeline("@RobertTravers",n=500)
t11 <- userTimeline("@BuzzBuzzHome",n=500)
t12 <- userTimeline("@emarks",n=500)
t13 <- userTimeline("@Lennar",n=500)
t14 <- userTimeline("@MortgageSpeak",n=500)
t15 <- userTimeline("@TheRobCampbell",n=500)
t16 <- userTimeline("@OREAinfo",n=500)
t17 <- userTimeline("@garyashton",n=500)
t18 <- userTimeline("@ChantalVignola",n=500)
t19 <- userTimeline("@Jackfusco",n=500)
t20 <- userTimeline("@SanazDesign",n=500)
t21 <- userTimeline("@roykohn",n=500)
t22 <- userTimeline("@NAR_Research",n=500)
tl <- c(t1,t2,t3,t4,t5,t6,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22)
p4df <- twListToDF(tl)
frames <- rbind(frames,p4df)

library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-90, 90), ylim = c(-90, 90), asp = 1)
points(frames$longitude, frames$latitude, col = "red", cex = .6)

for_rent=0
rent_df <- data.frame()
brooklyn_rent = 0
brooklyn_rent_text <- data.frame()

manhattan_rent = 0
manhattan_rent_text <- data.frame()

queens_rent = 0
queens_rent_text <- data.frame()

bronx_rent = 0
bronx_rent_text <- data.frame()

statenisland_rent = 0
statenisland_rent_text <- data.frame()

nyc_rent = 0
nyc_rent_text <- data.frame()

others_rent = 0
others_rent_text <- data.frame()

for (i in 1:nrow(frames)){
  if (grepl("rent",frames[i,]$text,ignore.case = TRUE) ){
    temp_df <- data.frame(as.character(frames[i,]$text),as.character(frames[i,]$screenName))
    for_rent = for_rent + 1
    if (grepl("brooklyn",frames[i,]$text,ignore.case = TRUE)){
      brooklyn_rent = brooklyn_rent + 1
      brooklyn_rent_text <- rbind(brooklyn_rent_text, temp_df)
    } 
    else if (grepl("manhattan",frames[i,]$text,ignore.case = TRUE)){
      manhattan_rent = manhattan_rent + 1
      manhattan_rent_text <- rbind(manhattan_rent_text,temp_df)
    } 
    else if (grepl("queens",frames[i,]$text,ignore.case = TRUE)){
      queens_rent = queens_rent + 1
      queens_rent_text <- rbind(queens_rent_text, temp_df)
    } 
    else if (grepl("bronx",frames[i,]$text,ignore.case = TRUE)){
      bronx_rent = bronx_rent + 1
      bronx_rent_text <- rbind(bronx_rent_text, temp_df)
    } 
    else if (grepl("statenisland",frames[i,]$text,ignore.case = TRUE)){
      statenisland_rent = statenisland_rent + 1
      statenisland_rent_text <- rbind(statenisland_rent_text,temp_df)
    } 
    
    else if (grepl("nyc",frames[i,]$text,ignore.case = TRUE) | grepl("#ny",frames[i,]$text,ignore.case = TRUE) | grepl("newyork",frames[i,]$text,ignore.case = TRUE)){
      nyc_rent_text <- rbind(nyc_rent_text, temp_df)
      nyc_rent = nyc_rent + 1
    } 
    else {
      others_rent_text <- rbind(others_rent_text, temp_df)
      others_rent = others_rent+1
    }
  } 
}

results = data.frame(tweets = c("Brooklyn", "Manhattan", "Queens", "Bronx","Statenisland","NYC"), numbers = c(brooklyn_rent,manhattan_rent,queens_rent,bronx_rent,statenisland_rent,nyc_rent))
barplot(results$numbers, names = results$tweets, xlab = "Burough", ylab = "NO of rental mentions", col = c("Green","Red","Blue","Yellow","Orange","Purple"))


results = data.frame(tweets = c("Brooklyn", "Manhattan", "Queens", "Bronx","Statenisland","NYC","Others"), numbers = c(brooklyn_rent,manhattan_rent,queens_rent,bronx_rent,statenisland_rent,nyc_rent,others_rent))
barplot(results$numbers, names = results$tweets, xlab = "Burough", ylab = "NO of rental mentions", col = c("Green","Red","Blue","Yellow","Orange","Purple","Blue"))




for_sale=0
sale_df <- data.frame()
brooklyn_sale = 0
brooklyn_sale_text <- data.frame()

manhattan_sale = 0
manhattan_sale_text <- data.frame()

queens_sale = 0
queens_sale_text <- data.frame()

bronx_sale = 0
bronx_sale_text <- data.frame()

statenisland_sale = 0
statenisland_sale_text <- data.frame()

nyc_sale = 0
nyc_sale_text <- data.frame()

others_sale = 0
others_sale_text <- data.frame()


for (i in 1:nrow(frames)){
  if ((grepl("House",frames[i,]$text,ignore.case = TRUE)  |grepl("apartment",frames[i,]$text,ignore.case = TRUE))&grepl( "sale",frames[i,]$text,ignore.case = TRUE)){
    temp_df <- data.frame(as.character(frames[i,]$text),as.character(frames[i,]$screenName))
    for_sale = for_sale + 1
    if (grepl("brooklyn",frames[i,]$text,ignore.case = TRUE)){
      brooklyn_sale = brooklyn_sale + 1
      brooklyn_sale_text <- rbind(brooklyn_sale_text, temp_df)
    } 
    else if (grepl("manhattan",frames[i,]$text,ignore.case = TRUE)){
      manhattan_sale = manhattan_sale + 1
      manhattan_sale_text <- rbind(manhattan_sale_text,temp_df)
    } 
    else if (grepl("queens",frames[i,]$text,ignore.case = TRUE)){
      queens_sale = queens_sale + 1
      queens_sale_text <- rbind(queens_sale_text, temp_df)
    } 
    else if (grepl("bronx",frames[i,]$text,ignore.case = TRUE)){
      bronx_sale = bronx_sale + 1
      bronx_sale_text <- rbind(bronx_sale_text, temp_df)
    } 
    else if (grepl("statenisland",frames[i,]$text,ignore.case = TRUE)){
      statenisland_sale = statenisland_sale + 1
      statenisland_sale_text <- rbind(statenisland_sale_text,temp_df)
    } 
    
    else if (grepl("nyc",frames[i,]$text,ignore.case = TRUE) | grepl("#ny",frames[i,]$text,ignore.case = TRUE) | grepl("newyork",frames[i,]$text,ignore.case = TRUE)){
      nyc_sale_text <- rbind(nyc_sale_text, temp_df)
      nyc_sale = nyc_sale + 1
    } 
    else {
      others_sale_text <- rbind(others_sale_text, temp_df)
      others_sale = others_sale+1
    }
  } 
}


for_buy=0
buy_df <- data.frame()
brooklyn_buy = 0
brooklyn_buy_text <- data.frame()

manhattan_buy = 0
manhattan_buy_text <- data.frame()

queens_buy = 0
queens_buy_text <- data.frame()

bronx_buy = 0
bronx_buy_text <- data.frame()

statenisland_buy = 0
statenisland_buy_text <- data.frame()

nyc_buy = 0
nyc_buy_text <- data.frame()

others_buy = 0
others_buy_text <- data.frame()


for (i in 1:nrow(frames)){
  if ((grepl("House",frames[i,]$text,ignore.case = TRUE)  |grepl("apartment",frames[i,]$text,ignore.case = TRUE))&grepl( "buy",frames[i,]$text,ignore.case = TRUE)){
    temp_df <- data.frame(as.character(frames[i,]$text),as.character(frames[i,]$screenName))
    for_buy = for_buy + 1
    if (grepl("brooklyn",frames[i,]$text,ignore.case = TRUE)){
      brooklyn_buy = brooklyn_buy + 1
      brooklyn_buy_text <- rbind(brooklyn_buy_text, temp_df)
    } 
    else if (grepl("manhattan",frames[i,]$text,ignore.case = TRUE)){
      manhattan_buy = manhattan_buy + 1
      manhattan_buy_text <- rbind(manhattan_buy_text,temp_df)
    } 
    else if (grepl("queens",frames[i,]$text,ignore.case = TRUE)){
      queens_buy = queens_buy + 1
      queens_buy_text <- rbind(queens_buy_text, temp_df)
    } 
    else if (grepl("bronx",frames[i,]$text,ignore.case = TRUE)){
      bronx_buy = bronx_buy + 1
      bronx_buy_text <- rbind(bronx_buy_text, temp_df)
    } 
    else if (grepl("statenisland",frames[i,]$text,ignore.case = TRUE)){
      statenisland_buy = statenisland_buy + 1
      statenisland_buy_text <- rbind(statenisland_buy_text,temp_df)
    } 
    
    else if (grepl("nyc",frames[i,]$text,ignore.case = TRUE) | grepl("#ny",frames[i,]$text,ignore.case = TRUE) | grepl("newyork",frames[i,]$text,ignore.case = TRUE)){
      nyc_buy_text <- rbind(nyc_buy_text, temp_df)
      nyc_buy = nyc_buy + 1
    } 
    else {
      others_buy_text <- rbind(others_buy_text, temp_df)
      others_buy = others_buy+1
    }
  } 
}

results = data.frame(tweets = c("Brooklyn", "Manhattan", "Queens", "Bronx","Statenisland","NYC"), numbers = c(brooklyn_buy,manhattan_buy,queens_buy,bronx_buy,statenisland_buy,nyc_buy))
ggplot(results,aes(x=results$tweets, y= results$numbers)) +
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="BOROUGH",y="No of tweets about selling the properties",title="buy")

results = data.frame(tweets = c("Brooklyn", "Manhattan", "Queens", "Bronx","Statenisland","NYC","Others"), numbers = c(brooklyn_buy,manhattan_buy,queens_buy,bronx_buy,statenisland_buy,nyc_buy,others_buy))
ggplot(results,aes(x=results$tweets, y= results$numbers)) +
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="BOROUGH",y="No of tweets about selling the properties",title="buy")

frames$loc <- (is.na(frames$longitude)) + 0
frames.loc <- frames[which(frames$loc == 0),]

map<- get_map(location = 'US', zoom = 4)
ggmap(map)

plot(newmap, xlim = c(-120, 90), ylim = c(-70, 70), asp = 1)
points(frames$longitude, frames$latitude, col = "red", cex = .6)
