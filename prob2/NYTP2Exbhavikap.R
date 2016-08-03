library("doBy")
library("ggplot2")


#urlpart1 <- "http://stat.columbia.edu/~rachel/datasets/nyt"

urlpart1 <- "C:/Users/Bhavika/Courses/CSE587/doing_data_science-master/dds_datasets/dds_ch2_nyt/nyt"
urlpart2 <- ".csv"

percentLoggedIn <- vector()
countTotal <- vector()
countFemale <- vector()
countMale <- vector()
meanCTR <- vector()
meanClicks <- vector()
days <- (1:31)

for(i in 1:31){

  wholeurl <- paste(paste(urlpart1,i,sep=""),urlpart2,sep="") #construct the url for different days urlpart1+i+urlpart2
  nytData <- read.csv(wholeurl)
  
  # create age buckets
  intervals <- c(-Inf,0,18,24,34,44,54,64,Inf)
  nytData$ageGroup <- cut(nytData$Age, intervals)
  
  nytData.loggedIn <- subset(nytData,Signed_In == 1)
  head(nytData.loggedIn)
  nytData.notLoggedIn <- subset(nytData, Signed_In  != 1)
  head(nytData.notLoggedIn)
  
  nytData.loggedIn.totalUsers<-nrow(subset(nytData.loggedIn))
  nytData.loggedIn.maleUsers<-nrow(subset(nytData.loggedIn,nytData.loggedIn$Gender == 1))
  print(nytData.loggedIn.maleUsers)
  nytData.loggedIn.femaleUsers <- nrow(subset(nytData.loggedIn,nytData.loggedIn$Gender == 0))

  percentLoggedIn <- c(percentLoggedIn,(nytData.loggedIn.totalUsers/length(nytData$Age))*100)
  countFemale <- c(countFemale,nytData.loggedIn.femaleUsers)
  countMale <- c(countMale,nytData.loggedIn.maleUsers)
  countTotal <- c(countTotal,nrow(nytData))
  meanCTR <- c(meanCTR,sum(nytData$Clicks)/sum(nytData$Impressions))
  meanClicks <- c(meanClicks,sum(nytData$Clicks)/nrow(subset(nytData,nytData$Clicks>0)))
  
  remove(nytData)
}

monthlySummary <- data.frame(days,percentLoggedIn,countTotal,countFemale,countMale,meanCTR,meanClicks)

number_ticks<-function(n){function(limits) pretty (limits,n)}

#Percentage of people logged in the system
ggplot(monthlySummary,aes(x=monthlySummary$days,y=monthlySummary$percentLoggedIn)) + 
  geom_line(stat="identity")+
  scale_x_continuous(breaks=number_ticks(31)) +
  labs(x="Days",y="% Logged In",title="Percent Logged In over Time")


# CTR over time
ggplot(monthlySummary,aes(x=monthlySummary$days, y=monthlySummary$meanCTR)) + 
  geom_line(stat="identity") +
  scale_x_continuous(breaks=number_ticks(31)) +
  labs(x="Days",y="CTR",title="CTR variation over Time")

# Mean Clicks over time
ggplot(monthlySummary,aes(x=monthlySummary$days, y=monthlySummary$meanClicks)) + 
  geom_line(stat="identity") +
  scale_x_continuous(breaks=number_ticks(31)) +
  labs(x="Days",y="Mean Clicks",title="Mean Clicks over Time")

# User count over time
ggplot(monthlySummary,aes(x=monthlySummary$days,y=monthlySummary$countTotal)) + 
  geom_line(stat="identity")+
  scale_x_continuous(breaks=number_ticks(31)) +
  labs(x="Days",y="# Users",title="User count variation over Time")


