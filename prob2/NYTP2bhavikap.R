library("doBy")
library("ggplot2")

setwd(directory)

nytData <-  read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))

head(nytData)

# 1. Create a new variable, age_group, that categorizes users as
#    "<18", "18-24", "25-34", "35-44", "45-54", "55-64", and "65+".
intervals <- c(-Inf,0,18,24,34,44,54,64,Inf)
#categories <- c("N/A","<18","18-24","25-34","35-44","45-54","55-64","65+")
nytData$ageGroup <- cut(nytData$Age, intervals)


#2: Plot the distributions of number impressions and click-through-rate
# CTR =# clicks/# impressions

#subset(nytData,(Clicks>0)&(Impressions == 0))

#print(head(nytData))

#print(nrow(subset(nytData, Clicks>0)))

#Number of impressions distribution plots:
ggplot(nytData, aes(x=Impressions,fill=ageGroup)) + geom_histogram(binwidth = 1)
ggplot(nytData, aes(x=ageGroup, y=Impressions, fill=ageGroup)) + geom_boxplot()

# Click-through-rate plots:
ggplot(subset(nytData, Impressions>0), aes(x=Clicks/Impressions,colour=ageGroup)) + geom_density()
ggplot(subset(nytData, Clicks>0), aes(x=Clicks/Impressions,colour=ageGroup)) + geom_density()

#click behavior across ageGroup
ggplot(subset(nytData, Clicks>0), aes(x=ageGroup, y=Clicks,fill=ageGroup)) + geom_boxplot()
ggplot(subset(nytData, Clicks>0), aes(x=Clicks, colour=ageGroup))+ geom_density()
ggplot(subset(nytData, Clicks>0), aes(x=Clicks, colour=ageGroup)) + stat_density()

#clicks are close to 1 in general so we can categorize them as below:

# Define a new variable to segment or categorize users based on
# their click behavior.
nytData$clickCat[nytData$Impressions == 0] <- "NO_IMPRESSIONS"
nytData$clickCat[nytData$Clicks == 0 & nytData$Impressions > 0] <- "NON_CLICKERS"
nytData$clickCat[nytData$Clicks > 0] <- "CLICKERS"
#nytData$clickCat <- factor(nytData$clickCat)


# BULLET 3:
# Explore the data and make visual and quantitative comparisons
# across user segments/demographics (<18-year-old males versus
# <18-year-old females or logged-in versus not, for example).

statistics <- function(x)
{
  # c func is used to make a vector (length gives the number of obs)
  c(length(x), min(x), max(x), mean(x)) 
}


# Users across ageGroup
nytData.users.byAgeGrp <- summaryBy(ageGroup~ageGroup, data = nytData, FUN=statistics)
colnames(nytData.users.byAgeGrp) <- c("AgeGroup","Count","minAge","maxAge","meanAge")

print(head(nytData.users.byAgeGrp))

ggplot(nytData.users.byAgeGrp,aes(x=AgeGroup, y=Count,fill=AgeGroup)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Age Group",y="# Users",title="Users Distributed across AgeGroups")  

nytData$Gender[nytData$Gender == 1] <- "MALE"
nytData$Gender[nytData$Gender == 0 & nytData$Signed_In == 1] <- "FEMALE"
nytData$Gender[nytData$Gender == 0 & nytData$Signed_In == 0] <- "NOT LOGGED IN"

# split the data for analysis based on Signed_In
nytData.loggedIn <- subset(nytData,Signed_In == 1)
nytData.notLoggedIn <- subset(nytData, Signed_In  != 1)

#debug
print(head(nytData.loggedIn))


##### Demographics based metric #############

# No of people signed in by age groups
nytData.loggedIn.countByAgeGrp <- summaryBy(ageGroup~ageGroup,data=nytData.loggedIn,FUN=length)
colnames(nytData.loggedIn.countByAgeGrp) <- c("AgeGroup","TotalLoggedIn")

head(nytData.loggedIn.countByAgeGrp)

#Gender distribution for total SignedIn People.
nytData.loggedIn.genderDist <- summaryBy(Gender~ageGroup+Gender,data=nytData.loggedIn,FUN = length)
colnames(nytData.loggedIn.genderDist) <- c("AgeGroup","Gender","ObsCount")

#debug
head(nytData.loggedIn.genderDist)

nytData.loggedIn.genderDist <- merge(nytData.loggedIn.genderDist,nytData.loggedIn.countByAgeGrp)
head(nytData.loggedIn.genderDist)

ggplot(nytData.loggedIn.genderDist, aes(x=AgeGroup, y=ObsCount, fill=Gender)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  xlab("Age Groups") +
  ylab("# Users") +
  labs(x="Age Groups",y="# Users",title="User distribution by Age Groups & Gender")+
  scale_colour_grey(name ="Gender")+
  theme(legend.background = element_rect(colour = "black"))

summary(nytData)

#Mean Click by AgeGroup:

meanClick <- function(x){
   mean(x)
}

meanClick.AgeAndGender <- summaryBy(Clicks~ageGroup+Gender,data=nytData.loggedIn, FUN=meanClick)
meanClick.AgeAndGender <- subset(meanClick.AgeAndGender,Gender != "NOT LOGGED IN")
meanClick.AgeGroup <- summaryBy(Clicks~ageGroup,data=nytData, FUN=meanClick)

print(head(meanClick.AgeAndGender))

# Mean Clicks across AgeGroup
ggplot(meanClick.AgeGroup,aes(x=ageGroup, y= Clicks.meanClick,fill=ageGroup)) + geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Age Groups",y="Mean Clicks",title="Mean Click across Age Group")

# Mean Clicks across AgeGrp and Gender
ggplot(meanClick.AgeAndGender,aes(x=ageGroup, y=Clicks.meanClick,fill=Gender)) + geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Age Groups",y="Mean Clicks",title="Mean Click by AgeGroup & Gender")+
  facet_grid(~Gender)

# Click Behaviour Analysis
nytData.clkBehaviour.ByGenderwithImpression <- summaryBy(nytData$Impressions~nytData$clickCat+Gender,data=nytData,FUN=statistics)
nytData.clkBehaviour.ByGenderwithImpression <- nytData.clkBehaviour.ByGenderwithImpression[,-4]
nytData.clkBehaviour.ByGenderwithImpression <- nytData.clkBehaviour.ByGenderwithImpression[,-4]
colnames(nytData.clkBehaviour.ByGenderwithImpression) <- c("clkBehaviour","Gender","Count","MeanImps")


nytData.ImprBias<-summaryBy(Impressions~ageGroup, data=nytData, FUN=mean)
head(nytData.ImprBias)

#### :: Mean Impressions vs. Age Group
ggplot(nytData.ImprBias,aes(x=ageGroup,y=Impressions.mean,fill=ageGroup)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Age Groups",y="Mean Impression",title="Impressions Mean by Age Group")


nyt.ImprBiasbyGender<-summaryBy(Impressions~ageGroup+Gender, data=nytData, FUN=mean)
head(nyt.ImprBiasbyGender)

#### :: Mean Impressions vs. Age Group + Gender
ggplot(nyt.ImprBiasbyGender,aes(x=ageGroup,y=Impressions.mean,fill=Gender)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Age Groups",y="Mean Impression",title="Impressions Mean by Age Group & Gender")+
  facet_grid(~Gender)

nytData.loggedIn.validImps <- subset(nytData.loggedIn,nytData.loggedIn$Impressions > 0)


# CTR analysis

nytData$clkCode[nytData$Impressions==0] <- "NoImps"
nytData$clkCode[nytData$Impressions >0] <- "Imps" 

summaryStatistics <- function(x)
{
  c(length(x),sum(x),mean(x),min(x),max(x))
}

nytData.impClkSummary<-summaryBy(Impressions+Clicks~clkCode+Gender+ageGroup,data = subset(nytData,nytData$Gender != "NOT LOGGED IN"), FUN=summaryStatistics)
colnames(nytData.impClkSummary)<-c("clkCode","Gender","AgeGrp","ObsCount","TotImp","MeanImp","MinImp","MaxImp","weedout","TotClk","MeanClk","MinClk","MaxClk")
nytData.impClkSummary <- nytData.impClkSummary[,-7]
nytData.impClkSummary$CTR <- nytData.impClkSummary$TotClk/nytData.impClkSummary$TotImp

# Impression Distribution By Age Group and Gender
ggplot(subset(nytData.impClkSummary,clkCode=="Imps"), aes(x=AgeGrp, y=TotImp,fill=Gender)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Age Groups",y="# Imp",title="Impression Distribution by Age Group & Gender") +
  theme(legend.background = element_rect(colour = "black"))

##### :: CTR by AgeGrp and Gender
ggplot(subset(nytData.impClkSummary,clkCode=="Imps"), aes(x=AgeGrp, y=CTR,fill=Gender)) + 
geom_bar(stat="identity",position=position_dodge()) +
labs(x="Age Groups",y="# NoImps",title="CTR By Age Group & Gender") +
theme(legend.background = element_rect(colour = "black"))

