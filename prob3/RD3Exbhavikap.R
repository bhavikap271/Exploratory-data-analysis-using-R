#library(gdata)
library(ggplot2)
library(plyr)
library(doBy)
library(gdata)

dataSourcePath <- "C:/Users/Bhavika/Courses/CSE587/doing_data_science-master/dds_datasets/"

setwd(dataSourcePath)

bk <-read.xls("rollingsales_statenisland.xls",pattern="BOROUGH")

mydata1 = read.xls("rollingsales_brooklyn.xls",pattern="BOROUGH")
#mydata1 = mydata1[keeps]
mydata2 = read.xls("rollingsales_bronx.xls",pattern="BOROUGH")
#mydata2 = mydata2[keeps]

mydata3 = read.xls("rollingsales_manhattan.xls",pattern="BOROUGH")
#mydata3 = mydata3[keeps]

mydata4 = read.xls("rollingsales_queens.xls",pattern="BOROUGH")
#mydata4 = mydata4[keeps]

mydata5 = read.xls("rollingsales_statenisland.xls",pattern="BOROUGH")
#mydata5 = mydata5[keeps]
                  
myfullData <- rbind.fill(mydata1,mydata2,mydata3,mydata4,mydata5)

names(myfullData) <- tolower(names(myfullData))


#cleaning the data
#myfullData$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",myfullData[ ,'SALE.PRICE']))
myfullData$sale.price.n <- as.numeric(gsub("[^[:digit:]]", "", myfullData$sale.price))
myfullData$gross.sqft <- as.numeric(gsub("[^[:digit:]]", "", myfullData$gross.square.feet))
myfullData$land.sqft <- as.numeric(gsub("[^[:digit:]]", "", myfullData$land.square.feet))
myfullData$sale.date <- as.Date(myfullData$sale.date)

#myfullData$year.built <- as.numeric(as.character(myfullData[,'YEAR.BUILT']))

# Taking only those data where sale price is > 0
myfullData.sale <- myfullData[myfullData$sale.price.n!=0,]
myfullData$year.built <- as.numeric(as.character(myfullData$year.built))

# Analysis
summaryStats <- function(x)
{
  # c func is used to make a vector (length gives the number of obs)
  c(length(x), min(x), max(x), mean(x)) 
}

# No of houses sold in each borough
data.salesPrice.Borough <- summaryBy(sale.price.n~borough, data=myfullData.sale, FUN=summaryStats,
          fun.names=c("Count","Min","Max","Mean"))

ggplot(data.salesPrice.Borough,aes(x=borough, y=sale.price.n.Count,fill=borough)) + geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Borough",y="# Houses Sold",title="No of Houses Sold In Each Borough")


# Mean Sales Price across each Borough
ggplot(data.salesPrice.Borough,aes(x=borough, y=sale.price.n.Mean,fill=borough)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Borough",y="Avg Sales Price",title="Mean Sales Price Across Borough")


# No of houses sold in each month across each Borough
myfullData.sale$sale.month <- format(myfullData.sale$sale.date,"%B")

myfullData.sale$sale.month <- factor(myfullData.sale$sale.month, levels= c("January", "February", "March", "April", "May", "June",
                                                             "July", "August", "September", "October", "November","December"))


data.monthly.Borough <- summaryBy(sale.price.n~borough+sale.month,data=myfullData.sale,FUN=length)
colnames(data.monthly.Borough) <- c("BOROUGH","MONTH","COUNT")

# need to find out histogram for plotting for this.
ggplot(data.monthly.Borough,aes(x=MONTH, y=COUNT,fill=BOROUGH)) + geom_bar(stat="identity",position=position_dodge()) +
  labs(x="MONTH",y="#Houses Sold",title="Houses sold in a month for each Borough")+
  facet_grid(~BOROUGH)

# mean gross square feet across each borough

summaryGrossStats <- function(x)
{
  # c func is used to make a vector (length gives the number of obs)
  c(min(x), max(x), mean(x)) 
}



data.grossSqt.Borough <- summaryBy(gross.sqft~borough, data=myfullData.sale, FUN=summaryGrossStats,
                         fun.names=c("Min","Max","Mean"))

# Mean gross square feet in each borough
ggplot(data.grossSqt.Borough,aes(x=borough, y= gross.sqft.Mean,fill=borough)) + 
   geom_bar(stat="identity",position=position_dodge()) +
   labs(x="Borough",y="Mean Gross Square Feet",title="Mean Gross Square Feet Across Borough")



# Mean land square feet in each borough
data.landsqft.Borough <- summaryBy(land.sqft~borough, data=myfullData.sale, FUN=summaryGrossStats,
                                   fun.names=c("Min","Max","Mean"))

ggplot(data.landsqft.Borough,aes(x=borough,y=land.sqft.Mean,fill=borough)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Borough",y="Mean Land Square Feet",title="Mean Land Square Feet Across Borough")


