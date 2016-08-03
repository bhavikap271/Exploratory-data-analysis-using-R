library(gdata)
library(ggplot2)
library(plyr)
library(doBy)

#dataSourcePath <- "C:/Users/Bhavika/Courses/CSE587/doing_data_science-master/dds_datasets/"

#setwd(dataSourcePath)

################################################################################
# First challenge: load in and clean up the data
################################################################################

bk <-read.xls("rollingsales_brooklyn.xls",pattern="BOROUGH")

# Set column names to lower case
names(bk) <-tolower(names(bk))

# Creates a numerical version of columns containing commas and/or dollar signs
bk$sale.price.n <- as.numeric(gsub("[^[:digit:]]", "", bk$sale.price))
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]", "", bk$gross.square.feet))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]", "", bk$land.square.feet))

################################################################################
# Next, conduct exploratory data analysis in order to find out where there are 
# outlier or missing values, decide how you will treat them, make sure the 
# dates are formatted correctly, make sure values you think are numerical are 
# being treated as such, etc.
################################################################################

# Making sure dates are formatted correctly
bk$sale.date <- as.Date(bk$sale.date)
# Making sure values I think are numerical are being treated as such
bk$year.built <- as.numeric(as.character(bk$year.built))

# Some exploration to make sure there's nothing weird going on with sale prices

# Total sales on record
ggplot(bk, aes(x=sale.price.n)) + 
  geom_histogram(binwidth = diff(range(bk$sale.price.n)))
# Total sales on record with a price more than 0
ggplot(subset(bk, sale.price.n>0), aes(x=sale.price.n)) + 
  geom_histogram(binwidth = diff(range(bk$sale.price.n)))
# Total sales on record with a price of 0
ggplot(subset(bk, sale.price.n==0), aes(x=gross.sqft)) + 
  geom_histogram(binwidth = diff(range(bk$gross.sqft)))

# We shall treat sales with price of 0 as missing values and omit them

# Keep only the actual sales
bk.sale <- bk[bk$sale.price.n!=0,]

# Let us look for some outliers
ggplot(bk.sale, aes(gross.sqft, sale.price.n)) + geom_point()
ggplot(bk.sale, aes(log(gross.sqft), log(sale.price.n))) + geom_point()

# Seems like we found some let's investigate further and see what we can do

# Factorize building.class.category
bk.sale$building.class.category <- factor(bk.sale$building.class.category)

# Let us look at building types and see what is most reasonable to work with
levels(bk.sale$building.class.category)

# We do want to work with building types that give us gross.sqft and lad.sqft
summaryBy(gross.sqft+land.sqft~building.class.category, 
          data=bk.sale, FUN=c(min, max))

# It seems most reasonable to work with 1, 2, and 3 family homes
bk.homes <- bk.sale[which(grepl("FAMILY",bk.sale$building.class.category)),]

# Let us look and see if we still have outliers
ggplot(bk.homes, aes(gross.sqft, sale.price.n)) + geom_point()
ggplot(bk.homes, aes(log(gross.sqft), log(sale.price.n))) + geom_point()

# We seem to have some, let's take a closer look
summaryBy(sale.price.n~address, data=subset(bk.homes, sale.price.n<10000), 
          FUN=c(length, min, max))

### Remove Outliers #######
# The summarized values above are definitely outliers and so we shall omit them
bk.homes$outliers <- (log(bk.homes$sale.price.n) <=5) + 0
bk.homes <- bk.homes[which(bk.homes$outliers==0),]

# Let us look and see if we still have outliers
ggplot(bk.homes, aes(gross.sqft, sale.price.n)) + geom_point()
ggplot(bk.homes, aes(log(gross.sqft), log(sale.price.n))) + geom_point()

##############Analysis Only for One/Two/Family Homes ###############

# Factorize neighborhood
bk.homes$neighborhood <- factor(bk.homes$neighborhood)

# Let's first see which neighborhoods are included in our data
levels(bk.homes$neighborhood)

# Let's explore the data quantitatively first:

metrics <- function(x) {
  c(length(x),                 # count
    round(mean(x)),            # mean
    min(x),                    # minimum
    max(x),                    # maximum
    max(x)-min(x))             # range
}


##### Average sales price across neighbourhoods #####
bk.homes.SalesPriceByNeighBourhood <- summaryBy(sale.price.n~neighborhood, data=bk.homes, FUN=metrics,
                                                fun.names=c("Count","Mean","Min","Max","Range"))

ggplot(bk.homes.SalesPriceByNeighBourhood,aes(x=neighborhood, y= sale.price.n.Mean,fill=neighborhood)) + geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Neighbourhood",y="Average Sale Price",title="Mean Sale Price Per NeighbourHood")


### No of homes sold in each neighbourhood #####

ggplot(bk.homes.SalesPriceByNeighBourhood,aes(x=neighborhood, y= sale.price.n.Count,fill=neighborhood)) + geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Neighbourhood",y="",title="No of Houses Sold Per NeighbourHood")




### Average grosss sqt feet across neighbourhood ####

bk.homes.GrossSqftByNeighBourhood <- summaryBy(gross.sqft~neighborhood, data=bk.homes, FUN=metrics,
                                                fun.names=c("Count","Mean","Min","Max","Range"))

ggplot(bk.homes.GrossSqftByNeighBourhood,aes(x=neighborhood, y= gross.sqft.Mean,fill=neighborhood)) + geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Neighbourhood",y="Gross Sqft",title="Mean GrossSqft Across NeighbourHood")


### Average Land sqft feet across neighbourhood ####

bk.homes.LandsqftByNeighBourhood <- summaryBy(land.sqft~neighborhood, data=bk.homes, FUN=metrics,
                                               fun.names=c("Count","Mean","Min","Max","Range"))

ggplot(bk.homes.LandsqftByNeighBourhood,aes(x=neighborhood, y=land.sqft.Mean,fill=neighborhood)) + geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Neighbourhood",y="Land Sqft",title="Mean LandSqft Across NeighbourHood")

#### No of homes sold per month ####
# Categorize sale dates by month and weekday
bk.homes$sale.month <- format(bk.homes$sale.date, "%B")
bk.homes$sale.day <- format(bk.homes$sale.date, "%A")

bk.homes$sale.month <- factor(bk.homes$sale.month, levels= c("January", "February", "March", "April", "May", "June",
                             "July", "August", "September", "October", "November","December"))

bk.sale.homeByMonth <- summaryBy(sale.price.n~sale.month, data=bk.homes, FUN=metrics,
                          fun.names=c("Count","Mean","Min","Max","Range"))

ggplot(bk.sale.homeByMonth,aes(x=sale.month, y=sale.price.n.Count,fill=sale.month)) + geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Month",y="# Count",title="Sales Per Month")


### Average Sales Price per month ####

ggplot(bk.sale.homeByMonth,aes(x=sale.month, y=sale.price.n.Mean,fill=sale.month)) + geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Month",y="# Sales Price",title="Average SalesPrice Per Month")

bk.sale.homeByDay <- summaryBy(sale.price.n~sale.day, data=bk.homes, FUN=metrics,
                                 fun.names=c("Count","Mean","Min","Max","Range"))

### No of sales of home per day ###
ggplot(bk.sale.homeByDay,aes(x=sale.day, y=sale.price.n.Count,fill=sale.day)) + geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Days",y="# Count",title="Sales Per Day")

### Mean sale price per day #####
ggplot(bk.sale.homeByDay,aes(x=sale.day, y=sale.price.n.Mean,fill=sale.day)) + geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Days",y="# Sales Price",title="Average SalesPrice Per Day")


### Year Built ####

bk.sale.yearBuilt <- summaryBy(sale.price.n~year.built, data=bk.homes, FUN=metrics,
                               fun.names=c("Count","Mean","Min","Max","Range"))

bk.sale.yearBuilt <- bk.sale.yearBuilt[bk.sale.yearBuilt$year.built != 0,]

ggplot(bk.sale.yearBuilt,aes(x=year.built, y=sale.price.n.Count,fill=year.built)) + geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Year Built",y="# Count",title="No of Houses Sold")


## Average Square feet/Price per zipCode

bk.homes.pricePerSqft <- bk.homes[bk.homes$gross.sqft != 0,]

bk.homes.pricePerSqft$PricePerSqft <- bk.homes.pricePerSqft$sale.price.n/bk.homes.pricePerSqft$gross.sqft

bk.homes.pricePerSqftByNeighbourhood <- summaryBy(PricePerSqft~neighborhood, data=bk.homes.pricePerSqft, FUN=metrics,
                               fun.names=c("Count","Mean","Min","Max","Range"))

ggplot(bk.homes.pricePerSqftByNeighbourhood,aes(x=neighborhood, y=PricePerSqft.Mean,fill=neighborhood)) + geom_bar(stat="identity",position=position_dodge()) +
  labs(x="Neighbourhood",y="# Avg SalesPrice/Sqft",title="Mean Price/Sqft per Neighbourhood")



