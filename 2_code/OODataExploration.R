library(ggplot2)
library(DataExplorer)
library(stringr)
library(PerformanceAnalytics)

#We'll start by reading in the complete data set, then subsetting the columns that we will be working with. 
data <- read.csv("bcn_listings.csv")
keep <- c("room_type", "accomodates", "bathrooms", "bedrooms", "beds", "bed_type", "amenities", "price", "security_deposit", "cleaning_fee", "guests_included", "extra_people")
data <- data[-c(1:52, 60, 62, 63, 68:97)]
head(data)

#Now that we have the columns that we want to work with, we will begin exploration of each variable. The first variable we will look at is 'room_type.' This variable tells us whether a particular listing is an entire home or apartment, private room or shared room. This variable is factor with 3 levels stated previously. My initial thought was making these variables categorical. Let's take a  look at some infromation to determine how we can go about that. 
#

summary(data$room_type) 
#From the summary, is look as though the majority of the enteries are split between 'Entire home/Apt' and 'Private Room'. Of the 18,531 enteries, only 178 was listed as 'Shared Room'. We'd like to see if this variable has any missing variables next. 
#
plot_missing(data)

#The 'plot_missing' function shows us that 'bedrooms' has 10 missing rows while 'beds' has about 21 missing rows. 'Bathrooms' contains the highest number of missing rows at 33. It doesn't look as though we will have to  do much with our other variables. The columns that contain missing enteries are all numerical so we could potentially impute the missing enteries. 
#
#In terms of whether we should keep this variable or not, we would say yes. We would expect it to have a relationship with the price on the premise of if you're getting a full apartment, you will be expected to pay more that you would for a room in an apartnent. 
#
#We are going to go ahead and make the 'room_type' options categorical.
#
data$room_type <- ifelse(data$room_type %in% c('Private room', 'Shared room'), 0, 1)

#We would like to plot this variable against price, which is our target variable. we're going to go ahead and clean up each variable and then use the DataExplorer package to plot again price and see is anything jumps out at us 
#
#
#The next variable that we will look at is 'accomodates'. This variable tells us how many people each listing is suitable for. It is an integer value and we believe we can keep it as such. The plot for missing data above showed it had no missing values. We'll go ahead and obtain the summary stats. 
#
summary(data$accommodates)
#
#The minimum value for this variable is 1 and the max is 16. We have a mean of 3.324. We'd like to do a histogram to see what kind of distribution we have. The histogram is definitely skewed. We don't think we need to do any transformations of this variable. We'd also like to do a box plot(later once we've cleaned up price).
#
qplot(data$accommodates,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for Accomodates", 
      xlab = "accommodates")

#We can take a look at the 'bathrooms' variable next. This is a numerical variable as well. We'll do a summary to get the min, max, etc. 
#
summary(data$bathrooms)

#The minimum number for bathrooms is 0, which you would expect in cases where it's just a private room or a shared room. The max value is 8.5. For this variable, we have 33 NA's. If we were to impute, we would replace the values with the mean value. We plan to use the MICE package later to do the imputations. 
#
qplot(data$bathrooms,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for Bathrooms", 
      xlab = "bathrooms")

#The next variable we will be examining is bedrooms. We will do the same thing that we did for the above variables. 
#
summary(data$bedrooms)

qplot(data$bedrooms,
      geom="histogram",
      binwidth = .75,  
      main = "Histogram for Bedrooms", 
      xlab = "bedrooms")

#The min value for bedrooms is 0 which I find odd, unless 0 is used to indicate shared room. The max is 12 bedrooms and we have 10 rows with NA's. In this case, I would like to think that the 12 is an outlier but once we do the boxplot, we will know. 
#
#The next variable is beds. This is simply the numnber of bed at the listing. It's an integer variable and we do not think we need to do much with this variable. We'll use the code above again just get get a glimpse of our variable. 
#
summary(data$beds)

qplot(data$beds,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for Beds", 
      xlab = "bed")

#The max number of beds is 26 while the min is 0. There are 21 rows with NA's. I would be interested to see if this has any effect on pricing. You would think the more beds there are the higher the prices would be but we shall see. 
#
#Our next few variables are factors and we will be converting them. The first variable is bed_type. As stated before, it's a factor containing 5 levels. 
#
summary(data$bed_type)

#looking at the summary output, the majority of the variables are Real Bed (18,322). The rest of the levels are negligible. We will not be moving forward with this variable, therefore I will remove the column.
#
data <- data[-6]
#
#Our next variable is amenities. This is also a factor with 16,284 levels. We can't simply convert to categorical because of this. We can try to count the top 10 amenities that occur, rank them and give a score for each on that a particular listing has.
#

#Although price is listed as a factor, it's simply because of the dollar signs. We will remove the dollar signs and save this variable as a numerical variable.
#

data$price <- as.numeric(gsub("\\$", "",data$price))

#price <- unname(sapply(data$price, str_replace_all, '[, $]', ''))
#price <- as.numeric(price)
#data$price <- price

summary(data$price)

qplot(data$price,
      geom="histogram",
      binwidth = 20,  
      main = "Histogram for Price", 
      xlab = "price")

#The code we initally attempted to use simply removed the dollar sign and removed the numbers following the decimal. We were able to get the job done with the code using gsub and it worked quite well. Our minimum value for price is €5. We are inclined to think that this may be a mistake as that seems really low. The maximum price listed is €9,120. I'm going to do the same thing for the security_deposit field, cleaning_fee, as well as the extra_peopl variable. 
#
data$security_deposit <- as.numeric(gsub("\\$", "",data$security_deposit))
data$cleaning_fee <- as.numeric(gsub("\\$", "",data$cleaning_fee))
data$extra_people <- as.numeric(gsub("\\$", "",data$extra_people))

summary(data$security_deposit)
summary(data$cleaning_fee)
summary(data$extra_people)

qplot(data$security_deposit,
      geom="histogram",
      binwidth = 30,  
      main = "Histogram for Security Deposit", 
      xlab = "security_deposit")

qplot(data$cleaning_fee,
      geom="histogram",
      binwidth = 20,  
      main = "Histogram for Cleaning Fee", 
      xlab = "cleaning_fee")

qplot(data$extra_people,
      geom="histogram",
      binwidth = 10,  
      main = "Histogram for Extra People", 
      xlab = "extra_people")

#Both of these fields have quite a bit of NA's we're not sure why that is especially since our plot showed them as not having any mising rows. We immediately thought it was possible that some place include the cleaning fee in with their total price. This does not explain the missing values from security_deposit as you think all places require some sort of security deposit. 
#
#The last variable  that we are going to look at are guests_included. The guests included is a numerical variable that tells you how many additional guest are allowed. There's a fee associated with this variable so I see the price being affected by this variable. 
#
summary(data$guests_included)

#The min and max for this variable is 1 and 16 respectively. We initally thought we could change this to a categorical varible with 0 for no guests included and 1 for guests included. Because the range is 1 - 16, this is not possible 
#
qplot(data$guests_included,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for Guests Included", 
      xlab = "guests_included")

#Now I will do some correlation plots to see if there's anything that is glaringly obvious. 
#
plot_boxplot(data, "price")
chart.Correlation(data)
