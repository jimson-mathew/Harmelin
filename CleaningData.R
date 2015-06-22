# 
#Jimson Mathew 
#6/19/2015
# 6/22/2015
# This program merges the zipcodes in the Harmelin Impressions File 
# to their respective latiudes and longitudes.
# 

setwd("Desktop/Project/Files/")
#
#orgImprssn is a dataframe that contains the Date, Impressions, Zip Codes, and 
#Clicks.
#orgWeathr is a dataframe that contains the daily weather information from NOAA
orgImprssn  <- read.csv(file ="impression-click_2-year.csv", header = TRUE)
orgWeathr   <- read.csv(file ="WeatherData.txt", header = TRUE )
# 
# Impressions and weather are copies of the respective files above.
impressions <- orgImprssn
weather     <- orgWeathr

# 
#Online, I found a package that had the information of zip codes in addition to 
#their latitudes and longitudes.
# 
install.packages("zipcode")
library(zipcode)
data("zipcode")
temp <- merge(impressions, zipcode, by.x = "ZIP.Postal.Code", by.y = "zip", all = TRUE)

#Ordering by the Date
temp <- temp[ with ( temp, order( temp$Date ) ), ] 

#
#Upon examining temp,  I noticed that one zip code had a "(not set)" value.
#I decided to remove its instances.
#
temp <- temp[temp$Date != "(not set)",]

#
#Checking if there are any NA values in the city, latitude, longitude, and state.
#
temp[ (is.na(temp$city) | (is.na(temp$latitude)) |  (is.na( temp$longitude ) ) | ( is.na( temp$state ) ) ),]

#
#Examining the original Impressions, and weather, and zipcode
#
#
#
str(orgImprssn)
str(orgWeathr)
str(orgImprssn)
str(zipcode)

#
#Checking whether there are an NAs in city or zip
#
#
is.na(zip$city)
is.na( zipcode$zip )

#
#finding the number of NAs in zip, city, state, latitude, longitude 
#in zipcode
#
#
sum( is.na( zipcode$zip ) )
sum( is.na( zipcode$city ) )
sum( is.na( zipcode$state ) )
sum( is.na( zipcode$latitude ) )
sum( is.na( zipcode$longitude ) )

#found rows where latitude and longitude were missing
zipcode[zipcode$latitude == NA]
zipcode[zipcode$latitude == NA,]

#To figure out which rows did not have NAs in them
zipcode[complete.cases(zipcode[,c(4,5)]),]

#trying to figure out the number of complete rows.
#
#
nrow( zipcode[complete.cases(zipcode[,c(4,5)]),] )

#
#I made a copy of the zipcodes with completed observations/
#
myzip <-  zipcode[complete.cases(zipcode[,c(4,5)]),]

#
#Checking whether the NAs are still present or not
#
sum( is.na( myzip$latitude ) )
sum( is.na( myzip$longitude ) )
sum( is.na( myzip$zip ) )
sum( is.na( myzip$city ) )
sum( is.na( myzip$state ) )


#
#Turns out in the original impression dataset, there were zip codes called "(not set)" about 760
# of them. I removed it from the files. This reduced it to 10032485.
#
impressions <- impressions[ ( impressions$ZIP.Postal.Code != "(not set)" ) , ]
temp2 <- merge(impressions, myzip, by.x = "ZIP.Postal.Code", by.y = "zip", all = TRUE)
temp2 <- temp2[ with ( temp2, order( temp2$Date ) ), ]

#
#Checking out how many NA's are still present in the latitude and longitude columns.
#
#
sum(is.na( temp2$latitude ) )
sum(is.na( temp2$longitude ) )

#
#Displaying all rows that had NAs in the city, latitude, longitude, and state in temp2
#
temp2[ (is.na(temp2$city) | (is.na(temp2$latitude)) |  (is.na( temp2$longitude ) ) | ( is.na( temp2$state ) ) ),]

#
#I noticed that these zip codes: 761906, 34906, and "T0K" had NAs in city, latitude, longitude, and state
#This was because the zipcode database did not have those zip codes.
#
temp2 <- temp2[ (temp2$ZIP.Postal.Code != 76190), ]
temp2 <- temp2[ (temp2$ZIP.Postal.Code != 34906), ]
temp2 <- temp2[ (temp2$ZIP.Postal.Code != ( "T0K" ) ), ]

#
#Checking out whether
#there are anymore NAs in city, latitude, longitude, and state
#
temp2[ (is.na(temp2$city) | (is.na(temp2$latitude)) |  (is.na( temp2$longitude ) ) | ( is.na( temp2$state ) ) ),]

#
#Seeing the rows that have NAs in the other columns
#
temp2[ (is.na(temp2$ZIP.Postal.Code) | (is.na(temp2$Date)) |  (is.na( temp2$Country ) ) | ( is.na( temp2$Impressions ) ) | ( is.na( temp2$Clicks ) )  ),]

#Obtaining the number of these rows.
nrow ( temp2[ (is.na(temp2$ZIP.Postal.Code) | (is.na(temp2$Date)) |  (is.na( temp2$Country ) ) | ( is.na( temp2$Impressions ) ) | ( is.na( temp2$Clicks ) )  ),] )
summary(impressions)

#
#Seeing the rows that have NAs in the Zip Code, Date, Country, Impressions, and Clicks
#
impressions[ (is.na(impressions$ZIP.Postal.Code) | (is.na(impressions$Date)) |  (is.na( impressions$Country ) ) | ( is.na( impressions$Impressions ) ) | ( is.na( impressions$Clicks ) )  ),]

#
#The number of rows that have NAs in ALL OF the other columns
#
nrow ( temp2[ (is.na(temp2$ZIP.Postal.Code) & (is.na(temp2$Date)) &  (is.na( temp2$Country ) ) & ( is.na( temp2$Impressions ) ) & ( is.na( temp2$Clicks ) )  ),] )
nrow ( temp2[ ((is.na(temp2$Date)) &  (is.na( temp2$Country ) ) & ( is.na( temp2$Impressions ) ) & ( is.na( temp2$Clicks ) )  ),] )
summary(temp2)

#Deleting the NA values in the columns: Zip Code, Date, Country, Impressions, and Clicks
temp2 <-  temp2[complete.cases(temp2[,c(2,3,4,5)]),]
nrow ( temp2[ ((is.na(temp2$Date)) &  (is.na( temp2$Country ) ) & ( is.na( temp2$Impressions ) ) & ( is.na( temp2$Clicks ) )  ),] )
temp2[ (is.na(temp2$ZIP.Postal.Code) | (is.na(temp2$Date)) |  (is.na( temp2$Country ) ) | ( is.na( temp2$Impressions ) ) | ( is.na( temp2$Clicks ) )  ),]
#Examining what happened
summary(temp2)

#6/22/2015:

#Saving the temp2 file into a CSV format
#Soon realized why the row.names was placed.
#
#
write.csv(temp2, file = "updatedHarmelinImpressions.csv",row.names = F)

#To check if everything had been written correctly.
temp3 <- read.csv("updatedHarmelinImpressions.csv", header = T)

#
#myzip does not contain any NA rows. I will use this in my
#kd tree.
#
write.csv(myzip,"completeZipCode.csv", row.names = F)

#To check if everything in myzip had been written correctly.
temp3 <- read.csv("completeZipCode.csv", header = T)

#
#finding the number of NAs in zip, city, state, latitude, longitude 
#in temp3
#
#
sum( is.na( temp3$zip ) )
sum( is.na( temp3$city ) )
sum( is.na( temp3$state ) )
sum( is.na( temp3$latitude ) )
sum( is.na( temp3$longitude ) )




