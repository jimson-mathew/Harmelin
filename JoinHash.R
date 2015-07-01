# Name: Jimson Mathew 
# Date: 6/25/2015
# Description: This program joins the Hash table with the Impression
# data. Thus, for every zip code, we have the closest weather station.
# However, a lot of cleaning was needed in order to remove the missing values.
#
#


# I use Distances in order to determine the distance between two weather stations. 
 source("/Users/Mathew/Desktop/Project/Distances.R")
 
# This filecontains the matches between the harmelin zip codes and the closest 
# weather station ID. 
 hashZip <- read.csv( file = "/Users/Mathew/Desktop/Project/Files/ZipWBAN_Hash.csv", header = T, 
                      colClasses = c("character", "integer")  )
#this  contains the Harmelin Data that contains the clicks, etc but not the closest weather station.
  orgImpressions <- read.csv(
         "/Users/Mathew/Desktop/Project/Files/NOSHORTESTDISTANCESHarmelinZipcodesWithMatchingCoordinates.csv", 
         header = T, colClasses = c("character", "Date", "character", "integer", 
                                    "integer", "character", "character", 
                                    "double", "double"))

# the weather station data
orgWeather <- read.csv("/Users/Mathew/Desktop/Project/Files/StationWeatherData.txt", header = T)

#applying operations on a copy of orgImpressions
impressions <- orgImpressions
weather     <- orgWeather



#merging the zip code file with the hashZip file based ont he Zip code.
#There are no NA values in the WBAN column of the result of the merge.
temp <-  merge( impressions, hashZip, by.x = "ZIP.Postal.Code", 
                by.y = "Zip.Code", all=F )

#adding the resulting merged file with the impressions and WBAN to data
write.csv( temp,"HarmelinWithMatchingClosestStation'sWeatherData.csv", row.names = F)

# harmelin <-temp
# rm(temp)

# reading the file. You could just comment this and uncomment the previous comments.
 harmelin <- read.csv(
         "/Users/Mathew/Desktop/Project/Files/HarmelinWithMatchingClosestStation'sWeatherData.csv",
         header = T, colClasses = c("character", "Date", "character", "integer", 
                                     "integer", "character", "character", 
                                     "double", "double", "integer"))

#To convert the Date column in the weather dataframe into a Date format.
weather$Date <- as.Date( as.character( weather$YearMonthDay ), format( "%Y%m%d" ) )

# temp <- merge( harmelin, weather, by.x = c( "Date", "WBAN" ), 
#                by.y = c( "Date", "WBAN" ), all = F )
# 
# 
# temp2<- merge( harmelin, weather, by.x = c( "Date", "WBAN" ), 
#                by.y = c( "Date", "WBAN" ), all = T )



#Left Join
temp3<- merge( harmelin, weather, by.x = c( "Date", "WBAN" ), 
               by.y = c( "Date", "WBAN" ), all.x = T, all.y = F )

#Checking the number of NA's in YearMonthDay. 
sum(is.na( temp3$YearMonthDay )) # 148274 about 1.5% of the entire data
#
#This weather data refers to weather at a station on a daythat was not present 
#in the NOAA data. On this day, and at this place, impressions and clicks 
#were registered.

######## ANALYZING THE NA VALUES #############

#temp3 <- temp3[!is.na( temp3$YearMonthDay ),] 
#extracting the NA values.
examine <- temp3[ is.na( temp3$YearMonthDay ),]
#Counting Number of WBAN's that were actually missing
examineDates <-aggregate( WBAN ~ Date, data = examine, FUN= length )
str(examineDates)

#graphing examineDate
library( ggplot2 )
myGraph <- ggplot(data= examineDates, aes(x = Date, y = Quantity))
myGraph + geom_histogram( aes(y= Quantity),stat = "identity" ) 
#myGraph + coord_cartesian(xlim = c("2013-06-15","2013-06-23"))


install.packages("ggmap")
library(ggmap)
# map <- get_map(location = "usa", zoom = 3)
# ggmap(map)

# extracting all values that were in 2015.
examine2015<- examine[ ( format(examine[1], "%Y") == 2015 ), c( 1,2,3 ) ]
# 
# examine2015 <- examine[  ( format(examine$Date, "%Y") == 2015 ) ,c(1,2,7:10)]
# examine2015agg<-aggregate( WBAN ~ latitude + longitude,
#                            data = examine2015, FUN= length )
# names( examine2015agg ) <- c("latitude", "longitude", "quantity")

# adding the station information to those latitude and longitudes.
# I realized a little too late that i could have avoided this
guess <- read.csv("Files/NOSHORTESTDISTANCESHarmelinZipcodesWithMatchingCoordinates.csv"
                  , header=T, colClasses = c("character", "Date", "character", "integer", 
                                             "integer", "character", "character", 
                                             "double", "double"))
#DO NOT EXECUTE THIS! The resulting data frame has ~34 million observations.
examine2015ZipWithCorLatLong <- merge( examine2015, guess[,c("ZIP.Postal.Code", "latitude", "longitude")], 
       by = "ZIP.Postal.Code", all.x = T, all.y = F)
#DO NOT EXECUTE THIS!
# examine2015LatLong had 34 million values. Extracting the unique ones.
unqueexamine2015LatLong <- unique(examine2015ZipWithCorLatLong)

# obtaining the Zip codes that are missing for a large part of 2015
NoOfMissingDays <- aggregate( Date ~ ZIP.Postal.Code + latitude + longitude, 
                              data = unqueexamine2015LatLong, FUN= length )
names(NoOfMissingDays) <- c("ZIP.Postal.Code", "latitude", "longitude", 
                            "quantity")
# Displaying the zipcodes on the USA map.
map <- get_map(location = "usa", zoom = 4)
mapPoints <- ggmap(map) +
        geom_point(aes(x = longitude, y = latitude, size = (quantity)),
                   data = NoOfMissingDays, alpha = .5)
mapPoints

# Reviewing what zipcodes had missing data for a greater part of the first
#quarter of 2015.
missing160 <- NoOfMissingDays[NoOfMissingDays$quantity >= 160,]
missing160$quantity <- as.factor( missing160$quantity )

map <- get_map(location = "usa", zoom = 4)
mapPoints <- ggmap(map) +
        geom_point(aes(x = longitude, y = latitude, size = (quantity)),
            data = missing160, alpha = .5)
mapPoints

######## Replacing the NA values with the next closest station that has weather data #######
#I'm going to strip examine of all of the weather data, join it with its
#corresponding latitude and longitude and then add the next closest information.


# this file contains the list of zip codes in America WITHOUT ANY NA VALUES
zip <- read.csv(file = "/Users/Mathew/Desktop/Project/Files/completeZipCode.csv",
                header = T, colClasses = c("character", "character", "character",
                                           "double", "double"))

#this package will help me in making a hashtable VERY QUICKLY
install.packages("hash")
library( hash )

#creating an empty hash.
weatherHash <- hash()

# I will be adding the date and the station data to a hash table.
#I just need it as a check. Thus, add nothing to it.
for ( i in 1:nrow( weather ) ) {
        weatherHash[[ paste( weather[ i, 'Date' ],weather[i, "WBAN" ] ) ]] <- NA 
}

# 
# In this method, I accept one row of the zipcode dataframe. 
# I will compare its distances to all of the latitudes
# and the longitudes in the weather data.
# Then, I will order it in increasing order. I will
# return the WBAN od these distances based on the 
# order and the WBAN's that are in a 30 mile radius of the zip code.
# 
shortestAvailable <- function( zip ) {
        # obtaining a list of distances from a zip code and all of the weather
        # information.
        dists <- distance( zip$impressions.latitude, 
                           zip$impressions.longitude, 
                           uniqueWeather$weather.Latitude, 
                           uniqueWeather$weather.Longitude)
#         print("Before ordering")
#         print(dists)
        
        #Arragning the distances in increasing order.
        #
        correctOrdr <- dists[ order(dists) ]
#         print("After ordering")
#         print(correctOrdr)
        
        #I want to find out at what instance the weather stations fall outside
        #of the 30 mile radius I decided earlier. 
        #
        for ( i in length(correctOrdr) ) {
                if ( correctOrdr[i] >= 48280.3 ) {
                        n <- i
                        break
                }
        }
        
        # I return the subset of WBAN's that are in a 30 mile radius of zip
        increase <- uniqueWeather[order(dists),1][1:n]
        increase
        
}

# I will get a dictionary of hash values of unique zips to make differences.
# Doing this first eases my work.  
# Otherwise, things get quite complex.
uniqueZip <- unique(x = data.frame( impressions$ZIP.Postal.Code, 
                                    impressions$latitude, 
                                    impressions$longitude ) )

#Obtaining the unique station information
uniqueWeather <- unique( x = data.frame(weather$WBAN, weather$Latitude, 
                                      weather$Longitude ) )

#Creating an empty hash that has the zip code as a key and a list of
#WBAN's that are close to it.
closestZipStation <- hash()

#
#In this loop, I will go through each zip code and will obtain the closest
#station IDs. Then, I will store them in the hash.
#
# j=0 #Testing
for (i in 1:nrow( uniqueZip ) ){
                #the index of the list is the Zip Code
                closestZipStation[[ paste( uniqueZip[i,'impressions.ZIP.Postal.Code'] ) ]] <- shortestAvailable( uniqueZip[i,] )
                
#                                  j <- j + 1 #Testing
#                                 if ( j == 1 ) { #Testing
#                                         break #Testing
#                                 }#Testing
        }

# print( closestZipStation )
        
#create a copy of temp3, just in case anything unfortunate happens to temp3.      
temp4 <- temp3

#Delete most of the columns of temp3
# I will use his later to replace the NA values with the closest ones.
temp3 <- temp3[ -c(12:73) ]

#obtaining only the NA and non- NA values.
temp3OnlyNA <- temp3[ is.na( temp3$YearMonthDay ), ]
temp3 <- temp3[ !is.na( temp3$YearMonthDay ), ]



#
#For every observation in temp3OnlyNA, I will check the closestZipStation 
#for WBANs that are closest to this station.  
#Then, I will add use the Date in temp3OnlyNA and the WBANs in closestZipStation
#to see if this value is present. If so, I will added the closest WBAN and continue
#the search to the next instances.
# 
for ( i in 1:nrow( temp3OnlyNA ) ) {
            #if ( has.key( paste( temp3OnlyNA[i,"ZIP.Postal.Code"] ), closestZipStation  ) ) {   #Testing
                    # print("This zip has nearest stations!")#Testing
        
        # Obtaining the WBANS for a given zip code.
                        for ( j in  closestZipStation[[ paste( temp3OnlyNA[i, "ZIP.Postal.Code" ] )  ]] )  {
                                #checking if this station has a key in weather Hash.
                                if ( has.key( paste( temp3OnlyNA[ i, "Date" ], j ), weatherHash  ) ){
                                         # print("TRUE!") #Testing
                                         # print(j)#Testing
                                        temp3OnlyNA[i, "WBAN"] <- j #since j has the WBAN number
                                         break#Once I find the closest one that 
                                         #has a value in the weather data, I do 
                                         #not need to look any further.
                                }
                        }
            #}#Testing
                
            # break #Testing
} # end for ( i in 1:nrow( temp3OnlyNA ) )

#Since the order does not matter, I can join both dataframes.
noNAHarmelin <- rbind( temp3OnlyNA, temp3 )
#When I will merge, the proper value of YearMonthDay will be added.
noNAHarmelin$YearMonthDay <- NULL

#Left Joining the new DataFrame
temp3<- merge( noNAHarmelin, weather, by.x = c( "Date", "WBAN" ), 
               by.y = c( "Date", "WBAN" ), all.x = T, all.y = F )

# Checking if NA values are still present.
sum(is.na(temp3$YearMonthDay))


#Saving the values to an RDS file.
saveRDS(temp3,file =" NONAValuesHarmelinHasTheClosestWeatherStation.rds" )
saveRDS(orgImpressions, file = "NOSHORTESTDISTANCESHarmelinZipcodesWithMatchingCoordinates.rds")

