#Name: Jimson Mathew
# Date 6/ 24-25/2015
# Description:
# This file contains two funcitons: findShortest(...) and createHash()
# findShortest uses the Distances.R file to obtain the closest
# weather station to a zipcode, which is given as input. The Weather Data
# is obtained from the NOAA data.
# 
#createHash() creates a dataframe between the zip codes and the closest station  
# Id. It makes use of findShortest(...) and ldply() to achieve its objective.
# 

#adding methods from Distances.R
source("/Users/Mathew/Desktop/Project/Distances.R")

#ldply()
install.packages("plyr")
library(plyr)

#reading in needed files.
globe <- read.csv("/Users/Mathew/Desktop/Project/Files/StationWeatherData.txt",
                  header = T)
orgImpressions <- read.csv(
        "/Users/Mathew/Desktop/Project/Files/updatedHarmelinImpressions.csv", 
        header = T, colClasses = c("character", "Date", "character", "integer", 
                                   "integer", "character", "character", 
                                   "double", "double"))
#applying operations on a copy of orgImpressions
impressions <- orgImpressions


#Obtaining the unique station information
uniqueGlobe <- unique( x = data.frame(globe$WBAN, globe$Latitude, 
                                      globe$Longitude ) )
#Obtaining the unique zip codes
uniqueZip <- unique(x = data.frame( impressions$ZIP.Postal.Code, 
                                    impressions$latitude, 
                                    impressions$longitude ) )

# print(nrow(uniqueGlobe))
# print(nrow(uniqueZip))


# findShortest uses the Distances.R file to obtain the closest
# weather station to a zipcode, which is given as input. The Weather Data
# is obtained from the NOAA data.
#It accepts a row of zip code as input. Then it runs the distance method on 
#latitude and longitude
#of that zip code amidst all of the data of uniqueGlobe. This returns a 
#vector of distances. USing order, I find the smallest one and return its
#WBAN no, or station id
#
findShortest <-  function( zipcode) {
        
        dist <- distance( zipcode$impressions.latitude, 
                          zipcode$impressions.longitude, 
                          uniqueGlobe$globe.Latitude, 
                          uniqueGlobe$globe.Longitude)
#         print(dist) #Testing
#         print(order(dist)) #Testing

        #       The 1 in the column is the WBAN number, which is what I  want
        uniqueGlobe[ ( order( dist )[1] ),  1 ]
        
}# end findShortest <-  function( zipcode)


# createHash() creates a dataframe between the zip codes and the closest station  
# Id. It makes use of findShortest(...) and ldply() to achieve its objective.
# findShortest returns the WBAN no of the station that is closest to 
# a given zip code. This is then added to a list whose index is the zip code
# itself. Finally, using ldply, I convert the entire list into a dataframe, and
# then save it to a csv file.
#
createHash <- function( ){
        ultmtHash <- list()
#         j=0
        for (i in 1:nrow( uniqueZip ) ){
#                 print("i: ") #Testing
#                 print(i) #Testing
#                 print("Corresponding ZipCode: ") #Testing
#                 print(uniqueZip[i,]$impressions.ZIP.Postal.Code) #Testing
                
                #the index of the list is the Zip Code
                ultmtHash[[ paste( uniqueZip[i,'impressions.ZIP.Postal.Code'] ) ]] <- findShortest( uniqueZip[i,] )
                
#                  j <- j + 1 #Testing
#                 if ( j == 8 ) { #Testing
#                         break #Testing
#                 }#Testing
        }
        
        
#         print("THE HASH TABLE:") #Testing
#         print(ultmtHash) #Testing
        
        #Converting the list into a data frame
        new_dataframe <-  ldply( ultmtHash )
        write.csv(new_dataframe,file = "ZipWBAN_Hash.csv", row.names = F)
        print("Done")  
} # end createHash <- function( )

# createHash()

# gumbo <-  read.csv("ZipWBAN_Hash.csv", header = T, colClasses = c("character","integer"))
# names(gumbo) <- c( "Zip Code", "WBAN")
# write.csv(gumbo,file = "ZipWBAN_Hash.csv", row.names = F)

