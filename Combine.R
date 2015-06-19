#Name: Jimson Mathew 
# File Name: Combine.R
# Date: 6/18/2015
# Summary: This program takes in the location of the directory in which
# the NOAA weather data are stored and returns a dataframe that contains 
# the data of all of the other data frames one below the other
# 
# 

#This is the file that I want to download
particulars = "daily.txt"

#
#Having this package allows me to combine all of my dataframes into
#one dataframe.
#
install.packages("plyr")
library(plyr)


#
#This program takes in the location of the directory in which 
# the NOAA weather data are stored and outputs a dataframe that contains 
# the data of all of the other data frames one below the other.
# To do this, I will use the ldply mthod and will make a list of dataframes.
# The ldplay method will join my dataframes one below the other.
#
collect <- function (fLocation) {
      #The empty list      
      mData <- list()
#       i=0 #For testing purposes

#       This loop loops through the folders located in the file location and looks
#       for files with the patten QCLD*. Then, it sets the working directory
#       to the name of the file in order to access the dataframe. 
#       Since the a substring of the name of the dataframe has the name of the 
#       folder in the txt file,  I will combine the substring with the desired
#       file name, i.e. the variable particulars.
#       I will append the dataframe to mData until the loop ends.      
# 
#       
      for (name in list.files( path = fLocation, pattern ="QCLD*") ){
        print(name)
        setwd( paste(fLocation,name,sep="/") )
        filename <- substr( name, 6, nchar( name ) )
        #reading in the dataframe
        sample <- read.csv( file = paste( filename, particulars, sep="" ), header = T )
        #appending the dataframe to mData
        mData[[length(mData) + 1]] <- sample
        #Returning to the directory where my files are stored
        setwd(fLocation)
#         i <- i+1  #Test case
#         if (i == 2){break}  #Test case
      }
      
#   Using the ldply method, I am able to convert the list of dataframes into 
#   one big dataframe with all of the elements of the lists added in the
#   desired order, viz. one below the other.     
       new_dataframe <-  ldply(mData)
       #I don't need this, but I'm placing this here in case 
       setwd(fLocation)
       #writing this dataframe to a text file
       write.table(new_dataframe, file = "WeatherData.txt", sep=",", row.names = FALSE )
       print ("Done!")
       
}#end of collect(fLocation)

collect( "/Users/Mathew/Desktop/Project/Files" )