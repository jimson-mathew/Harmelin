# Name : Jimson Mathew
# Date: Since about July 7. Then July 14, 16 - Aug 7 (with some breaks in between.)
# This file examines the HArmelin data and seeks to make sense of the 
# same.
# 
# removing the default variables in Rstudio's workspace.
rm(  fact, x, y, m )
setwd("Desktop/Project/")
# This is the file on wich I want to train my predictors, if any.
temp3 <-readRDS("/Users/Mathew/Desktop/Project/Files/NONAValuesHarmelinHasTheClosestWeatherStation.rds")

# Obtaining random values in order to make the training set, development
# set, and the test set.
# 
x = sample(1:nrow(temp3), ceiling( 0.4 * ( nrow(temp3) ) ))
test <-temp3[x,]
train <-temp3[-x,]

x = sample(1:nrow(test), ceiling( 0.2 * ( nrow(test) ) ))
dev  <- test[x,]
test <- test[-x,]

# Saving the same
saveRDS(train, "TRAININGSET-NONAValuesHarmelinHasTheClosestWeatherStation.rds")
saveRDS(dev, "DEV-NONAValuesHarmelinHasTheClosestWeatherStation.rds")
saveRDS(test, "TEST-NONAValuesHarmelinHasTheClosestWeatherStation.rds")

# removing these variables from my workspace.
rm(temp3)
rm(test, dev)

# reading the Training set.
train <- readRDS("TRAININGSETNONAValuesHarmelinHasTheClosestWeatherStation.rds")
copy <- train
rm(train)


library( data.table )
?setDT

DTcopy <- setDT( copy )
# str(DTcopy)

#  rmnames <- c( "ClimateDivisionStateCode", "Max2DirFlag", "Max2SpeedFlag", 
#                "AvgSpeedFlag", "Max5SpeedFlag", "ResultDirFlag", 
#                "ResultSpeedFlag", "SeaLevelFlag", "StnPressureFlag", 
#                "PrecipTotalFlag", "SnowFallFlag", "Water1Flag", "DepthFlag", 
#                "CodeSumFlag", "SunsetFlag", "SunriseFlag", "CoolFlag", 
#                "HeatFlag", "WetBulbFlag", "DewPointFlag", "DepartFlag", 
#                "TavgFlag", "TminFlag", "TmaxFlag", "YearMonthDay")

# Selecting features that I do not understand or do not believe can contribute to
# our goal.
# 
DTcopy[,  c( "ClimateDivisionStateCode", "Max2DirFlag", "Max2SpeedFlag", 
             "AvgSpeedFlag", "Max5SpeedFlag", "ResultDirFlag", 
             "ResultSpeedFlag", "SeaLevelFlag", "StnPressureFlag", 
             "PrecipTotalFlag", "SnowFallFlag", "Water1Flag", "DepthFlag", 
             "CodeSumFlag", "SunsetFlag", "SunriseFlag", "CoolFlag", 
             "HeatFlag", "WetBulbFlag", "DewPointFlag", "DepartFlag", 
             "TavgFlag", "TminFlag", "TmaxFlag", "YearMonthDay"):= NULL]
rm(copy)
copy  <- setDF( DTcopy )

# # # # # # # # # # Speed test on lm # # # # # # # # # # 

print("10 elements")
system.time( guess <- lm( formula = Impressions ~ Tmax, data = copy[1:10,] ) )

print("100 elements")
system.time( guess <- lm( formula = Impressions ~ Tmax, data = copy[1:100,] ) )

print("1000 elements")
system.time( guess <- lm( formula = Impressions ~ Tmax, data = copy[1:1000,] ) )

print("10000 elements")
system.time( guess <- lm( formula = Impressions ~ Tmax, data = copy[1:10000,] ) )

print("100,000 elements")
system.time( guess <- lm( formula = Impressions ~ Tmax, data = copy[1:100000,] ) )

print("1,000,000 elements")
system.time( guess <- lm( formula = Impressions ~ Tmax, data = copy[1:1000000,] ) )

print("10,000,000 elements")
system.time( guess <- lm( formula = Impressions ~ Tmax, data = copy[1:10000000,] ) )
# # # # # # 


# I noticed that R converted a lot of the columns into facors. This considerably
# increased the computation time of lm. So, I am removing the factors from those
# columns tht are NOT supposed to have factors
# 
copy$Tmax <- as.numeric( as.character(copy$Tmax) )
copy$Tmin <- as.numeric( as.character(copy$Tmin) )
copy$Tavg <- as.numeric( as.character(copy$Tavg) )
copy$DewPoint <- as.numeric( as.character(copy$DewPoint) )
copy$Sunrise <- as.numeric( as.character(copy$Sunrise) )
copy$Sunset <- as.numeric( as.character(copy$Sunset) )
copy$SnowFall <- as.numeric( as.character(copy$SnowFall) )
copy$PrecipTotal <- as.numeric( as.character(copy$PrecipTotal) ) 
copy$StnPressure <- as.numeric( as.character(copy$StnPressure) )
copy$SeaLevel <- as.numeric( as.character(copy$SeaLevel) )
copy$ResultSpeed <- as.numeric( as.character(copy$ResultSpeed) )
copy$ResultDir <- as.numeric( as.character(copy$ResultDir) )
copy$AvgSpeed <- as.numeric( as.character(copy$AvgSpeed) )

system.time( fit <- lm( formula = Impressions ~ Tavg, copy ) )
summary(fit)
AIC(fit)

#  Plotting the relationship between Tavg and Impressions
install.packages("ggplot2")
library(ggplot2)
line.Tavg <- ggplot( copy[1:100000,], aes(x = Tavg, y = Impressions  ) )
line.Tavg + geom_point()  + geom_smooth(method=lm, color = "red") +
        coord_cartesian(xlim = c(40,80), ylim = c(0, 1000)) 


install.packages( "speedglm" )
library( speedglm )

#  Fitting a linear model between  Tavg, DewPoint, 
#  PrecipTotal, SnowFall, Sunrise, Sunset, 
# ResultSpeed, and Impressions
system.time( fit <- lm( formula = Impressions ~ Tavg + DewPoint 
                        + PrecipTotal + SnowFall + Sunrise + Sunset +
                                ResultSpeed, copy ) )
summary(fit)
AIC(fit)

# Doing the above with speedlm , since it is faster.
system.time( fit <- speedlm( formula = Clicks ~ Tavg + DewPoint + 
                                     PrecipTotal + SnowFall +
                                     Sunrise + Sunset + ResultSpeed
                             , copy ) )
summary(fit)
AIC(fit)


# Converting CodeSum to a character
copy$CodeSum <- as.character( copy$CodeSum )


# Since data tables are much faster, I will use them to make modifications to 
# my data frame.
library(data.table)
# for the sake of testing.
DTcopy <- setDT( copy[1:10000,] )

#  
#  I felt CodeSum would have values in it that would point me to some predictors. 
#  Thus, I plan to create columns that have binary values in them. 
#  In order to do this, I will use a regexpression.
#  
library(plyr)
# 
# This method accepts a dataframe. Then it looks for instances of various codes. 
# It creates a separate data frame with all zeros and places ones wherever 
#  Code Sum has a given code.
#  
# 
sepWeatherCodes <- function ( dframe ) {
#          print( typeof(dframe) )
#          print( "length of the parameter")
#          print( length(dframe) )
#          print("first  CodeSum Value ")
#          print( dframe[1,"CodeSum"] )
        
        # The regsexpression.
        regString <-c("VC|MI|BC|PR|TS|BL|SH|DR|FZ|DZ|RA|SN|SG|IC|PL|GR|GS|UP|BR|FG|FU|VA|SA|HZ|PY|DU|SQ|SS|DS|PO|FC|\\+FC|\\+|\\-")
        
        #  this empty datatable will be joined to the dframes. This will contain
        # the list of ones and zeros baased on CodeSum
        tempReg <- data.table(  VC = rep(0, nrow( dframe )), MI = rep(0, nrow( dframe )),
                                BC = rep(0, nrow( dframe )), PR = rep(0, nrow( dframe )),
                                TS = rep(0, nrow( dframe )), BL = rep(0, nrow( dframe )),
                                SH = rep(0, nrow( dframe )), DR = rep(0, nrow( dframe )), 
                                FZ = rep(0, nrow( dframe )), DZ = rep(0, nrow( dframe )),
                                RA = rep(0, nrow( dframe )), SN = rep(0, nrow( dframe )),
                                SG = rep(0, nrow( dframe )), IC = rep(0, nrow( dframe )),
                                PL = rep(0, nrow( dframe )), GR = rep(0, nrow( dframe )),
                                GS = rep(0, nrow( dframe )), UP = rep(0, nrow( dframe )),
                                BR = rep(0, nrow( dframe )), FG = rep(0, nrow( dframe )),
                                FU = rep(0, nrow( dframe )), VA = rep(0, nrow( dframe )),
                                SA = rep(0, nrow( dframe )), HZ = rep(0, nrow( dframe )),
                                PY = rep(0, nrow( dframe )), DU = rep(0, nrow( dframe )),
                                SQ = rep(0, nrow( dframe )), SS = rep(0, nrow( dframe )),
                                DS = rep(0, nrow( dframe )), PO = rep(0, nrow( dframe )),
                                FC = rep(0, nrow( dframe )), PLUSFC = rep(0, nrow( dframe )),
                                PLUS = rep(0, nrow( dframe )), MINUS = rep(0, nrow( dframe )) )
        print("About to gregexpr")
        # obtaining the positions of matches
        findings <- gregexpr(regString, text = dframe$CodeSum) 
        print("About to get the unique values")
        #  getting the matching codes.
        p <- unique (regmatches( dframe$CodeSum , findings)[[1]] )
        # p <- unique (regmatches( dframe$CodeSum , findings)[[1]] )
        print("About to place the 1s")
        
        # for every code in list p, I will assign a one in tempReg
        for ( i in p) {
                # Special cases due to the plus and minus signs
                if ( i == "+FC" ){ 
                        tempReg[["PLUSFC"]] <- 1
                }
                else if ( i == "+" ){
                        tempReg[["PLUS"]] <- 1}
                
                else if ( i == "-" ) {
                        tempReg[["MINUS"]] <- 1
                }
                else {
                        tempReg[[i]] <- 1 
                }
        }
        print("Cbinding for my benefit")
        # joining dfrme and tempReg
        new <- cbind(dframe, tempReg)
        # returning the new data table
        new
        
}

# Converting CodeSum to character( Not sure why I did it twice. )
copy$CodeSum <- as.character( copy$CodeSum )
# converting copy into a data table
DTcopy <- setDT( copy )
# calling sepWeatherCodes via ddply
system.time ( jill <- ddply(.data = DTcopy,"CodeSum" ,.fun = sepWeatherCodes) )
saveRDS(jill, "trainingCodeSpread.rds")
jill <- readRDS("trainingCodeSpread.rds")
table( jill$Impressions, jill$VC)


library(pastecs)
stat.desc(cbind( jill$Impressions, jill$Clicks, jill$VC ) )

library(ggplot2)

# 
# The following ggplot code involves the examination of the various variables
# in jill.
# 
# jill$Impressions, jill$Clicks, jill$Tmax
line.Tavg <- ggplot( copy[1:100000,], aes(x = Tmax, y = Impressions  ) )
line.Tavg + geom_point()  
#  + geom_smooth(method=lm, color = "red") +
#          coord_cartesian(xlim = c(40,80), ylim = c(0, 1000)) 

line.Tavg <- ggplot( jill[1:1000,], aes(x = WetBulb, y = Impressions ) )
line.Tavg + geom_point()  + geom_point( aes(x = DewPoint, colour = "DewPoint" ) ) +
        coord_cartesian( ylim = c(0, 1000) ) + ggtitle(" Zoomed Dew Point, Tavg vs Impressions")
geom_point( aes(x = Tmin, colour = "Tmin" ) ) +
        coord_cartesian( ylim = c(0, 1000) ) +
        ggtitle(" Zoomed Tavg, Tmin, Tmax vs Impressions")
click.Tavg <- ggplot( jill[1:1000,], aes(x = WetBulb, y = Clicks  ) )
click.Tavg + geom_point()   + 
        ggtitle("Zoomed WetBulb vs Clicks ")
+ geom_point( aes(x = Tavg, colour = "Tavg" ) ) +
        geom_point( aes(x = Tmin, colour = "Tmin" ) ) + 
        coord_cartesian( ylim = c(0, 250) ) +
        ggtitle(" Zoomed Tavg, Tmin, Tmax vs Clicks")


jill$WetBulb <- as.numeric( as.character( jill$WetBulb ) )


line.Tavg <- ggplot( jill[1:1000,], aes(x = WetBulb, y = Impressions ) )
line.Tavg + geom_point() + 
        geom_point( aes(x = Tavg, colour = "Tavg" ) ) +
        ggtitle(" Zoomed WetBulb, Tavg vs Impressions") + coord_cartesian(ylim = c( 0, 1000 ))

click.Tavg <- ggplot( jill[1:1000,], aes(x = DewPoint, y = Clicks  ) )
click.Tavg + geom_point(position = "jitter")   + coord_cartesian( ylim = c(0,250) ) +
        ggtitle("Zoomed - vs Clicks ") + geom_smooth(method = "lm")


install.packages( "corrplot" )
library(corrplot)
str(jill)


completeJ <-  jill[ complete.cases( jill[ c( 11:13, 15:16, 19:20, 24:30 , 49:82 )  ]  ), c( 11:13, 15:16, 19:20, 24:30 , 49:82 )  ]

M <- cor( completeJ )
corrplot(M, method = "circle")


#  binarize a spot smal, middle and large.
#  pick pout fe variables, run cor --> coorelation matrix
#  plot, get a matrix
#  
#  plot correlation matrix



# Examining the relaionship between Dew Points and Clicks
click.Tavg <- ggplot( jill[1:1000,], aes(x = DewPoint, y = Clicks  ) )
click.Tavg + geom_point(position = "jitter")   + coord_cartesian( ylim = c(0,250) ) +
        ggtitle("Zoomed - vs Clicks ") + geom_smooth(method = "lm")

# 
# This method will plot the variables input, impClick on the dataframe df.
# imput and impClick are strings that will be passed to the method. These strings
# represent columns in df.
# sizeN is the number of randomn observations that ought to be taken from df in plotting 
# the graphs.
# If the column input is of type logical, I will plot a historgram with a density
# Otherwise, a scatter plot will be generated.
# 
plotting <- function ( input, df, imPClick, sizeN ) {
        if (missing(input))
                stop("Need to specify y-axis variable for calculations.")
        else if (missing(df))
                stop("Need to specify data Frame for calculations.")
        else if (missing(imPClick))
                stop("Need to specify x-axis variable for calculations.")
        else if (missing(sizeN))
                stop("Need to specify number of elements for calculations.")
        
        
         library( ggplot2 )
        # 
        # Using sample, I will generate a column of row numbers in a randomn 
        # order. The size of this column is based on sizeN.
        nos <- sample( nrow( df ), sizeN )
        if ( typeof( df[[input]] ) == "logical" ) {
                # generating a histogram
                h <- ggplot( df[nos,], aes_string( x = imPClick, y = "..density.." ) ) + geom_histogram(  aes_string( color = input )  )
                h
        }
        else {
                # generating a scatterplot.
                h <- ggplot( df[nos,], aes_string( y = imPClick, x = input ) ) + geom_point()
                h
        }
        
}#  end plotting <- function ( input, df, imPClick, sizeN )

#  Converting the Code Columns from double to logical.
system.time( jill[c(49:82)] <- lapply( jill[c(49:82)], function( x ) x == 1  )   )

# Testing plotting and also analyzing the relationship between columns in jill
# and Impressions and Clicks
plotting("Tavg", jill, "Impressions", 10000 ) + geom_smooth( method = lm ) +
        coord_cartesian( ylim = c( 0, 20000 ) ) +
        ggtitle("Zoomed Relationship between Tavg and Impressions for 10,000 elements")
plotting("DewPoint", jill, "Impressions", 10000 ) + coord_cartesian( ylim = c( 0, 2500 ) ) +
        geom_smooth( method = lm ) + ggtitle("Zoomed Relationship between DewPoint and Impressions")
plotting("DewPoint", jill, "Tavg", 10000 ) + 
        ggtitle("Relationship between DewPoint and Tavg")
plotting("Tavg", jill, "Clicks", 100000 ) + 
        coord_cartesian( ylim = c( 0.5, 200) ) + scale_y_log10("Log Clicks")
h <- ggplot(aes(x=log10(Impressions)), data = jill ) + geom_histogram(binwidth=.025)
h 
ggtitle("Zoomed Relationship between Tavg and Impressions for 10,000 elements")
plotting("DewPoint", jill, "Impressions", 10000 ) + coord_cartesian( ylim = c( 0, 2500 ) ) +
        geom_smooth( method = lm ) + ggtitle("Zoomed Relationship between DewPoint and Impressions")
plotting("DewPoint", jill, "Tavg", 10000 ) + 
        ggtitle("Relationship between DewPoint and Tavg")


# 
# When we plotted, we realized that since we had different dates for a given
# zip code, this resulted in a non normal distribution. In order to ensure this,
# we aim to add weights between two desired columns. 
# 
# In order to do this, I will order my data based on the zip code and the Date.
# To do this, I will convert my dataframe into a data table.
# 
jill <- setDT( jill )
setorder(jill, ZIP.Postal.Code, Date )
jill <- setDF( jill )

# 
# This method adds weights to the columns of a dataframe:
# col1 and col2: Strings that specify column names that are ALREADY present in 
# the data TABLE df.
# 
# the method will note down the differences of elements that are consecutive 
# based on date. (I was able to achieve his earlier using set order.) These
# differences will be added as seprate columns to df and 
# a copy of df will be returned to 
# the calling method
# 



addDeltas <- function( col1, col2, df ){
#          print( "In addDeltas" )
        # Checking whether col1 and col2 are column names in data table df.
        # 
        # 
        if (  !(col1 %in% names( df ) ) | !( col2 %in% names( df ) )  ) {
                stop ( "col1 and col2 must be in the passed data frame/table. ")
        }
        # Checking that df is a data table
        if( is.data.table( df ) ) {
                # Obtaining the differences between the dats
                df$DateDiff <- c( NA, diff(df$Date) )
                # any DateDiff that is equal to 1 is part of a consecutive
                # set of dates and is given True
                df$DateDiff <- sapply( df$DateDiff , function( x ) x == 1  )
                # Computing the consecutive differences between the columns in df
                df[, c( ( (paste(col1, c("Diff"), sep="_") ) ),
                        ( (paste(col2, c("Diff"), sep="_") ) ) )  := 
                           list(  c(NA,diff( df[,get(col1)] )),
                                  c(NA, diff( df[,get(col2)] )  ) )  ]
                # placing NAs in columns in places where DateDiff is False, i.e.
                # not consecutive, or the starting of a consecutive case.
                df[ !df$DateDiff, c(  c(paste(col1, c("Diff"), sep="_") ) ,
                                      c(paste(col2, c("Diff"), sep="_") ) )   := NA  ]
                # deleting DateDiff from df
                df[, DateDiff := NULL ]
                # putting df in m
                m <- df
                # returning m
                m
        }# end if( is.data.table( df ) ) 
}#  end addDeltas <- function( col1, col2, df )

# 
# Now, jill is a dataframe. Since addDeltas requires a data table, I decided to create
# a data frame copy of the same (jack) in case something might go wrong with jill.
# 
jack <- jill
jill <- setDT(jill)
system.time( deltas <- addDeltas( "Impressions", "Tavg", jill)  )
saveRDS( deltas, "trainingData with Deltas for Tavg and Impressions 7-14-2015.RDS" )
deltas <- readRDS("trainingData with Deltas for Tavg and Impressions 7-14-2015.RDS")


# 
# Hank mentioned that since we chose our data randomly to plot, a lot of NAs 
# would remain, inspite of the weights. Thus, we decided to do this: either 
# randomly choose a month and 
# group the data based on a month, or based on zip codes. I decided to make a 
# function for both and see which option would lead in a larger number of NAs. 
# 
library(lubridate)
# this will be used later in plottingMonth
deltas$Month <- month( deltas$Date )
# this will be used later in plottingZips
uniqueZips <- unique(deltas$ZIP.Postal.Code)

# 
# This month is simillar to plotting in that instead of sizeN, it 
# plots a graph (histogram or scatter plot) based on a random month. Thus,
# I subset the data table df based on the random month chosen and will display 
# the graph.
# 
plottingMonth <- function ( input, df, imPClick ) {
        # Error case
        if( !is.data.table( df ) ) {
                stop (" Greetings Human. You mahve made an error: df MUST be a DataTable.")
        }
        
        library( ggplot2 )
        # choosing a random month
        temp <- sample( 1:12 )
        tempDT <- subset( df, Month == temp )
        df <- setDF( tempDT ) # not tested but shuld work
#          
#          print( "the Number of NAs in Impressions_Diff" )
#          print( sum( is.na( tempDT$Impressions_Diff ) ) )
#          print( "No. of values" )
#          print( nrow(tempDT) )

        #  PLease NOTE the following code has not been tested. The previous print 
        #  statements were sufficient to help us infer something new about our data set.
        if ( typeof( df[[input]] ) == "logical" ) {
                print("in the histogram if")
                h <- ggplot( df[nos,], aes_string( x = imPClick, y = "..density.." ) ) + geom_histogram(  aes_string( color = input )  )
                h
        }
        else {
                print("in points")
                h <- ggplot( df[nos,], aes_string( y = imPClick, x = input ) ) + geom_point()
                h
        }
        
}
# 
# This month is simillar to plotting in that instead of sizeN, it 
# plots a graph (histogram or scatter plot) based on a random zip code Thus,
# I subset the data table df based on the random month chosen and will display 
# the graph.
# 
plottingZips <- function ( input, df, imPClick) {
        library( ggplot2 )
        # selecting a random zip code
        temp <- sample(uniqueZips)[1]
        tempDT <- subset( df, ZIP.Postal.Code == temp )
#          print( "the Number of NAs in Impressions_Diff" )
#          print( sum( is.na( tempDT$Impressions_Diff ) ) )
#          print( "No. of values" )
#          print( nrow(tempDT) )
        
        #  PLease NOTE the following code has not been tested. The previous print 
        #  statements were sufficient to help us infer something new about our data set.
        
        #          if ( !xor (imPClick != "Impressions" , imPClick != "Clicks" ) )
        #                  stop( "imPClick can only take value: Impressions OR Clicks" )
        df <- setDF( tempDT )
        if ( typeof( df[[input]] ) == "logical" ) {
                print("in the histogram if")
                h <- ggplot( df[nos,], aes_string( x = imPClick, y = "..density.." ) ) + geom_histogram(  aes_string( color = input )  )
                h
        }
        else {
                print("in points")
                h <- ggplot( df[nos,], aes_string( y = imPClick, x = input ) ) + geom_point()
                h
        }
        
}

# 
# Testing the above methods and observing the number of NAs in order to choose
# what grouping was favorable.
# 
# 
for( i in 1:10){
        print ( plottingMonth("Tavg", deltas, "Impressions") )
        print("# # # # # # # # # # # # # # # ")
}
print("*********************************************")
for( i in 1:10){
       print ( system.time( plottingZips("Tavg", deltas, "Impressions") ) )
        print("@@@@@@@@@@@@@@@@@@@!!!!!!!!!!@@@@@@@@@@@@@@@@@@@@@@@@@")
}

# 
# Here I am going to figure out the total no. of Impressions from a certain Zip Code.
# I want to find out which Zip Codes have a large number of Impressions.
# I want to set a threshold by which I can delete other data.
# 
a <- aggregate( Impressions ~ ZIP.Postal.Code + latitude + longitude + Year, data = deltas, FUN =  sum )
b <- a[order(-a$Impressions),]

# Using ggmaps, I want to see which zip codes have the most Impressions.
library(data.table)

b <- setDT( b )
tempDelDF <- setDF( subset(x = b, Impressions >= 10000 ) )
library( ggplot2 )
library( ggmap )
map <- get_map(location = "pennsylvania", zoom = 7)
mapPoints <- ggmap(map) +
        geom_point(aes(x = longitude, y = latitude, size = (Impressions)),
                   data = tempDelDF, alpha = 1) 
mapPoints + ggtitle("Pennsylvania Plot of Impressions")


library(lubridate)
# this will be used later in plottingMonth
deltas$Year <- year( deltas$Date )
deltas[,Month:= month( Date )]
a <- aggregate( Impressions ~ ZIP.Postal.Code + latitude + longitude + Year + Month, data = deltas, FUN =  sum )
b <- a[order(-a$Impressions),]

b <- setDT(b)
tempDF2013 <- subset(b, (Impressions >= 10000 & Month <= 5 & Year == 2013) )
tempDF2014 <- subset(b, (Impressions >= 10000 & Month <= 5 & Year == 2014) )
tempDF2015 <- subset(b, (Impressions >= 10000 & Month <= 5 & Year == 2015) )
map <- get_map(location = "pennsylvania", zoom = 7)
mapPoints <- ggmap(map) +
        geom_point(aes(x = longitude, y = latitude, size = (Impressions)),
                   data = tempDF2015, alpha = 1) 
mapPoints + ggtitle("Pennsylvania Impressions 2015")
nrow( tempDF2013 )
nrow( tempDF2014 )
nrow( tempDF2015 )


# I will bring the original daaset back into the workspace, make all of the above changes.
# Then, I will keep only those observations that have impressions of at least 10,000.
# After that, I will group again on zip code.
# I have read temp3, but then placed temp3 into the variable 
# copy and executed everything from there.
saveRDS(jill, "CODESPREADNONAValuesHarmelinHasTheClosestWeatherStation.rds")
saveRDS(deltas,"DELTASTavgCODESPREADNONAValuesHarmelinHasTheClosestWeatherStation.rds")
deltas <- readRDS("/Users/Mathew/Desktop/Project/Files/DELTASTavgCODESPREADNONAValuesHarmelinHasTheClosestWeatherStation.rds")
# I forgot to write this earlier, but I included the following now :).
# Now, I will weed out those Zip Codes that have Impressions < 10,000
# I will compute a and b again here because I like keeping this side compact
# After that, I will delete from b Impressions that are < 10,000.
# Then I will give the zipcodes in b to uniqueZips.
# 
a <- aggregate( Impressions ~ ZIP.Postal.Code , data = deltas, FUN =  sum )
b <- a[order(-a$Impressions),]
b <- setDT(b)
b <- subset( b, Impressions >= 10000 )
uniqueZips <- b$ZIP.Postal.Code

#
# In the following lines of code, I aim to make training, test, and development 
# tests. In order to do so, I will create a vector that will contain the random 
# proportion of zipcodes. After that, I will use lapply and then subset the
# concerned observations from the data table and append them to a list. Finally,
# I will run it. 
library(plyr)
?llply

x <- sample(1:length(uniqueZips), ceiling( 0.4 * ( length(uniqueZips) )) )
testZip <-uniqueZips[x]
trainZip <-uniqueZips[-x]

x = sample(1:length(uniqueZips), ceiling( 0.2 * ( length(uniqueZips) ) ))
devZip  <- testZip[x]
testZip <- testZip[-x]

# Making the training set
system.time( train <- subset(deltas, ZIP.Postal.Code %in% trainZip  ) )

# Making the dev set.
system.time( dev <- subset(deltas, ZIP.Postal.Code %in% devZip  ) )

# Making the test set.
system.time( test <- subset(deltas, ZIP.Postal.Code %in% testZip  ) )

#
# Saving the training, deltas, and training sets.
#
saveRDS( train, "TRAINING SETDELTASTavgCODESPREADNONAValuesHarmelinHasTheClosestWeatherStation.RDS" )
saveRDS( dev, "DEVELOPMENT SETDELTASTavgCODESPREADNONAValuesHarmelinHasTheClosestWeatherStation.RDS" )
saveRDS( test, "TEST SETDELTASTavgCODESPREADNONAValuesHarmelinHasTheClosestWeatherStation.RDS" )

train <- readRDS("TRAINING SETDELTASTavgCODESPREADNONAValuesHarmelinHasTheClosestWeatherStation.RDS")
demo <- train[1:5,]

# Plotting the correlation of deltas as a heatmap.
# http://www.r-bloggers.com/using-r-correlation-heatmap-take-2/
library(ggplot2)
library(reshape2)
qplot(x=Var1, y=Var2, data=melt(cor(train[ c( 11:13, 15:16, 19:20, 24:30 , 49:84 )  ],
                                    use="p")), fill=value, geom="tile") + 
        scale_fill_gradient2(limits=c(-1, 1)) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Examining the relataionship between the delta temperatures and Impressions
plotting("Tavg_Diff", train, "Impressions_Diff", 100000) + geom_smooth(method = lm, color = "red")

# Examining the relationship between Impressions and the continous variables
train.model.Impressions <- lm ( Impressions_Diff ~ Tavg_Diff +  PrecipTotal + 
                                        ResultDir + SnowFall, train)
summary( train.model.Impressions )
AIC( train.model.Impressions )


#  By now, we relaized that our predictors may not be good, and we probably
# have to add new ones. One of the ones, we're adding is percentage 
# differences of Impressions.
# 

# 
# To calculate percentage differences, I will use the shift(). This is not 
# available in 1.9.4, but is present in the development version:
# I am using the development version of data.table: 1.9.5. The most recent 
# version of this file as of 7/30/2015: 1.9.4
install.packages("devtools")
library(devtools)
install_github("Rdatatable/data.table", build_vignettes = FALSE)
library(data.table)

# Impressions_Diff contains the differences between the consective elements.
# In order to calculate the percentage diffrences, I have to divide the 
# differences by the original value of the Impressions. Then, I multiply this
# answer by 100 in order to get the answer.
# 
system.time(train[, Prcnt_Change_Impressions := 
                          (Impressions_Diff/shift(Impressions, n = 1L, type =
                                                          "lag")) * 100 ] )

spend <- read.csv("6643_query_20150729_162039_144571641.csv", header = T,
                  colClasses = c("character", "character", "character", "double",
                                 "double", "double"),stringsAsFactors = F, skip = 8 )
spend <- setDT(spend)
spend$Date <- as.Date(spend$Date,format( "%Y-%m-%d"))
saveRDS(spend,"spend.RDS")
library(zipcode)
spend$ZIP.Postal.Code <- clean.zipcodes( spend$ZIP.Postal.Code )
saveRDS(spend,"spend.RDS")
spend <- readRDS("spend.RDS")

plotting("Tavg_Diff", df = train,"Prcnt_Change_Impressions", 100000)

remove.packages("data.table")         # First remove the current version
install.packages("data.table")        # Then install the CRAN version
library(data.table)

spend <- setDT(spend)
# Removing the last rows that contains the Grand Total.
spend <- subset(spend, !is.na(Date))
setorder(spend, ZIP.Postal.Code, Date )

is.data.table(train)
is.data.table(spend)

# We decided to run a LEFT JOIN on deltas and spend. When we did it, the result 
# showed a problem. The result of the merge was larger than deltas. Thus, we 
# decided to aggregate on the Zip Code and the Date. We assumed that for some
# values of the left Join, there were more than one value of spend. So, this 
# could be a reason for the extra value.
# We preserved the commented code in order to make sense of why we added the 
# aggregate method, otherwise it would be plain Crae to have it there without any
# explanation
# 
# temp <- merge(deltas, spend[, c(1,2,6), with = F], by= c("ZIP.Postal.Code", "Date"),
#               all.x = T, all.y = F)
# 

 
# Refer to the previous comment why the aggregate the method was done.
system.time( jack <- aggregate( Media.Cost ~ ZIP.Postal.Code + Date, spend, sum ) )

jack <- setDT( jack )

# Merging the spend Cost with the deltas data set based on Zip Code and Date.
temp <- merge(deltas, jack, by= c("ZIP.Postal.Code", "Date"),
              all.x = T)
deltas <- temp

# THE SPLITTING OF deltas INTO THE test, dev, AND train datasets 
# is COPIED FROM LINES 576 - 618.
# 
# 
# Now, I will weed out those Zip Codes that have Impressions < 10,000
# I will compute a and b again here because I like keeping this side compact
# After that, I will delete from b Impressions that are < 10,000.
# Then I will give the zipcodes in b to uniqueZips.
# 
a <- aggregate( Impressions ~ ZIP.Postal.Code , data = deltas, FUN =  sum )
b <- a[order(-a$Impressions),]
b <- setDT(b)
b <- subset( b, Impressions >= 10000 )
uniqueZips <- b$ZIP.Postal.Code

#
# In the following lines of code, I aim to make training, test, and development 
# tests. In order to do so, I will create a vector that will contain the random 
# proportion of zipcodes. After that, I will use lapply and then subset the
# concerned observations from the data table and append them to a list. Finally,
# I will run it. 

x <- sample(1:length(uniqueZips), ceiling( 0.4 * ( length(uniqueZips) )) )
testZip <-uniqueZips[x]
trainZip <-uniqueZips[-x]

x = sample(1:length(uniqueZips), ceiling( 0.2 * ( length(uniqueZips) ) ))
devZip  <- testZip[x]
testZip <- testZip[-x]

# Making the training set
system.time( train <- subset(deltas, ZIP.Postal.Code %in% trainZip  ) )

# Making the dev set.
system.time( dev <- subset(deltas, ZIP.Postal.Code %in% devZip  ) )

# Making the test set.
system.time( test <- subset(deltas, ZIP.Postal.Code %in% testZip  ) )

#
# Saving the training, deltas, and training sets.
#
saveRDS( train, "TRAINING SET DELTAS_WITH SPEND COST.RDS" )
saveRDS( dev, "DEVELOPMENT SET DELTAS_WITH SPEND COST.RDS" )
saveRDS( test, "TEST SET DELTAS_WITH SPEND COST.RDS" )


# Examining the relationship between Impressions_Diff and the continous variables
train.model.Impressions <- lm ( Impressions_Diff ~ Tavg_Diff +  PrecipTotal + 
                                        ResultDir + SnowFall + MI + SH + DR +
                                        SG + IC +  GR + GS + FU + VA + SA +
                                        DU + SQ + DS + BC + PR, train)
summary( train.model.Impressions )
AIC( train.model.Impressions )

# Examining the relationship between Impressions and the continous variables
train.model.Impressions <- lm ( Impressions ~ Tavg+  PrecipTotal + 
                                        ResultDir + SnowFall + MI + SH + DR +
                                        SG + IC +  GR + GS + FU + VA + SA +
                                        DU + SQ + DS + BC + PR, train)
summary( train.model.Impressions )
AIC( train.model.Impressions )



# Examining the relationship between Impressions_Diff and the continous variables
train.model.Impressions <- lm ( Impressions_Diff ~ Tavg_Diff +  PrecipTotal + 
                                        ResultDir + SnowFall + MI + SH + DR +
                                        SG + IC +  GR + GS + FU + VA + SA +
                                        DU + SQ + DS + BC + PR + Media.Cost, train)
summary( train.model.Impressions )
AIC( train.model.Impressions )

# Examining the relationship between Impressions and the continous variables
train.model.Impressions <- lm ( Impressions ~ Tavg+  PrecipTotal + 
                                        ResultDir + SnowFall + MI + SH + DR +
                                        SG + IC +  GR + GS + FU + VA + SA +
                                        DU + SQ + DS + BC + PR + Media.Cost, train)
summary( train.model.Impressions )
AIC( train.model.Impressions )


plotting("Impressions_Diff", train, "Media.Cost" , 1000000)

remove.packages("data.table")
install.packages("devtools")
library(devtools)
install_github("Rdatatable/data.table", build_vignettes = FALSE)
library(data.table)

# Adding Percentage Change to the training set
system.time(train[, Prcnt_Change_Impressions := 
                          (Impressions_Diff/shift(Impressions, n = 1L, type =
                                                          "lag")) * 100 ] )

remove.packages("data.table")
install.packages("data.table")
library(data.table)

system.time ( train <- addDeltas("Media.Cost","MINUS", train) )
train[, MINUS_Diff := NULL]

plotting("Impressions_Diff", train, "Media.Cost_Diff" , 1000000) + coord_cartesian(xlim = c(-300000, 300000),
                                                                                   ylim = c(-1000000, 1000000))

plotting("Impressions", train, "Media.Cost" , 100000) + coord_cartesian(xlim = c(0, 500000),
                                                                        ylim = c(0, 250000) )
spend <- readRDS("spend.RDS")

plotting("Impressions", spend, "Media.Cost" , 1000000)


placespend <-  read.csv("WITH THE RATE 6643_query_20150803_104912_146169498.csv", header = T,
                        colClasses = c("character", "character", "character", "character", "double",
                                       "double", "double"),stringsAsFactors = F, skip = 8 )

saveRDS ( placespend, "spend with COST DATA STRUCTURE.RDS" )

placespend <- readRDS("spend with COST DATA STRUCTURE.RDS")

library( data.table )
placespend <- setDT( placespend )
CPM <- subset(placespend, Placement.Cost.Structure == "CPM" )
CPA <- subset(placespend, Placement.Cost.Structure == "CPA" )
CPC <- subset(placespend, Placement.Cost.Structure == "CPC" )
FRImp <- subset(placespend, Placement.Cost.Structure == "Flat Rate - Impressions" )
FRClicks <- subset(placespend, Placement.Cost.Structure == "Flat Rate - Clicks" )

plotting("Impressions", CPM, "Media.Cost", 500000)
plotting("Impressions", CPA, "Media.Cost", 500000)
plotting("Impressions", CPC, "Media.Cost", 500000)
plotting("Impressions", FRImp, "Media.Cost", 500000)
plotting("Impressions", FRClicks, "Media.Cost", 2171)


# Removing the last rows that contains the Grand Total.
placespend <- subset(placespend, Placement.Cost.Structure != "---")

# Converting strings to factors
placespend[,Placement.Cost.Structure := as.factor(Placement.Cost.Structure)]
saveRDS ( placespend, "spend with COST DATA STRUCTURE.RDS" )
placespend <- readRDS("spend with COST DATA STRUCTURE.RDS")

library(ggplot2)
nos <- sample( nrow( placespend ), 50000 )
p1 <- ggplot(placespend[nos,], aes(x=Media.Cost, y=Impressions, colour=Placement.Cost.Structure, 
                                   group = Placement.Cost.Structure)) +
        geom_point() +
        ggtitle("Placement Cost Structure")
p1 + coord_cartesian(xlim = c(0, 500), ylim = c(0, 25000))

placespend <- CPM
saveRDS(placespend,"CPM spend with COST DATA STRUCTURE.RDS")
train <- readRDS( "TRAINING SET DELTAS_WITH SPEND COST.RDS" )
train[, Media.Cost := NULL ]

# I am converting placespend's Date from String to the type Date
# Then, I will order placespend based on Zip code and Date.
# I assume this might make merge a bit faster.
# 
placespend <- setDT(placespend)
placespend[, Date :=  as.Date(Date,format( "%Y-%m-%d") ) ]
setorder(placespend, ZIP.Postal.Code, Date )

CPM <- subset(placespend, Placement.Cost.Structure == "CPM" )
placespend <- CPM
saveRDS(placespend,"CPM spend with COST DATA STRUCTURE.RDS")

train <- readRDS( "TRAINING SET DELTAS_WITH SPEND COST.RDS" )
train[, Media.Cost := NULL ]

# Testing to see if LEft Join will result in more values.
# Indeed it did.
# temp <- merge(train, placespend, by = c("ZIP.Postal.Code", "Date"),
#                all.x = T )
# So, now I will run aggregate in order to remove the extra values.
# 

# Refer to the previous comment why the aggregate the method was done.
system.time( jack <- aggregate( cbind(Media.Cost, Impressions, Clicks) ~ ZIP.Postal.Code + Date, 
                                placespend, sum ) )

jack <- setDT( jack )
saveRDS(jack,"jack.RDS")

jack <- readRDS("jack.RDS")

# Prior to the following code, we ran a model and found that Media Cost 
# helped our model quite a bit. I will merge the Impressions and clicks onto all
# of the data, split the data into the different sets, and then make a model.
# 

# Merging the spend Cost with the deltas data set based on Zip Code and Date.
deltas <- readRDS("/Users/Mathew/Desktop/Project/Files/DELTASTavgCODESPREADNONAValuesHarmelinHasTheClosestWeatherStation.rds")
deltas <- setDT(deltas)
deltas[, c("Impressions_Diff", "Tavg_Diff") := NULL]
temp <- merge(deltas, jack, by= c("ZIP.Postal.Code", "Date"),
              all.x = T)
deltas <- temp
deltas[, c("Impressions.x", "Clicks.x") := NULL]
setnames(deltas, "Impressions.y", "Impressions")
setnames(deltas, "Clicks.y", "Clicks")

# Find Deltas of Temperature, Impressions, and Media Cost.
system.time( deltas <- addDeltas("Impressions", "Clicks",deltas) )
system.time( deltas <- addDeltas("Media.Cost", "MINUS",deltas) )
system.time( deltas <- addDeltas("Tavg", "MINUS",deltas) )
# I don't need deltas for MINUS
deltas[,"MINUS_Diff" := NULL]
str(deltas)
saveRDS(deltas,"DELTAS WITH CPM.RDS")
deltas<- readRDS("DELTAS WITH CPM.RDS")
# THE SPLITTING OF deltas INTO THE test, dev, AND train datasets 
# is COPIED FROM LINES 576 - 618.
# 
# 
# Now, I will weed out those Zip Codes that have Impressions < 10,000
# I will compute a and b again here because I like keeping this side compact
# After that, I will delete from b Impressions that are < 10,000.
# Then I will give the zipcodes in b to uniqueZips.
# 
a <- aggregate( Impressions ~ ZIP.Postal.Code , data = deltas, FUN =  sum )
b <- a[order(-a$Impressions),]
b <- setDT(b)
b <- subset( b, Impressions >= 10000 )
uniqueZips <- b$ZIP.Postal.Code

#
# In the following lines of code, I aim to make training, test, and development 
# tests. In order to do so, I will create a vector that will contain the random 
# proportion of zipcodes. After that, I will use lapply and then subset the
# concerned observations from the data table and append them to a list. Finally,
# I will run it. 

x <- sample(1:length(uniqueZips), ceiling( 0.4 * ( length(uniqueZips) )) )
testZip <-uniqueZips[x]
trainZip <-uniqueZips[-x]

x = sample(1:length(uniqueZips), ceiling( 0.2 * ( length(uniqueZips) ) ))
devZip  <- testZip[x]
testZip <- testZip[-x]

# Making the training set
system.time( train <- subset(deltas, ZIP.Postal.Code %in% trainZip  ) )

# Making the dev set.
system.time( dev <- subset(deltas, ZIP.Postal.Code %in% devZip  ) )

# Making the test set.
system.time( test <- subset(deltas, ZIP.Postal.Code %in% testZip  ) )

#
# Saving the training, deltas, and training sets.
#
saveRDS( train, "TRAINING SET WITH CPM.RDS" )
saveRDS( dev, "DEVELOPMENT SET WITH CPM.RDS" )
saveRDS( test, "TEST SET WITH CPM.RDS" )


# Examining the various predictors in addition to
# a weighted spends.
train <- readRDS("/Users/Mathew/Desktop/Project/TRAINING SET WITH CPM.RDS")
train.model.Impressions.MANY <- lm ( Impressions_Diff ~ Tavg_Diff +  PrecipTotal + 
                                        ResultDir + SnowFall + MI + SH + DR +
                                        SG + IC +  GR + GS + FU + VA + SA +
                                        DU + SQ + DS + BC + PR + Media.Cost_Diff, train)
summary( train.model.Impressions.MANY )
AIC( train.model.Impressions.MANY )

print ( "#************************************************************#")

train.model.Impressions.FEW <- lm ( Impressions_Diff ~ Tavg_Diff +  PrecipTotal + 
                                        ResultDir + SnowFall + Media.Cost_Diff, train)
summary( train.model.Impressions.FEW )
AIC( train.model.Impressions.FEW )

############$$$$$$$$$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Adding weights to Snowfall 
system.time( train <- addDeltas("SnowFall", "MINUS",train) )
# I don't need deltas for MINUS
train[,"MINUS_Diff" := NULL]


############$$$$$$$$$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Checking whether the adding the Snowfall deltas made any difference
# 

train.model.SNOW.Impressions.MANY <- lm ( Impressions_Diff ~ Tavg_Diff +  PrecipTotal + 
                                        ResultDir + MI + SH + DR +
                                        SG + IC +  GR + GS + FU + VA + SA +
                                        DU + SQ + DS + BC + PR + Media.Cost_Diff +
                                        SnowFall_Diff, train)
summary( train.model.SNOW.Impressions.MANY )
AIC( train.model.SNOW.Impressions.MANY )

print ( "#************************************************************#")

train.model.SNOW.Impressions.FEW <- lm ( Impressions_Diff ~ Tavg_Diff +  PrecipTotal + 
                                        ResultDir +  Media.Cost_Diff + SnowFall_Diff, train)
summary( train.model.SNOW.Impressions.FEW )
AIC( train.model.SNOW.Impressions.FEW )

library(ggplot2)
resd <- train.model.Impressions.FEW[["residuals"]]

qplot(resid, geom = "histogram")

h <- ggplot(data=train.model.Impressions.FEW, aes(train.model.Impressions.FEW[["residuals"]])) + geom_density()

h + coord_cartesian(xlim = c(-5000,5000))

dev <- readRDS("DEVELOPMENT SET WITH CPM.RDS")


############$$$$$$$$$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
system.time( dev <- addDeltas("SnowFall", "MINUS",dev) )
# I don't need deltas for MINUS
dev[,"MINUS_Diff" := NULL]

############################################
test <- readRDS("TEST SET WITH CPM.RDS")   #
############################################

############$$$$$$$$$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
system.time( test <- addDeltas("SnowFall", "MINUS",test) )
# I don't need deltas for MINUS
test[,"MINUS_Diff" := NULL]

# NOTE: From lines 1055 - 1221, I am examining the accuracy of my model. I have not
# made any changes to the data yet.

# Here I examine the residuals of the "delta Snow" model
# I obtain tthe residuals from the dev and test set, obtain
# their means and examine which model has the lowest number of 
# test means.
predictions <- predict(train.model.SNOW.Impressions.FEW,dev)
dev_residuals <- dev$Impressions - predictions
dev_residuals = dev_residuals * dev_residuals

predictions <- predict(train.model.SNOW.Impressions.FEW,test)
test_residuals <-  test$Impressions - predictions
test_residuals = test_residuals * test_residuals


mean_dev_residuals1 = mean(dev_residuals, na.rm = T)
mean_test_residuals1 = mean(test_residuals, na.rm = T)
mean_dev_residuals1
mean_test_residuals1

# Checking the model of few predictors.
# I obtain tthe residuals from the dev and test set, obtain
# their means and examine which model has the lowest number of 
# test means.
predictions <- predict(train.model.Impressions.FEW,dev)
dev_residuals <- dev$Impressions - predictions
dev_residuals = dev_residuals * dev_residuals

predictions <- predict(train.model.Impressions.FEW,test)
test_residuals <-  test$Impressions - predictions
test_residuals = test_residuals * test_residuals


mean_dev_residuals2 = mean(dev_residuals, na.rm = T)
mean_test_residuals2 = mean(test_residuals, na.rm = T)
mean_dev_residuals2
mean_test_residuals2


# I will be checking the model with many perdictors but NO weighted snowFall.
# I obtain tthe residuals from the dev and test set, obtain
# their means and examine which model has the lowest number of 
# test means.

predictions <- predict(train.model.Impressions.MANY,dev)
dev_residuals <- dev$Impressions - predictions
dev_residuals = dev_residuals * dev_residuals

predictions <- predict(train.model.Impressions.MANY,test)
test_residuals <-  test$Impressions - predictions
test_residuals = test_residuals * test_residuals


mean_dev_residuals2 = mean(dev_residuals, na.rm = T)
mean_test_residuals2 = mean(test_residuals, na.rm = T)
mean_dev_residuals2
mean_test_residuals2

library(stats)
?prcomp()

deltas <- readRDS("DELTAS WITH CPM.RDS")

# Please ignore the SNOW in the name.
# I just needed to use a variable, so I chose it.
train.model.SNOW.Impressions.FEW <- lm ( Impressions_Diff ~ Tavg_Diff +  PrecipTotal + 
                                                 ResultDir +  Media.Cost_Diff + SnowFall, train)
summary( train.model.SNOW.Impressions.FEW )
AIC( train.model.SNOW.Impressions.FEW )


# I want to plot the actual and predicted values.
nos <- sample( nrow( test ), 100000 )

# Plotting a graph of Actial Impressions and Date, then Predicted impressions
# over it.

# For this, I predict the model over the test set, obtain the predicted Impressionns, and
# then overlap it with the existing graph.
predictions <- predict(train.model.SNOW.Impressions.FEW,test[nos,])
predict_team <- data.table( "Impressions_Diff" = predictions, "Date" = test[nos,]$Date)


ggplot( test[nos,], aes_string( y = "Impressions_Diff", x = "Date" ) ) + geom_point(aes(colour = "Actual")) +
        geom_point(data = predict_team,aes(x = Date, y=Impressions_Diff, colour = "Predicted"),  alpha = 1 ) +
        scale_colour_manual(name="Legend",
                           values=c("black", "red")) +
        xlab("Date") + ylab("Weighted Impressions") + 
        coord_cartesian(ylim = c(-50000,50000) ) +
        ggtitle("Actual and Predicted Weighted Impressions between 2013 and 2015 for 100,000 elements.")


# I want to plot the actual Weighted Impresssion Data.
ggplot( test[nos,], aes_string( y = "Impressions_Diff", x = "Date" ) ) + geom_point(aes())  +
        xlab("Date") + ylab("Weighted Impressions") +
        coord_cartesian(ylim = c(-50000,50000) ) +
        ggtitle("Actual Weighted Impressions between 2013 and 2015 for 100,000 Elements")
# plot the residuals difference as well. 


# To make residual plots of train, test and dev sets.
# 
inference_train = lm ( Impressions_Diff ~ Tavg_Diff +  PrecipTotal + 
                              ResultDir +  Media.Cost_Diff + SnowFall, train[nos,])
inference_dev  = lm( Impressions_Diff ~ Tavg_Diff +  PrecipTotal + 
                             ResultDir +  Media.Cost_Diff + SnowFall, dev[nos,] )
inference_test = lm ( Impressions_Diff ~ Tavg_Diff +  PrecipTotal + 
                              ResultDir +  Media.Cost_Diff + SnowFall, test[nos,])



h <- ggplot(data=inference_train, aes(inference_train[["residuals"]],
                                      colour="r")) + 
        geom_histogram(binwidth =10, alpha = 0.4) +
        coord_cartesian(xlim = c(-1000,1000))  +
        geom_histogram(data= inference_test, aes(inference_test[["residuals"]],
                                                 fill="blue", colour="b"),
                       fill="blue",
                       binwidth = 10,
                       alpha = 0.001) +
        
        geom_histogram(data= inference_dev, aes(inference_dev[["residuals"]],
                                                fill="green", colour="g"),fill= "green",
                       alpha = 0.2,binwidth = 10) +
        scale_colour_manual(name="Legend", values=c("r" = "red", "b"="blue", "g"="green"),
                            labels=c("b"="Test", "r"="Train", "g" = "Development")) +
        xlab("Residuals") + ylab("Count") +
        ggtitle("Histogram showing comparison of residuals for the training, development, and test set")
h

# I was thinking of just plotting the impact of just SnowFall on Impressions.
# I just adapted the following code from the Graph whose title was,
# "Actual and Predicted Weighted Impressions between 2013 and 2015 for 100,000 elements"
# in lines 1131-1141
inference_train1 = lm ( Impressions_Diff ~ SnowFall, train[nos,])
summary(inference_train1)

nos <- sample( nrow( test ), 100000 )
predictions <- predict(inference_train1,test[nos,])
predict_team <- data.table( "Impressions_Diff" = predictions, "Date" = test[nos,]$Date)

ggplot( test[nos,], aes_string( y = "Impressions_Diff", x = "Date" ) ) + geom_point(aes(colour = "Actual")) +
        geom_point(data = predict_team,aes(x = Date, y=Impressions_Diff, colour = "Predicted"),  alpha = 1 ) +
        scale_colour_manual(name="Legend",
                            values=c("black", "red")) +
        xlab("Date") + ylab("Weighted Impressions") + 
        coord_cartesian(ylim = c(-5000,5000) ) +
        ggtitle("Actual and Predicted Weighted Impressions between 2013 and 2015 for 100,000 elements.")

# I was thinking of just plotting the impact of just Precipitation on Impressions.
# I just adapted the following code from the Graph whose title was,
# "Actual and Predicted Weighted Impressions between 2013 and 2015 for 100,000 elements"
# in lines 1131-1141

inference_train2 = lm ( Impressions_Diff ~ PrecipTotal, train[nos,])
summary(inference_train2)

nos <- sample( nrow( test ), 100000 )
predictions <- predict(inference_train2,test[nos,])
predict_team <- data.table( "Impressions_Diff" = predictions, "Date" = test[nos,]$Date)

ggplot( test[nos,], aes_string( y = "Impressions_Diff", x = "Date" ) ) + geom_point(aes(colour = "Actual")) +
        geom_point(data = predict_team,aes(x = Date, y=Impressions_Diff, colour = "Predicted"),  alpha = 1 ) +
        scale_colour_manual(name="Legend",
                            values=c("black", "red")) +
        xlab("Date") + ylab("Weighted Impressions") + 
        coord_cartesian(ylim = c(-5000,5000) ) +
        ggtitle("Actual and Predicted Weighted Impressions between 2013 and 2015 for 100,000 elements.")






                                                             
                                                               