#
#This method receives the latitude in radians
#and computes the length of a degree of
#latitude in Meters.
#Formula obtained from Wikipedia(Latitude)
#
degLngthLat <- function(lat) {
        answer <- 111132.954 - ( 559.822 * cos( 2 * lat ) ) + 1.175 * cos( 4 * lat )
        answer
}

#
#This method receives the latitude in radians
#and computes the length of a degree of
#longitude in Meters.
#Formula obtained from Wikipedia(Longitude)
#
degLngthLong <- function(lat) {
        c <-  6356752.3142 / 6378137.0 #polar radius(a) / equatorial radius (a)
        a<-   6378137.0 #equatorial radius
        answer <- (pi/180) * a  * ( cos( atan( c*tan(lat) ) ) )

        answer
}

#
#This method receives the degree and converts it into radians. Then, it returns 
#the same.
#
toRadians <- function(degree) {
        constant <- pi/180
        degree   <- degree * constant
        degree
}

#
#This method receives the latitude and longitude information of two points.
#Then it calculates the distance between the two points.
#Using the midpoint of the latitude, I will calculate the length of a degree
#of the longitude and that of a latitude. These lengths are the unit lengths
#that will give me a good estimate of the difference between the two points.
#
#I will multiply these unit lenghts to the unit co-ordinates. Thus, I will
#get the point co-ordinates in units that will allow me to find the 
#distance.
#
#The desired distance can now be obtained through the Pythagoran theorem
#
#
distance <- function(lat1, long1, lat2, long2) {
#         print((lat1+lat2)/2)
#         print(" **** ")
        
#       obtaining the midpoint.
        midLat        <- ( lat2 + lat1 ) / 2
        # print(midLat)
        midLngthLat             <- degLngthLat( toRadians( midLat )  )
#          print("length Latitude")
#          print( midLngthLat )
        midLngthLng             <- degLngthLong( toRadians( midLat ) )
#        print("length Longitude")
        
#        print( midLngthLng )
#       Obtaining the desired points. With the proper units asssigned to them.        
        newlat1  <- lat1  * midLngthLat
        newlat2  <- lat2  * midLngthLat
        newlong1 <- long1 * midLngthLng
        newlong2 <- long2 * midLngthLng
#        Obtaining the distance
        distance <- sqrt( ( ( (newlat1 - newlat2) ^ 2 ) + ( (newlong1-newlong2) ^ 2 )   ) )
        distance
}#end distance <- function(lat1, long1, lat2, long2) 

#print ( distance( 42.07061, -72.62029, 42.37765, -72.50323) / 1000)

