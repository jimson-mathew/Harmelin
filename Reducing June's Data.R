setwd("Desktop/Project/QCLCD201306/")
w_station  <- read.delim(file = "201306station.txt", sep ="|", header = T)
w_daily    <- read.csv(file = "201306daily.txt", header = T)
impressions <- read.csv(file = "../impression-click_2-year.csv", header = T)
w_hour      <- read.csv(file = "201306hourly.txt", header = T)

install.packages("stringr")
library(stringr)
# 
# Creating copies of the original data sets and doing manipulations on them
w_daily$Station <- w_station$Name[w_daily$WBAN == w_station$WBAN]
Cw_daily    <-     w_daily
Cw_station  <-     w_station
Cimpresions <-     impressions
Cw_hour     <-     w_hour

#Note: the following lines till the *****@@@***** were tested out previously but commented.
#The order in which they had been executed had been maintained, but the testing toward this
#was carried out in the Rstudio console and NOT the Working Directory. I don't believe 
#I have missed anything.


#Adding the names of the station and Location into the daily weather data file
##I will add this after I finish adding the information of all of the files 
 Cw_daily$Name <- Cw_station$Name[ which( Cw_station$WBAN %in%  Cw_daily$WBAN) ]
 Cw_daily$Location <- Cw_station$Location[ which( Cw_station$WBAN %in%  Cw_daily$WBAN) ]


 
 #Converting the YearMonthDate column into a Date variable.
 Cw_daily$Date <- as.Date( as.character(Cw_daily$YearMonthDay), format("%Y%m%d"))

 #*****@@@***** 

#Deleting all rows before the year 2013-06-15
#Although I delete them by date, I will remove the column called date. 
# I will add it back once I finish adding all of the data from the other months.
# 
temp <- Cw_daily[(Cw_daily$Date >= as.Date("2013-06-15") ),]
Cw_daily <- temp
Cw_daily$Date <- NULL
w_daily <- Cw_daily

#writing an updated form of the daily values in the June 2013 file.
write.table(w_daily, "201306daily.txt", sep=",", row.names= FALSE)

str(w_daily)

#6/23/2015:


