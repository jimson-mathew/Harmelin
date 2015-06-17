"""
Name: Jimson Mathew
Date: 6/15 -6/17
Function:
This method will go to the url and will download zip files from
noaa's weather data.
I will use BeautifulSoup and Requests to do this.
Then the method DecompressandConvert.decompress() will be called
in order to extract the downloaded zip files.

Citations:
http://stackoverflow.com/questions/29827479/beautifulsoup-
				download-all-zip-files-from-google-patent-search

http://stackoverflow.com/questions/16694907/how-to-download-large-file-in-python-with-requests-py

"""
from bs4 import BeautifulSoup
import pprint
import requests
import datetime
import os
import Extracting




#output folder
output = os.getcwd() + "/"
print "The Current Directory: ", output


#needed url
url = "http://www.ncdc.noaa.gov/orders/qclcd/"

#fetching the url
html = requests.get(url).text

#Crreating a corresponding data structure of the HTML file
soup = BeautifulSoup(html)

allNames = soup.findAll('a', href=True)
#print "The type of allNames: ", type(allNames)

#i=0

""" Getting a file that is compressed and not a "column header"   """
for name in allNames:
	zipurl = name['href']
	#print "The ZipUrl:  ", zipurl
	if(  ( zipurl.endswith('.zip') ) ):
		"""   
		Here, I want the dates of only the zip files that are after June
		2013.
		For this, since zipurl is a string, I will extract the year and month
		from the string. Then I will cast the appropriate parts of it 
		to the respective variables.
		Finally, I will compare and see whether any of the zip files are 
		after June 2015. 
		If so, then I will download them.
		"""
		dateTemp = zipurl[5:zipurl.index(".")] 
		year = int( dateTemp[0:4] )
		month = int( dateTemp[4:6] )
		baseDate = datetime.date(2013, 06, 01)
		newDate = datetime.date(year, month, 01)

		if newDate >= baseDate:
			print "Found: ", zipurl
			# print "The type of Zipurl: ", type(zipurl)
			

			outputfile = output + zipurl

			downloadURL = url + zipurl
			#print "downloadURL is made"
			print "Downloading"
			r = requests.get(downloadURL, stream = False)
			#print "r= requests.get(...) has been made"

			
			"""
			If the connection is successful, then I begin downloading the file.
			Since Stream is equal to False, I will get the file as it is and 
			not in separate parts.

			"""
			if r.status_code == requests.codes.ok :
				#print "Inside if if r.status_code == requests.codes.ok "
				with open( outputfile, 'wb') as fd:
					#print " inside 	with open( downloadURL, 'wb') as fd:"
					for chunk in r.iter_content(chunk_size=1024):
						#print " Inside for chunk in r.iter_content(chunk_size=1024): "
						if chunk:
						#	print " Inside if chunk: "
							fd.write(chunk)
						#	print" Completed fd.write(chunk) "
					fd.close()

				print "Finished Downloading the Zip File"
				"""
				#I wanted to test and down load only two files.
				i = i+1
				if i == 2:
					break
				"""
		#for testing purposes		
		# else:
			# print "not yet"
					


#print "Outside fd.close()"

print"Completed Downloading All Appropriate Files"

#Calling the extract method.
Extracting.decompress()
