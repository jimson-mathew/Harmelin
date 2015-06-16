"""
Citations:
http://stackoverflow.com/questions/29827479/beautifulsoup-
				download-all-zip-files-from-google-patent-search

http://stackoverflow.com/questions/16694907/how-to-download-large-file-in-python-with-requests-py

"""
from bs4 import BeautifulSoup
import pprint
import requests


def pretty(d, indent=0):
   for key, value in d.iteritems():
      print '\t' * indent + str(key)
      if isinstance(value, dict):
         pretty(value, indent+1)
      else:
         print '\t' * (indent+1) + str(value)



#output folder
output = "/Users/Mathew/Desktop/Project/"


#needed url
url = "http://www.ncdc.noaa.gov/orders/qclcd/"

#fetching the url
html = requests.get(url).text

#Crreating a corresponding data structure of the HTML file
soup = BeautifulSoup(html)

allNames = soup.findAll('a', href=True)
print "The type of allNames: ", type(allNames)

""" Getting a file that is compressed and not a "column header"   """
for name in allNames:
	zipurl = name['href']
	print "The ZipUrl:  ", zipurl
	if(  ( zipurl.endswith('.zip') )or ( zipurl.endswith('.gz') ) ):
		print "Success: ", zipurl
		print "The type of Zipurl: ", type(zipurl)

		break
	else:
		print "not yet"

#location on my computer
outputfile = output + zipurl


downloadURL = url + zipurl
print "downloadURL is made"
#making the connection with the remote copy
r = requests.get(downloadURL, stream = False)
print "r= requests.get(...) has been made"

#ensuring nothing wrong has happened
if r.status_code == requests.codes.ok :
	print "Inside if if r.status_code == requests.codes.ok "
	with open( outputfile, 'wb') as fd:
		print " inside 	with open( downloadURL, 'wb') as fd:"
		for chunk in r.iter_content(chunk_size=1024):
			print " Inside for chunk in r.iter_content(chunk_size=1024): "
			if chunk:
				print " Inside if chunk: "
				#downloding the file
				fd.write(chunk)
				print" Completed fd.write(chunk) "
		fd.close()

print "Outside fd.close()"





