"""
Name: Jimson Mathew
Date: 6/16/2015
Update: 6/17/2015
In this program, I will obtain a .tar.gz file from the local memory.
I will extract it and will convert it to a CSV format. I will extnd this to Zip files as well.

Citation:
http://pythonicprose.blogspot.com/2009/10/python-extract-targz-archive.html
http://stackoverflow.com/questions/10220412/convert-tab-delimited-txt-file-into-a-csv-file-using-python

"""

import gzip
import zipfile
import os
import csv

def decompress():
	"""
	This is the working directory of the current file. I will have to switch to
	this several times in order to obtain the next zip file to extract.
	If I use os.getcwd() as is , this will constantly change. 
	"""
	orgnlFldrAdrss = os.getcwd()

	#print "Hank wrote: ", os.path.dirname(os.path.abspath(__file__))

	#print "File names:", os.listdir( os.getcwd() )

	#The list of files in my directory
	orgnlListFiles = os.listdir( os.getcwd() )

	""" 
	For every file in my surrent working directory, I will
	check whether its extention is .zip.
	If so, I will make a new folder with the name of the zip file,
		change my current working directory(CWD) to that folder,
		extract it there, and then changethe CWD to the previous 
		folder, whith all of the zip files. I use orgnlFldrAdrss to do this.
	"""
	for filename in orgnlListFiles:
		print "FileName: ", filename
		#print "Full Path Name:", os.path.join(os.getcwd(), filename)

		#obtaining the name of the file in base.
		base, extension = os.path.splitext(filename)
		# print "Base: ", base
		# print "Type of base variable: ", type( base )
		# print "extension", extension

		if filename.endswith(".zip"):
			#if there isn't a new folder with the name of the file,
			#create one
			if ( not os.path.isdir( base ) ) :
				os.mkdir( base )

			#Changing the CWD to the new folder.
			os.chdir( base )

			#extracting the zip files, which are in the original folder, into 
			#the new one
			with zipfile.ZipFile( os.path.join( orgnlFldrAdrss , filename ) )  as zf:
				zf.extractall()  
			print "Done!"

			#changing the CWD back to the folder with all of the zip files.
			os.chdir( orgnlFldrAdrss ) 
			#deleting the zip file
			os.remove ( filename)
		
			
		#used while testing.
		else:
			#print "*********** IGNORE *************"
			continue

	#print "****** COMPLETED *****"

if __name__=="__main__":
 	pass