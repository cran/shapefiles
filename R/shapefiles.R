#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Shapefile Format - Read/Write shapefile format within R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Created 2/13/03 Ben Stabler benjamin.stabler@odot.state.or.us
# Revised 6/11/03 Ben Stabler benjamin.stabler@odot.state.or.us
# Revised 7/7/03  Ben Stabler benjamin.stabler@odot.state.or.us

# Copyright (C) 2003  Oregon Department of Transportation
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DESCRIPTION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#source("shapefiles.R")
#myshape <- read.shapefile("myshapewithnoextension")
#myshape$shp - the shp file 
#myshape$shx - the index file
#myshape$dbf - the dbf file

#Each of the three lists of the myshape contain a header and some data. So there is...
#myshape$shp$header (the header info) and myshape$shp$shp (the geographic data)
#myshape$shx$header and myshape$shx$index (the index data)
#myshape$dbf$header and myshape$dbf$dbf (the dbf stored as a R data.frame)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Read in a point, line or polygon SHP file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read.shp <- function(shp.name) {
	
	infile<-file(shp.name,"rb")

	#Header
	file.code <- readBin(infile,integer(),1, endian="big")
	unused <- readBin(infile,integer(),5, endian="big")
	file.length <- readBin(infile,integer(),1, endian="big")
	file.version <- readBin(infile,integer(),1, endian="little")
	shape.type <- readBin(infile,integer(),1, endian="little")
	xmin <- readBin(infile,double(),1, endian="little")
	ymin <- readBin(infile,double(),1, endian="little")
	xmax <- readBin(infile,double(),1, endian="little")
	ymax <- readBin(infile,double(),1, endian="little")
	zmin <- readBin(infile,double(),1, endian="little")
	zmax <- readBin(infile,double(),1, endian="little")
	mmin <- readBin(infile,double(),1, endian="little")
	mmax <- readBin(infile,double(),1, endian="little")
	header <- list(file.code,file.length,file.version,shape.type,xmin,ymin,xmax,ymax,zmin,zmax,mmin,mmax)
	names(header) <- c("file.code","file.length","file.version","shape.type","xmin","ymin","xmax","ymax","zmin","zmax","mmin","mmax")
	rm(file.code,file.length,file.version,shape.type,xmin,ymin,xmax,ymax,zmin,zmax,mmin,mmax,unused)
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	#Point Shapefile
	if (header$shape.type==1) {
		record <- integer()
		shape.type <- integer()
		x <- double()
		y <- double()
		record.num <- 1
		while (length(record.num)!=0) {
			#Record Number	
			record.num <- readBin(infile,integer(),1, 4, endian="big")
			if (length(record.num)==0) break
			record <- c(record,record.num)
			#Content Length
			content.length <- readBin(infile,integer(),1, 4, endian="big")
			#Shape Type (should be 1)
			temp.type <- readBin(infile,integer(),1, 4, endian="little")
			shape.type <- c(shape.type, temp.type)
			if (temp.type==1) {
				#X and Y values
				x <- c(x,readBin(infile,double(),1, 8, endian="little"))
				y <- c(y,readBin(infile,double(),1, 8, endian="little"))
				#To read in extra data created by GeoMedia shapefile export
				if (content.length>10) {
					temp <- readBin(infile,integer(), ((content.length-10)/2), 4, endian="big")
				}
			}
			if (temp.type==0) {
				#Null shape type has no data
				x <- c(x,NA)
				y <- c(y,NA)			
			}	
		}	
		point.data <- cbind(record=record, x=x, y=y, shape.type=shape.type)
		close(infile)
	}
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	#PolyLine and Polygon Shapefile
	if (header$shape.type==3 || header$shape.type==5) {
		shape <- list()
		record.num <- 1
		while (length(record.num)!=0) {
			#Record Number	
			record.num <- readBin(infile,integer(),1, endian="big")
			if (length(record.num)==0) break
			#Content Length
			content.length <- readBin(infile,integer(),1, endian="big")	
			#Shape Type
			shape.type <- readBin(infile, integer(), 1, endian="little")
			if (shape.type==3 || shape.type==5) {
				box <- readBin(infile, double(), 4, endian="little")
				names(box) <- c("xmin","ymin","xmax","ymax")
				num.parts <- readBin(infile, integer(), 1, endian="little")
				num.points <- readBin(infile, integer(), 1, endian="little")
				parts <- readBin(infile, integer(), num.parts, endian="little")
				#Returns X and Y in order for each point of each part
				points <- readBin(infile, double(), num.points*2, endian="little")
				X <- points[seq(1,(num.points*2),by=2)]
				Y <- points[seq(2,(num.points*2),by=2)]
				points <- data.frame(X,Y)
			}
			if (shape.type==0) {
				#Null shape type has no data
				box <- rep(NA,4)
				num.parts <- NA
				num.points <- NA
				parts <- NA
				points <- data.frame(X=NA,Y=NA)				
			}	
			shape.info <- list(record=record.num, content.length=content.length, shape.type=shape.type, box=box, num.parts=num.parts, num.points=num.points, parts=parts, points=points)
			shape <- c(shape,list(shape.info))
		}
			close(infile)
	}
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	if (header$shape.type==1) { return <- point.data }
	if (header$shape.type==3 || header$shape.type==5) { return <- shape }
	list(shp=return,header=header)
}	


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Write out a point, line or polygon SHP file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write.shp <- function(shp, out.name) {
	
	outfile<-file(out.name,"wb")
	
	#Header
	writeBin(as.integer(9994), outfile, endian="big")
	writeBin(as.integer(rep(0,5)), outfile, endian="big")
	writeBin(as.integer(shp$header$file.length), outfile, endian="big")
	writeBin(as.integer(1000), outfile, endian="little")
	writeBin(as.integer(shp$header$shape.type), outfile, endian="little")
	writeBin(as.double(shp$header$xmin), outfile, endian="little")
	writeBin(as.double(shp$header$ymin), outfile, endian="little")
	writeBin(as.double(shp$header$xmax), outfile, endian="little")
	writeBin(as.double(shp$header$ymax), outfile, endian="little")
	writeBin(as.double(shp$header$zmin), outfile, endian="little")
	writeBin(as.double(shp$header$zmax), outfile, endian="little")
	writeBin(as.double(shp$header$mmin), outfile, endian="little")
	writeBin(as.double(shp$header$mmax), outfile, endian="little")
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	if (shp$header$shape.type==1) {
		for (record in 1:nrow(shp$shp)) {
			if (shp$shp[record,4]==1) {
				#Record Number
				writeBin(as.integer(shp$shp[record,1]), outfile, endian="big")
				#Content Length
				writeBin(as.integer(10), outfile, endian="big")
				#Shape Type
				writeBin(as.integer(1), outfile, endian="little")
				#X - For Points only
				writeBin(as.double(shp$shp[record,2]), outfile, endian="little")
				#Y - For Points only
				writeBin(as.double(shp$shp[record,3]), outfile, endian="little")
			}
			if (shp$shp[record,4]==0) {
				#Record Number
				writeBin(as.integer(shp$shp[record,1]), outfile, endian="big")
				#Content Length
				writeBin(as.integer(4), outfile, endian="big")
				#Shape Type
				writeBin(as.integer(0), outfile, endian="little")
			}
		}
	}
	
	if (shp$header$shape.type==3 || shp$header$shape.type==5) {
		for (record in 1:length(shp$shp)) {
			if (shp$shp[[record]]$shape.type==3 || shp$shp[[record]]$shape.type==5) {
				#Record Number
				writeBin(as.integer(shp$shp[[record]]$record), outfile, endian="big")
				#Content Length
				writeBin(as.integer(shp$shp[[record]]$content.length), outfile, endian="big")
				#Shape Type
				writeBin(as.integer(shp$shp[[record]]$shape.type), outfile, endian="little")
				#Bounding Box
				writeBin(as.double(shp$shp[[record]]$box[1]), outfile, endian="little")
				writeBin(as.double(shp$shp[[record]]$box[2]), outfile, endian="little")
				writeBin(as.double(shp$shp[[record]]$box[3]), outfile, endian="little")
				writeBin(as.double(shp$shp[[record]]$box[4]), outfile, endian="little")
				#Number of parts (segments)
				writeBin(as.integer(shp$shp[[record]]$num.parts), outfile, endian="little")
				#Number of total points
				writeBin(as.integer(shp$shp[[record]]$num.points), outfile, endian="little")
				#Parts
				writeBin(as.integer(shp$shp[[record]]$parts), outfile, endian="little")
				#Points - merge the X and Y points into one vector
				point.stream <- rep(0,length(shp$shp[[record]]$points$X)*2)
				point.stream[seq(1,length(shp$shp[[record]]$points$X)*2,by=2)] <- shp$shp[[record]]$points$X
				point.stream[seq(2,length(shp$shp[[record]]$points$Y)*2,by=2)] <- shp$shp[[record]]$points$Y
				writeBin(as.double(point.stream), outfile, endian="little")
				#Need to check to make sure first and last vertex are the same for Polygon
			}
			if (shp$shp[[record]]$shape.type==0) {
				#Record Number
				writeBin(as.integer(shp$shp[[record]]$record), outfile, endian="big")
				#Content Length
				writeBin(as.integer(shp$shp[[record]]$content.length), outfile, endian="big")
				#Shape Type
				writeBin(as.integer(shp$shp[[record]]$shape.type), outfile, endian="little")
			}
		}
	}
	close(outfile)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Read in the SHX file (index)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

read.shx <- function(shx.name) {
	
	infile<-file(shx.name,"rb")
	
	#Header
	file.code <- readBin(infile,integer(),1, endian="big")
	unused <- readBin(infile,integer(),5, endian="big")
	file.length <- readBin(infile,integer(),1, endian="big")
	file.version <- readBin(infile,integer(),1, endian="little")
	shape.type <- readBin(infile,integer(),1, endian="little")
	xmin <- readBin(infile,double(),1, endian="little")
	ymin <- readBin(infile,double(),1, endian="little")
	xmax <- readBin(infile,double(),1, endian="little")
	ymax <- readBin(infile,double(),1, endian="little")
	zmin <- readBin(infile,double(),1, endian="little")
	zmax <- readBin(infile,double(),1, endian="little")
	mmin <- readBin(infile,double(),1, endian="little")
	mmax <- readBin(infile,double(),1, endian="little")
	header <- list(file.code,file.length,file.version,shape.type,xmin,ymin,xmax,ymax,zmin,zmax,mmin,mmax)
	names(header) <- c("file.code","file.length","file.version","shape.type","xmin","ymin","xmax","ymax","zmin","zmax","mmin","mmax")
	records <- (file.length-50)/4
	
	#Record Data
	index.data <- matrix(0,records,2)
	colnames(index.data) <- c("Offset","Length")
	for (record in 1:records) {
		#First record offset is 50
		index.data[record,1] <- readBin(infile,integer(),1, endian="big")
		#Content length
		index.data[record,2] <- readBin(infile,integer(),1, endian="big")
	}
	close(infile)
	list(index=index.data,header=header)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to calculate header information for a point file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Used to clean up GeoMedia shapefile export files

calc.header <- function(shapefile) {
	#1) Calculate shp header info
	#Calculate number of bytes for null record headers
	num.null.record.bytes <- length(shapefile$shp$shp[,2][is.na(shapefile$shp$shp[,2])]) * 4 
	#Calculate number of bytes for complete record record headers
	num.record.header.bytes <- length(shapefile$shp$shp[,2]) * 8
	#Calculate number of bytes for records (both geoMedia and generic shapefile)
	#Should be value 20 (56 for GeoMedia files)
	num.record.bytes <- (length(shapefile$shp$shp[,2]) - length(shapefile$shp$shp[,2][is.na(shapefile$shp$shp[,2])])) * 20
	#Sum the bytes to get total bytes (100 = header bytes)
	total.bytes <- 100 + sum(num.null.record.bytes, num.record.header.bytes, num.record.bytes)
	#Divide by 2 to get number of 16-bit words 
	file.length <- total.bytes / 2
	#Replace file.length in shapefile header with new file.length calculation
	shapefile$shp$header$file.length <- file.length
	#Replace lengths of 28 with 10 for shx from GeoMedia
	
	#2) Calculate shx info
	if (shapefile$shx$index[1,2]==28) {
		shapefile$shx$index[,2][shapefile$shx$index[,2]==28] <- 10
	}
	#Set the content.length to 14 in order to calculate offset
	shapefile$shx$index[,2][shapefile$shx$index[,2]==10]<-14
	#Set null value content.lengths from 2 to 6 to calculate offsets
	shapefile$shx$index[,2][shapefile$shx$index[,2]==2]<-6
	#Calculate new offset values for each record from beginning of shp file
	new.offset <- cumsum(c(50,shapefile$shx$index[,2]))
	#Replace the existing offsets with the new ones
	shapefile$shx$index[,1] <- new.offset[1:(length(new.offset)-1)]
	#Set the content.lenghts back to their respective values
	shapefile$shx$index[,2][shapefile$shx$index[,2]==14]<-10
	shapefile$shx$index[,2][shapefile$shx$index[,2]==6]<-2

	shapefile
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Write out SHX file (index)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write.shx <- function(shx, out.name) {
	
	outfile<-file(out.name,"wb")
	
	#Header
	writeBin(as.integer(9994), outfile, endian="big")
	writeBin(as.integer(rep(0,5)), outfile, endian="big")
	writeBin(as.integer(shx$header$file.length), outfile, endian="big")
	writeBin(as.integer(1000), outfile, endian="little")
	writeBin(as.integer(shx$header$shape.type), outfile, endian="little")
	writeBin(as.double(shx$header$xmin), outfile, endian="little")
	writeBin(as.double(shx$header$ymin), outfile, endian="little")
	writeBin(as.double(shx$header$xmax), outfile, endian="little")
	writeBin(as.double(shx$header$ymax), outfile, endian="little")
	writeBin(as.double(shx$header$zmin), outfile, endian="little")
	writeBin(as.double(shx$header$zmax), outfile, endian="little")
	writeBin(as.double(shx$header$mmin), outfile, endian="little")
	writeBin(as.double(shx$header$mmax), outfile, endian="little")
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	for (record in 1:nrow(shx$index)) {
		#Record Offset
		writeBin(as.integer(shx$index[record,1]), outfile, endian="big")
		#Content Length
		writeBin(as.integer(shx$index[record,2]), outfile, endian="big")
	}
	close(outfile)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Read DBF format
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

read.dbf <- function(dbf.name) {
	
	infile<-file(dbf.name,"rb")
	
	#Header
	file.version <- readBin(infile,integer(), 1, size=1, endian="little")
	file.year <- readBin(infile,integer(), 1, size=1, endian="little")
	file.month <- readBin(infile,integer(), 1, size=1, endian="little")
	file.day <- readBin(infile,integer(), 1, size=1, endian="little")
	num.records <- readBin(infile,integer(), 1, size=4, endian="little")
	header.length <- readBin(infile,integer(), 1, size=2, endian="little")
	record.length <- readBin(infile,integer(), 1, size=2, endian="little")
	file.temp <- readBin(infile,integer(), 20, size=1, endian="little")
	header <- list(file.version,file.year, file.month, file.day, num.records, header.length, record.length)
	names(header) <- c("file.version","file.year","file.month","file.day","num.records","header.length","record.length")
	rm(file.version,file.year, file.month, file.day, num.records, header.length, record.length)
		
	#Calculate the number of fields
	num.fields <- (header$header.length-32-1)/32
	field.name <- NULL
	field.type <- NULL
	field.length <- NULL
	
	#Field Descriptions (32 bytes each)
	for (i in 1:num.fields) {
		field.name.test <- readBin(infile,character(), 1, size=10, endian="little")
		field.name <- c(field.name,field.name.test)
		if (nchar(field.name.test)!=10) {
			file.temp <- readBin(infile,integer(), 10-(nchar(field.name.test)), 1, endian="little")
		}	
		field.type <- c(field.type,readChar(infile, 1))
		file.temp <- readBin(infile,integer(), 4, 1, endian="little")
		field.length <- c(field.length,readBin(infile,integer(), 1, 1, endian="little"))
		file.temp <- readBin(infile,integer(), 15, 1, endian="little")
	}
	
	#Create a table of the field info
	fields <- data.frame(NAME=field.name,TYPE=field.type,LENGTH=field.length)
	#Set all fields with length<0 equal to correct number of characters
	fields$LENGTH[fields$LENGTH<0]<-(256+fields$LENGTH[fields$LENGTH<0])
	#Read in end of attribute descriptions terminator - should be integer value 13
	file.temp <- readBin(infile,integer(), 1, 1, endian="little")
	#Increase the length of field 1 by one to account for the space at the beginning of each record	
	fields$LENGTH[1]<-fields$LENGTH[1]+1
	#Add fields to the header list
	header <- c(header,fields=NULL)
	header$fields <- fields
	
	#Read in each record to a list element
	all.records <- list()
	for (i in 1:header$num.records) {
		all.records <- c(all.records, list(readChar(infile, header$record.length)))
	}
	#Close the dbf file connection
	close(infile)
	
	#Function to split the strings and replace all " " with "" at the end of string
	format.record <- function(record) {
		record <- substring(record, c(1,cumsum(fields$LENGTH)[1:length(cumsum(fields$LENGTH))-1]+1),cumsum(fields$LENGTH))
		record <- gsub(" +$","", record)
		record
	}
	
	#Split each record into columns and save as data.frame
	dbf <- data.frame(t(data.frame(lapply(all.records, format.record))))
	rm(all.records)
	dimnames(dbf) <- list(1:header$num.records, header$fields$NAME)
	
	#Set the numeric fields to numeric
	for (i in 1:ncol(dbf)) {
		if(fields$TYPE[i]=="C") { dbf[[i]] <- as.character(dbf[[i]]) }
		if(fields$TYPE[i]=="N") { dbf[[i]] <- as.numeric(as.character(dbf[[i]])) }
		if(fields$TYPE[i]=="F") { dbf[[i]] <- as.numeric(as.character(dbf[[i]])) 
			warning("Possible trouble converting numeric field in the DBF\n")
		}
	}
	
	#If the first field is of type character then remove the first 
	#character of each record since the DBF stores a space for a 
	#valid record and an * for a deleted record.
	#If the field is numeric then R removes the white space
	if(fields[1,2]=="C") { dbf[[1]] <- gsub("^[ *]", "", as.character(dbf[[1]])) }
	
	colnames(dbf) <- as.character(fields$NAME)
	colnames(dbf) <- gsub("_",".",colnames(dbf))
	
	#Return the dbf as a list with a data.frame and a header list
	list(dbf=dbf, header=header)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Write out DBF format
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write.dbf <- function(dbf, out.name) {
	
	outfile<-file(out.name,"wb")
		
	#File Header
	writeBin(as.integer(3), outfile, 1, endian="little")
	writeBin(as.integer(paste("1",substr(strsplit(date()," ")[[1]][5],3,5), sep="")), outfile, 1, endian="little")
	writeBin(as.integer(1), outfile, 1, endian="little") 
	writeBin(as.integer(strsplit(date()," ")[[1]][3]), outfile, 1, endian="little")
	writeBin(as.integer(dbf$header$num.records), outfile, 4, endian="little")
	writeBin(as.integer(dbf$header$header.length), outfile, 2, endian="little")
	writeBin(as.integer(dbf$header$record.length), outfile, 2, endian="little")
	for (i in 1:20) { writeBin(as.integer(0), outfile, 1, endian="little") }
	
	#Remove field length + 1 for record start value when writing out
	dbf$header$fields[1,3] <- dbf$header$fields[1,3] - 1
	
	#Field Attributes (32 bytes for each field)
	for (field in 1:ncol(dbf$dbf)) {
		#Write field name
		field.name <- colnames(dbf$dbf)[field]
		writeBin(as.character(field.name), outfile, 10, endian="little")
		if (nchar(as.character(field.name))<10) {
			for (i in 1:(10-nchar(field.name))) { writeBin(as.integer(0), outfile, 1, endian="little") }
		}
		#Write field type (C,N,Y,D)
		writeChar(as.character(dbf$header$fields[field,2]), outfile, 1, eos = NULL)
		for (i in 1:4) { writeBin(as.integer(0), outfile, 1, endian="little") }
		#Write field length
		writeBin(as.integer(dbf$header$fields[field,3]), outfile, 1, endian="little")
		for (i in 1:15) { writeBin(as.integer(0), outfile, 1, endian="little") }
	}
	#Write end of header value
	writeBin(as.integer(13), outfile, 1, endian="little")
	
	#Convert dbf to characters
	data.as.char <- apply(dbf$dbf,2,as.character)
	field.lengths <- dbf$header$fields$LENGTH

	#Write out records
	for (i in 1:nrow(dbf$dbf)) {
		#Write out record start value
		writeBin(as.integer(32), outfile, 1, endian="little")
		#Calculate white space to add to fields in each record
		rep.amount <- field.lengths-nchar(data.as.char[i,])
		white.space <- sapply(rep.amount, function(x) rep(" ", x))
		#Concatenate white space and existing field data
		white.space <- matrix(lapply(white.space, function(x) paste(x, collapse="")))
		record <- paste(data.as.char[i,],white.space, sep="", collapse="")
		#Write out record data
		writeChar(as.character(record), outfile, nchar(record), eos = NULL)
	}
	
	#Write end of file value
	writeBin(as.integer(26), outfile, 1, endian="little")
	close(outfile)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Function to Read in a Shapefile
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

read.shapefile <- function(shape.name) {
	shp.data <- read.shp(paste(shape.name, ".shp", sep=""))
	shx.data <- read.shx(paste(shape.name, ".shx", sep=""))
	dbf.data <- read.dbf(paste(shape.name, ".dbf", sep=""))
	shapefile <- list(shp=shp.data,shx=shx.data,dbf=dbf.data)		
	shapefile
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Function to Write out a Shapefile
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write.shapefile <- function(shapefile, out.name) {
	write.shp(shapefile$shp, paste(out.name, ".shp", sep=""))
	write.shx(shapefile$shx, paste(out.name, ".shx", sep=""))
	write.dbf(shapefile$dbf, paste(out.name, ".dbf", sep=""))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Function to Add the X and Y Coordinates to the DBF of a shapefile
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

add.xy <- function(shapefile) {
	if (shapefile$shp$header$shape.type !=1) {
		stop ("Must be point shapefile")
	}
	shapefile$dbf[[1]]$XCOORD <- shapefile$shp[[1]][,2]
	shapefile$dbf[[1]]$YCOORD <- shapefile$shp[[1]][,3]
	shapefile
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Function to scale the X and Y Coordinates of the shapefile
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

scaleXY <- function(shapefile, scale.factor) {
	#Scale the bounding box of the shapefile by the scale factor
	shapefile$shp$header$xmin <- shapefile$shp$header$xmin/scale.factor
	shapefile$shp$header$ymin <- shapefile$shp$header$ymin/scale.factor
	shapefile$shp$header$xmax <- shapefile$shp$header$xmax/scale.factor
	shapefile$shp$header$ymax <- shapefile$shp$header$ymax/scale.factor
	
	#Scale the bounding box of the shx file by the scale factor
	shapefile$shx$header$xmin <- shapefile$shx$header$xmin/scale.factor
	shapefile$shx$header$ymin <- shapefile$shx$header$ymin/scale.factor
	shapefile$shx$header$xmax <- shapefile$shx$header$xmax/scale.factor
	shapefile$shx$header$ymax <- shapefile$shx$header$ymax/scale.factor
	
	#Scale the X and Y if a point shape
	if (shapefile$shp$header$shape.type == 1) {
		shapefile$shp[[1]][,2] <- shapefile$shp[[1]][,2]/scale.factor
		shapefile$shp[[1]][,3] <- shapefile$shp[[1]][,3]/scale.factor
	}
	#Scale the X and Y point values and bounding box if a line or polyon shape
	if (shapefile$shp$header$shape.type==3 || shapefile$shp$header$shape.type==5) {
		for (shape in 1:length(shapefile$shp$shp)) {
			shapefile$shp[[1]][[shape]]$points[,1] <- shapefile$shp[[1]][[shape]]$points[,1]/scale.factor
			shapefile$shp[[1]][[shape]]$points[,2] <- shapefile$shp[[1]][[shape]]$points[,2]/scale.factor
			shapefile$shp[[1]][[shape]]$box <- shapefile$shp[[1]][[shape]]$box/scale.factor
		}
	}
	shapefile
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to create batchin emme2 network (d211.in)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create emme2 batchin network file from links and nodes shapefiles
#links <- read.shapefile("links")
#nodes <- read.shapefile("nodes")

emme2.shp <- function(nodes, links, file.name="d211.in") {
	
	outfile <- file(file.name, "w")
	
	#Nodes
	writeLines(paste("c R emme2 batchin file  ", date(), sep=""), outfile)
	writeLines("t nodes init", outfile)
	node.records <- paste("a",nodes$dbf$dbf[,2],round(nodes$shp$shp[,2],3),round(nodes$shp$shp[,3],3),nodes$dbf$dbf$UD1,nodes$dbf$dbf$UD2,nodes$dbf$dbf$UD3)
	writeLines(node.records, outfile)
	#Links
	writeLines(paste("c R emme2 batchin file  ", date(), sep=""), outfile)
	writeLines("t links init", outfile)
	link.records <- paste("a", links$dbf$dbf[,1], links$dbf$dbf[,2], links$dbf$dbf[,3], links$dbf$dbf[,4], links$dbf$dbf[,5], links$dbf$dbf[,6], links$dbf$dbf[,7], links$dbf$dbf[,8], links$dbf$dbf[,9], links$dbf$dbf[,10])
	writeLines(link.records, outfile)
	close(outfile)
}

