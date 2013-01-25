\name{shapefiles}
\alias{read.shapefile}
\alias{read.shp}
\alias{read.shx}
\alias{read.dbf}
\alias{write.shapefile}	
\alias{write.shp}	
\alias{write.shx}	
\alias{write.dbf}	
\alias{calc.header}
\alias{add.xy}
\alias{scaleXY}
\alias{convert.to.shapefile}
\alias{convert.to.simple}
\alias{change.id}
\alias{dp}

\title{Read and write ESRI shapefiles}
\description{This package includes functions to read and write ESRI shapefiles.}

\usage{
read.shapefile(shape.name)
read.shp(shp.name)
read.shx(shx.name) 
read.dbf(dbf.name, header=FALSE) 
write.shapefile(shapefile, out.name, arcgis=FALSE)
write.shp(shp, out.name)
write.shx(shx, out.name)
write.dbf(dbf, out.name, arcgis=FALSE)
calc.header(shapefile)
add.xy(shapefile)
scaleXY(shapefile, scale.factor)
convert.to.shapefile(shpTable, attTable, field, type)
convert.to.simple(shp)
change.id(shpTable, newFieldAsVector)
dp(points, tolerance)

}

\arguments{
	\item{shape.name}{String of the shapefile file name without an extension}
	\item{shp.name}{String of the shp file name with an extension}
	\item{shx.name}{String of the shx file name with an extension}
	\item{dbf.name}{String of the dbf file name with an extension}
	\item{shapefile}{The shapefile object of lists created by read.shapefile}
	\item{out.name}{Filename to write the data to}
	\item{shp}{shp portion (list) of the shapefile object of lists}
	\item{shx}{shx portion (list) of the shapefile object of lists}
	\item{dbf}{dbf portion (list) of the shapefile object of lists}
	\item{scale.factor}{Number to divide the shapefile geography by}
	\item{arcgis}{Replace "." with "\_" in column names for ArcGIS}
	\item{shpTable}{data.frame with columns in order Id, X, and Y}
	\item{attTable}{data.frame with first column names "Id" - polygon id (key)}
	\item{type}{ESRI Shape type 1=point, 3=polyLine, 5=polygon}
	\item{field}{A field name in the attTable}
	\item{newFieldAsVector}{A vector of Ids to replace to the Ids in the shpTable}
	\item{points}{A named list of two vectors (x and y) representing points}
	\item{tolerance}{A tolerance setting for the DP polyLine simplification algorithm}
	\item{header}{Should read.dbf return the header?}
}

\details{
	ESRI shapefiles consist of three files.  The first file (*.shp) contains the geography 
	of each shape.  The second file (*.shx) is an index file which contains record offsets.
	The third file (*.dbf) contains feature attributes with one record per feature.  
	\cr \cr
	\code{read.shapefile} calls \code{read.shp, read.shx}, and \code{read.dbf} to read
	in an entire shapefile.  The result of \code{read.shapefile} is a list of many more
	lists.  The sublists are \code{shp}, \code{shx}, and \code{dbf}.  Each sublist contains
	a header list and some sort of data list.  The shp list is a list of 
	\code{$shp$header} and \code{$shp$shp}.  The shx list is a list of 
	\code{$shx$header} and \code{$shx$index}.  The dbf list is a list of 
	\code{$dbf$header} and \code{$dbf$dbf}.  
	\cr \cr
	The write functions write out a shp, shx, and dbf file from the shapefile list structure.
	To write out a shapefile from simple R data, you need to run \code{convert.to.shapefile}.  
	The inputs to this function are a simple data frame of points (for points, polyLines, or
	polygons) and a data frame representing the dbf file.  Examples are below.
	\cr\cr
	The package reads shape types 1 (point), 3 (polyLine), 5 (polygon), 13 (polyLineZ), 
	and 15 (polygonZ).  Reading of shape type 13 and 15 from Don MacQueen, \email{macq@llnl.gov}
	\cr\cr
	The package writes shape types 1 (point), 3 (polyLine), 5 (polygon), 13 (polyLineZ), 
	and 15 (polygonZ).  Conversion of simple polygons to shapefile format from Manuel 
	Chirouze, \email{Manuel.Chirouze@benfieldgroup.com}
	\cr\cr
	For simple features, the only difference between polyLines and polygons is that the first 
	and last point is the same for a polygon.  The \code{convert.to.simple} function
	can be used to simplify the shp file to a simple data.frame.  The \code{change.id} function
	can then be used to change the Id field for the simple shp data.frame to a field from
	a data.frame (dbf).
	\cr \cr
	For details about the ESRI shapefile structure refer to 
	\url{http://www.esri.com/library/whitepapers/pdfs/shapefile.pdf}.  A detailed description
	of DBF files can be found at \url{http://www.e-bachmann.dk/docs/xbase.htm}.  The \code{arcgis}
	argument to \code{write.dbf} replaces "." with "\_" in field names since ArcGIS does not allow
	the former.  Note that the \code{read.dbf} and \code{write.dbf} functions in the foreign
	package are now used for reading and writing dbfs, which greatly improves the speed of 
	reading/writing dbfs.
	\cr\cr
	Function \code{dp} is an implementation of the Douglas-Peucker polyLine simplification
	algorithm.  Douglas, D. and Peucker, T. (1973). "Algorithms for the reduction of the number
	of points required to represent a digitized line or its caricature." The Canadian
	Cartographer 10(2). 112-122.  \code{dp} currently uses the line, not the line segment to determine
	the distance of the points from the line. This can result in the omission of extreme
	"outlier-like" points. See \url{http://www.lgc.com/resources/Doug_Peucker.pdf} for more information.
}

\value{
 	\tabular{lll}{
	\code{read.shapefile} \tab list \tab shapefile list object \cr
	\code{read.shp} \tab list \tab shp list object \cr
	\code{read.shx} \tab list \tab shx list object \cr
	\code{read.dbf} \tab list \tab DBF list object \cr
	\code{write.shapefile} \tab NA \tab Nothing returned \cr
	\code{write.shp} \tab NA \tab Nothing returned \cr
	\code{write.shx} \tab NA \tab Nothing returned \cr
	\code{write.dbf} \tab NA \tab Nothing returned \cr
	\code{calc.header} \tab list \tab shapefile list object \cr
	\code{add.xy} \tab list \tab shapefile list object \cr
	\code{scaleXY} \tab list \tab shapefile list object \cr
	\code{convert.to.shapefile} \tab list \tab shapefile list object \cr
	\code{convert.to.simple} \tab list \tab data.frame list data.frame \cr
	\code{change.id} \tab list \tab data.frame list data.frame \cr
	\code{dp} \tab list \tab data.frame list data.frame \cr
 	} 
}

\examples{
\dontrun{
#Read entire shapefile
shapefile <- read.shapefile("links")

#Write entire shapefile
write.shapefile(shapefile, "temp", T)

#Read shp, shx, or dbf file
dbf <- read.dbf("links.dbf")

#Write shp, shx, or dbf file
write.dbf(dbf, "links.dbf", T)

#Calculate header (to clean up GeoMedia shapefile exports)
shapefile <- calc.header(shapefile)

#Add the X and Y coordinates to the dbf list of the shapefile list object
shapefile <- add.xy(shapefile)

#Scale the shapefile by scale.factor
shapefile <- scaleXY(shapefile, scale.factor)

#Samples of using the convert.to.shapefile function to write out simple shapefiles
#from basic R data.frames

#Point
dd <- data.frame(Id=c(1,2),X=c(3,5),Y=c(9,6))
ddTable <- data.frame(Id=c(1,2),Name=c("Item1","Item2"))
ddShapefile <- convert.to.shapefile(dd, ddTable, "Id", 1)
write.shapefile(ddShapefile, "c:/test", arcgis=T)

#PolyLine
dd <- data.frame(Id=c(1,1,1,2,2,2),X=c(3,5,8,6,7,8),Y=c(9,8,3,6,7,4))
ddTable <- data.frame(Id=c(1,2),Name=c("Item1","Item2"))
ddShapefile <- convert.to.shapefile(dd, ddTable, "Id", 3)
write.shapefile(ddShapefile, "c:/test", arcgis=T)

#Polygon
dd <- data.frame(Id=c(1,1,1,1,2,2,2,2),X=c(3,5,8,3,6,7,8,6),Y=c(9,8,3,9,6,7,4,6))
ddTable <- data.frame(Id=c(1,2),Name=c("Item1","Item2"))
ddShapefile <- convert.to.shapefile(dd, ddTable, "Id", 5)
write.shapefile(ddShapefile, "c:/test", arcgis=T)

#Convert to list of shapes
ddAsList <- by(dd,dd$Id, function(x) x)

#Convert to data.frame
dd <- do.call(rbind, ddAsList)

#Read in shp file and convert to simple format
shpTest <- read.shp("c:/test.shp")
simpleShpFormat <- convert.to.simple(shpTest)
simpleShpFormat <- change.id(simpleShpFormat, c("a","b"))
simpleAsList <- by(simpleShpFormat, simpleShpFormat[,1], function(x) x)
backToShape <- convert.to.shapefile(simpleShpFormat, 
	data.frame(index=c("a","b")), "index", 5)
write.shapefile(backToShape, "c:/test", arcgis=T)

#Polyline simplification with dp algorithm
x <- c(5,3,4,1,8,9,10,11)
y <- c(6,4,2,1,1,5,2,3)
points <- list(x=x,y=y)
plot(points, type="l")
simpleLine <- dp(points, 2)
lines(simpleLine, type="l", col="blue")

}
}

\author{Ben Stabler <\email{benstabler@yahoo.com}> \cr
}
\keyword{programming}
