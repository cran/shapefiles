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
\alias{emme2.shp}

\title{Read and write ESRI shapefiles}
\description{This package includes functions to read and write ESRI shapefiles.}

\usage{
read.shapefile(shape.name)
read.shp(shp.name)
read.shx(shx.name) 
read.dbf(dbf.name) 
write.shapefile(shapefile, out.name, arcgis=FALSE)
write.shp(shp, out.name)
write.shx(shx, out.name)
write.dbf(dbf, out.name, arcgis=FALSE)
calc.header(shapefile)
add.xy(shapefile)
scaleXY(shapefile, scale.factor)
emme2.shp(nodes, links, file.name="d211.in")
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
	\item{nodes}{Points shapefile object of lists}
	\item{links}{Lines shapefile object of lists}
	\item{file.name}{String of EMME/2 d211 import file to save to}
	\item{arcgis}{Replace "." with "\_" in column names for ArcGIS}
}

\details{
	ESRI shapefiles consist of three files.  The first file (*.shp) contains the geography of each 
	shape.  The second file (*.shx) is an index file which contains record offsets.  The 
	third file (*.dbf) contains feature attributes with one record per feature.  
	\cr \cr
	\code{read.shapefile} calls \code{read.shp, read.shx}, and \code{read.dbf} to read
	in an entire shapefile.  The result of \code{read.shapefile} is a list of many more
	lists.  The sublists are \code{shp, shx}, and \code{dbf}.  Each sublist contains
	a header list and some sort of data list.  The shp list is a list of 
	\code{$shp$header} and \code{$shp$shp}.  The shx list is a list of 
	\code{$shx$header} and \code{$shx$index}.  The dbf list is a list of 
	\code{$dbf$header} and \code{$dbf$dbf}.  
	\cr \cr
	Currently only the read functions are really that useful.  The write functions work fine, 
	but building the necessary data in R to write out all the appropriate data in the shp and
	dbf files is absent at this point.
	\cr \cr
	For details about the ESRI shapefile structure refer to 
	\url{http://www.esri.com/library/whitepapers/pdfs/shapefile.pdf}.  A detailed description
	of DBF files can be found at \url{http://www.e-bachmann.dk/docs/xbase.htm}.  The \code{arcgis}
	argument to \code{write.dbf} replaces "." with "\_" in field names since ArcGIS does not allow
	the former.  Note that the \code{RODBC} package function \code{odbcConnectDbase} is 
	faster than \code{read.dbf}.
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
	\code{emme2.shp} \tab NA \tab Nothing returned
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

#Create an EMME/2 network batch-in file
emme2.shp(nodes, links, "d211.in")
}
}

\author{Ben Stabler <\email{benjamin.stabler@odot.state.or.us}> \cr
	\url{http://www.odot.state.or.us/tddtpau/R.html}
}
\keyword{programming}
