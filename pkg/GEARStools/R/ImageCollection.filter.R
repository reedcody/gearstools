#' @import foreach parallel rgeos rgdal
#' @export
#' @lubridate

ImageCollection.filter <- function(ImageCollection,
		filterDate.range=NULL,filterDOY=NULL,filterMonths=NULL,
		filterDate.exact=NULL,
		filterBounds=NULL,
		filterRawMetadata = NULL,
		filterImageNums=NULL,
		ImageCollection_fname=tempfile(fileext=".Rdata"),
		verbose=FALSE)
{
	# browser()
	if(is.character(ImageCollection))
	{
		if(file.exists(ImageCollection)) load(ImageCollection) else stop("ImageCollection was not found...")
	}
	
	if(!is.null(filterImageNums))
#	if(!missing(filterImageNums))
	{
		if(verbose) message("Filtering by image number...")
		ImageCollection$Images <- ImageCollection$Images[filterImageNums]
	} else filterImageNums=NULL

	# Spatial Filters
	if(!is.null(filterBounds))
	  
	  #	if(!missing(filterBounds))
	{
	  if(verbose) message("Filtering by filterBounds...")
	  
	  #		ImageCollection$Images <- lapply(ImageCollection$Images,function(X,filterDOY)
	  #				{
	  ImageCollection.sf <- ImageCollection.as.sf(ImageCollection)
	  filterBounds_repro <- st_transform(filterBounds,crs=st_crs(ImageCollection.sf))
	  intersect_check <- st_intersects(ImageCollection.sf,filterBounds_repro,sparse=F)
	  ImageCollection$Images <- ImageCollection$Images[intersect_check]
	  
	} else filterBounds=NULL

# FILTER METADATA
			
# TEMPORAL FILTERS
	#Date Range
	if(!is.null(filterDate.range))

	
#	if(!missing(filterDate.range))	
	{
		if(verbose) message("Filtering by filterDate.range...")
		
		ImageCollection$Images <- lapply(ImageCollection$Images,function(X,filterDate.range)
				{
					if(X$metadata$acquisition_datetime >= as.POSIXct(filterDate.range[1]) && X$metadata$acquisition_datetime < as.POSIXct(filterDate.range[2]))
					{
						return(X)
					} else
					{
						return(NULL)
					}	
				},filterDate.range=filterDate.range)
		
		# Clean it by removing null entries:
		ImageCollection$Images <- ImageCollection$Images[!sapply(ImageCollection$Images, is.null)]
		
	} else filterDate.range=NULL
	
	# Day of year filter:
	if(!is.null(filterDOY))
	
#	if(!missing(filterDOY))
	{
		if(verbose) message("Filtering by filterDOY...")
		
		ImageCollection$Images <- lapply(ImageCollection$Images,function(X,filterDOY)
				{
					#strftime(ImageCollection$Images[[1]]$acquisition_datetime,"%j")
					acquisition_datetime_doy <- as.numeric(strftime(X$metadata$acquisition_datetime,"%j"))
					
					if(acquisition_datetime_doy %in% filterDOY)
					{
						return(X)
					} else
					{
						return(NULL)
					}	
				},filterDOY=filterDOY)
		
		# Clean it by removing null entries:
		ImageCollection$Images <- ImageCollection$Images[!sapply(ImageCollection$Images, is.null)]
		
	} else filterDOY=NULL
	
	# Month filter:
	if(!is.null(filterMonths))
	
#	if(!missing(filterMonths))
	{
		if(verbose) message("Filtering by filterMonths...")
		
		ImageCollection$Images <- lapply(ImageCollection$Images,function(X,filterMonths)
				{
					#strftime(ImageCollection$Images[[1]]$acquisition_datetime,"%j")
					acquisition_datetime_month <- as.numeric(strftime(X$metadata$acquisition_datetime,"%m"))
					
					if(acquisition_datetime_month %in% filterMonths)
					{
						return(X)
					} else
					{
						return(NULL)
					}	
				},filterMonths=filterMonths)
		
		# Clean it by removing null entries:
ImageCollection$Images <- ImageCollection$Images[!sapply(ImageCollection$Images, is.null)]
		
	} else filterMonths=NULL
	
	# Filter by exact dates
	if(!is.null(filterDate.exact))
	  {
	  if(verbose) message("Filtering by filterDate.exact...")
		if(!is.Date(filterDate.exact))
	  		{
		  filterDate.exact<-as_date(filterDate.exact)
		  if(any(is.na(filterDate.exact))){
		    stop("Dates are not formatted correctly. Please reformat as %Y%m%d")
		    }
		  else {
	  dates_df <- foreach(i=seq(ImageCollection$Images),.combine=cbind) %do%
	    {
	      tempdate <- as.numeric(strftime(ImageCollection$Images[[i]]$metadata$acquisition_datetime,format="%Y%m%d"))
	                             }
  index<-sapply(filterDate.exact, function (X) {which.min(abs(as.numeric(dates_df)-as.numeric(strftime(X, "%Y%m%d"))))})
  ImageCollection$Images<-ImageCollection$Images[index]
		  }
		}
	  }
	# Clean it by removing null entries:
	ImageCollection$Images <- ImageCollection$Images[!sapply(ImageCollection$Images, is.null)]

  #Filter by spatial data
	if(!is.null(filterBounds))
	
#	if(!missing(filterBounds))
	{
		if(verbose) message("Filtering by filterBounds...")
		
#		ImageCollection$Images <- lapply(ImageCollection$Images,function(X,filterDOY)
#				{
		ImageCollection.sf <- ImageCollection.as.sf(ImageCollection)
		filterBounds_repro <- st_transform(filterBounds,crs=st_crs(ImageCollection.sf))
		# intersect_check <- st_intersects(ImageCollection.sf,filterBounds_repro,sparse=F)
		# ImageCollection$Images <- ImageCollection$Images[intersect_check]
		intersect_check <- st_intersects(filterBounds_repro,ImageCollection.sf,sparse=F)
		intersectIndices <- which(intersect_check, arr.ind = T)
		scenesNeeded <- as.vector(base::unique(intersectIndices[,2]))
		ImageCollection$Images <- ImageCollection$Images[scenesNeeded]
		
	} else filterBounds=NULL
	
# Filter Image Collection by parameters
ImageCollection$filterparameters <- list(
  filterDate.range=filterDate.range,
  filterDOY=filterDOY,
  filterDate.exact=filterDate.exact
  filterMonths=filterMonths,
  filterBounds=filterBounds,
  filterImageNums=filterImageNums,
  ImageCollection_fname=ImageCollection_fname
)
ImageCollection$buildtime <- Sys.time()
save(ImageCollection,file=ImageCollection_fname)
return(ImageCollection)

}

setwd("/Users/codyreed/Dropbox/Meadows GHG/Meadows Research/Remote Sensing/Image Collection Code")
ImageCollection<-readRDS("TestImageCollection.Rdata")
filterDate.exact<-c("1984-05-01", "1985-06-20", "1986-07-11", "1987-05-16", "1986-12-01")

##		filterDate = c(as.Date("2010-01-01"),as.Date("2012-01-01")),
##		filterBounds = 

#filterSP <- readOGR(dsn="~/test_imagecollection/TRPA_Boundary",layer="")
#
#filterBounds <- st_read("~/test_imagecollection/TRPA_Boundary/TRPA_Boundary.shp")

# Test some filtering:
# 