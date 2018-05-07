#' @import foreach parallel rgeos rgdal
#' @export

ImageCollection.filter <- function(ImageCollection,
		filterDate,filterDOY,filterMonths,
		filterBounds,
#		filterRawMetadata,
		filterImageNums,
		ImageCollection_fname=tempfile(fileext=".Rdata"))
{
	if(is.character(ImageCollection))
	{
		if(file.exists(ImageCollection)) load(ImageCollection) else stop("ImageCollection was not found...")
	}
	
	
	if(!missing(filterImageNums))
	{
		ImageCollection$Images <- ImageCollection$Images[filterImageNums]
	} else filterImageNums=NULL
	
	# Datefilter:
	# TODO: include low/upper bound?
	if(!missing(filterDate))
	{
#		if(!is.Date(filterDate))
#		{
#			
#		}
		
		ImageCollection$Images <- lapply(ImageCollection$Images,function(X,filterDate)
				{
					if(X$acquisition_datetime >= as.POSIXct(filterDate[1]) && X$acquisition_datetime < as.POSIXct(filterDate[2]))
					{
						return(X)
					} else
					{
						return(NULL)
					}	
				},filterDate=filterDate)
		
		# Clean it by removing null entries:
		ImageCollection$Images <- ImageCollection$Images[!sapply(ImageCollection$Images, is.null)]
		
	} else filterDate=NULL
	
	# Day of year filter:
	if(!missing(filterDOY))
	{
		ImageCollection$Images <- lapply(ImageCollection$Images,function(X,filterDOY)
				{
					#strftime(ImageCollection$Images[[1]]$acquisition_datetime,"%j")
					acquisition_datetime_doy <- as.numeric(strftime(X$acquisition_datetime,"%j"))
					
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
	if(!missing(filterMonths))
	{
		ImageCollection$Images <- lapply(ImageCollection$Images,function(X,filterMonths)
				{
					#strftime(ImageCollection$Images[[1]]$acquisition_datetime,"%j")
					acquisition_datetime_month <- as.numeric(strftime(X$acquisition_datetime,"%m"))
					
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
	
	if(!missing(filterBounds))
	{
#		ImageCollection$Images <- lapply(ImageCollection$Images,function(X,filterDOY)
#				{
		ImageCollection.sf <- ImageCollection.as.sf(ImageCollection)
		filterBounds_repro <- st_transform(filterBounds,crs=st_crs(ImageCollection.sf))
		intersect_check <- st_intersects(ImageCollection.sf,filterBounds_repro,sparse=F)
		ImageCollection$Images <- ImageCollection$Images[intersect_check]
		
	} else filterBounds=NULL
	
	ImageCollection$filterparameters <- list(
			filterDate=filterDate,
			filterDOY=filterDOY,
			filterMonths=filterMonths,
			filterBounds=filterBounds,
			filterImageNums=filterImageNums,
			ImageCollection_fname=ImageCollection_fname
	)
	ImageCollection$buildtime <- Sys.time()
	save(ImageCollection,file=ImageCollection_fname)
	return(ImageCollection)
	
}
##		filterDate = c(as.Date("2010-01-01"),as.Date("2012-01-01")),
##		filterBounds = 

#filterSP <- readOGR(dsn="~/test_imagecollection/TRPA_Boundary",layer="")
#
#filterBounds <- st_read("~/test_imagecollection/TRPA_Boundary/TRPA_Boundary.shp")

# Test some filtering:
# 