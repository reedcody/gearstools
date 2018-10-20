#' @import RStoolbox
#' @export

Image.metadata.ETM_Landsat_7_sr <- function(fname,decompressed_dir=tempdir(),return_raw=T,extras=NULL,verbose=F)
{
	driver="ETM_Landsat_7_sr"
	
	if(grepl(pattern=".tar.gz$",x=fname))
	{
	  if(verbose) { message("decompressing...")}
	  compressed=TRUE
	  # Extract the metadata info:
	  # tempdir <- tempdir()
	  outdir <- file.path(decompressed_dir,sub('\\.tar.gz$', '',basename(fname)))
	  #			browser()
	  # QUESTION: CAN I DO WILDCARD EXTRACTIONS?
	  fname_files_list <- untar(fname,list=T)
	  xml_fname <- fname_files_list[grepl(pattern=".xml$",fname_files_list)]
	  xml_fname_uncompressed <- file.path(outdir,xml_fname)
	  txt_fname <- fname_files_list[grepl(pattern="_MTL.txt$",fname_files_list)]
	  txt_fname_uncompressed <- file.path(outdir,txt_fname)
	  
	  if(!file.exists(xml_fname_uncompressed)) 
	  {
	    suppressWarnings(untar(fname,xml_fname,exdir=outdir,extras=extras))
	  }
	  
	  if(!file.exists(txt_fname_uncompressed))
	  {
	    suppressWarnings(untar(fname,txt_fname,exdir=outdir,extras=extras))
	  }
	  
	} else
	{
	  compressed=FALSE
	  fname_files_list <- list.files(fname)
	  xml_fname_uncompressed <- file.path(fname,fname_files_list[grepl(pattern=".xml$",fname_files_list)])
	  txt_fname_uncompressed <- file.path(fname,fname_files_list[grepl(pattern="_MTL.txt$",fname_files_list)])
	}
	
	# Read band metadata from local file, this will need to be tweaked for a package:
	# band_raw <- read.csv("~/code/R/GEARStools/pkg/GEARStools/inst/extdata/OLI_Landsat_8_bandinfo.csv")
	#
	if(verbose) { message("reading metadata...")}
	
	xml_raw <- readMeta(xml_fname_uncompressed,raw=T)
	txt_raw <- readLines(txt_fname_uncompressed)
	txt_raw<-grep(pattern="CLOUD_COVER =", txt_raw, ignore.case=T, value=T)
	txt_raw<-as.numeric(gsub(pattern="CLOUD_COVER =", "" ,txt_raw, ignore.case = T))

	metadata <- list()
	metadata$driver = driver
	
	metadata$basename = sub('\\.tar.gz$', '',basename(fname))
	
	# Date and Time
	metadata$acquisition_datetime <- as.POSIXct(paste(
					xml_raw$global_metadata$acquisition_date,
					xml_raw$global_metadata$scene_center_time))
	
	# Projection info
#	utm_zone <- as.numeric(xml_raw$global_metadata$projection_information$utm_proj_params$zone_code)
#	if(utm_zone < 0) south="+south" else south=""
#	utm_zone <- abs(utm_zone)
#
#	metadata$proj <- paste("+proj=utm +zone=",utm_zone," ",south,"+ellps=WGS84 +datum=WGS84 +units=m +no_defs",sep="")
	
	metadata$proj <- "+proj=latlong +datum=WGS84"
	
	# Bounding Box:
	xmin <- min(as.numeric(c(xml_raw$global_metadata$bounding_coordinates$west,
							xml_raw$global_metadata$bounding_coordinates$east)))
	
	xmax <- max(as.numeric(c(xml_raw$global_metadata$bounding_coordinates$west,
							xml_raw$global_metadata$bounding_coordinates$east)))
	
	ymin <- min(as.numeric(c(xml_raw$global_metadata$bounding_coordinates$north,
							xml_raw$global_metadata$bounding_coordinates$south)))
	
	ymax <- max(as.numeric(c(xml_raw$global_metadata$bounding_coordinates$north,
							xml_raw$global_metadata$bounding_coordinates$south)))

	
	bbox_coords <- matrix(c(
					xmin,ymin,
					xmax,ymin,
					xmax,ymax,
					xmin,ymax,
					xmin,ymin),
			ncol=2,byrow=T
	)
	metadata$bbox <- st_sf(st_sfc(st_polygon(list(bbox_coords)),crs=metadata$proj))
	
	metadata$mask_file <- file.path(fname,fname_files_list[grepl(pattern="_pixel_qa.tif$",fname_files_list)])
	
	metadata$cloudiness <- txt_raw #Percent cloudiness not cloud mask
	
	metadata$mask_function <- function(qa)
	{
		return(qa==66 | qa==130)
	}
	
	# DUMP ALL RAW DATA HERE:
	metadata$raw <- xml_raw
	
	return(metadata)
	
}
