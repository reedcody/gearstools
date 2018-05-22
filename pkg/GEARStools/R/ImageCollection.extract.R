#' @export

ImageCollection.extract <- function(
		ImageCollection,
		# Vector stuff:
		
		
		# Image stuff:
		retrieve_stack,
		RasterStacks_names,
		overwrite=F,
		# Filter stuff:
		filterDate=NULL,filterDOY=NULL,filterMonths=NULL,
		filterBounds=NULL,
		filterImageNums=NULL,
		filtered_ImageCollection_fname=tempfile(fileext=".Rdata"),
		# Parallel options:
		parallel_engine="rslurm",
		rslurm_options=list(submit=FALSE),
		job_folder=file.path(path.expand("~"),"rslurm"),
		debugmode=F,verbose=F)
{
	# TODO: ... to filter and other...
	
	if(is.character(ImageCollection))
	{
		if(file.exists(ImageCollection)) load(ImageCollection) else stop("ImageCollection was not found...")
	}
	
	if(!is.null(filterDate) || !is.null(filterDOY) || !is.null(filterMonths) || 
			!is.null(filterBounds) || !is.null(filterImageNums))
	{
		# Filter the imagecollection
		ImageCollection <- ImageCollection.filter(
				ImageCollection=ImageCollection,
				filterDate=filterDate,filterDOY=filterDOY,filterMonths=filterMonths,
				filterBounds=filterBounds,
#		filterRawMetadata,
				filterImageNums=filterImageNums,
				ImageCollection_fname=filtered_ImageCollection_fname)
	}
	
	decompressed_dirs <- ImageCollection$buildparameters$decompressed_dirs
	
}