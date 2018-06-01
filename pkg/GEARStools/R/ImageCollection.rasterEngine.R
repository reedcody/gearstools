#' @export

ImageCollection.rasterEngine <- function(
		ImageCollection,
		# rasterEngine stuff:
		fun,args=NULL,
		outdirectory,filesuffix, # Where to store files + suffix
		chunk_format="data.frame",
		blocksize=NULL, 
		# Image stuff:
		retrieve_stack,
		RasterStacks_names=NULL,
		overwrite=F,
		# Filter stuff:
		filterDate=NULL,filterDOY=NULL,filterMonths=NULL,
		filterBounds=NULL,
		filterImageNums=NULL,
		filtered_ImageCollection_fname=tempfile(fileext=".Rdata"),
		# Parallel options:
		parallel_engine="batchtools",
		# Batchtools options:
		batchtools_reg=NULL,
		batchtools_resources=list(ncpus=1),
		batchtools_chunk.size=1,
#		batchtools_cluster.functions=NULL,
		debugmode=F,verbose=F)
{
	# TODO: ... to filter and other...
	# Save the parameters to a list that will be preserved in the ImageCollection:
	ImageCollection.rasterEngine_parameters  <- mget(ls())
	
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
	
	
	# These will be loaded into the cluster:
	ImageCollection.rasterEngine_params_objects <- c(
			# rasterEngine stuff:
			"fun","args",
			# "outdirectory","filesuffix",
			"chunk_format","blocksize",
			# Image stuff:
			"decompressed_dirs","retrieve_stack",
			"RasterStacks_names",
			"overwrite",
			# Other parameters:
			"verbose"
	)
	
	ImageCollection.rasterEngine_function <- function(
			# Image parameters:
			fname,driver,decompressed_dirs,
			retrieve_stack,RasterStacks_names,
			# rasterEngine parameters:
			fun,args,
			# outdirectory,filesuffix, # Where to store files + suffix
			outname,
			chunk_format,blocksize, 
			# Other parameters:
			verbose,
			#		rslurm_objects_file,
			overwrite
	)
	{
		require("GEARStools")
		# print(rslurm_objects_file)
		# Hack:
		# if(!missing(rslurm_objects_file)) { load(rslurm_objects_file) }
		
		# Create an image object:
		Image_retrieved <- Image(
				fname=fname,
				driver=driver,
				retrieve_metadata=F,
				retrieve_stack = retrieve_stack,
				decompressed_dir = decompressed_dirs, 
				verbose=verbose, overwrite=overwrite
		)
		
		# NEED TO FIX IMAGECOLLECTION TO CREATE BASENAMES:
		# Image_retrieved$metadata$basename <- sub('\\.tar.gz$', '',basename(Image_retrieved$metadata$fname))
		
		# filename <- file.path(outdirectory,paste(Image_retrieved$metadata$basename,filesuffix,sep=""))
		
		if(!is.null(RasterStacks_names))
		{
			names(Image_retrieved$RasterStacks) <- RasterStacks_names			
		}
		
		if(!overwrite && file.exists(outname))
		{
			Image_rasterEngine <- brick(outname)			
		} else
		{
			# Now apply function to this image:
			Image_rasterEngine <- Image.rasterEngine(
					Image=Image_retrieved,
					fun=fun,
					args=args,
					chunk_format=chunk_format,
					verbose=verbose,
					output_fname=outname,
					#		RasterStacks_names=RasterStacks_names,
					blocksize=blocksize)
		}
		return(Image_rasterEngine)
	}
	
	if(parallel_engine=="batchtools")
	### BATCHTOOLS IMPLEMENTATION
	{
		if(verbose) message("Using batchtools for processing...")
		# Need github stuff here:
		if(!require("batchtools")) install.packages("batchtools")
		# This should be moved higher up.
		if(is.null(batchtools_reg))
			stop("Please create a batchtools registry first, and pass it to this function.")
		else setDefaultRegistry(batchtools_reg)
		
		ImageCollection.rasterEngine_params <- data.frame(
				fname=sapply(ImageCollection$Images,function(X) 
						{ return(X$metadata$fname) } ),
				driver=sapply(ImageCollection$Images,function(X) 
						{ return(X$metadata$driver) } ),
				outname=sapply(ImageCollection$Images,function(X) 
						{ return(file.path(outdirectory,paste(X$metadata$basename,filesuffix,sep=""))) } ),
				stringsAsFactors=F)
				
		# add output filenames here:
		# filename <- file.path(outdirectory,paste(Image_retrieved$metadata$basename,filesuffix,sep=""))
	
		ImageCollection.rasterEngine_params_objects_get <- lapply(ImageCollection.rasterEngine_params_objects,FUN=function(X) get(X))
		names(ImageCollection.rasterEngine_params_objects_get) <- ImageCollection.rasterEngine_params_objects
		
		ids = batchMap(fun=ImageCollection.rasterEngine_function,
				args=ImageCollection.rasterEngine_params,
				more.args=ImageCollection.rasterEngine_params_objects_get)
		ids$chunk = chunk(x=seq(nrow(ids)),chunk.size=batchtools_chunk.size)
		
		submitJobs(ids=ids,resources = batchtools_resources)
		
	}
	
	browser()
	# Build the imagecollection here
	fnames <- ImageCollection.rasterEngine_params$outname
	buildparameters <- ImageCollection.rasterEngine_parameters
}