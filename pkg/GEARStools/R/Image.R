#' @import RStoolbox raster
#' @export

# stack_format: "RasterList", "vrt", GDALdrivers...

Image <- function(fname,driver="auto",retrieve_metadata=T,retrieve_stack=F,stack_format="RasterList",
		decompressed_dir=tempdir(),metadata_additions=NULL,overwrite=F,verbose=F)
{
	if(!file.exists(fname))
	{
		stop("File/folder does not exist in the path specified.")
	}
	
	if(!overwrite)
	{
		extras="--keep-old-files"
	} else
	{
		extras=NULL
	}
	
	if(driver=="auto")
	{
	  filename=basename(fname)
	  if(grepl(pattern="^LT08", filename)){driver="OLI_Landsat_8_sr"}
	  if(grepl(pattern="^LT07", filename)){driver="TM_Landsat_7_sr"}
	  if(grepl(pattern="^LT05", filename)){driver="TM_Landsat_5_sr"}
	  if(grepl(pattern="^LT04", filename)){driver="TM_Landsat_4_sr"}
	}

	# For shortcuts:
	if(is.character(retrieve_stack) || retrieve_stack==T)
	{
		retrieve_stack <- retrieve_stack_shortcuts(retrieve_stack,driver)
	}
	
	# browser()
	
	# Check for compression, add other if need be.
	if(grepl(pattern=".tar.gz$",x=fname))
	{
		fname_files_list <- untar(fname,list=T)
		outdir <- file.path(decompressed_dir,sub('\\.tar.gz$', '',basename(fname)))
		compressed=T
		dir_for_metadata<-outdir
	} else
	{
		if(file.info(fname,isdir))
		{
			fname_files_list <- list.files(fname)
			dir_for_metadata<-fname
			
		} else
		{
			fname_files_list <- fname
			dir_for_metadata<-dirname(fname)
		}
	}
	
	# READ METADATA:
	if(driver=="OLI_Landsat_8_sr")
	{
		if(verbose) { message("driver=OLI_Landsat_8_sr")}
		metadata <- Image.metadata.OLI_Landsat_8_sr(fname=fname,decompressed_dir=decompressed_dir,extras=extras,verbose=verbose)	
	}
	
	if(driver=="ETM_Landsat_7_sr")
	{
		if(verbose) { message("driver=ETM_Landsat_7_sr")}
		metadata <- Image.metadata.ETM_Landsat_7_sr(fname=fname,decompressed_dir=decompressed_dir,extras=extras,verbose=verbose)	
	}
	
	if(driver=="TM_Landsat_5_sr")
	{
		if(verbose) { message("driver=TM_Landsat_5_sr")}
		metadata <- Image.metadata.TM_Landsat_5_sr(fname=fname,decompressed_dir=decompressed_dir,extras=extras,verbose=verbose)	
	}
	
	if(driver=="TM_Landsat_4_sr")
	{
		if(verbose) { message("driver=TM_Landsat_4_sr")}
		metadata <- Image.metadata.TM_Landsat_4_sr(fname=fname,decompressed_dir=decompressed_dir,extras=extras,verbose=verbose)	
	}
	
	metadata$fname <- fname
	metadata$fname_files_list <- fname_files_list
	metadata$decompressed_dir<-dir_for_metadata
	
	if(is.list(retrieve_stack))
	{
#		browser()
		if(verbose) { message("retrieving stack...")}
		
#			browser()
		match_fnames <- function(pattern,x)
		{
			# Should get some error checking in here.
			return(sapply(X=pattern,FUN=function(X,x) {
								fnames <-  x[grepl(pattern=X,x=x)]
								names(fnames) <- names(X)
								return(fnames) }
							,x=x))
		}
		
		stack_fnames <- lapply(retrieve_stack,FUN=match_fnames,x=fname_files_list)
		
		if(compressed)
		{
			# Need to figure out how to supress messages
			#	outdir <- file.path(decompressed_dir,sub('\\.tar.gz$', '',basename(fname)))
#			if(is.null(extras))
#			{
#				suppressWarnings(untar(fname,files=unlist(stack_fnames),exdir=outdir))
#				
#			} else
#			{
			if(!overwrite)
			{
				stack_fnames_todecompress <- unlist(sapply(X=unlist(stack_fnames),FUN=function(X)
								{
									if(!file.exists(file.path(outdir,X)))
									{
										return(X)
									} else
									{
										return(NULL)
									}
								}))
			} else
			{
				stack_fnames_todecompress <- unlist(stack_fnames)
			}
			if(length(stack_fnames_todecompress) > 0) 
			{
				suppressWarnings(untar(fname,files=stack_fnames_todecompress,exdir=outdir,extras=extras))
			}
			exdir <- outdir
		} else
		{
			# Won't work for single band files
			exdir <- fname
		}
		
		if(stack_format=="RasterList")
		{
			library("raster")
			RasterStacks <- lapply(X=stack_fnames,
					FUN=function(X,exdir)
					{
						RasterStack <- stack(file.path(exdir,X))
						names(RasterStack) <- names(X)
						return(RasterStack)
					},exdir=exdir)
			
			RasterList <- list(RasterStacks=RasterStacks,metadata=metadata)
			return(RasterList)
		}		
	} else
	{
		return(metadata)
	}
}


## TESTS HERE:
## fname <- "/data/gpfs/assoc/gears/shared_data/rsdata/OLI_Landsat_8/L2/sr/compressed/LC080470312017102501RT-SC20171102170644.tar.gz"
#	
#	retrieve_stack <- list(sr=c("_sr_band2.tif$","_sr_band3.tif$","_sr_band4.tif$","_sr_band5.tif$","_sr_band6.tif$","_sr_band7.tif$"),
#			qa="_pixel_qa.tif$")
#	
## Themiscyra:
#	fname <- "/Users/jgrn307/Google\ Drive/Work/projects/temp/OLI_Landsat_8/L2/sr/compressed/LC080380342013060901T1-SC20171102140615.tar.gz"
#	driver="OLI_Landsat_8_sr"
#	
#	decompressed_dir <- "/Users/jgrn307/Google\ Drive/Work/projects/temp/OLI_Landsat_8/L2/sr/decompressed"

# WORKING!
#testImage <- Image(
#		fname="/Users/jgrn307/Google Drive/Work/projects/temp/OLI_Landsat_8/L2/sr/compressed/LC080380342013060901T1-SC20171102140615.tar.gz",
#		driver="OLI_Landsat_8_sr",
#		retrieve_metadata=F,
#		retrieve_stack = list(
#				sr=c(
#						B1="_sr_band2.tif$",
#						B2="_sr_band3.tif$",
#						B3="_sr_band4.tif$",
#						B4="_sr_band5.tif$",
#						B5="_sr_band6.tif$",
#						B7="_sr_band7.tif$"),qa=c(qa="_pixel_qa.tif$")),
#		decompressed_dir = "/Users/jgrn307/Google Drive/Work/projects/temp/OLI_Landsat_8/L2/sr/decompressed",
#		verbose=T
#)
#
#testImage <- Image(
#		fname="/Users/jgrn307/Google Drive/Work/projects/temp/OLI_Landsat_8/L2/sr/decompressed/LC080380342013060901T1-SC20171102140615",
#		driver="OLI_Landsat_8_sr",
#		retrieve_metadata=F,
#		retrieve_stack = list(
#				sr=c(
#						B1="_sr_band2.tif$",
#						B2="_sr_band3.tif$",
#						B3="_sr_band4.tif$",
#						B4="_sr_band5.tif$",
#						B5="_sr_band6.tif$",
#						B7="_sr_band7.tif$"),qa=c(qa="_pixel_qa.tif$")),
#		decompressed_dir = "/Users/jgrn307/Google Drive/Work/projects/temp/OLI_Landsat_8/L2/sr/decompressed",
#		verbose=T
#)
#
#testImage <- Image(
#		fname="/Users/jgrn307/Google Drive/Work/projects/temp/ETM_Landsat_7/L2/sr/compressed/LE070380341999071301T2-SC20171102050818.tar.gz",
#		driver="ETM_Landsat_7_sr",
#		retrieve_metadata=T,
#		retrieve_stack = list(
#				sr=c(
#						B1="_sr_band1.tif$",
#						B2="_sr_band2.tif$",
#						B3="_sr_band3.tif$",
#						B4="_sr_band4.tif$",
#						B5="_sr_band5.tif$",
#						B7="_sr_band7.tif$"),qa=c(qa="_pixel_qa.tif$")),
#		decompressed_dir = "/Users/jgrn307/Google Drive/Work/projects/temp/ETM_Landsat_7/L2/sr/decompressed",
#		verbose=T
#)

#testImage <- Image(
#		fname="/Users/jgrn307/Google Drive/Work/projects/temp/TM_Landsat_5/L2/sr/compressed/LT050380341985071401T1-SC20171101132559.tar.gz",
#		driver="TM_Landsat_5_sr",
#		retrieve_metadata=T,
#		retrieve_stack = list(
#				sr=c(
#						B1="_sr_band1.tif$",
#						B2="_sr_band2.tif$",
#						B3="_sr_band3.tif$",
#						B4="_sr_band4.tif$",
#						B5="_sr_band5.tif$",
#						B7="_sr_band7.tif$"),qa=c(qa="_pixel_qa.tif$")),
#		decompressed_dir = "/Users/jgrn307/Google Drive/Work/projects/temp/TM_Landsat_5/L2/sr/decompressed",
#		verbose=T
#)

