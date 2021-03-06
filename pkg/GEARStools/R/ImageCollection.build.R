#' @import foreach parallel doParallel
#' @export

ImageCollection.build <- function(fnames,drivers,decompressed_dirs,
		ImageCollection_fname=tempfile(fileext=".Rdata"),
		parallel_engine="foreach",
		foreach_options=list(type="PSOCK",spec=floor(detectCores()/2),methods=FALSE),
		buildonly,
		verbose=F)
{
  if(drivers=="auto"){
    fnames_df <- foreach(i=seq(fnames),.combine=rbind) %do%
    {
      tempfiles <- list.files(fnames[i],full.names=T)
      tempfnames <- data.frame(fname=tempfiles,driver=drivers,decompressed_dir=decompressed_dirs[i],
                               stringsAsFactors=F)
    }
  } else {
    fnames_df <- foreach(i=seq(fnames),.combine=rbind) %do%
    {
      tempfiles <- list.files(fnames[i],full.names=T)
      tempfnames <- data.frame(fname=tempfiles,driver=drivers[i],decompressed_dir=decompressed_dirs[i],
                               stringsAsFactors=F)
    } 
  }
	
	if(verbose) message(paste0("Number of files found:",nrow(fnames_df)))
	
	if(!missing(buildonly))
	{
		# For testing only.
		fnames_df <- fnames_df[buildonly,]
	}
	
	ImageCollection <- list()
	
	if(parallel_engine=="foreach")
	{
		if(verbose) message(paste0("Using foreach to process..."))
		cl <- makeCluster(spec=foreach_options$spec,type=foreach_options$type,methods=foreach_options$methods)
		setDefaultCluster(cl=cl)
		registerDoParallel(cl)
		
		ImageCollection$Images <- foreach(i=seq(nrow(fnames_df)),.packages="GEARStools"
		                                  ) %dopar% #%do% can't use dopar without compiling GEARStools package
				{
					ImageMetadata <- list()
					ImageMetadata$metadata <- Image(
							fname=fnames_df[i,]$fname,
							decompressed_dir=fnames_df[i,]$decompressed_dir,			
							driver=fnames_df[i,]$driver)
					return(ImageMetadata)
				}
		
		registerDoSEQ()
		tryCatch(stopCluster(cl),error=function(e){})
	}
	
	ImageCollection$buildparameters <- list(fnames=fnames,
			drivers=drivers,
			decompressed_dirs=decompressed_dirs)
	ImageCollection$buildtime <- Sys.time()
	save(ImageCollection,file=ImageCollection_fname)
	
	return(ImageCollection)
		
}


#testImageCollection <- ImageCollection.build(
#		fnames =c("/Users/jgrn307/Google Drive/Work/projects/temp/OLI_Landsat_8/L2/sr/compressed/",
#				fname="/Users/jgrn307/Google Drive/Work/projects/temp/TM_Landsat_5/L2/sr/compressed/"),
#		decompressed_dirs = c("/Users/jgrn307/Google Drive/Work/projects/temp/OLI_Landsat_8/L2/sr/decompressed",
#				"/Users/jgrn307/Google Drive/Work/projects/temp/TM_Landsat_5/L2/sr/decompressed"),
#		drivers = c("OLI_Landsat_8_sr","TM_Landsat_5_sr"),
##		filterDate = c(as.Date("1995-01-01"),as.Date("2015-01-01")),
#		ImageCollection_fname = "~/testcollection.Rdata",
#		parallel_engine="rslurm"
#)
#
#library(foreach)
#library(rslurm)
#library(GEARStools)
#
#testImageCollection <- ImageCollection.build(
#		fnames =c("/Users/jgrn307/Google Drive/Work/projects/temp/OLI_Landsat_8/L2/sr/compressed/",
#				fname="/Users/jgrn307/Google Drive/Work/projects/temp/TM_Landsat_5/L2/sr/compressed/"),
#		decompressed_dirs = c("/Users/jgrn307/Google Drive/Work/projects/temp/OLI_Landsat_8/L2/sr/decompressed",
#				"/Users/jgrn307/Google Drive/Work/projects/temp/TM_Landsat_5/L2/sr/decompressed"),
#		drivers = c("OLI_Landsat_8_sr","TM_Landsat_5_sr"),
##		filterDate = c(as.Date("1995-01-01"),as.Date("2015-01-01")),
#		ImageCollection_fname = "~/testcollection.Rdata",
#		parallel_engine="rslurm",
#		rslurm_options=list(jobname="~/testrlurm",nodes = 30, cpus_per_node = 30, 
#		submit = FALSE,rscript_path="pathto/RSCRIPT")
#)

