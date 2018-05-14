#' @import foreach parallel doParallel
#' @export

ImageCollection.build <- function(fnames,drivers,decompressed_dirs,
		# filterDate,
		ImageCollection_fname=tempfile(fileext=".Rdata"),
		parallel_engine="foreach",
		foreach_options=list(type="PSOCK",spec=floor(detectCores()/2),methods=FALSE),
		rslurm_options=list(submit=FALSE),buildonly,
		job_folder=file.path(path.expand("~"),"rslurm"),
		verbose=F)
{
	fnames_df <- foreach(i=seq(fnames),.combine=rbind) %do%
			{
				tempfiles <- list.files(fnames[i],full.names=T)
				tempfnames <- data.frame(fname=tempfiles,driver=drivers[i],decompressed_dir=decompressed_dirs[i],
						stringsAsFactors=F)
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
		
		ImageCollection$Images <- foreach(i=seq(nrow(fnames_df)),.packages="GEARStools") %dopar%
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
	
# browser()
	
	if(parallel_engine=="rslurm")
	{
		if(verbose) message(paste0("Using rslurm to process..."))
		
		#	i=seq(nrow(fnames_df))
		
		# rslurm_params <- fnames_df
		slurm_job_fname <- file.path(job_folder,paste("_rslurm_",rslurm_options$jobname,sep=""),"slurm_job.Rdata")
		
		if(verbose) message(paste0("slurm_job_fname:",slurm_job_fname))
		
		if(file.exists(slurm_job_fname))
		{
			if(verbose) { message("Job was already created, attempting to collate it (assuming it completed)...") }
			# load(slurm_job_fname)
			# browser()
			# NEED SOME ERROR CHECKING HERE:
			ImageCollection$Images <- get_slurm_out(dirname(slurm_job_fname),verbose=verbose)
		} else
		{	
			
			sjob <- slurm_apply(Image, params=fnames_df, 
					jobname = rslurm_options$jobname,
					nodes = rslurm_options$nodes, cpus_per_node = rslurm_options$cpus_per_node, 
					submit = rslurm_options$submit,rscript_path=rslurm_options$rscript_path,
					job_folder = job_folder)
			
			if(!rslurm_options$submit)
			{
				submit_job <- file.path(job_folder,paste("_rslurm_",rslurm_options$jobname,sep=""),"submit.sh")
				message("Jobs are prepared and ready to be submitted. The most likely command is:")
				message(paste("sbatch",submit_job))
				message("ImageCollection will exit.  After execution is complete, please re-run this command with the same parameters...")
				return(sjob)
			}
		}		
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

