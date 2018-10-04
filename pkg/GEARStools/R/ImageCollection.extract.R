#' @import spatial.tools raster plyr
#' @export

ImageCollection.extract <- function(
		ImageCollection,
		outfile.name, #full file path, including file name and csv extension
		# Filter stuff:
		filterDate.range=NULL,filterDOY=NULL,filterMonths=NULL,
		filterDate.exact=NULL,
		filterCloudiness = NULL, # for filtering % cloud cover not cloud mask
		filterImageNums=NULL,
		filtered_ImageCollection_fname=tempfile(fileext=".Rdata"),
		# Vector stuff:
		retrieve_stack=T, # or list of band names
		overwrite=F,
		verbose=F,
		extract.sp = NULL, # default to filterBounds if NULL, can be file path or memory object
		extract.params = NULL #list written as passed to extract  function (e.g. fun = mean, buffer = 30)
		extract.raster = NULL, # file path to raster
		qa.mask = F,
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
	
	if(!is.null(filterDate.range) || !is.null(filterDOY) || !is.null(filterMonths) || !is.null(filterDate.exact) || !is.null(filterCloudiness) ||
			!is.null(filterBounds) || !is.null(filterImageNums))
	{
#### Filter stuff ####
	  if(is.null(extract.sp)&is.null(extract.raster)){
	    stop("Provide valid spatial extract file")
	  }
	  if(!is.null(extract.sp)&(!is.null(extract.raster))){
	    stop("Both raster and shape extract files provided. Only one permitted.")
	  }
	  if(!is.null(extract.sp)){
		ImageCollection <- ImageCollection.filter(
				ImageCollection=ImageCollection,
				filterDate.range=filterDate.range,filterDOY=filterDOY,filterMonths=filterMonths, filterDate.exact=filterDate.exact, filterCloudiness=filterCloudiness,
				filterBounds=extract.sp,
				filterImageNums=filterImageNums,
				ImageCollection_fname=filtered_ImageCollection_fname)
	  }
	  if(!is.null(extract.raster)){
	    extract.raster_read<-brick(extract.raster)
	    extract.raster_bbox<-bbox_to_SpatialPolygons(extract.raster_read)
	    ImageCollection <- ImageCollection.filter(
	      ImageCollection=ImageCollection,
	      filterDate.range=filterDate.range,filterDOY=filterDOY,filterMonths=filterMonths, filterDate.exact=filterDate.exact, filterCloudiness=filterCloudiness,
	      filterBounds=extract.raster_bbox,
	      filterImageNums=filterImageNums,
	      ImageCollection_fname=filtered_ImageCollection_fname)
	  }
	}

####	Vector Stuff ####
if(!is.null(extract.sp)){
  if(is.character(extract.sp){
    if(file.exists(extract.sp)){
      extract.sp_read<-st_read(dsn=dirname(extract.sp), layer=gsub(c("\\.shp$","\\.kml$","\\.kmz$"),"",basename(filterBounds)))
    }}
    else{extract.sp_read=extract.sp}
    
    datalist<-foreach(i=seq(length(ImageCollection$Images)))
    #,.packages="GEARStools"
  ) %do% #change to %dopar% when GEARStools package loaded
  {
    tempimage<-Image(fname=ImageCollection$Images[[i]]$metadata$fname,driver=ImageCollection$Images[[i]]$metadata$driver,retrieve_metadata=F,retrieve_stack=retrieve_stack,stack_format="RasterList",
          decompressed_dir=ImageCollection$Images[[i]]$metadata$decompressed_dir,metadata_additions=NULL,overwrite=overwrite,verbose=verbose)
    tempdata<-lapply(tempimage$RasterStacks, function(x){
      as.data.frame(extract(x,extract.sp_read, extract.params, sp=T))
    }
    if(length(tempdata>1)){
      dfimage<-join_all(tempimage)
    }else{dfimage<-as.data.frame(tempdata[[1]])
    }
    dfimage$driver<-tempimage$metadata$driver
    dfimage$basename<-tempimage$metadata$basename
    dfimage$acquisition_datetime<-tempimage$metadata$acquisition_datetime
    return(dfimage)
    }
  finaldf<-foreach(i=seq(datalist), .combine = rbind)%do%{
   df.all<-datalist[[i]] 
  }
  write.csv(finaldf, file=outfile.name)
  return(finaldf)
}

	#foreach statement: foreach image in image collection read the bands or read the raster stack
	#read in the bands or stack according to retrieve_stack information
	#get name of decompressed_dir and get list of all files in directory
	#Get the paths to each of the bands and the quality image.
	# fnames <- dir(temporary_directory)
	# B1 <- file.path(temporary_directory,fnames[grep("_sr_band2.tif",fnames)])
	# B2 <- file.path(temporary_directory,fnames[grep("_sr_band3.tif",fnames)])
	# B3 <- file.path(temporary_directory,fnames[grep("_sr_band4.tif",fnames)])
	# B4 <- file.path(temporary_directory,fnames[grep("_sr_band5.tif",fnames)])
	# B5 <- file.path(temporary_directory,fnames[grep("_sr_band6.tif",fnames)])
	# B7 <- file.path(temporary_directory,fnames[grep("_sr_band7.tif",fnames)])
	# qa <- file.path(temporary_directory,fnames[grep("_pixel_qa.tif",fnames)])
	# 
	# initial_stack_fname <- paste(landsat_basename,"_initialstack.tif",sep="")
	# 
	# gdalfile=c(B1,B2,B3,B4,B5,B7,qa)
	# 
	# initial_landsatSR_stack <- mosaic_rasters(gdalfile=gdalfile,
	#                                           dst_dataset=file.path(temporary_directory,initial_stack_fname),
	#                                           output_Raster=T,separate=T)
	# 
	# landsat_qa <- raster(mask_band)
	# if(grep("OLI_Landsat_8", landsat_file)==1) { ##arreglar esto!!!
	#   landsat_qa_mask <- (landsat_qa == 322) + (landsat_qa == 386)
	# }else{
	#   landsat_qa_mask <- (landsat_qa == 66) + (landsat_qa == 130)
	# }
	# 
	#   if(!is.null(extract.raster)){
	#     #Load raster to extract
	#     raster_to_extract<-raster(extract.raster)
	#     #Create an extent object
	#     boundary<-bbox_to_SpatialPolygons(raster_to_extract)
	#     
	#     #Check that the files have the same projection
	#     if(proj4string(initial_landsatSR_stack_masked)!=proj4string(boundary)) {
	#       
	#       boundary<-spTransform(boundary, CRS(proj4string(initial_landsatSR_stack_masked)))
	#     }
	#     
	#     #Using the bbox of the resulting Veg_classes raster, subset the landsat image.
	#     landsat_cropped_stack<-crop(initial_landsatSR_stack_masked, boundary)
	#     #Save the cropped stack in the US_lifeform_cover project directory.
	#     landsat_directory<-paste0(extract_directory_path,"/", gsub(c("^.{10}|.{6}T1-SC.*"), "", landsat_basename),"/",landsat_basename)
	#     if(!(dir.exists(landsat_directory)))
	#     {
	#       dir.create(landsat_directory,showWarnings=TRUE,recursive=TRUE)
	#     }
	#     writeRaster(x=landsat_cropped_stack, filename=paste0(landsat_directory,"/","landsat_cropped_stack"), format="GTiff", overwrite=overwrite)
	#     
	#     #Create a Mask layer for the vegetation classes
	#     mask_veg<-calc(is.na(veg_classes)==TRUE, function(x){max(x)})
	#     name_parts<-unlist(strsplit(veg_file, "/"))  
	#     output_file<-paste0(dirname(veg_file),"/",name_parts[10],"_veg_mask")
	#     writeRaster(x=mask_veg, filename=output_file, format="GTiff", overwrite=overwrite)
	#     
	#     #Creating temporal separate rasters from the veg_classes raster
	#     for(i in seq(band_names)){
	#       writeRaster(x=veg_classes[[i]], filename=paste0(temporary_directory,"/",name_parts[10],band_names[i]), format="GTiff", overwrite=overwrite)
	#       
	#     }
	#     
	#     #Wrap the vegetation classes raster to a 30 m pixel and align the vegetation classes to the landsat image
	#     #Note: align_raster does not work with bricks, only with one band at a time, the same applies for gdalwarp.
	#     vegetation_classes<-list()
	#     for(i in seq(band_names)){
	#       vegetation_classes[[i]]<-align_rasters(unaligned =paste0(temporary_directory,"/",name_parts[10],band_names[i],".tif") , reference =paste0(landsat_directory,"/","landsat_cropped_stack.tif"), dstfile = paste0(dirname(veg_file),"/",name_parts[10],"_",band_names[i], "_30.tif"), 
	#                                              output_Raster = T, r="average", overwrite=overwrite) 
	#       
	#     }
	#     
	#     vegetation_classes[[length(band_names)+1]]<-align_rasters(unaligned =paste0(dirname(veg_file),"/",name_parts[10],"_veg_mask",".tif"), reference =paste0(landsat_directory,"/","landsat_cropped_stack.tif"), dstfile = paste0(dirname(veg_file),"/",name_parts[10], "_veg_mask_30.tif"), 
	#                                                               output_Raster = T, r="average", overwrite=overwrite) 
	#     
	#     
	#     veg_classes_30<-stack(vegetation_classes)
	#     final_stack<-stack(veg_classes_30, landsat_cropped_stack)
	#     
	#     dataset<-as.data.frame(final_stack)
	#     dataset2<-na.omit(dataset)
	#     colnames(dataset2)<-c(band_names,"mask", "band1", "band2", "band3", "band4", "band5", "band7") 
	#     final_dataset<-dataset2[dataset2$mask==0,]
	#     
	#     write.csv(final_dataset[,c(1:4,6:11)], file=paste0(dirname(veg_file),"/",name_parts[10],"_final_dataset.csv"),row.names=FALSE)
	#   }
#	  }
#### Parallel Processing ####	  
#	  if ....

}

