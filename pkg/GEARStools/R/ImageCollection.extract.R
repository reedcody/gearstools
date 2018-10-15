#' @import spatial.tools raster plyr sf foreach sp gdalUtils
#' @export

ImageCollection.extract <- function(ImageCollection,
                                    outfile.name, #full file path, including file name and csv extension
                                    # Filter stuff:
                                    filterDate.range = NULL,
                                    filterDOY = NULL,
                                    filterMonths = NULL,
                                    filterDate.exact = NULL,
                                    filterCloudiness = NULL, # for filtering % cloud cover not cloud mask
                                    filterImageNums = NULL,
                                    filtered_ImageCollection_fname = tempfile(fileext = ".Rdata"),
                                    # Vector stuff:
                                    retrieve_stack = T, # or list of band names
                                    overwrite = F,
                                    extract.sp = NULL, # default to filterBounds if NULL, can be file path or memory object
                                    extract.params = NULL, #list written as passed to extract  function (e.g. fun = mean, buffer = 30)
                                    extract.raster = NULL, # file path to raster
                                    qa.mask = F, # create "Mask" column in output dataframe indicating pixel clear (T/F) based on mask function from driver
                                    # Parallel options:
                                    parallel_engine = "rslurm",
                                    rslurm_options = list(submit = FALSE),
                                    job_folder = file.path(path.expand("~"), "rslurm"),
                                    debugmode = F,
                                    verbose = F)
{
  if (is.character(ImageCollection))
  {
    if (file.exists(ImageCollection)) {
      load(ImageCollection)
    } else {
      stop("ImageCollection was not found...")
    }
  }
  
  if (!is.null(filterDate.range) ||
      !is.null(filterDOY) ||
      !is.null(filterMonths) ||
      !is.null(filterDate.exact) || 
      !is.null(filterCloudiness) || 
      !is.null(filterImageNums) ||
      !is.null(extract.sp)||
      !is.null(extract.raster)
      )
  {
    #### Filter stuff ####
    if (is.null(extract.sp) & is.null(extract.raster)) {
      stop("Provide valid spatial extract file")
    }
    if ((!is.null(extract.sp)) & (!is.null(extract.raster))) {
      stop("Both raster and shape extract files provided. Only one permitted.")
    }
    if (!is.null(extract.sp)) {
      ImageCollection <- ImageCollection.filter(
        ImageCollection = ImageCollection,
        filterDate.range = filterDate.range,
        filterDOY = filterDOY,
        filterMonths = filterMonths,
        filterDate.exact = filterDate.exact,
        filterCloudiness = filterCloudiness,
        filterBounds = extract.sp,
        filterImageNums = filterImageNums,
        ImageCollection_fname = filtered_ImageCollection_fname
      )
    }
    if (!is.null(extract.raster)) {
      extract.raster_read <- brick(extract.raster)
      extract.raster_bbox <- st_as_sf(bbox_to_SpatialPolygons(extract.raster_read))
      ImageCollection <- ImageCollection.filter(
        ImageCollection = ImageCollection,
        filterDate.range = filterDate.range,
        filterDOY = filterDOY,
        filterMonths = filterMonths,
        filterDate.exact = filterDate.exact,
        filterCloudiness = filterCloudiness,
        filterBounds = extract.raster_bbox,
        filterImageNums = filterImageNums,
        ImageCollection_fname = filtered_ImageCollection_fname
      )
    }
  }
  
  ####	Vector Stuff ####
  if (!is.null(extract.sp)) {
    if (is.character(extract.sp)) {
      if (file.exists(extract.sp)) {
        extract.sp_read <- st_read(dsn = dirname(extract.sp),
                                   layer = gsub("\\.shp$|\\.kml$|\\.kmz$","",
                                                basename(extract.sp)))
      }
    } else{
      extract.sp_read <- extract.sp
    }
    
    datalist <- foreach(i = seq(length(ImageCollection$Images))) %do% {
      #,.packages="GEARStools") %dopar% {  #change to %dopar% when GEARStools package loaded
      tempimage <- Image(
        fname = ImageCollection$Images[[i]]$metadata$fname,
        driver = ImageCollection$Images[[i]]$metadata$driver,
        retrieve_metadata = F,
        retrieve_stack = retrieve_stack,
        stack_format = "RasterList",
        decompressed_dir = ImageCollection$Images[[i]]$metadata$decompressed_dir,
        overwrite = overwrite,
        verbose = verbose
      )
      if (is.null(extract.params)){
        tempdata <- lapply(tempimage$RasterStacks, function(x) {
          as.data.frame(extract(x, extract.sp_read, sp = T))
        })
      } else {
        tempdata <- lapply(tempimage$RasterStacks, function(x) {
          as.data.frame(extract(x, extract.sp_read, extract.params, sp = T))
        }) 
      }
      if (length(tempdata) > 1) {
        dfimage <- join_all(tempdata)
      } else{
        dfimage <- as.data.frame(tempdata[[1]])
      }
      dfimage$driver <- tempimage$metadata$driver
      dfimage$basename <- tempimage$metadata$basename
      dfimage$acquisition_datetime <- tempimage$metadata$acquisition_datetime
      if(qa.mask){dfimage$mask <- tempimage$metadata$mask_function(dfimage$qa)} # based on mask function given by the driver - no clouds, no open water
      return(dfimage)
    }
    
    finaldf <- foreach(i = seq(datalist), .combine = rbind) %do%
    {
      df.all <- datalist[[i]]
    }
    write.csv(finaldf, file = outfile.name)
    return(finaldf)
  }
 ####	Raster extraction ####
  if (!is.null(extract.raster)) {
    extract.raster_read <- brick(extract.raster)
    datalist <- foreach(i = seq(length(ImageCollection$Images))) %do% {
      #,.packages="GEARStools") %dopar% {  #change to %dopar% when GEARStools package loaded
      tempimage <- Image(
        fname = ImageCollection$Images[[i]]$metadata$fname,
        driver = ImageCollection$Images[[i]]$metadata$driver,
        retrieve_metadata = F,
        retrieve_stack = retrieve_stack,
        stack_format = "RasterList",
        decompressed_dir = ImageCollection$Images[[i]]$metadata$decompressed_dir,
        overwrite = overwrite,
        verbose = verbose)
      
      boundary<-bbox_to_SpatialPolygons(extract.raster_read)
      tempstack<-stack(tempimage[[1]])
      if(proj4string(tempstack)!=proj4string(boundary)) {
         boundary<-spTransform(boundary, CRS(proj4string(tempbrick)))
         }
      
      cropped_stack<-crop(tempstack, boundary)
      writeRaster(x=cropped_stack, filename=paste0(ImageCollection$Images[[i]]$metadata$decompressed_dir,"/","cropped_stack"), format="GTiff", overwrite=overwrite)
     if(dim(extract.raster_read)[3]>1){
       aligned_rasters<-list()
       for(i in seq(dim(extract.raster_read)[3])){
         writeRaster(x=extract.raster_read[[i]], filename=paste0(ImageCollection$Images[[i]]$metadata$decompressed_dir,"/",names(extract.raster_read)[i]), format="GTiff", overwrite=overwrite)
          aligned_rasters[[i]]<-align_rasters(unaligned=paste0(ImageCollection$Images[[i]]$metadata$decompressed_dir,"/",names(extract.raster_read)[i],".tif"), 
                                              reference =paste0(ImageCollection$Images[[i]]$metadata$decompressed_dir,"/","cropped_stack.tif"), 
                                              dstfile =paste0(ImageCollection$Images[[i]]$metadata$decompressed_dir,"/",names(extract.raster_read)[i], "_aligned.tif"),
                                                   output_Raster= T, r="average", overwrite=overwrite)
                }
       
       aligned_rasters<-stack(aligned_rasters)
       tempdata<-stack(aligned_rasters, cropped_stack)
     }else{
       aligned_raster<-align_rasters(unaligned=extract.raster_read, reference =paste0(ImageCollection$Images[[i]]$metadata$decompressed_dir,"/","cropped_stack.tif"), 
                                     dstfile =paste0(ImageCollection$Images[[i]]$metadata$decompressed_dir,"/",names(extract.raster_read), "_aligned.tif"), output_Raster= T, 
                                     r="average", overwrite=overwrite)
      tempdata<-stack(aligned_raster, cropped_stack)
         }
     
     dfimage <- as.data.frame(tempdata) #check how to go from raster to data frame keeping the pixel coordinates.
     dfimage$driver <- tempimage$metadata$driver
     dfimage$basename <- tempimage$metadata$basename
     dfimage$acquisition_datetime <- tempimage$metadata$acquisition_datetime
     dfimage$mask <- tempimage$metadata$mask_function(dfimage$qa) # based on mask function given by the driver - no clouds, no open water
      return(dfimage)
    }
    
    finaldf <- foreach(i = seq(datalist), .combine = rbind) %do%
    {
      df.all <- datalist[[i]]
    }
    write.csv(finaldf, file = outfile.name)
    return(finaldf)
  }
  #### Parallel Processing ####
  #	  if ....
  #
}
