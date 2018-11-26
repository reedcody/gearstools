#' @import raster sf
#' @export

Image.metadata.Climate <- function(fname, decompressed_dir=tempdir(), verbose=F, extras = NULL){
  driver = "Climate"
  fname_files_list <- list.files(fname)
  fname_unlist=unlist(strsplit(basename(fname),"_"))
  metadata <- list()
  metadata$driver=driver
  metadata$basename=basename(fname)
  metadata$variable = fname_unlist[1]
  date = as.numeric(gsub("\\..*","",fname_unlist[3]))
  metadata$acquisition_datetime = as.Date(sprintf("%08d", date), format = "%Y%m%d")
  raster_file<-raster(fname)
  raster_proj<-proj4string(raster_file)
  raster_bbox<-st_as_sf(bbox_to_SpatialPolygons(raster_file))
  metadata$bbox<-st_transform(raster_bbox,crs="+proj=latlong +datum=WGS84")
  metadata$mask_file=NA
  metadata$cloudiness = 0
  metadata$mask_function=NA
  return(metadata)
}
