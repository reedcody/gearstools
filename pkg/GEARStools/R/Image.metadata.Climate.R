

Image.metadata.Climate <- function(fname, decompressed_dir=tempdir(), verbose=F, extras = NULL){
  driver = "Climate"
  fname_files_list <- list.files(fname)
  fname_unlist=unlist(strsplit(basename(fname),"_"))
  metadata <- list()
  metadata$driver=driver
  metadata$basename=basename(fname)
  metadata$variable = fname_unlist[1]
  metadata$acquisition_datetime = gsub("\\..*","",fname_unlist[2])
  #Bounding box
  metadata$proj <- "+proj=aea +lat_1=30 +lat_2=50 +lat_0=40 +lon_0=-125 +x_0=0 +y_0=0+datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  raster_file<-raster(fname)
  xmin<-raster_file@extent[1]
  xmax<-raster_file@extent[2]
  ymin<-raster_file@extent[3]
  ymax<-raster_file@extent[4]
  bbox_coords <- matrix(c(
    xmin,ymin,
    xmax,ymin,
    xmax,ymax,
    xmin,ymax,
    xmin,ymin),
    ncol=2,byrow=T)
  metadata$bbox <- st_sf(st_sfc(st_polygon(list(bbox_coords)),crs=metadata$proj))
  metadata$mask_file=NA
  metadata$cloudiness = 0
  metadata$mask_function=NA
  return(metadata)
}
