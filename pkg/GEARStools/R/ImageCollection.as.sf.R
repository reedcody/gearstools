#' @import sf
#' @export

ImageCollection.as.sf <- function(ImageCollection,crs="+proj=latlong +datum=WGS84")
{
	temp_bbox_list <- lapply(ImageCollection$Images,FUN=function(X)
			{
				temp_bbox <- X$metadata$bbox
				temp_bbox_repro <- st_transform(temp_bbox,crs=crs)
				return(temp_bbox_repro)	
			})
	
	ImageCollection.sf <- do.call(rbind,temp_bbox_list)
	return(ImageCollection.sf)
	
}

# https://github.com/r-spatial/sf/wiki/migrating