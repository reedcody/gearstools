#' @import spatial.tools
#' @export

Image.rasterEngine <- function(Image,fun,args=NULL,
		output_fname,
		chunk_format="data.frame",
#		RasterStacks_names,
		blocksize=NULL, 
		debugmode=F,verbose=F)
{

#	if(!missing(RasterStacks_names))
#	{
#		names(Image$RasterStacks) <- RasterStacks_names
#	}
		
	prediction <- rasterEngine(newdata=Image$RasterStacks,
			fun=fun,args=args,
			filename=output_fname,
			chunk_format=chunk_format,
#			ncores=ncores,
			blocksize=blocksize,
			debugmode=debugmode,verbose=verbose)

	return(prediction)
}