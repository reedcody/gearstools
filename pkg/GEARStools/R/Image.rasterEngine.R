#' @import spatial.tools
#' @export

Image.rasterEngine <- function(Image,fun,args=NULL,
		output_fname,
		chunk_format="data.frame",
#		RasterStacks_names,
		blocksize=NULL, 
		debugmode=F,verbose=F)
{

	prediction <- rasterEngine(x=Image$RasterStacks,
			fun=fun,args=args,
			filename=output_fname,
			chunk_format=chunk_format,
#			ncores=ncores,
			blocksize=blocksize,
			debugmode=debugmode,verbose=verbose)

	return(prediction)
}