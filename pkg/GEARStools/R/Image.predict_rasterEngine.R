#' @import spatial.tools
#' @export

Image.predict_rasterEngine <- function(Image,model,RasterStacks_index,ncores=1,output_fname,debugmode=F,verbose=F)
{
	if(length(Image$RasterStacks)>1)
	{
		if(missing(RasterStacks_index)) stop("You will need to specify which of the RasterStack ids to use for the prediction...")
		if(is.numeric(RasterStacks_index))
		{
			newdata=Image$RasterStacks[[RasterStacks_index]]
		} else
		{
			newdata=Image$RasterStacks[[which(names(Image$RasterStacks)==RasterStacks_index)]]
		}
	} else
	{
		newdata=Image$RasterStacks[[1]]
	}
	
	if(class(model)=="character")
	{
		if(!file.exists(model)) stop("Model object file not found.")
		model <- readRDS(file=model) 
	}
	
	prediction <- predict_rasterEngine(object=model,newdata=newdata,ncores=ncores,filename=output_fname,
			debugmode=debugmode,verbose=verbose)
	return(prediction)
}