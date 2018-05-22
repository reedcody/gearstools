#' @export
# Various pre-made rasterEngine functions

predict.rfsrc.rasterEngine <- function(object,newdata,prob,ncores=1,verbose=F,...)
{
	# For randomForestSRC, set rasterEngine to chunk_format="data.frame"
#	if(nrow(newdata) > 2) browser()
#	if(verbose) message(print(getOption("rf.cores")))
#	browser()
	
	local_objects <- ls()
	model_parameters <- setdiff(local_objects,c("newdata","object","prob","ncores","verbose"))
	
	# Parallel processing 
	
	options(rf.cores = ncores)
	options(mc.cores = 1)
	
	if(is.character(object))
	{
		if(file.exists(object)){
			object <- readRDS(object)
		} else
		{
			stop(paste(object,"does not exist on filesystem.  Please fix."))
		}
	}
	
#	if(verbose) message(print(getOption("rf.cores")))
	
	# newdata_nrow <- nrow(newdata)
	
	# if(nrow(newdata) > 3) browser()
	
	if(verbose) message("Checking for complete cases...")
	# This could be sped up by removing incomplete cases...
	newdata_complete_index <- complete.cases(newdata)
	if(sum(newdata_complete_index) > 0)
	{
		newdata <- newdata[newdata_complete_index,]
	} else
	{	# Placeholder:
		newdata <- newdata[1,]
		newdata[,] <- 1
	}
#	newdata[is.na(newdata)] <- 1
	
#	browser()
	
	if(verbose) message("Predicting...")
	if(length(model_parameters)>0)
	{
		predict_output <- predict(object=object,newdata=newdata,mget(model_parameters))
	} else
	{
		# Fix for rfsrc quantileReg:
		if(!missing(prob))
		{
			predict_output <- quantileReg(obj=object,prob=prob,newdata=newdata)
		} else
		{
			predict_output <- predict(object=object,newdata=newdata)
		}
	}
	
	# Get the correct columns...
	if(!("family" %in% names(predict_output)))
	{
		# quantreg
		
		predict_output <- predict_output$quantiles
		colnames(predict_output) <- paste("Q",format(round(prob, 4), nsmall = 4),sep="")
	} else
	{
		
		if(predict_output$family == "regr+")
		{
			predict_output <- sapply(predict_output$regrOutput,function(x) return(x$predicted)) 
		} else
		{
			if(predict_output$family == "regr")
			{
				predict_output <- as.matrix(predict_output$predicted,nrow=nrow(newdata))
			} else
			{
				if(predict_output$family == "class")
				{
					predict_output <- predict_output$class	
				}
			}
			
		}
	}
	
	if(verbose) message("Fixing output...")
#	if(nrow(newdata) > 3) browser()
#	predict_output <- as.matrix(predict_output)
	
	# Create a blank matrix to fill in value:
	predict_output_matrix <- matrix(nrow=length(newdata_complete_index),ncol=ncol(predict_output))
	colnames(predict_output_matrix) <- colnames(predict_output)
	if(sum(newdata_complete_index) > 0) predict_output_matrix[newdata_complete_index,] <- predict_output
	
	predict_output <- as.data.frame(predict_output_matrix)
	
	# Fill in nodata locations...
#	if(!is.null(newdata_complete))
#	{
#		if(!is.null(dim(predict_output)))
#		{
#			if(length(dim(predict_output))>1)
#			{
#				predict_output[!newdata_complete,] <- NA
#			} else
#			{
#				predict_output[!newdata_complete] <- NA
#			}
#		} else
#		{
#			predict_output[!newdata_complete] <- NA
#		}
#		
#	}
#	predict_output <- as.data.frame(predict_output_matrix)
	if(verbose) message(dim(predict_output))
	
#	browser()
	
	
	return(predict_output)
}