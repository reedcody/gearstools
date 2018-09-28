#' @export

retrieve_stack_shortcuts <- function(retrieve_stack,driver)
{
	if(retrieve_stack=="LANDSAT_SR" || retrieve_stack==T) 
	{
		if(driver=="OLI_Landsat_8_sr")
		{
			# sensors
			retrieve_stack = list(
					sr=c(
							B1="_sr_band2.tif$",
							B2="_sr_band3.tif$",
							B3="_sr_band4.tif$",
							B4="_sr_band5.tif$",
							B5="_sr_band6.tif$",
							B7="_sr_band7.tif$"),
					qa=c(qa="_pixel_qa.tif$"))
		} 
		if(driver=="ETM_Landsat_7_sr" || driver=="TM_Landsat_5_sr" || driver=="TM_Landsat_4_sr")	
		{
			retrieve_stack = list(
					sr=c(
							B1="_sr_band1.tif$",
							B2="_sr_band2.tif$",
							B3="_sr_band3.tif$",
							B4="_sr_band4.tif$",
							B5="_sr_band5.tif$",
							B7="_sr_band7.tif$"),
					qa=c(qa="_pixel_qa.tif$"))
		}
	}
	
	if(retrieve_stack=="LANDSAT_SR_NOMASK")
	{
		if(driver=="OLI_Landsat_8_sr")
		{
			# sensors
			retrieve_stack = list(
					sr=c(
							B1="_sr_band2.tif$",
							B2="_sr_band3.tif$",
							B3="_sr_band4.tif$",
							B4="_sr_band5.tif$",
							B5="_sr_band6.tif$",
							B7="_sr_band7.tif$")
			)
		} 
		if(driver=="ETM_Landsat_7_sr" || driver=="TM_Landsat_5_sr" || driver=="TM_Landsat_4_sr")	
		{
			retrieve_stack = list(
					sr=c(
							B1="_sr_band1.tif$",
							B2="_sr_band2.tif$",
							B3="_sr_band3.tif$",
							B4="_sr_band4.tif$",
							B5="_sr_band5.tif$",
							B7="_sr_band7.tif$")
			)
		}
	}
	
	
	
	return(retrieve_stack)
}
