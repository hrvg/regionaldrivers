#' Pretty the name of variables
#' @param df a `data.frame`
#' @param var `character` the column of `df` where the variable id are
#' @return a `data.frame`
#' @export
#' @keywords function
fix_varnames <- function(df, var = "var"){
	df[[var]] <- gsub("H.", "Hurst Coefficient ", df[[var]], fixed = TRUE)
	df[[var]] <- gsub("SLOPE", "GIS-Slope", df[[var]])
	df[[var]] <- gsub("SO", "Stream order", df[[var]])
	df[[var]] <- gsub("vc.dist", "Valley confinement", df[[var]])
	df[[var]] <- gsub("CONFINEMEN", "Valley confinement", df[[var]])
	df[[var]] <- gsub("WsAreaSqKm", "Watershed Drainage area", df[[var]])
	df[[var]] <- gsub("Ac", "Watershed Drainage area", df[[var]], fixed = TRUE)
	df[[var]] <- gsub("CatAreaSqKm", "Local Drainage area", df[[var]])
	df[[var]] <- gsub("LDD", "Local Drainage Density", df[[var]])
	df[[var]] <- gsub("_skew", " skewness ", df[[var]])
	df[[var]] <- gsub("_sd", " standard deviation ", df[[var]])
	df[[var]] <- gsub("_min", " min ", df[[var]])
	df[[var]] <- gsub("_max", " max ", df[[var]])
	df[[var]] <- gsub("_mean", " mean ", df[[var]])
	df[[var]] <- gsub("_median", " median ", df[[var]])
	df[[var]] <- gsub("flowdir", "Flow direction", df[[var]])
	df[[var]] <- gsub("curvplan", "Curvature (planform)", df[[var]])
	df[[var]] <- gsub("curvprof", "Curvature (profile)", df[[var]])
	df[[var]] <- gsub("aspect", "Aspect", df[[var]])
	df[[var]] <- gsub(".rstr", "(raster)", df[[var]])
	df[[var]] <- gsub(".nrch", "(near)", df[[var]])
	df[[var]] <- gsub("tpi", "TPI", df[[var]])
	df[[var]] <- gsub("tri", "TRI", df[[var]])
	df[[var]] <- gsub("roughness", "Roughness", df[[var]])
	df[[var]] <- gsub("elevation", "Elevation", df[[var]])
	df[[var]] <- gsub("layer", "Elevation", df[[var]])
	df[[var]] <- gsub("north_california_NED_13", "Elevation", df[[var]])
	df[[var]] <- gsub("slope", "Slope", df[[var]])
	df[[var]] <- gsub("CV_bf.d", "CV bankful depth", df[[var]])
	df[[var]] <- gsub("CV_bf.w", "CV bankful width", df[[var]])
	df[[var]] <- gsub("bf.w.d", "Bankful width/depth", df[[var]])
	df[[var]] <- gsub("bf.d", "Bankful depth", df[[var]])
	df[[var]] <- gsub("bf.d", "Bankful depth", df[[var]])
	df[[var]] <- gsub("bf.w", "Bankful width", df[[var]])
	df$type <- sapply(df[[var]], function(var){
		if (grepl("Hurst", var)){
			return("Statistical roughness")
		} else if (grepl("\\(", var)) {
			return("TAM")
		} else if (var %in% c("Valley confinement", "GIS-Slope")){
			return("GIS")
		} else if (var %in% c("Local Drainage area", "Watershed Drainage area", "Local Drainage Density", "Stream order")){
			return("Topology")
		} else if (var %in% c("Slope", "D84", "D50", "CV bankful depth", "CV bankful width", "Bankful width/depth", "Bankful depth", "Bankful depth", "Bankful width")){
			return("In-situ measurement")
		} else {	
			return("Contextual")
		}
	})
	return(df)
}