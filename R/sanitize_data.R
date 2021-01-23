#' Ensures that non-finite values are flagged as `NA`
#' @param data_df a `data.frame`
#' @return a `data.frame`
#' @export
#' @keywords function
sanitize_data <- function(data_df){
	sanitized_data <- data_df
	sanitized_data <- do.call(data.frame, lapply(sanitized_data, function(x) replace(x, is.infinite(x), NA))) 
	sanitized_data <- as.data.frame(sapply(sanitized_data, as.numeric), colnames = names(sanitized_data))
	return(sanitized_data)
}