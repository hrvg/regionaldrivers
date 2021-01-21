#' Compute feature importance
#' @param training_data a `data.frame`
#' @param target_colname `character` the name of the column containing the target (output)
#' @param methods `list` of `character` accepted by `mlr::generateFilterValuesData()`
#' @param .iters `numeric` number of iterations for the subsampling, default to 500
#' @param .first `numeric`, number of feature to display, default to 30
#' @param .split `numeric`, ratio of the subsampling splitting ratio, default to 0.8
#' @param .stratify `logical`, should the subsampling be stratified, default to `TRUE`
#' @param .seed `numeric`, fix seed for reproducible example
#' @return a list with two elements a list of `data.frame` for each method and a list of `ggplot` objects
#' @importFrom rlang .data
#' @import foreach
#' @import doFuture
#' @keywords function
#' @export
feature_importance <- function(training_data, target_colname, methods = c("FSelectorRcpp_information.gain"), .iters = 500, .first = 30, .split = .8, .stratify = TRUE, .seed = 1789){
	df <- m <- NULL
	set.seed(.seed)
	target <- training_data[[target_colname]]
	training_data <- training_data %>% dplyr::select(-target_colname) %>% sanitize_data()
	training_data[[target_colname]] <- target 
	training_data <- training_data %>% stats::na.omit()
	task <- mlr::makeClassifTask(data = training_data, target = target_colname)
	resampleDesc <- mlr::makeResampleDesc("Subsample", stratify = .stratify, iters = .iters, split = .split)
	rs <- mlr::makeResampleInstance(resampleDesc, task)$train.inds
	feature_importances <- 	lapply(methods, function(m){
		fvs <- lapply(rs, function(ind){
			set.seed(.seed)
			resampled_task <- mlr::makeClassifTask(data = training_data[ind, ], target = target_colname)
			fv <- mlr::generateFilterValuesData(resampled_task, method = m)
			fv$data
		})
		df <- do.call(rbind, fvs)
		df <- df %>%
			dplyr::group_by(.data$name) %>%
			dplyr::summarize(Overall = mean(.data$value)) %>%
			dplyr::rename(var = .data$name) %>%
			dplyr::arrange(-.data$Overall)
		return(df)
	})
	p_field_data <- foreach(df = feature_importances, m = methods) %do% {
		make_feature_importance_plot(df, first = .first) + ggplot2::labs(x = "value", title = m)
	}
	return(list(feature_importances = feature_importances, p = p_field_data))
}