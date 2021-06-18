#' Compute feature importance
#' @param training_data a `data.frame`
#' @param target_colname `character` the name of the column containing the target (output)
#' @param filter_methods `list` of `character` accepted by `mlr::generateFilterValuesData()`
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
feature_importance <- function(
	training_data,
	target_colname,
	filter_methods = c("FSelectorRcpp_information.gain"),
	.iters = 500,
	.first = 30,
	.split = .8,
	.stratify = TRUE,
	.seed = 1789) {
	df <- ind <- m <- NULL
	set.seed(.seed)
	target <- training_data[[target_colname]]
	training_data <- training_data %>% dplyr::select(-target_colname) %>% sanitize_data()
	training_data[[target_colname]] <- target 
	training_data <- training_data %>% stats::na.omit()
	if (is.factor(target) || is.integer(target) || is.character(target)){
		task <- mlr::makeClassifTask(data = training_data, target = target_colname)
		resampleDesc <- mlr::makeResampleDesc("Subsample", stratify = .stratify, iters = .iters, split = .split)
	} else {
		task <- mlr::makeRegrTask(data = training_data, target = target_colname)
		resampleDesc <- mlr::makeResampleDesc("Subsample", iters = .iters, split = .split)

	}
	rs <- mlr::makeResampleInstance(resampleDesc, task)$train.inds
	feature_importances <- 	foreach(m = filter_methods) %do% {
		fvs <- foreach(ind = rs, .combine = rbind) %do% {
			set.seed(.seed)
			if (is.factor(target) || is.integer(target) || is.character(target)){
				resampled_task <- mlr::makeClassifTask(data = training_data[ind, ], target = target_colname)
			} else {
				resampled_task <- mlr::makeRegrTask(data = training_data[ind, ], target = target_colname)
			}	
			fv <- mlr::generateFilterValuesData(resampled_task, method = m)
			return(as.data.frame(fv$data))
		}
		df <- fvs %>% 
			dplyr::rename(var = .data$name) %>%
			dplyr::group_by(.data$var) %>%
			dplyr::summarize(Overall = mean(.data$value)) %>%
			dplyr::arrange(-.data$Overall)
		return(df)
	}
	p_field_data <- foreach(df = feature_importances, m = filter_methods) %do% {
		make_feature_importance_plot(df, first = .first) + ggplot2::labs(x = "value", title = m)
	}
	return(list(feature_importances = feature_importances, p = p_field_data))
}