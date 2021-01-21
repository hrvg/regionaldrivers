#' Makes a dot chart of feature importance
#' @param feature_importance a `data.frame` with column `Overall` and `var`
#' @param first `numeric`, number of feature to display
#' @param best `logical`, default to `FALSE`, to highlight which features are selected in an optimal (best) set
#' @return a `ggplot` object
#' @importFrom rlang .data
#' @keywords function
#' @export
make_feature_importance_plot <- function(feature_importance, first = 20, best = FALSE){
	feature_importance <- fix_varnames(feature_importance, var = "var")
	feature_importance$type <- factor(feature_importance$type, 
		levels = c("TAM", "GIS", "Statistical roughness", "Topology", "Contextual"),
		labels = c("TAM", "GIS", "Statistical roughness", "Topology", "Contextual"))
	feature_importance <- utils::head(feature_importance, first)
	if (best){
		p <-    ggplot2::ggplot(feature_importance, ggplot2::aes(color = .data$type)) +
		        ggplot2::geom_point(ggplot2::aes(x=.data$Overall, y=forcats::fct_reorder(.data$var,.data$Overall))) +
				ggplot2::geom_segment(ggplot2::aes(linetype = .data$best, x = 0, xend = .data$Overall, y = forcats::fct_reorder(.data$var,.data$Overall), yend = forcats::fct_reorder(.data$var,.data$Overall))) +
		        ggplot2::labs(title = "RF model importance (top 20 variables)", y = "variable", x = "variable importance") + 
				ggplot2::theme_minimal() +
		        ggplot2::scale_color_discrete(drop = FALSE)
		p	
	} else {
		p <-    ggplot2::ggplot(feature_importance, ggplot2::aes(color = .data$type)) +
		        ggplot2::geom_point(ggplot2::aes(x=.data$Overall, y=forcats::fct_reorder(.data$var,.data$Overall))) +
				ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$Overall, y = forcats::fct_reorder(.data$var,.data$Overall), yend = forcats::fct_reorder(.data$var,.data$Overall))) +
		        ggplot2::labs(title = "RF model importance (top 20 variables)", y = "variable", x = "variable importance") + 
				ggplot2::theme_minimal() +
		        ggplot2::scale_color_discrete(drop = FALSE)
		p		
	}
}