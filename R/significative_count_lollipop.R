#' Makes a lollipop chart from pair-wise comparison data
#' @param pairwise_results a `list` passed from `significative_diff_boxplot()`
#' @param pvalue `numeric` the threshold p-value
#' @param normalize `logical` if `TRUE`, normalize the count by the number of comparison
#' @param first `numeric` controls the number of variables plotted
#' @importFrom rlang .data
#' @importFrom utils head
#' @export
#' @keywords function
#' @returns a named list with two elements `p` a `ggplot` plot and `stats` the count table of the significative value
significative_count_lollipop <- function(pairwise_results, pvalue = 0.05, normalize = FALSE, first = 25){
	signif_count <- pairwise_results$stats %>% 
		dplyr::filter(.data$p.adj <= pvalue) %>%
		dplyr::group_by(.data$variable) %>%
		dplyr::group_modify(~ data.frame(n = nrow(.))) %>%
		dplyr::arrange(-.data$n) %>%
		fix_varnames(var = "variable")
	if (normalize){
		signif_count$n <- signif_count$n / choose(max(as.numeric(pairwise_results$stats$group2)), 2) 
	}	

	signif_count$type <- factor(signif_count$type, 
		levels = c("TAM", "GIS", "Statistical roughness", "Topology", "Contextual", "In-situ measurement"),
		labels = c("TAM", "GIS", "Statistical roughness", "Topology", "Contextual", "In-situ measurement"))

	p <- ggpubr::ggdotchart(
		head(signif_count, first), 
		x = "variable",
		y = "n",
		color = "type",
		add = "segments",
		sorting = "descending",
		rotate = TRUE,
		ggtheme = ggpubr::theme_pubr(),
		dot.size = 5) + 
		ggplot2::scale_color_discrete(drop = FALSE) +
		ggpubr::theme_cleveland()

	if (normalize) {
		p <- p + ggplot2::labs(y = "proportion of significant pair-wise comparisons", x = "variable")
	} else {
		p <- p + ggplot2::labs(y = "number of significant pair-wise comparisons", x = "variable")
	}
	return(list(stats = signif_count, p = p))
}