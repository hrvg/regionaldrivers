#' Visualize the p-value result from bootstrapping analysis
#' @param l_boot a `list` generated with `bootstrap_freq()`
#' @param alpha `numeric`, a confidence level, default to `0.95`.
#' @return a `ggplot` object
#' @export 
#' @keywords function
#' @importFrom rlang .data
plot_bootstrap_pvalue <- function(l_boot, alpha = 0.95){
	viz_pvalue <- l_boot[[1]] %>% 
		dplyr::add_rownames(var = "geology") %>% 
		reshape2::melt(id.vars = "geology") %>% 
		dplyr::rename(channel_type = .data$variable) %>%
		dplyr::mutate_if(is.character, as.factor) %>%
		dplyr::mutate(
			p.value = signif(.data$value, 2),
			geology = forcats::fct_rev(.data$geology), 
			opacity = ifelse(.data$value <= (1 - alpha), 1, 0.33)
		)
	p <- ggplot2::ggplot(viz_pvalue, ggplot2::aes(x = .data$channel_type, y = .data$geology, fill = .data$value, group = .data$p.value, alpha = .data$opacity)) +
		ggplot2::geom_tile(color = "white") +
		ggplot2::scale_fill_viridis_c(option = "cividis", direction = 1, begin = 0, end = 1, limits = c(0,1))  +
		ggplot2::labs(fill = "p-value",
			x = "channel type",
			y = "geology",
			title = "Bootstrapping ggplot2::results (p-value)"
			) +
		ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) +
		ggpubr::theme_pubr() +
		ggplot2::coord_fixed(ratio = 1)
	return(p)	
}

#' Visualize the probability result from bootstrapping analysis
#' @param l_boot a `list` generated with `bootstrap_freq()`
#' @param alpha `numeric`, a confidence level, default to `0.95`.
#' @importFrom rlang .data
#' @return a `ggplot` object
#' @export 
#' @keywords function
plot_bootstrap_probability <- function(l_boot, alpha = 0.95){
	viz_probability <- l_boot[[2]] %>% 
		dplyr::add_rownames(var = "number_groupings") %>% 
		reshape2::melt(id.vars = "number_groupings") %>% 
		dplyr::rename(channel_type = .data$variable) %>%
		dplyr::mutate_if(is.character, as.factor) %>%
		dplyr::mutate(
			probability = signif(.data$value, 2),
			number_groupings = forcats::fct_rev(.data$number_groupings),
			opacity = ifelse(.data$probability <= (1 - alpha), 1, 0.33)
		)
	p <- ggplot2::ggplot(viz_probability, ggplot2::aes(x = .data$channel_type, y = .data$number_groupings, fill = .data$value, group = .data$probability, alpha = .data$opacity)) +
		ggplot2::geom_tile(color = "white") +
		ggplot2::scale_fill_viridis_c(option = "cividis", direction = 1, begin = 0, end = 1, limits = c(0,1))  +
		ggplot2::labs(fill = "p-value",
			x = "channel type",
			y = "number of groupings",
			title = "Bootstrapping results (probability of having a given number of groupings)"
			) +
		ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) +
		ggpubr::theme_pubr() +
		ggplot2::coord_fixed(ratio = 1)
	return(p)
}

#' Visualize the p-value result from the statistical pair-wise analysis
#' @param stats a statistic table generated from `signif_diff_boxplot()`
#' @param alpha `numeric`, a confidence level, default to `0.95`.
#' @importFrom rlang .data
#' @importFrom utils head
#' @return a `ggplot` object
#' @export 
#' @keywords function
plot_pairwise_pvalue <- function(stats, alpha = 0.95){
	stats$group1 <- forcats::fct_reorder(stats$group1, as.numeric(stats$group1))
	stats$group2 <- forcats::fct_reorder(stats$group2, as.numeric(stats$group2))
	viz_pvalue <- stats %>% 
		dplyr::mutate(
			p.value = signif(.data$p.adj, 2),
			opacity = ifelse(.data$p.adj <= (1 - alpha), 1, 0.33)
		)
	p <- ggplot2::ggplot(viz_pvalue, ggplot2::aes(x = .data$group1, y = .data$group2, fill = .data$p.adj, group = .data$p.value, alpha = .data$opacity)) +
		ggplot2::geom_tile(color = "white") +
		ggplot2::scale_fill_viridis_c(option = "cividis", direction = 1, begin = 0, end = 1, limits = c(0,1))  +
		ggplot2::labs(fill = "p-value",
			x = "channel type",
			y = "channel type",
			title = "Pair-wise comparison results (p-value)"
			) +
		ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) +
		ggpubr::theme_pubr() +
		ggplot2::facet_wrap(~ .data$variable) +
		ggplot2::coord_fixed(ratio = 1)
	return(p)	
}