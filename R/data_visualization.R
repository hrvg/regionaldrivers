#' Visualize the p-value result from bootstrapping analysis
#' @param l_boot a `list` generated with `bootstrap_freq()`
#' @return a `ggplot` object
#' @export 
#' @keywords function
#' @importFrom rlang .data
plot_bootstrap_pvalue <- function(l_boot){
	viz_pvalue <- l_boot[[1]] %>% 
		dplyr::add_rownames(var = "geology") %>% 
		reshape2::melt(id.vars = "geology") %>% 
		dplyr::rename(channel_type = .data$variable) %>%
		dplyr::mutate_if(is.character, as.factor) %>%
		dplyr::mutate(
			p.value = signif(.data$value, 2),
			geology = forcats::fct_rev(.data$geology)
		)
	p <- ggplot2::ggplot(viz_pvalue, ggplot2::aes(x = .data$channel_type, y = .data$geology, fill = .data$value, group = .data$p.value)) +
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
#' @importFrom rlang .data
#' @return a `ggplot` object
#' @export 
#' @keywords function
plot_bootstrap_probability <- function(l_boot){
	viz_probability <- l_boot[[2]] %>% 
		dplyr::add_rownames(var = "number_groupings") %>% 
		reshape2::melt(id.vars = "number_groupings") %>% 
		dplyr::rename(channel_type = .data$variable) %>%
		dplyr::mutate_if(is.character, as.factor) %>%
		dplyr::mutate(
			probability = signif(.data$value, 2),
			number_groupings = forcats::fct_rev(.data$number_groupings)
		)
	p <- ggplot2::ggplot(viz_probability, ggplot2::aes(x = .data$channel_type, y = .data$number_groupings, fill = .data$value, group = .data$probability)) +
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
#' @param diag `logical` if `TRUE` the diagonal elements of the pairwise matrix are added to the plot
#' @importFrom rlang .data
#' @importFrom utils head
#' @return a `ggplot` object
#' @export 
#' @keywords function
plot_pairwise_pvalue <- function(stats, diag = FALSE){
	if (diag){
		grps <- unique(c(as.character(stats$group1), as.character(stats$group2))) %>% as.numeric()
		ns <- unique(c(as.character(stats$n1), as.character(stats$n2))) %>% as.numeric()
		.stats <- data.frame(
			variable = unique(stats$variable) %>% rep(each = length(grps)),
			.y. = unique(stats$.y.),
			group1 = grps,
			group2 = grps,
			n1 = ns,
			n2 = ns,
			statistic = NA,
			p = 0,
			p.adj = 0,
			p.adj.signif = "****",
			y.position = unique(stats$y.position) %>% rep(each = length(grps))
			)
		stats <- rbind(stats, .stats)
	}
	stats$group1 <- forcats::fct_reorder(stats$group1, as.numeric(stats$group1))
	stats$group2 <- forcats::fct_reorder(stats$group2, as.numeric(stats$group2))
	viz_pvalue <- stats %>% 
		dplyr::mutate(
			p.value = signif(.data$p.adj, 2)		)
	p <- ggplot2::ggplot(viz_pvalue, ggplot2::aes(x = .data$group1, y = .data$group2, fill = .data$p.adj, group = .data$p.value)) +
		ggplot2::facet_wrap(~ .data$variable) +
		ggplot2::geom_tile() +
		ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) +
		ggpubr::theme_pubr() +
		ggplot2::scale_fill_viridis_c(option = "cividis", direction = 1, begin = 0, end = 1, limits = c(0,1))  +
		ggplot2::labs(fill = "p-value",
			x = "channel type",
			y = "channel type",
			title = "Pair-wise comparison results (p-value)"
			) +
		ggplot2::coord_fixed(ratio = 1) +
		ggplot2::scale_alpha(guide = 'none')
	return(p)	
}