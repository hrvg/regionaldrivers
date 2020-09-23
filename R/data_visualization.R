#' Visualize the p-value result from bootstrapping analysis
#' @param l_boot a `list` generated with `bootstrap_freq()`
#' @return a `ggplot` object
#' @export 
#' @keywords function
#' @import ggplot2
plot_bootstrap_pvalue <- function(l_boot){
	viz_pvalue <- l_boot[[1]] %>% 
		dplyr::mutate(geology = row.names(.)) %>% 
		reshape2::melt(id.vars = "geology") %>% 
		dplyr::rename(channel_type = variable) %>%
		dplyr::mutate_if(is.character, as.factor) %>%
		dplyr::mutate(p.value = signif(value, 2)) %>%
		dplyr::mutate(geology = forcats::fct_rev(geology))
	p <- ggplot(viz_pvalue, aes(x = channel_type, y = geology, fill = value, group = p.value)) +
		geom_tile(color = "white") +
		scale_fill_viridis_c(option = "viridis", direction = 1, begin = 0, end = 1, limits = c(0,1))  +
		labs(fill = "p-value",
			x = "channel type",
			y = "geology",
			title = "Bootstrapping results (p-value)"
			) +
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
		ggpubr::theme_pubr() +
		coord_fixed(ratio = 1)
	return(p)	
}

#' Visualize the probability result from bootstrapping analysis
#' @param l_boot a `list` generated with `bootstrap_freq()`
#' @return a `ggplot` object
#' @export 
#' @keywords function
#' @import ggplot2
plot_bootstrap_probability <- function(l_boot){
	viz_prob <- l_boot[[2]] %>% 
		dplyr::mutate(number_groupings = row.names(.)) %>% 
		reshape2::melt(id.vars = "number_groupings") %>% 
		dplyr::rename(channel_type = variable) %>%
		dplyr::mutate_if(is.character, as.factor) %>%
		dplyr::mutate(prob = signif(value, 2)) %>%
		dplyr::mutate(number_groupings = forcats::fct_rev(number_groupings))
	p <- ggplot(viz_prob, aes(x = channel_type, y = number_groupings, fill = value, group = prob)) +
		geom_tile(color = "white") +
		scale_fill_viridis_c(option = "viridis", direction = 1, begin = 0, end = 1, limits = c(0,1))  +
		labs(fill = "p-value",
			x = "channel type",
			y = "number of groupings",
			title = "Bootstrapping results (probability of having a given number of groupings"
			) +
		theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
		ggpubr::theme_pubr() +
		coord_fixed(ratio = 1)
	return(p)
}