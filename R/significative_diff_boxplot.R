#' Performs a pair-wise statistical test with posthoc test 
#' 
#' This is a convenience function wrapper around `rstatix` and `ggpubr` functions.
#' 
#' @param melted a `data.frame` with a column `value`, the numeric value of the variable to test. Usually `melted` will be obtained using `reshape2::melt()
#' @param type `character`, controls the switch between different types of tests: `Dunn` uses `rstatix::dunn_test_test()` `Tukey` uses `rstatix::tukey_hsd()` and `Wilcox` uses `rstatix::wilcox_test()`.
#' @param padjmeth `character`, passed to `p.adjust.method`, controls the type of p-value adjustement
#' @param ypos `numeric`, graphical parameter
#' @param step_increase `numeric`, graphical parameter
#' @importFrom magrittr %>%
#' @export
#' @keywords function
#' @returns a named list with two elements `p` a `ggplot` plot and `stats` the statistics table of the test
significative_diff_boxplot <- function(melted, type = "Tukey", padjmeth = "bonferroni", ypos = 1.5, step_increase = 0.1){
	melted <- dplyr::group_by(melted, variable)
	stat_test <- switch(type, 
		"Dunn" = melted %>% rstatix::dunn_test(value ~ ward.grp, p.adjust.method = padjmeth),
		"Tukey" = melted %>% rstatix::tukey_hsd(value ~ ward.grp, p.adjust.method = padjmeth),
		"Wilcox" = melted %>% rstatix::wilcox_test(value ~ ward.grp, p.adjust.method = padjmeth)
		)
	stat_test <- merge(stat_test, dplyr::summarize_at(melted, "value", max) %>% dplyr::rename(y.position = value))
	p <- ggpubr::ggboxplot(melted, 
		x = "ward.grp", 
		y = "value", 
		color = "ward.grp", 
		facet.by = "variable",
		scales = "free",
		title = paste(type, padjmeth, sep = ", ")) +
		ggpubr::stat_pvalue_manual(
			stat_test %>% as.data.frame(), 
			label = "p.adj.signif", 
			step.increase = step_increase, 
			step.group.by = c("variable"), 
			hide.ns = TRUE)
	return(list(p = p, stats = stat_test))	
}