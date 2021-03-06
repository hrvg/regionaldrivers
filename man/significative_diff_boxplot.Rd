% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/significative_diff_boxplot.R
\name{significative_diff_boxplot}
\alias{significative_diff_boxplot}
\title{Performs a pair-wise statistical test with posthoc test}
\usage{
significative_diff_boxplot(
  melted,
  type = "Tukey",
  padjmeth = "bonferroni",
  ypos = 1.5,
  step_increase = 0.1,
  pvalue = 0.05,
  no_label = FALSE
)
}
\arguments{
\item{melted}{a `data.frame` with a column `value`, the numeric value of the variable to test. Usually `melted` will be obtained using `reshape2::melt()}

\item{type}{`character`, controls the switch between different types of tests: `Dunn` uses `rstatix::dunn_test_test()` `Tukey` uses `rstatix::tukey_hsd()` and `Wilcox` uses `rstatix::wilcox_test()`.}

\item{padjmeth}{`character`, passed to `p.adjust.method`, controls the type of p-value adjustement}

\item{ypos}{`numeric`, graphical parameter}

\item{step_increase}{`numeric`, graphical parameter}

\item{pvalue}{`numeric`, p-value threshold}

\item{no_label}{`logical`, if `TRUE` remove the labels on the plot}
}
\value{
a named list with three elements `p` a `ggplot` plot, `stats` the statistics table of the test and `p_nop` a `ggplot` plot
}
\description{
This is a convenience function wrapper around `rstatix` and `ggpubr` functions.
}
\keyword{function}
