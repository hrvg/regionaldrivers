% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_visualization.R
\name{plot_pairwise_pvalue}
\alias{plot_pairwise_pvalue}
\title{Visualize the p-value result from the statistical pair-wise analysis}
\usage{
plot_pairwise_pvalue(stats, diag = FALSE)
}
\arguments{
\item{stats}{a statistic table generated from `signif_diff_boxplot()`}

\item{diag}{`logical` if `TRUE` the diagonal elements of the pairwise matrix are added to the plot}
}
\value{
a `ggplot` object
}
\description{
Visualize the p-value result from the statistical pair-wise analysis
}
\keyword{function}
