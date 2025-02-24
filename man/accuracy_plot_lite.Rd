% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accuracy_plot_lite.R
\name{accuracy_plot_lite}
\alias{accuracy_plot_lite}
\title{Accuracy analysis plot}
\usage{
accuracy_plot_lite(
  accuracy_results,
  cor_types = NULL,
  anchor_colors = science_colours_six()[c(1, 2)],
  supported_cor_types = supported_cor_expr()$cor_type,
  supported_cor_exprs = supported_cor_expr()$cor_expr,
  mae_txt_format = "\%1.3f",
  direction = "v",
  n_row = 3,
  n_row_legend = 5,
  ...
)
}
\arguments{
\item{accuracy_results, }{accuracy results form accuracy_db()}

\item{cor_types, }{correlation names}

\item{anchor_colors, }{colors}

\item{supported_cor_types, }{supported correlation types, as the levels of cor_types}

\item{supported_cor_exprs, }{the expressions of supported correlation types}

\item{mae_txt_format, }{the MAE text format}

\item{direction, }{the direction for facet_wrap, with "v" as default}

\item{n_row, }{number of rows for facet_wrap}

\item{n_row_legend, }{number of rows in the legend}
}
\value{
a ggplot2 object
}
\description{
plot the results of accuracy analysis with ggplot2
}
\details{
The workflow for complete accuracy analysis is as follows: 
\enumerate{
  \item Call accuracy_db() to generate the basic results
  \item Call accuracy_results_frm_db() to generate the accuracy results data.frame
  \item Call accuracy_plot_lite() to draw the figures
}
}
