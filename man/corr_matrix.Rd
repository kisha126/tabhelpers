% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor-tab.R
\name{corr_matrix}
\alias{corr_matrix}
\title{Display a Correlation Matrix Table in R Command Line}
\usage{
corr_matrix(
  data,
  method = NULL,
  corr = NULL,
  var1 = NULL,
  var2 = NULL,
  statistic = NULL,
  pval = NULL,
  ci_lc = NULL,
  ci_uc = NULL,
  LT = TRUE,
  UT = TRUE,
  layout_view = FALSE,
  digits = 3,
  center_table = FALSE,
  center_layout = FALSE,
  col_mapping = NULL,
  border_char = getOption("tab_default")$border_char,
  style = list(),
  ...
)
}
\arguments{
\item{data}{Either a symmetric correlation matrix or a data frame containing pairwise
correlation results with variable names, correlation coefficients, and optionally
test statistics, p-values, and confidence intervals.}

\item{method}{Character string specifying the correlation method (e.g., "Pearson", "Spearman").
If NULL, tries to extract from the data.}

\item{corr}{Column name containing correlation coefficient values. If NULL (default),
tries to automatically identify using standard names like "cor", "r", "estimate", or "correlation".}

\item{var1}{Column name containing first variable names. If NULL (default), tries to
automatically identify using standard names like "var1", "variable1", "x", or "X".}

\item{var2}{Column name containing second variable names. If NULL (default), tries to
automatically identify using standard names like "var2", "variable2", "y", or "Y".}

\item{statistic}{Column name containing test statistics (e.g., t-values). If the column
doesn't exist, a warning is issued and statistics aren't displayed.}

\item{pval}{Column name containing p-values. If the column doesn't exist, a warning is
issued and p-values aren't displayed.}

\item{ci_lc}{Column name containing lower confidence interval bounds. If the column
doesn't exist, a warning is issued.}

\item{ci_uc}{Column name containing upper confidence interval bounds. If the column
doesn't exist, a warning is issued.}

\item{LT}{Logical. If TRUE (default), shows correlations in the lower triangle of the matrix.}

\item{UT}{Logical. If TRUE (default), shows correlations in the upper triangle of the matrix.}

\item{layout_view}{Logical. If TRUE, displays a visual representation of the output layout.}

\item{digits}{Integer specifying the number of decimal places to display.}

\item{center_table}{Logical. If TRUE, centers the entire table in the console.}

\item{center_layout}{Logical. If TRUE, centers the layout view in the console.}

\item{col_mapping}{Named list mapping column types to specific column names in the data.
Supported types are "variable1", "variable2", "correlation", "statistic", "pvalue",
"lower_ci", and "upper_ci".}

\item{border_char}{A character to draw border. Default is \code{"\\u2500"}.}

\item{style}{A list controlling the visual styling of table elements using ANSI formatting.
Can include the following components:
\itemize{
\item \code{corr}: Styling for the correlation values.
\item \code{statistic}: Styling for the test statistics.
\item \code{pval}: Styling for the p-values.
\item \code{lower_ci}: Styling for the lower Confidence Interval values.
\item \code{upper_ci}: Styling for the upper Confidence Interval values.
\item \code{border_text}: Styling for the border.
\item \code{title}: Styling for the title.
}

Each style component can be either a predefined style string (e.g., "blue", "red_italic", "bold")
or a function that takes a context list with/without a \code{value} element and returns the styled text.}

\item{...}{Additional arguments (not currently used).}
}
\value{
Invisibly returns the formatted data that was printed.
}
\description{
Creates and prints a formatted correlation matrix from either a correlation matrix
or a data frame containing pairwise correlation results (such as from \code{rstatix::cor_test()}).
}
\details{
The function can handle two types of inputs:
\itemize{
\item A symmetric correlation matrix (e.g., from \code{cor()})
\item A data frame with pairwise correlations (e.g., from \code{rstatix::cor_test()})
}

For data frames, the function tries to identify relevant columns using standard names,
but you can explicitly specify column names using the function parameters or the
\code{col_mapping} parameter.

The \code{col_mapping} parameter accepts a named list that maps column types to specific
column names in your data frame. For example:
\code{list(correlation = "cor_value", statistic = "t_value", pvalue = "p_adjusted")}
}
\examples{
# With a correlation matrix
corr_matrix(cor(mtcars[, 1:4]), method = "Pearson")

# With a data frame from rstatix::cor_test()

require(rstatix)

mtcars |>
    rstatix::cor_test(disp, wt, hp) |>
    corr_matrix(
      statistic = statistic,
      pval = p,
      layout_view = TRUE
    )

mtcars |>
    rstatix::cor_test(disp, wt, hp) |>
    corr_matrix(
        col_mapping = list(
            statistic = "statistic",
            pvalue = "p"
        )
    )

# With style

mtcars |>
    rstatix::cor_test(disp, wt, hp) |>
    corr_matrix(
        statistic = "statistic",
        pval = "p",
        ci_lc = conf.low,
        ci_uc = conf.high,
        style = list(
            corr = 'blue',
            statistic = 'red',
            pval = 'green',
            lower_ci = 'magenta',
            upper_ci = 'cyan',
            title = "red",
            border_text = function(x) {
                st_style <- cli::make_ansi_style("limegreen")
                st_style(x)
            }
        ),
        layout_view = TRUE
    )

# Try conditional formatting in `corr` value
iris |>
    rstatix::cor_test(Sepal.Length, Sepal.Width, Petal.Length) |>
    corr_matrix(
        statistic = "statistic",
        pval = "p",
        ci_lc = conf.low,
        ci_uc = conf.high,
        style = list(
            corr = function(x) {
                # First try to convert to numeric
                num_x <- suppressWarnings(as.numeric(x))
                if (!is.na(num_x)) {
                    # If numeric, apply the conditional formatting
                    num_x <- abs(num_x)
                    if (num_x >= 0.8 & num_x <= 1) {
                        cli::col_red(x)
                    } else if (num_x >= 0.6 & num_x < 0.8) {
                        cli::col_blue(x)
                    } else if (num_x >= 0.4 & num_x < 0.6) {
                        cli::col_yellow(x)
                    } else {
                        cli::col_green(x)
                    }
                } else {
                    cli::col_red(x)
                }
            },
            pval = \(x) if (x > 0.05) cli::style_italic(x) else cli::col_red(x),
            lower_ci = 'magenta',
            upper_ci = 'cyan',
            title = "red"
        ),
        layout_view = TRUE
    )

}
