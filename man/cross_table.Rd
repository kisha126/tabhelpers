% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross-tab.R
\name{cross_table}
\alias{cross_table}
\title{Generate and Display a Cross Tabulation Table}
\usage{
cross_table(
  data,
  percentage = NULL,
  layout = TRUE,
  expected = TRUE,
  layout_center = FALSE,
  center_table = FALSE,
  style = NULL,
  ...
)
}
\arguments{
\item{data}{A matrix or table containing the observed frequencies for the cross-tabulation.}

\item{percentage}{A character vector or logical value specifying which percentages to display.
Options include:
\itemize{
\item \code{TRUE} or \code{"all"}: Display all percentages (row, column, and total).
\item \code{"by_row"}: Display row percentages.
\item \code{"by_col"}: Display column percentages.
\item \code{"by_total"}: Display total percentages.
\item \code{FALSE} or \code{NULL}: Do not display any percentages.
}}

\item{layout}{Logical. If \code{TRUE}, a layout box explaining the table structure is displayed.}

\item{expected}{Logical. If \code{TRUE}, expected frequencies are displayed alongside observed frequencies.}

\item{layout_center}{Logical. If \code{TRUE}, the layout box is centered in the terminal.}

\item{center_table}{Logical. If \code{TRUE}, the entire table is centered in the terminal.}

\item{style}{Default to \code{NULL}. If provided, the components in the displayed table can be styled.
It has 9 components to be styled:
\itemize{
\item \code{observed} You can style the displayed observed values.
\item \code{expected}: You can style the displayed expected values.
\item \code{total}: You can style the displayed total frequencies (marginal and grand total).
\item \code{row_percentage}: You can style the displayed row percentages.
\item \code{col_percentage}: You can style the displayed column percentages.
\item \code{total_percentage}: You can style the marginal percentages.
\item \code{title}: You can style the title.
\item \code{border}: You can choose what symbol you can use to draw the table.
\item \code{border_text}: You can style the table
}}

\item{...}{Additional arguments (not currently used).}
}
\value{
Invisibly returns the formatted cross-tabulation table as a matrix. The table is printed to the console/command line.
}
\description{
This function creates a cross-tabulation (contingency) table from a given data matrix or table.
It supports displaying observed frequencies, expected frequencies, and various percentages
(row, column, and total). The table can be formatted and centered in the terminal for better readability.
}
\examples{
# Example 1: Basic cross-tabulation
data <- matrix(c(10, 20, 30, 40), nrow = 2, byrow = TRUE)
cross_table(data)

# Example 2: Display row and column percentages
cross_table(data, percentage = c("by_row", "by_col"))

# Example 3: Overall component with style
matrix(c(10, 20, 30, 40), nrow = 2, byrow = TRUE) |> # Use `\%>\%` from `magrittr` if preferred
    cross_table(
        percentage = "all",
        style = list(
            observed = \(ctx) cli::col_yellow(ctx$formatted_text),
            expected = \(ctx) cli::style_italic(ctx$formatted_text),
            row_percentage = "blue_italic",
            col_percentage = "blue_italic",
            total_percentage = "red_italic",
            total = \(ctx) cli::style_italic(ctx$formatted_text),
            border = "=",
            border_text = "cyan",
            title = \(ctx) cli::style_bold(cli::col_blue(ctx$formatted_text))
        )
    )

}
