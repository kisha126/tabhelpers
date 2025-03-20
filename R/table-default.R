#' Display a table in the command line with customizable formatting.
#'
#' This function takes a x frame or tibble and displays it in the command line with various formatting options.
#' The table can be customized in terms of alignment, number formatting, column width, and more.
#'
#' @param x A data frame or tibble to display.
#' @param ... Additional arguments passed to specific methods.
#'
#' @export
table_default <- function(x, ...) UseMethod("table_default")

#' @describeIn table_default Default method for displaying tables.
#' @param justify_cols Controls column text alignment. Can be:
#'   - A single string ("left", "right", "center") to apply to all columns
#'   - A vector of strings to apply to columns by position
#'   - A named list (e.g., `list("1" = "left", "mpg" = "right")`) for specific columns
#' @param pos Logical. If `TRUE`, positive numbers are prefixed with a plus sign. Default is `FALSE`.
#' @param digits Number of digits to round numeric columns to. Default is `3`.
#' @param digits_by_col Named list specifying the number of digits for specific columns. Default is `NULL`.
#' @param scientific Logical. If `TRUE`, numeric values are displayed in scientific notation. Default is `FALSE`.
#' @param na_print Character string to represent missing values. Default is an empty string `""`.
#' @param min_width Minimum column width. Default is `NULL`.
#' @param border_char Character used for borders. Default is `"─"`.
#' @param show_row_names Logical. If `TRUE`, row names are displayed. Default is `FALSE`.
#' @param center_table Logical. If `TRUE`, the table is centered in the terminal. Default is `FALSE`.
#' @param n_space Number of spaces between columns. Default is `2`.
#' @param style_colnames Styling for column headers. Can be:
#'   - A character vector or list specifying cli color/style functions
#'     (e.g., `list("mpg" = "red", "cyl" = "blue_bold")`)
#'   - A list of lambda functions that take a context object and return a styled string
#'     (e.g., `list("mpg" = function(ctx) { cli::col_red(ctx$formatted_value) })`)
#' @param style_columns Styling for data cells. Can be:
#'   - A character vector or list specifying cli color/style functions
#'     (e.g., `list("mpg" = "green", "cyl" = "cyan")`)
#'   - A list of lambda functions that take a context object and return a styled string
#'     (e.g., `list("mpg" = function(ctx) { if(as.numeric(ctx$value) > 20) cli::col_green(ctx$formatted_value) else cli::col_red(ctx$formatted_value) })`)
#'
#' @examples
#' # Basic usage
#' table_default(head(mtcars))
#'
#' # Justify columns differently
#' table_default(head(mtcars), justify_cols = list("mpg" = "left", "cyl" = "right", "disp" = "center"))
#'
#' # Apply styling to column names
#' table_default(head(mtcars), style_colnames = list("mpg" = "red", "cyl" = "blue", "disp" = "green"))
#'
#' # Apply styling to data columns
#' table_default(head(mtcars), style_columns = list("mpg" = "cyan", "cyl" = "magenta"))
#'
#' # Using lambda functions for conditional styling
#' table_default(head(mtcars),
#'               style_columns = list(
#'                 "mpg" = function(ctx) {
#'                   if(as.numeric(ctx$value) > 20) {
#'                     cli::col_green(ctx$formatted_value)
#'                   } else {
#'                     cli::col_red(ctx$formatted_value)
#'                   }
#'                 },
#'                 "cyl" = function(ctx) {
#'                   if(as.numeric(ctx$value) == 4) {
#'                     cli::style_italic(cli::col_blue(ctx$formatted_value))
#'                   } else {
#'                     ctx$formatted_value
#'                   }
#'                 }
#'               ))
#'
#' # Comprehensive example with lambda functions
#' table_default(head(mtcars),
#'               justify_cols = list("mpg" = "left", "cyl" = "right"),
#'               style_colnames = list(
#'                 "mpg" = function(ctx) cli::col_red(cli::style_bold(ctx$formatted_value)),
#'                 "cyl" = "blue_italic"
#'               ),
#'               style_columns = list(
#'                 "mpg" = function(ctx) {
#'                   val <- as.numeric(ctx$value)
#'                   if(val > 20) cli::col_green(ctx$formatted_value)
#'                   else if(val > 15) cli::col_yellow(ctx$formatted_value)
#'                   else cli::col_red(ctx$formatted_value)
#'                 }
#'               ),
#'               digits = 1,
#'               border_char = "=",
#'               center_table = TRUE)
#'
#' @importFrom dplyr mutate across everything
#' @importFrom tibble as_tibble
#' @importFrom tidyselect where
#' @importFrom cli console_width col_red col_blue col_green
#'
#' @export
table_default.default <- function(x,
                                  justify_cols = NULL,
                                  pos = FALSE,
                                  digits = 3,
                                  digits_by_col = NULL,
                                  scientific = FALSE,
                                  na_print = "",
                                  min_width = NULL,
                                  border_char = "─",
                                  show_row_names = FALSE,
                                  center_table = FALSE,
                                  n_space = 2,
                                  style_colnames = NULL,
                                  style_columns = NULL,
                                  ...) {
    x <- tibble::as_tibble(x)
    original_x <- x

    if (show_row_names) {
        x <- dplyr::mutate(x, row_names = rownames(as.data.frame(x)), .before = 1)
        original_x <- dplyr::mutate(original_x, row_names = rownames(as.data.frame(original_x)), .before = 1)
    }

    x <- dplyr::mutate(x, dplyr::across(tidyselect::where(is.factor), as.character))
    x <- dplyr::mutate(x, dplyr::across(dplyr::everything(), ~ifelse(is.na(.), na_print, .)))

    if (!is.null(digits_by_col)) {
        for (col_name in names(digits_by_col)) {
            if (col_name %in% names(x) && is.numeric(x[[col_name]])) {
                x[[col_name]] <- if(scientific) {
                    format(x[[col_name]], digits = digits_by_col[[col_name]], scientific = TRUE)
                } else {
                    format(round(x[[col_name]], digits = digits_by_col[[col_name]]),
                           nsmall = digits_by_col[[col_name]])
                }
            }
        }
    }

    x <- dplyr::mutate(x, dplyr::across(tidyselect::where(is.numeric), ~{
        if(all(. %% 1 == 0, na.rm = TRUE)) {
            as.character(.)
        } else if(scientific) {
            format(., digits = digits, scientific = TRUE)
        } else {
            format(round(., digits = digits), nsmall = digits)
        }
    }))

    x <- dplyr::mutate(x, dplyr::across(dplyr::everything(), as.character))

    col_names <- colnames(x)
    x_chars <- as.matrix(x)

    col_widths <- pmax(nchar(col_names),
                       apply(x_chars, 2, function(x) max(nchar(x))))

    if (!is.null(min_width)) {
        col_widths <- pmax(col_widths, min_width)
    }

    total_width <- sum(as.numeric(col_widths)) + n_space * (length(col_widths) - 1) + 4

    horizontal_line <- paste0(rep(border_char, total_width), collapse = "")

    terminal_width <- if (center_table) {
        tryCatch({
            cli::console_width()
        }, error = function(e) {
            as.double(options("width"))
        })
    } else {
        0
    }

    left_padding <- if (center_table && terminal_width > total_width) {
        paste0(rep(" ", floor((terminal_width - total_width) / 2)), collapse = "")
    } else {
        ""
    }

    cat(left_padding, horizontal_line, "\n", sep = "")

    # Format and print column names with styling
    header_row <- format_row(col_names, col_widths, justify_cols = justify_cols,
                             pos = pos, n_space = n_space, styles = style_colnames,
                             col_data = original_x, is_header = TRUE)
    cat(left_padding, header_row, "\n", sep = "")

    cat(left_padding, horizontal_line, "\n", sep = "")

    # Format and print data rows with styling
    for (i in 1:nrow(x_chars)) {
        cat(left_padding, format_row(x_chars[i,], col_widths, justify_cols = justify_cols,
                                     pos = pos, n_space = n_space, styles = style_columns,
                                     col_data = original_x, is_header = FALSE), "\n", sep = "")
    }

    cat(left_padding, horizontal_line, "\n", sep = "")
}

