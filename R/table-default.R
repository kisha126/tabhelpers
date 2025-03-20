#' Display a table in the command line with customizable formatting.
#'
#' @param data A data frame or tibble to display.
#' @param left_align_first Logical. If TRUE, the first column is left-aligned.
#' @param pos Logical. If TRUE, positive numbers are prefixed with a plus sign.
#' @param digits Number of digits to round numeric columns to.
#' @param digits_by_col Named list specifying digits for specific columns.
#' @param scientific Logical. If TRUE, numeric values are displayed in scientific notation.
#' @param na_print Character string to represent missing values.
#' @param min_width Minimum column width.
#' @param border_char Character used for borders.
#' @param show_row_names Logical. If TRUE, row names are displayed.
#' @param center_table Logical. If TRUE, the table is centered in the terminal.
#' @param n_space Number of spaces between columns.
#' @param ... Additional arguments (not used).
#'
#' @importFrom dplyr mutate across everything
#' @importFrom tibble as_tibble
#' @importFrom tidyselect where
#' @importFrom cli console_width
#'
#' @export
table_default <- function(data,
                          left_align_first = FALSE,
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
                          ...) {
    data <- as_tibble(data)

    if (show_row_names) {
        data <- data |>
            mutate(row_names = rownames(as.data.frame(data)), .before = 1)
    }

    data <- mutate(data, across(where(is.factor), as.character))

    data <- mutate(data, across(everything(), ~ifelse(is.na(.), na_print, .)))

    if (!is.null(digits_by_col)) {
        for (col_name in names(digits_by_col)) {
            if (col_name %in% names(data) && is.numeric(data[[col_name]])) {
                data[[col_name]] <- if(scientific) {
                    format(data[[col_name]], digits = digits_by_col[[col_name]], scientific = TRUE)
                } else {
                    format(round(data[[col_name]], digits = digits_by_col[[col_name]]),
                           nsmall = digits_by_col[[col_name]])
                }
            }
        }
    }

    data <- mutate(data, across(where(is.numeric), ~{
        if(all(. %% 1 == 0, na.rm = TRUE)) {
            as.character(.)
        } else if(scientific) {
            format(., digits = digits, scientific = TRUE)
        } else {
            format(round(., digits = digits), nsmall = digits)
        }
    }))

    data <- mutate(data, across(everything(), as.character))

    col_names <- colnames(data)
    data_chars <- as.matrix(data)

    col_widths <- pmax(nchar(col_names),
                       apply(data_chars, 2, function(x) max(nchar(x))))

    if (!is.null(min_width)) {
        col_widths <- pmax(col_widths, min_width)
    }

    # Change here - use n_space instead of hardcoded 3
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
    cat(left_padding, format_row(col_names, col_widths, left_align_first = left_align_first, pos = pos, n_space = n_space), "\n", sep = "")
    cat(left_padding, horizontal_line, "\n", sep = "")
    apply(data_chars, 1, function(row)
        cat(left_padding, format_row(row, col_widths, left_align_first = left_align_first, pos = pos, n_space = n_space), "\n", sep = ""))
    cat(left_padding, horizontal_line, "\n", sep = "")
}
