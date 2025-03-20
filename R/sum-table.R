#' Summarize and Display a Two-Column Data Frame as a Formatted Table
#'
#' This function takes a two-column data frame and formats it into a readable table.
#' The table can be optionally split into two parts, centered, and given a title.
#' It is useful for displaying summary information in a clean, tabular format.
#'
#' @param data A data frame with exactly two columns. The data to be summarized and displayed.
#' @param title A character string. An optional title to be displayed above the table.
#' @param l An integer. The number of rows to include in the left part of a split table.
#'           If `NULL`, the table is not split.
#' @param header A logical value. If `TRUE`, the column names of `data` are displayed as a header.
#' @param center_table A logical value. If `TRUE`, the table is centered in the terminal.
#' @param border_char Character used for borders. Default is `"─"`.
#' @param ... Additional arguments (currently unused).
#'
#' @return This function does not return a value. It prints the formatted table to the console.
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(
#'   Category = c("A", "B", "C", "D", "E"),
#'   Value = c(10, 20, 30, 40, 50)
#' )
#'
#' # Display the table with a title and header
#' table_summary(df, title = "Sample Table", header = TRUE)
#'
#' # Split the table after the second row and center it
#' table_summary(df, l = 2, center_table = TRUE)
#'
#' @export
table_summary <- function(data,
                          title = NULL,
                          l = NULL,
                          header = FALSE,
                          center_table = FALSE,
                          border_char = "─", ...) {
    if (!is.data.frame(data) || ncol(data) != 2) {
        stop("Input must be a data frame with exactly 2 columns")
    }

    col_names <- colnames(data)
    data_matrix <- as.matrix(data)

    n_rows <- nrow(data)
    is_split <- !is.null(l) && l < n_rows

    if (is_split) {
        left_table <- data_matrix[1:l, ]
        right_table <- data_matrix[(l+1):n_rows, ]

        left_left_width <- max(nchar(left_table[, 1]), nchar(col_names[1]))
        left_right_width <- max(nchar(left_table[, 2]), nchar(col_names[2]))
        right_left_width <- max(nchar(right_table[, 1]), nchar(col_names[1]))
        right_right_width <- max(nchar(right_table[, 2]), nchar(col_names[2]))

        left_table_width <- left_left_width + left_right_width + 8
        right_table_width <- right_left_width + right_right_width + 8
        full_width <- left_table_width + right_table_width
    } else {
        left_width <- max(nchar(data_matrix[, 1]), nchar(col_names[1]))
        right_width <- max(nchar(data_matrix[, 2]), nchar(col_names[2]))
        full_width <- left_width + right_width + 8
    }

    horizontal_line <- strrep(border_char, full_width)

    # Apply centering if center_table is TRUE
    prefix <- ""
    if (center_table) {
        # Get terminal width if possible, otherwise use a reasonable default
        term_width <- tryCatch({
            as.numeric(system("tput cols", intern = TRUE))
        }, error = function(e) {
            as.double(options("width"))  # Default terminal width
        })

        # Calculate left padding for centering
        left_padding <- max(0, floor((term_width - full_width) / 2))
        prefix <- strrep(" ", left_padding)
    }

    if (!is.null(title)) {
        if (center_table) {
            cat("\n", prefix, align_test(title, full_width), "\n", sep = "")
        } else {
            cat("\n", align_test(title, full_width), "\n", sep = "")
        }
    }

    cat(prefix, horizontal_line, "\n", sep = "")

    if (header) {
        if (is_split) {
            header_row <- paste0(
                format_row_summary(col_names[1], col_names[2], left_left_width, left_right_width),
                "  ",
                format_row_summary(col_names[1], col_names[2], right_left_width, right_right_width)
            )
        } else {
            header_row <- format_row_summary(col_names[1], col_names[2], left_width, right_width)
        }
        cat(prefix, header_row, "\n", sep = "")
        cat(prefix, horizontal_line, "\n", sep = "")
    }

    if (!is_split) {
        for (i in 1:n_rows) {
            cat(prefix, format_row_summary(data_matrix[i, 1], data_matrix[i, 2], left_width, right_width), "\n", sep = "")
        }
    } else {
        for (i in 1:max(l, nrow(right_table))) {
            left_row <- if (i <= l) left_table[i, ] else c("", "")
            right_row <- if (i <= nrow(right_table)) right_table[i, ] else c("", "")
            row_output <- paste0(
                format_row_summary(left_row[1], left_row[2], left_left_width, left_right_width),
                "  ",
                format_row_summary(right_row[1], right_row[2], right_left_width, right_right_width)
            )
            cat(prefix, row_output, "\n", sep = "")
        }
    }

    cat(prefix, horizontal_line, "\n", sep = "")
}
