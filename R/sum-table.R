#' Summarize and Display a Two-Column Data Frame as a Formatted Table
#'
#' This function takes a two-column data frame and formats it into a readable table.
#' The table can be optionally split into two parts, centered, and given a title.
#' It is useful for displaying summary information in a clean, tabular format.
#' The function also supports styling with ANSI colors and text formatting through
#' the `cli` package and column alignment options.
#'
#' @param data A data frame with exactly two columns. The data to be summarized and displayed.
#' @param title A character string. An optional title to be displayed above the table.
#' @param l An integer. The number of rows to include in the left part of a split table.
#'           If `NULL`, the table is not split.
#' @param header A logical value. If `TRUE`, the column names of `data` are displayed as a header.
#' @param center_table A logical value. If `TRUE`, the table is centered in the terminal.
#' @param border_char Character used for borders. Default is `"─"`.
#' @param style A list controlling the visual styling of table elements using ANSI formatting.
#'   Can include the following components:
#'   - `left_col`: Styling for the left column values.
#'   - `right_col`: Styling for the right column values.
#'   - `border`: Character to use for borders (overrides `border_char`).
#'   - `border_text`: Styling for the border.
#'   - `title`: Styling for the title.
#'   - `sep`: Separator character between left and right column.
#'
#'   Each style component can be either a predefined style string (e.g., "blue", "red_italic", "bold")
#'   or a function that takes a context list with/without a `value` element and returns the styled text.
#' @param align Controls the alignment of column values.
#'   Can be specified in three ways:
#'   - A single string: affects only the left column (e.g., "left", "center", "right").
#'   - A vector of two strings: affects both columns in order (e.g., c("left", "right")).
#'   - A list with named components: explicitly specifies alignment for each column
#'
#' @param ... Additional arguments (currently unused).
#'
#' @return This function does not return a value. It prints the formatted table to the console.
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(
#'     Category = c("A", "B", "C", "D", "E"),
#'     Value = c(10, 20, 30, 40, 50)
#' )
#'
#' # Display the table with a title and header
#' table_summary(df, title = "Sample Table", header = TRUE)
#'
#' # Split the table after the second row and center it
#' table_summary(df, l = 2, center_table = TRUE)
#'
#' # Use styling and alignment
#' table_summary(
#'     df, header = TRUE,
#'     style = list(
#'         left_col = "blue_bold",
#'         right_col = "red",
#'         title = "green",
#'         border_text = "yellow"
#'     ),
#'     align = c("center", "right")
#' )
#'
#' # Use custom styling with lambda functions
#' table_summary(
#'     df, header = TRUE,
#'     style = list(
#'         left_col = \(ctx) cli::col_red(ctx), # ctx$value is another option
#'         right_col = \(ctx) cli::col_blue(ctx)
#'     ),
#'     align = list(left_col = "left", right_col = "right")
#' )
#'
#' @export
table_summary <- function(data,
                          title = NULL,
                          l = NULL,
                          header = FALSE,
                          center_table = FALSE,
                          border_char = "─",
                          style = NULL,
                          align = NULL, ...) {
    if (!is.data.frame(data) || ncol(data) != 2) {
        stop("Input must be a data frame with exactly 2 columns")
    }

    # Get border character from style if provided
    if (!is.null(style) && !is.null(style$border)) {
        border_char <- style$border
    }

    col_names <- colnames(data)
    data_matrix <- as.matrix(data)

    n_rows <- nrow(data)
    is_split <- !is.null(l) && l < n_rows

    sep_width <- 4
    if (!is.null(style) && !is.null(style$sep)) {
        sep_width <- nchar(paste0(" ", style$sep, " "))
    }

    if (is_split) {
        left_table <- data_matrix[1:l, ]
        right_table <- data_matrix[(l+1):n_rows, ]

        left_left_width <- max(nchar(left_table[, 1]), nchar(col_names[1]))
        left_right_width <- max(nchar(left_table[, 2]), nchar(col_names[2]))
        right_left_width <- max(nchar(right_table[, 1]), nchar(col_names[1]))
        right_right_width <- max(nchar(right_table[, 2]), nchar(col_names[2]))

        left_table_width <- left_left_width + left_right_width + 4 + sep_width  # 4 for padding + separator width
        right_table_width <- right_left_width + right_right_width + 4 + sep_width
        full_width <- left_table_width + right_table_width
    } else {
        left_width <- max(nchar(data_matrix[, 1]), nchar(col_names[1]))
        right_width <- max(nchar(data_matrix[, 2]), nchar(col_names[2]))
        full_width <- left_width + right_width + 4 + sep_width
    }

    horizontal_line <- strrep(border_char, full_width)

    # Style border if specified
    if (!is.null(style) && !is.null(style$border_text)) {
        if (is.function(style$border_text)) {
            horizontal_line <- style$border_text(list(value = horizontal_line))
        } else if (is.character(style$border_text)) {
            # Apply predefined border styles
            if (style$border_text == "bold") {
                horizontal_line <- cli::style_bold(horizontal_line)
            } else if (style$border_text == "italic") {
                horizontal_line <- cli::style_italic(horizontal_line)
            } else if (style$border_text == "blue") {
                horizontal_line <- cli::col_blue(horizontal_line)
            } else if (style$border_text == "red") {
                horizontal_line <- cli::col_red(horizontal_line)
            } else if (style$border_text == "green") {
                horizontal_line <- cli::col_green(horizontal_line)
            } else if (style$border_text == "yellow") {
                horizontal_line <- cli::col_yellow(horizontal_line)
            }
            # Add more border style combinations as needed
        }
    }

    prefix <- ""
    if (center_table) {
        term_width <- tryCatch({
            as.numeric(system("tput cols", intern = TRUE))
        }, error = function(e) {
            as.double(options("width"))
        })

        # Calculate left padding for centering
        left_padding <- max(0, floor((term_width - full_width) / 2))
        prefix <- strrep(" ", left_padding)
    }

    if (!is.null(title)) {
        formatted_title <- align_test(title, full_width)

        # Style title if specified
        if (!is.null(style) && !is.null(style$title)) {
            if (is.function(style$title)) {
                formatted_title <- style$title(list(value = formatted_title))
            } else if (is.character(style$title)) {
                # Apply predefined title styles
                if (style$title == "bold") {
                    formatted_title <- cli::style_bold(formatted_title)
                } else if (style$title == "italic") {
                    formatted_title <- cli::style_italic(formatted_title)
                } else if (style$title == "blue") {
                    formatted_title <- cli::col_blue(formatted_title)
                } else if (style$title == "red") {
                    formatted_title <- cli::col_red(formatted_title)
                } else if (style$title == "green") {
                    formatted_title <- cli::col_green(formatted_title)
                } else if (style$title == "yellow") {
                    formatted_title <- cli::col_yellow(formatted_title)
                } else if (style$title == "blue_bold") {
                    formatted_title <- cli::col_blue(cli::style_bold(formatted_title))
                }
                # Add more title style combinations as needed
            }
        }

        if (center_table) {
            cat("\n", prefix, formatted_title, "\n", sep = "")
        } else {
            cat("\n", formatted_title, "\n", sep = "")
        }
    }

    cat(prefix, horizontal_line, "\n", sep = "")

    if (header) {
        if (is_split) {
            header_row <- paste0(
                format_row_summary(col_names[1], col_names[2], left_left_width, left_right_width, style = style),
                "  ",
                format_row_summary(col_names[1], col_names[2], right_left_width, right_right_width, style = style)
            )
        } else {
            header_row <- format_row_summary(col_names[1], col_names[2], left_width, right_width, style = style)
        }
        cat(prefix, header_row, "\n", sep = "")
        cat(prefix, horizontal_line, "\n", sep = "")
    }

    if (!is_split) {
        for (i in 1:n_rows) {
            row_output <- format_row_summary(
                data_matrix[i, 1],
                data_matrix[i, 2],
                left_width,
                right_width,
                align = align,
                style = style
            )
            cat(prefix, row_output, "\n", sep = "")
        }
    } else {
        for (i in 1:max(l, nrow(right_table))) {
            left_row <- if (i <= l) left_table[i, ] else c("", "")
            right_row <- if (i <= nrow(right_table)) right_table[i, ] else c("", "")

            left_output <- format_row_summary(
                left_row[1],
                left_row[2],
                left_left_width,
                left_right_width,
                align = align,
                style = style
            )

            right_output <- format_row_summary(
                right_row[1],
                right_row[2],
                right_left_width,
                right_right_width,
                align = align,
                style = style
            )

            row_output <- paste0(left_output, "  ", right_output)
            cat(prefix, row_output, "\n", sep = "")
        }
    }

    cat(prefix, horizontal_line, "\n", sep = "")
}
