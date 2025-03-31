#' Display a default matrix table in the command line with customizable formatting.
#'
#' @param data A matrix or table.
#' @param center_table Logical. If `TRUE`, the entire table is centered in the terminal.
#' @param header_underline If `TRUE`, the width of the line that indicates the "response" is shorten.
#' @param style Default to `NULL`. If provided, the components in the displayed table can be styled.
#'   It has 9 components that can be styled:
#'   - `value`: Style for the displayed values.
#'   - `title`: Style for the title of the table.
#'   - `border`: Character used to draw the table borders (must be a single character).
#'   - `border_text`: Style for the table borders.
#'
#'   Each style component can be specified in two ways:
#'   1. As a string: Use a combination of style names from the `cli` package, separated by
#'      underscores (e.g., "blue_italic", "red_bold"). Available styles include colors
#'      (e.g., "red", "blue", "green") and text formats (e.g., "bold", "italic", "underline").
#'   2. As a function: A lambda function that takes a context object and returns formatted text.
#'      The context object contains:
#'      - `formatted_text`: The text to be styled
#'      - `value`: The original numeric value
#'      - `type`: The component type (e.g., "observed", "expected")
#'      - `is_total`: Logical indicating if the cell is a total
#'      - `raw`: Logical indicating if the value is raw (TRUE) or formatted (FALSE)
#' @param ...
#'
#' @return Invisibly returns the formatted matrix, while the table is
#'   printed to the console/command line.
#'
#' @examples
#' # Without Style
#'
#' matrix(c(1.5, 2.3, 3.7, 4.1, 5.5, 6.8), nrow = 2, ncol = 3) |>
#'     matrix_table(header_underline = FALSE, center_table = FALSE)
#'
#' # With Style
#'
#' matrix(c(1.5, 2.3, 3.7, 4.1, 5.5, 6.8), nrow = 2, ncol = 3) |>
#'     matrix_table(header_underline = TRUE, center_table = TRUE, style = list(
#'         value = \(x) cli::style_italic(cli::style_bold(x$formatted_text))
#'     ))
#'
#' @export
matrix_table <- function(data,
                         center_table = FALSE,
                         header_underline = FALSE,
                         style = NULL, ...) {

    data_matrix <- as.matrix(data)

    row_names <- if (is.null(rownames(data_matrix))) paste0("Row", 1:nrow(data_matrix)) else rownames(data_matrix)
    col_names <- if (is.null(colnames(data_matrix))) paste0("Col", 1:ncol(data_matrix)) else colnames(data_matrix)

    main_row_name <- if (is.null(names(dimnames(data_matrix))[1])) "Rows" else names(dimnames(data_matrix))[1]
    main_col_name <- if (is.null(names(dimnames(data_matrix))[2])) "Columns" else names(dimnames(data_matrix))[2]

    default_style <- list(
        value = function(ctx) ctx$formatted_text,
        title = function(ctx) ctx$formatted_text,
        border = options('tab_default')$tab_default$border_char %||% "-",
        border_text = function(ctx) ctx$formatted_text,
        x_side = function(ctx) ctx$formatted_text,
        y_side = function(ctx) ctx$formatted_text
    )

    style <- utils::modifyList(default_style, style %||% list())

    has_cli <- requireNamespace("cli", quietly = TRUE)

    process_style <- function(style_item, context) {
        if (is.function(style_item)) {
            result <- tryCatch({
                style_item(context)
            }, error = function(e) {
                context$formatted_text
            })
            if (is.character(result)) return(result)
            return(context$formatted_text)
        } else if (is.character(style_item) && has_cli) {
            text <- context$formatted_text
            style_parts <- unlist(strsplit(style_item, "_"))

            for (style_part in style_parts) {
                text <- tryCatch({
                    if (exists(paste0("col_", style_part), where = asNamespace("cli"))) {
                        style_fn <- get(paste0("col_", style_part), envir = asNamespace("cli"))
                        text <- style_fn(text)
                    } else if (exists(paste0("style_", style_part), where = asNamespace("cli"))) {
                        style_fn <- get(paste0("style_", style_part), envir = asNamespace("cli"))
                        text <- style_fn(text)
                    } else if (exists(style_part, where = asNamespace("cli"))) {
                        style_fn <- get(style_part, envir = asNamespace("cli"))
                        text <- style_fn(text)
                    } else {
                        text
                    }
                }, error = function(e) {
                    text
                })
            }
            return(text)
        }
        return(context$formatted_text)
    }

    format_cell <- function(value) {
        if (is.numeric(value)) {
            if (abs(value - round(value)) < 1e-10) {
                formatted <- sprintf("%d", round(value))
            } else {
                formatted <- sprintf("%.2f", value)
            }
        } else {
            formatted <- as.character(value)
        }
        cell_ctx <- list(
            value = value,
            formatted_text = formatted,
            type = "value"
        )
        styled_cell <- process_style(style$value, cell_ctx)

        return(styled_cell)
    }

    formatted_matrix <- matrix(nrow = nrow(data_matrix), ncol = ncol(data_matrix))
    for (i in 1:nrow(data_matrix)) {
        for (j in 1:ncol(data_matrix)) {
            formatted_matrix[i, j] <- format_cell(data_matrix[i, j])
        }
    }

    combined <- cbind(row_names, formatted_matrix)
    col_names <- c(main_row_name, col_names)
    get_content_width <- function(cell) {
        if (is.character(cell)) {
            parts <- unlist(strsplit(cell, "\n"))
            return(max(nchar(strip_ansi(parts))))
        }
        return(0)
    }

    col_widths <- sapply(1:ncol(combined), function(j) {
        header_width <- nchar(strip_ansi(col_names[j]))
        cell_widths <- sapply(1:nrow(combined), function(i) {
            get_content_width(combined[i, j])
        })
        max(header_width, max(cell_widths))
    })

    total_width <- sum(col_widths) + 3 * (ncol(combined) - 1) + 4
    main_col_width <- sum(col_widths[2:length(col_widths)]) + 3 * (length(col_widths) - 2) + 4
    border_char <- if (is.character(style$border) && nchar(style$border) == 1) {
        style$border
    } else {
        options('tab_default')$tab_default$border_char
    }

    horizontal_border <- paste0(rep(border_char, total_width), collapse = "")
    border_ctx <- list(
        formatted_text = horizontal_border,
        type = "border"
    )
    horizontal_line <- process_style(style$border_text, border_ctx)

    build_table <- function() {
        title_text <- paste("Matrix Table:", main_row_name, "by", main_col_name)
        title_ctx <- list(
            formatted_text = title_text,
            value = title_text,
            type = "title"
        )
        styled_title <- process_style(style$title, title_ctx)
        title <- center_text_x2(styled_title, total_width)

        header_row <- paste(sapply(1:ncol(combined), function(j) {
            header_text <- col_names[j]
            y_side_ctx <- list(
                formatted_text = header_text,
                value = col_names[j],
                column_index = j,
                type = "y_side"
            )
            styled_header <- process_style(style$y_side, y_side_ctx)

            if (j == 1) {
                sprintf("  %-*s", col_widths[j], styled_header)
            } else {
                center_text_x2(styled_header, col_widths[j])
            }
        }), collapse = "   ")

        table_lines <- c()
        table_lines <- c(table_lines, title)
        table_lines <- c(table_lines, horizontal_line)

        y_main_ctx <- list(
            formatted_text = main_col_name,
            value = main_col_name,
            type = "y_side",
            is_main = TRUE
        )
        styled_main_col <- process_style(style$y_side, y_main_ctx)
        first_col_width <- col_widths[1]
        spacing_after_first_col <- 3
        col_name_padding <- (main_col_width - nchar(strip_ansi(styled_main_col))) / 2
        col_name_header <- sprintf("  %-*s%s%s",
                                   first_col_width + spacing_after_first_col - 2,
                                   "",
                                   center_text_x2(styled_main_col, main_col_width),
                                   "")

        table_lines <- c(table_lines, col_name_header)

        if (header_underline) {
            underline_width <- main_col_width
            header_underline_char <- border_char
            spacing_after_first_col <- spacing_after_first_col - 2
            # spacing_after_first_col <- if (center_table) {
            #     spacing_after_first_col - 1
            # } else {
            #     spacing_after_first_col - 2
            # }

            header_underline_line <- paste0(
                sprintf("  %-*s", first_col_width + spacing_after_first_col, ""),  # Space before first data column
                paste0(rep(header_underline_char, underline_width), collapse = "")  # The underline itself
            )

            table_lines <- c(table_lines, header_underline_line)
        } else {
            table_lines <- c(table_lines, horizontal_line)
        }

        table_lines <- c(table_lines, paste0(header_row, "  "))
        table_lines <- c(table_lines, horizontal_line)

        for (i in 1:nrow(combined)) {
            row_texts <- c()

            for (j in 1:ncol(combined)) {
                cell_text <- combined[i, j]

                if (j == 1) {
                    x_side_ctx <- list(
                        formatted_text = cell_text,
                        value = row_names[i],
                        row_index = i,
                        type = "x_side"
                    )
                    cell_text <- process_style(style$x_side, x_side_ctx)
                    formatted <- sprintf("  %-*s", col_widths[j], cell_text)
                } else {
                    formatted <- center_text_x2(cell_text, col_widths[j])
                }

                row_texts <- c(row_texts, formatted)
            }

            table_lines <- c(table_lines, paste0(paste(row_texts, collapse = "   "), "  "))

            if (i < nrow(combined)) table_lines <- c(table_lines, "")
        }

        table_lines <- c(table_lines, horizontal_line)

        return(table_lines)
    }

    table_lines <- build_table()

    for (line in table_lines) {
        print_centered(line, center_table)
    }

    invisible(data_matrix)
}
