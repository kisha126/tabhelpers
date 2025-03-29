#' Generate and Display a Cross Tabulation Table
#'
#' This function creates a cross-tabulation (contingency) table from a given data matrix or table.
#' It supports displaying observed frequencies, expected frequencies, and various percentages
#' (row, column, and total). The table can be formatted and centered in the terminal for better readability.
#'
#' @param data A matrix or table containing the observed frequencies for the cross-tabulation.
#' @param percentage A character vector or logical value specifying which percentages to display.
#'   Options include:
#'   - `TRUE` or `"all"`: Display all percentages (row, column, and total).
#'   - `"by_row"`: Display row percentages.
#'   - `"by_col"`: Display column percentages.
#'   - `"by_total"`: Display total percentages.
#'   - `FALSE` or `NULL`: Do not display any percentages.
#' @param layout Logical. If `TRUE`, a layout box explaining the table structure is displayed.
#' @param expected Logical. If `TRUE`, expected frequencies are displayed alongside observed frequencies.
#' @param layout_center Logical. If `TRUE`, the layout box is centered in the terminal.
#' @param header_underline If `TRUE`, the width of the line that indicates the "response" is shorten.
#' @param center_table Logical. If `TRUE`, the entire table is centered in the terminal.
#' @param style Default to `NULL`. If provided, the components in the displayed table can be styled.
#'   It has 9 components that can be styled:
#'   - `observed`: Style for the displayed observed values.
#'   - `expected`: Style for the displayed expected values.
#'   - `total`: Style for the displayed total frequencies (marginal and grand total).
#'   - `row_percentage`: Style for the displayed row percentages.
#'   - `col_percentage`: Style for the displayed column percentages.
#'   - `total_percentage`: Style for the marginal percentages.
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
#'
#'
#'
#' @param ... Additional arguments (not currently used).
#'
#' @return Invisibly returns the formatted cross-tabulation table as a matrix. The table is printed to the console/command line.
#'
#' @examples
#' # Example 1: Basic cross-tabulation
#' data <- matrix(c(10, 20, 30, 40), nrow = 2, byrow = TRUE)
#' cross_table(data)
#'
#' # Example 2: Display row and column percentages
#' cross_table(data, percentage = c("by_row", "by_col"))
#'
#' # Example 3: Overall component with style
#' matrix(c(10, 20, 30, 40), nrow = 2, byrow = TRUE) |> # Use `%>%` from `magrittr` if preferred
#'     cross_table(
#'         percentage = "all",
#'         style = list(
#'             observed = \(ctx) cli::col_yellow(ctx$formatted_text),
#'             expected = \(ctx) cli::style_italic(ctx$formatted_text),
#'             row_percentage = "blue_italic",
#'             col_percentage = "blue_italic",
#'             total_percentage = "red_italic",
#'             total = \(ctx) cli::style_italic(ctx$formatted_text),
#'             border = "=",
#'             border_text = "cyan",
#'             title = \(ctx) cli::style_bold(cli::col_blue(ctx$formatted_text))
#'         )
#'     )
#'
#' # A large table (from https://blogs.sas.com/content/iml/2018/04/02/chi-square-rows-columns-sas.html)
#' matrix(
#'     c(6, 51, 69, 68, 28,
#'       16, 94, 90, 94, 47,
#'       0, 37, 69, 55, 38),
#'     byrow = TRUE,
#'     ncol = 5,
#'     dimnames = list(
#'         "Eyes" = c("blue", "brown", "green"),
#'         "Hair" = c("black", "dark", "fair", "medium", "red")
#'     )
#' ) |>
#'     cross_table(
#'         percentage = "all",
#'         header_underline = T,
#'         style = list(
#'             expected = "italic"
#'         ),
#'         center_table = T
#'     )
#' # The previous example without using `cli` package
#' matrix(
#'     c(6, 51, 69, 68, 28,
#'       16, 94, 90, 94, 47,
#'       0, 37, 69, 55, 38),
#'     byrow = TRUE,
#'     ncol = 5,
#'     dimnames = list(
#'         "Eyes" = c("blue", "brown", "green"),
#'         "Hair" = c("black", "dark", "fair", "medium", "red")
#'     )
#' ) |>
#'     cross_table(
#'         percentage = "all",
#'         header_underline = TRUE,
#'         style = list(
#'             # Italic using ANSI escape sequence
#'             expected = \(ctx) paste0("\033[3m", ctx$formatted_text, "\033[23m"),
#'             # Bold using ANSI escape sequence
#'             total_percentage = \(ctx) paste0("\033[1m", ctx$formatted_text, "\033[22m"),
#'             # Blue text using ANSI escape sequence
#'             observed = \(ctx) paste0("\033[34m", ctx$formatted_text, "\033[39m"),
#'             # Change border character
#'             border = "-"
#'         ),
#'         center_table = TRUE
#'     )
#'
#'
#' @export
cross_table <- function(data,
                        percentage = NULL,
                        layout = TRUE,
                        expected = TRUE,
                        layout_center = FALSE,
                        center_table = FALSE,
                        header_underline = FALSE,
                        style = NULL, ...) {
    observed <- as.matrix(data)
    row_totals <- rowSums(observed)
    col_totals <- colSums(observed)
    grand_total <- sum(observed)

    expected_values <- outer(row_totals, col_totals) / grand_total

    format_percentage <- function(x) {
        ifelse(is.na(x) | x == 100, "", sprintf("%.0f%%", x))
    }

    if (percentage == FALSE || is.null(percentage) || length(percentage) == 0) {
        show_row_pct <- show_col_pct <- show_total_pct <- FALSE
    } else if (percentage == TRUE || "all" %in% percentage) {
        show_row_pct <- show_col_pct <- show_total_pct <- TRUE
    } else {
        show_row_pct <- "by_row" %in% percentage
        show_col_pct <- "by_col" %in% percentage
        show_total_pct <- "by_total" %in% percentage
    }

    row_percentages <- sweep(observed, 1, row_totals, "/") * 100
    col_percentages <- sweep(observed, 2, col_totals, "/") * 100
    grand_percentages <- observed / grand_total * 100

    default_style <- list(
        observed = function(ctx) ctx$formatted_text,
        expected = function(ctx) ctx$formatted_text,
        row_percentage = function(ctx) ctx$formatted_text,
        col_percentage = function(ctx) ctx$formatted_text,
        total_percentage = function(ctx) ctx$formatted_text,
        total = function(ctx) ctx$formatted_text,
        title = function(ctx) ctx$formatted_text,
        border = options('tab_default')$tab_default$border_char,
        border_text = function(ctx) ctx$formatted_text
    )

    style <- utils::modifyList(default_style, style %||% list())

    # if (is.null(style$y_side) && !identical(style$header, default_style$header)) {
    #     style$y_side <- style$header
    # }

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

    format_cell <- function(obs, exp, row_pct, col_pct, grand_pct, is_total = FALSE) {
        cell_parts <- list()

        if (expected && !is_total) {
            obs_text <- sprintf("%d", obs)
            exp_text <- sprintf("(%s)", format_number_x2(exp))

            obs_ctx <- list(
                value = obs,
                formatted_text = obs_text,
                type = "observed",
                is_total = is_total,
                raw = TRUE
            )
            styled_obs <- process_style(style$observed, obs_ctx)

            exp_ctx <- list(
                value = exp,
                formatted_text = exp_text,
                type = "expected",
                is_total = is_total,
                raw = TRUE
            )
            styled_exp <- process_style(style$expected, exp_ctx)

            cell_parts$main <- paste(styled_obs, styled_exp)
        } else {
            obs_text <- sprintf("%d", obs)
            obs_ctx <- list(
                value = obs,
                formatted_text = obs_text,
                type = if (is_total) "total" else "observed",
                is_total = is_total,
                raw = TRUE
            )
            cell_parts$main <- process_style(if (is_total) style$total else style$observed, obs_ctx)
        }

        if (show_row_pct && !is.na(row_pct)) {
            pct_text <- format_percentage(row_pct)
            if (pct_text != "") {
                row_pct_ctx <- list(
                    value = row_pct,
                    formatted_text = pct_text,
                    type = "row_percentage",
                    is_total = is_total,
                    raw = TRUE
                )
                cell_parts$row_pct <- process_style(style$row_percentage, row_pct_ctx)
            }
        }

        if (show_col_pct && !is.na(col_pct)) {
            pct_text <- format_percentage(col_pct)
            if (pct_text != "") {
                col_pct_ctx <- list(
                    value = col_pct,
                    formatted_text = pct_text,
                    type = "col_percentage",
                    is_total = is_total,
                    raw = TRUE
                )
                cell_parts$col_pct <- process_style(style$col_percentage, col_pct_ctx)
            }
        }

        if (show_total_pct && !is.na(grand_pct)) {
            pct_text <- format_percentage(grand_pct)
            if (pct_text != "") {
                total_pct_ctx <- list(
                    value = grand_pct,
                    formatted_text = pct_text,
                    type = "total_percentage",
                    is_total = is_total,
                    raw = TRUE
                )
                cell_parts$total_pct <- process_style(style$total_percentage, total_pct_ctx)
            }
        }

        cell <- cell_parts$main
        if (!is.null(cell_parts$row_pct)) cell <- paste0(cell, "\n", cell_parts$row_pct)
        if (!is.null(cell_parts$col_pct)) cell <- paste0(cell, "\n", cell_parts$col_pct)
        if (!is.null(cell_parts$total_pct)) cell <- paste0(cell, "\n", cell_parts$total_pct)

        attr(cell, "parts") <- cell_parts
        cell
    }

    combined <- matrix(nrow = nrow(observed) + 1, ncol = ncol(observed) + 1)
    for (i in 1:nrow(observed)) {
        for (j in 1:ncol(observed)) {
            combined[i,j] <- format_cell(observed[i,j], expected_values[i,j],
                                         row_percentages[i,j], col_percentages[i,j], grand_percentages[i,j])
        }
        combined[i, ncol(combined)] <- format_cell(row_totals[i], row_totals[i], 100, NA, row_totals[i]/grand_total*100, TRUE)
    }
    for (j in 1:ncol(observed)) {
        combined[nrow(combined), j] <- format_cell(col_totals[j], col_totals[j], NA, 100, col_totals[j]/grand_total*100, TRUE)
    }

    if (is.null(percentage) || length(percentage) == 0) {
        combined[nrow(combined), ncol(combined)] <- format_cell(grand_total, grand_total, NA, NA, NA, TRUE)
    } else {
        combined[nrow(combined), ncol(combined)] <- format_cell(grand_total, grand_total, NA, NA, 100, TRUE)
    }

    row_names <- if (is.null(rownames(observed))) paste0("Row", 1:nrow(observed)) else rownames(observed)
    col_names <- if (is.null(colnames(observed))) paste0("Col", 1:ncol(observed)) else colnames(observed)
    row_names <- c(row_names, "TOTAL")
    col_names <- c(col_names, "TOTAL")

    main_row_name <- if (is.null(names(dimnames(observed))[1])) "x" else names(dimnames(observed))[1]
    main_col_name <- if (is.null(names(dimnames(observed))[2])) "y" else names(dimnames(observed))[2]

    combined <- cbind(row_names, combined)
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
    main_col_width <- sum(col_widths[2:length(col_widths)]) + 3 * (length(col_widths) - 2)

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

    if (layout) {
        cat("\n")
        layout_width <- 36
        layout_box <- c(
            paste0("┌", paste0(rep("─", layout_width - 2), collapse = ""), "┐"),
            paste0("| ", center_text_x2("Layout for Cont. Table", layout_width - 4), " |"),
            paste0("├", paste0(rep("─", layout_width - 2), collapse = ""), "┤")
        )

        if (expected) {
            freq_text <- "< Freq >"
            freq_ctx <- list(
                formatted_text = freq_text,
                value = freq_text,
                type = "observed",
                is_total = FALSE,
                raw = TRUE
            )
            styled_freq <- process_style(style$observed, freq_ctx)

            exp_text <- "(< Expected Value >)"
            exp_ctx <- list(
                formatted_text = exp_text,
                value = exp_text,
                type = "expected",
                is_total = FALSE,
                raw = TRUE
            )
            styled_exp <- process_style(style$expected, exp_ctx)

            styled_line <- paste(styled_freq, styled_exp)
            layout_box <- c(layout_box,
                            paste0("| ", center_text_x2(styled_line, layout_width - 4), " |"))
        } else {
            freq_text <- "< Freq >"
            freq_ctx <- list(
                formatted_text = freq_text,
                value = freq_text,
                type = "observed",
                is_total = FALSE,
                raw = TRUE
            )
            styled_freq <- process_style(style$observed, freq_ctx)

            layout_box <- c(layout_box,
                            paste0("| ", center_text_x2(styled_freq, layout_width - 4), " |"))
        }

        if (show_row_pct) {
            row_pct_text <- "< % by total row >"
            row_pct_ctx <- list(
                formatted_text = row_pct_text,
                value = row_pct_text,
                type = "row_percentage",
                is_total = FALSE,
                raw = TRUE
            )
            styled_row_pct <- process_style(style$row_percentage, row_pct_ctx)

            layout_box <- c(layout_box,
                            paste0("| ", center_text_x2(styled_row_pct, layout_width - 4), " |"))
        }

        if (show_col_pct) {
            col_pct_text <- "< % by total column >"
            col_pct_ctx <- list(
                formatted_text = col_pct_text,
                value = col_pct_text,
                type = "col_percentage",
                is_total = FALSE,
                raw = TRUE
            )
            styled_col_pct <- process_style(style$col_percentage, col_pct_ctx)

            layout_box <- c(layout_box,
                            paste0("| ", center_text_x2(styled_col_pct, layout_width - 4), " |"))
        }

        if (show_total_pct) {
            total_pct_text <- "< % by grand total >"
            total_pct_ctx <- list(
                formatted_text = total_pct_text,
                value = total_pct_text,
                type = "total_percentage",
                is_total = FALSE,
                raw = TRUE
            )
            styled_total_pct <- process_style(style$total_percentage, total_pct_ctx)

            layout_box <- c(layout_box,
                            paste0("| ", center_text_x2(styled_total_pct, layout_width - 4), " |"))
        }

        layout_box <- c(layout_box,
                        paste0("└", paste0(rep("─", layout_width - 2), collapse = ""), "┘"))

        if (layout_center) {
            for (line in layout_box) {
                print_centered(line, center_table = center_table, layout_center = layout_center)
            }
        } else {
            for (line in layout_box) {
                cat(line, "\n")
            }
        }
        cat("\n")
    }

    build_table <- function() {
        title_text <- paste("Cross Tabulation:", main_row_name, "by", main_col_name)
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
                type = "y_side",
                is_total = (j == ncol(combined))
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

        if (center_table) {
            col_name_header <- sprintf("%-*s%s", col_widths[1] - 3, "",
                               center_text_x2(styled_main_col, main_col_width))
        } else {
            col_name_header <- sprintf("  %-*s%s  ", col_widths[1], "",
                               center_text_x2(styled_main_col, main_col_width))
        }

        table_lines <- c(table_lines, col_name_header)

        if (header_underline) {
            header_underline_width <- main_col_width
            if (center_table) {
                header_underline_prefix <- sprintf("  %-*s", col_widths[1] - 5, "")
            } else {
                header_underline_prefix <- sprintf("  %-*s", col_widths[1] + 1, "")
            }

            header_underline_line <- paste0(header_underline_prefix,
                                            paste0(rep(options('tab_default')$tab_default[[5]], header_underline_width - 4), collapse = ""),
                                            "  ")
            table_lines <- c(table_lines, header_underline_line)
        } else {
            table_lines <- c(table_lines, horizontal_line)
        }

        table_lines <- c(table_lines, paste0(header_row, "  "))
        table_lines <- c(table_lines, horizontal_line)

        for (i in 1:(nrow(combined) - 1)) {
            cell_lines <- list()
            max_lines <- 1

            for (j in 1:ncol(combined)) {
                if (!is.null(combined[i, j])) {
                    lines <- unlist(strsplit(as.character(combined[i, j]), "\n"))
                    cell_lines[[j]] <- lines
                    max_lines <- max(max_lines, length(lines))
                } else {
                    cell_lines[[j]] <- ""
                }
            }

            for (line_num in 1:max_lines) {
                line_texts <- c()

                for (j in 1:ncol(combined)) {
                    cell_text <- if (line_num <= length(cell_lines[[j]])) {
                        cell_lines[[j]][line_num]
                    } else {
                        ""
                    }

                    if (j == 1) {
                        if (line_num == 1) {
                            x_side_ctx <- list(
                                formatted_text = cell_text,
                                value = row_names[i],
                                row_index = i,
                                type = "x_side",
                                is_total = FALSE
                            )
                            cell_text <- process_style(style$x_side, x_side_ctx)
                        }
                        formatted <- sprintf("  %-*s", col_widths[j], cell_text)
                    } else {
                        formatted <- center_text_x2(cell_text, col_widths[j])
                    }

                    line_texts <- c(line_texts, formatted)
                }

                table_lines <- c(table_lines, paste0(paste(line_texts, collapse = "   "), "  "))
            }

            if (i < (nrow(combined) - 1)) table_lines <- c(table_lines, "")
        }

        table_lines <- c(table_lines, horizontal_line)

        i <- nrow(combined)
        cell_lines <- list()
        max_lines <- 1

        for (j in 1:ncol(combined)) {
            lines <- unlist(strsplit(as.character(combined[i, j]), "\n"))
            cell_lines[[j]] <- lines
            max_lines <- max(max_lines, length(lines))
        }

        for (line_num in 1:max_lines) {
            line_texts <- c()

            for (j in 1:ncol(combined)) {
                cell_text <- if (line_num <= length(cell_lines[[j]])) {
                    cell_lines[[j]][line_num]
                } else {
                    ""
                }

                if (j == 1) {
                    if (line_num == 1) {
                        x_side_ctx <- list(
                            formatted_text = cell_text,
                            value = "TOTAL",
                            row_index = i,
                            type = "x_side",
                            is_total = TRUE
                        )
                        cell_text <- process_style(style$x_side, x_side_ctx)
                    }
                    formatted <- sprintf("  %-*s", col_widths[j], cell_text)
                } else {
                    formatted <- center_text_x2(cell_text, col_widths[j])
                }

                line_texts <- c(line_texts, formatted)
            }

            table_lines <- c(table_lines, paste0(paste(line_texts, collapse = "   "), "  "))
        }

        table_lines <- c(table_lines, horizontal_line)

        return(table_lines)
    }

    table_lines <- build_table()

    if (center_table) {
        for (line in table_lines) {
            print_centered(line, center_table = TRUE)
        }
    } else {
        for (line in table_lines) {
            cat(line, "\n")
        }
    }
}


strip_ansi <- function(text) {
    gsub("\033\\[[0-9;]*m", "", text)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

print_centered <- function(text, center_table = FALSE, layout_center = FALSE) {
    has_cli <- requireNamespace("cli", quietly = TRUE)

    if (!center_table && !layout_center) {
        cat(text, "\n")
        return()
    }

    if (has_cli) {
        term_width <- cli::console_width()
    } else {
        term_width <- as.double(options("width"))
    }

    padding <- max(0, floor((term_width - nchar(strip_ansi(text))) / 2))
    cat(paste0(strrep(" ", padding), text), "\n")
}
