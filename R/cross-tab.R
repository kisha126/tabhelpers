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
#' @param center_table Logical. If `TRUE`, the entire table is centered in the terminal.
#' @param ... Additional arguments (not currently used).
#'
#' @return Invisibly returns the formatted cross-tabulation table as a matrix. The table is printed to the console.
#'
#' @examples
#' # Example 1: Basic cross-tabulation
#' data <- matrix(c(10, 20, 30, 40), nrow = 2, byrow = TRUE)
#' cross_table(data)
#'
#' # Example 2: Display row and column percentages
#' cross_table(data, percentage = c("by_row", "by_col"))
#'
#' # Example 3: Display expected frequencies and center the table
#' cross_table(data, expected = TRUE, center_table = TRUE)
#'
#' @export
cross_table <- function(data,
                        percentage = NULL,
                        layout = TRUE,
                        expected = TRUE,
                        layout_center = FALSE,
                        center_table = FALSE, ...) {
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

    format_cell <- function(obs, exp, row_pct, col_pct, grand_pct) {
        if (expected) {
            cell <- sprintf("%d (%s)", obs, format_number_x2(exp))
        } else {
            cell <- sprintf("%d", obs)
        }

        if (show_row_pct) cell <- paste0(cell, "\n", format_percentage(row_pct))
        if (show_col_pct) cell <- paste0(cell, "\n", format_percentage(col_pct))
        if (show_total_pct) cell <- paste0(cell, "\n", format_percentage(grand_pct))
        cell
    }

    combined <- matrix(nrow = nrow(observed) + 1, ncol = ncol(observed) + 1)
    for (i in 1:nrow(observed)) {
        for (j in 1:ncol(observed)) {
            combined[i,j] <- format_cell(observed[i,j], expected_values[i,j],
                                         row_percentages[i,j], col_percentages[i,j], grand_percentages[i,j])
        }
        combined[i, ncol(combined)] <- format_cell(row_totals[i], row_totals[i], 100, NA, row_totals[i]/grand_total*100)
    }
    for (j in 1:ncol(observed)) {
        combined[nrow(combined), j] <- format_cell(col_totals[j], col_totals[j], NA, 100, col_totals[j]/grand_total*100)
    }

    if (is.null(percentage) || length(percentage) == 0) {
        if (expected) {
            combined[nrow(combined), ncol(combined)] <- sprintf("%d (%s)", grand_total, format_number_x2(grand_total))
        } else {
            combined[nrow(combined), ncol(combined)] <- sprintf("%d", grand_total)
        }
    } else {
        if (expected) {
            combined[nrow(combined), ncol(combined)] <- sprintf("%d (%s)\n\n\n100%%", grand_total, format_number_x2(grand_total))
        } else {
            combined[nrow(combined), ncol(combined)] <- sprintf("%d\n\n\n100%%", grand_total)
        }
    }

    row_names <- if (is.null(rownames(observed))) paste0("Row", 1:nrow(observed)) else rownames(observed)
    col_names <- if (is.null(colnames(observed))) paste0("Col", 1:ncol(observed)) else colnames(observed)
    row_names <- c(row_names, "TOTAL")
    col_names <- c(col_names, "TOTAL")

    main_row_name <- if (is.null(names(dimnames(observed))[1])) "x" else names(dimnames(observed))[1]
    main_col_name <- if (is.null(names(dimnames(observed))[2])) "y" else names(dimnames(observed))[2]

    combined <- cbind(row_names, combined)
    col_names <- c(main_row_name, col_names)

    col_widths <- sapply(1:ncol(combined), function(j) {
        max(nchar(col_names[j]), max(nchar(unlist(strsplit(combined[,j], "\n")))))
    })

    total_width <- sum(col_widths) + 3 * (ncol(combined) - 1) + 4  # 2 spaces on each side
    main_col_width <- sum(col_widths[2:length(col_widths)]) + 3 * (length(col_widths) - 2)

    horizontal_line <- paste0(rep("─", total_width), collapse = "")

    has_cli <- requireNamespace("cli", quietly = TRUE)

    print_centered <- function(text) {
        if (!center_table && !layout_center) {
            cat(text, "\n")
            return()
        }

        if (has_cli) {
            term_width <- cli::console_width()
        } else {
            term_width <- as.double(options("width"))
        }

        padding <- max(0, floor((term_width - nchar(text)) / 2))
        cat(paste0(strrep(" ", padding), text), "\n")
    }

    if (layout) {
        cat("\n")
        layout_width <- 36
        layout_box <- c(
            paste0("┌", paste0(rep("─", layout_width - 2), collapse = ""), "┐"),
            paste0("| ", center_text_x2("Layout for Cont. Table", layout_width - 4), " |"),
            paste0("├", paste0(rep("─", layout_width - 2), collapse = ""), "┤")
        )

        if (expected) {
            layout_box <- c(layout_box,
                            paste0("| ", center_text_x2("< Freq >    (< Expected Value >)", layout_width - 4), " |"))
        } else {
            layout_box <- c(layout_box,
                            paste0("| ", center_text_x2("< Freq >", layout_width - 4), " |"))
        }

        if (show_row_pct)
            layout_box <- c(layout_box,
                            paste0("| ", center_text_x2("< % by total row >", layout_width - 4), " |"))
        if (show_col_pct)
            layout_box <- c(layout_box,
                            paste0("| ", center_text_x2("< % by total column >", layout_width - 4), " |"))
        if (show_total_pct)
            layout_box <- c(layout_box,
                            paste0("| ", center_text_x2("< % by grand total >", layout_width - 4), " |"))

        layout_box <- c(layout_box,
                        paste0("└", paste0(rep("─", layout_width - 2), collapse = ""), "┘"))

        if (layout_center) {
            for (line in layout_box) {
                print_centered(line)
            }
        } else {
            for (line in layout_box) {
                cat(line, "\n")
            }
        }
        cat("\n")
    }

    # Build the table as a string with consistent margins
    build_table <- function() {
        title <- center_text_x2(paste("Cross Tabulation:", main_row_name, "by", main_col_name), total_width)

        header_row <- paste(sapply(1:ncol(combined), function(j) {
            if (j == 1) {
                sprintf("  %-*s", col_widths[j], col_names[j])
            } else {
                center_text_x2(col_names[j], col_widths[j])
            }
        }), collapse = "   ")

        table_lines <- c()

        table_lines <- c(table_lines, title)
        table_lines <- c(table_lines, horizontal_line)
        table_lines <- c(table_lines, sprintf("  %-*s%s  ", col_widths[1], "",
                                              center_text_x2(main_col_name, main_col_width)))
        table_lines <- c(table_lines, horizontal_line)
        table_lines <- c(table_lines, paste0(header_row, "  "))
        table_lines <- c(table_lines, horizontal_line)

        for (i in 1:(nrow(combined) - 1)) {
            rows <- strsplit(combined[i,], "\n")
            max_rows <- max(sapply(rows, length))
            for (j in 1:max_rows) {
                row_data <- sapply(1:length(rows), function(k) {
                    if (j <= length(rows[[k]])) rows[[k]][j] else ""
                })
                formatted_row <- paste(sapply(1:length(row_data), function(k) {
                    if (k == 1) {
                        sprintf("  %-*s", col_widths[k], row_data[k])
                    } else {
                        center_text_x2(row_data[k], col_widths[k])
                    }
                }), collapse = "   ")
                table_lines <- c(table_lines, paste0(formatted_row, "  "))
            }
            if (i < (nrow(combined) - 1)) table_lines <- c(table_lines, "")
        }

        table_lines <- c(table_lines, horizontal_line)
        rows <- strsplit(combined[nrow(combined),], "\n")
        max_rows <- max(sapply(rows, length))
        for (j in 1:max_rows) {
            row_data <- sapply(1:length(rows), function(k) {
                if (j <= length(rows[[k]])) rows[[k]][j] else ""
            })
            formatted_row <- paste(sapply(1:length(row_data), function(k) {
                if (k == 1) {
                    sprintf("  %-*s", col_widths[k], row_data[k])
                } else {
                    center_text_x2(row_data[k], col_widths[k])
                }
            }), collapse = "   ")
            table_lines <- c(table_lines, paste0(formatted_row, "  "))
        }
        table_lines <- c(table_lines, horizontal_line)

        return(table_lines)
    }

    table_lines <- build_table()

    if (center_table) {
        for (line in table_lines) {
            print_centered(line)
        }
    } else {
        for (line in table_lines) {
            cat(line, "\n")
        }
    }
}
