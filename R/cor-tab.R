cor_table <- function(data,
                      method,
                      left_align_first = FALSE,
                      var_lookup = NULL,
                      CI = FALSE,
                      digits = 2,
                      border_char = "─",
                      center_table = FALSE, ...) {
    data <- tibble::as_tibble(data)
    data <- dplyr::mutate(data, dplyr::across(where(is.factor), as.character))
    data <- dplyr::mutate(data, dplyr::across(everything(), ~ifelse(is.na(.), "", .)))
    data <- dplyr::mutate(data, dplyr::across(where(is.numeric), ~{
        if(all(. %% 1 == 0, na.rm = TRUE)) {
            as.character(.)
        } else {
            formatted <- format(round(., digits), nsmall = digits)
            formatted <- sub("(\\.0+)$", "", formatted)
            formatted <- sub("\\.00$", "", formatted)
            formatted
        }
    }))
    data <- dplyr::mutate(data, dplyr::across(everything(), as.character))

    col_names <- colnames(data)
    if (!is.null(var_lookup)) {
        col_names[-1] <- var_lookup[col_names[-1]]
    }
    data_chars <- as.matrix(data)

    # Remove empty rows
    non_empty_rows <- apply(data_chars, 1, function(x) any(nzchar(trimws(x))))
    if (any(!non_empty_rows)) {
        data_chars <- data_chars[non_empty_rows, , drop = FALSE]
    }

    col_widths <- pmax(nchar(col_names), apply(data_chars, 2, function(x) max(nchar(x))))

    total_width <- sum(as.numeric(col_widths)) + 3 * (length(col_widths) - 1) + 4

    horizontal_line <- paste0(rep(border_char, total_width), collapse = "")

    title <- glue::glue("{method} Correlation Matrix") |> as.character()

    # Apply centering to the entire table if requested
    if (center_table) {
        term_width <- cli::console_width()
        padding <- max(0, floor((term_width - total_width) / 2))
        padding_str <- paste0(rep(" ", padding), collapse = "")
    } else {
        padding_str <- ""
    }

    cat("\n", padding_str, center_text(title, total_width), "\n", sep = "")
    cat(padding_str, horizontal_line, "\n", sep = "")
    cat(padding_str, format_row_cm(col_names, col_widths, left_align_first), "\n", sep = "")
    cat(padding_str, horizontal_line, "\n", sep = "")

    # Determine number of variables by looking at the first column pattern
    first_col_vals <- data_chars[, 1]
    unique_vars <- unique(first_col_vals[nzchar(first_col_vals)])
    var_count <- length(unique_vars)

    # Get rows per variable based on total rows divided by variable count
    rows_per_var <- nrow(data_chars) / var_count

    # Print each variable group
    for (i in 1:var_count) {
        start_idx <- (i - 1) * rows_per_var + 1
        end_idx <- i * rows_per_var

        # Print all rows for this variable without separator lines between them
        for (j in start_idx:end_idx) {
            if (j <= nrow(data_chars)) {  # Safety check
                row_data <- data_chars[j, ]
                if (any(nzchar(trimws(row_data)))) {
                    cat(padding_str, format_row_cm(row_data, col_widths, left_align_first), "\n", sep = "")
                }
            }
        }

        # Print a horizontal line only after each complete variable group (not between rows)
        if (i < var_count) {
            cat(padding_str, horizontal_line, "\n", sep = "")
        }
    }

    cat(padding_str, horizontal_line, "\n", sep = "")
}

# cor_table <- function(data, method, left_align_first = FALSE, var_lookup = NULL, CI = FALSE, digits = 2, center_table = FALSE, ...) {
#     data <- tibble::as_tibble(data)
#     data <- dplyr::mutate(data, dplyr::across(where(is.factor), as.character))
#     data <- dplyr::mutate(data, dplyr::across(everything(), ~ifelse(is.na(.), "", .)))
#     data <- dplyr::mutate(data, dplyr::across(where(is.numeric), ~{
#         if(all(. %% 1 == 0, na.rm = TRUE)) {
#             as.character(.)
#         } else {
#             formatted <- format(round(., digits), nsmall = digits)
#             formatted <- sub("(\\.0+)$", "", formatted)
#             formatted <- sub("\\.00$", "", formatted)
#             formatted
#         }
#     }))
#     data <- dplyr::mutate(data, dplyr::across(everything(), as.character))
#
#     col_names <- colnames(data)
#     if (!is.null(var_lookup)) {
#         col_names[-1] <- var_lookup[col_names[-1]]
#     }
#     data_chars <- as.matrix(data)
#
#     # Remove empty rows
#     non_empty_rows <- apply(data_chars, 1, function(x) any(nzchar(trimws(x))))
#     if (any(!non_empty_rows)) {
#         data_chars <- data_chars[non_empty_rows, , drop = FALSE]
#     }
#
#     col_widths <- pmax(nchar(col_names), apply(data_chars, 2, function(x) max(nchar(x))))
#
#     total_width <- sum(as.numeric(col_widths)) + 3 * (length(col_widths) - 1) + 4
#
#     horizontal_line <- paste0(rep("─", total_width), collapse = "")
#
#     title <- glue::glue("{method} Correlation Matrix") |> as.character()
#
#     # Apply centering to the entire table if requested
#     if (center_table) {
#         term_width <- cli::console_width()
#         padding <- max(0, floor((term_width - total_width) / 2))
#         padding_str <- paste0(rep(" ", padding), collapse = "")
#     } else {
#         padding_str <- ""
#     }
#
#     cat("\n", padding_str, center_text(title, total_width), "\n", sep = "")
#     cat(padding_str, horizontal_line, "\n", sep = "")
#     cat(padding_str, format_row_cm(col_names, col_widths, left_align_first), "\n", sep = "")
#     cat(padding_str, horizontal_line, "\n", sep = "")
#
#     # Always use rows per var = 1 for matrix type input (no CI, stats, etc.)
#     rows_per_var <- 1
#
#     # For cor_test type input, determine rows dynamically based on what columns exist
#     if (CI) {
#         rows_per_var <- 4  # corr, statistic, p-value, CI
#     }
#
#     # Print each variable with its separator line
#     var_count <- nrow(data_chars) / rows_per_var
#
#     for (i in 1:var_count) {
#         start_idx <- (i - 1) * rows_per_var + 1
#         end_idx <- min(i * rows_per_var, nrow(data_chars))
#
#         for (j in start_idx:end_idx) {
#             row_data <- data_chars[j, ]
#             if (any(nzchar(trimws(row_data)))) {
#                 cat(padding_str, format_row_cm(row_data, col_widths, left_align_first), "\n", sep = "")
#             }
#         }
#
#         # Print horizontal line after each variable group (except after the last one)
#         if (i < var_count) {
#             cat(padding_str, horizontal_line, "\n", sep = "")
#         }
#     }
#
#     cat(padding_str, horizontal_line, "\n", sep = "")
# }

#' Display a Correlation Matrix Table in R Command Line
#'
#' Creates and prints a formatted correlation matrix from either a correlation matrix
#' or a data frame containing pairwise correlation results (such as from \code{rstatix::cor_test()}).
#'
#' @param data Either a symmetric correlation matrix or a data frame containing pairwise
#'   correlation results with variable names, correlation coefficients, and optionally
#'   test statistics, p-values, and confidence intervals.
#' @param method Character string specifying the correlation method (e.g., "Pearson", "Spearman").
#'   If NULL, tries to extract from the data.
#' @param statistic Column name containing test statistics (e.g., t-values). If the column
#'   doesn't exist, a warning is issued and statistics aren't displayed.
#' @param pval Column name containing p-values. If the column doesn't exist, a warning is
#'   issued and p-values aren't displayed.
#' @param ci_lc Column name containing lower confidence interval bounds. If the column
#'   doesn't exist, a warning is issued.
#' @param ci_uc Column name containing upper confidence interval bounds. If the column
#'   doesn't exist, a warning is issued.
#' @param LT Logical. If TRUE (default), shows correlations in the lower triangle of the matrix.
#' @param UT Logical. If TRUE (default), shows correlations in the upper triangle of the matrix.
#' @param layout_view Logical. If TRUE, displays a visual representation of the output layout.
#' @param CI Logical. If TRUE, displays confidence intervals (requires at least one CI column).
#' @param digits Integer specifying the number of decimal places to display.
#' @param center_table Logical. If TRUE, centers the entire table in the console.
#' @param center_layout Logical. If TRUE, centers the layout view in the console.
#' @param col_mapping Named list mapping column types to specific column names in the data.
#'   Supported types are "statistic", "pvalue", "lower_ci", and "upper_ci".
#' @param ... Additional arguments (not currently used).
#'
#' @details
#' The function can handle two types of inputs:
#' \itemize{
#'   \item A symmetric correlation matrix (e.g., from \code{cor()})
#'   \item A data frame with pairwise correlations (e.g., from \code{rstatix::cor_test()})
#' }
#'
#' For data frames, the function tries to identify relevant columns using standard names,
#' but you can explicitly specify column names using the function parameters or the
#' \code{col_mapping} parameter.
#'
#' The \code{col_mapping} parameter accepts a named list that maps column types to specific
#' column names in your data frame. For example:
#' \code{list(statistic = "t_value", pvalue = "p_adjusted")}
#'
#' @return Invisibly returns the formatted data that was printed.
#'
#' @examples
#' # With a correlation matrix
#' corr_matrix(cor(mtcars[, 1:4]), method = "Pearson")
#'
#' # With a data frame from rstatix::cor_test()
#' if (requireNamespace("rstatix", quietly = TRUE)) {
#'   mtcars |>
#'     rstatix::cor_test(disp, wt, hp) |>
#'     corr_matrix(
#'       statistic = statistic,
#'       pval = p,
#'       layout_view = TRUE
#'     )
#'
#'   # Using col_mapping for custom column names
#'   mtcars |>
#'     rstatix::cor_test(disp, wt, hp) |>
#'     corr_matrix(
#'       col_mapping = list(
#'         statistic = "statistic",
#'         pvalue = "p"
#'       )
#'     )
#' }
#'
#' @export
corr_matrix <- function(data,
                        method = NULL,
                        statistic = NULL,
                        pval = NULL,
                        ci_lc = NULL,
                        ci_uc = NULL,
                        LT = TRUE,
                        UT = TRUE,
                        layout_view = FALSE,
                        CI = FALSE,
                        digits = 3,
                        center_table = FALSE,
                        center_layout = FALSE,
                        col_mapping = NULL, ...) {

    # Fix NSE by properly capturing symbols before evaluating
    statistic_col <- if (!is.null(substitute(statistic))) deparse(ensym(statistic))
    pval_col <- if (!is.null(substitute(pval))) deparse(ensym(pval))
    ci_lc_col <- if (!is.null(substitute(ci_lc))) deparse(ensym(ci_lc))
    ci_uc_col <- if (!is.null(substitute(ci_uc))) deparse(ensym(ci_uc))

    # Check if input is a matrix
    if (is.matrix(data)) {
        # Handling for correlation matrix remains the same
        if (isSymmetric(unname(as.matrix(data)))) {
            # Default method if not provided
            if (is.null(method)) method <- "Unknown"

            # Extract variable names and create a simple data frame structure
            vars <- colnames(data)
            if (is.null(vars)) vars <- paste0("Var", seq_len(ncol(data)))

            # Create data frame in the expected format for cor_table
            n <- length(vars)
            result_matrix <- matrix("", nrow = n, ncol = n + 1)

            for (i in 1:n) {
                for (j in 1:n) {
                    if (i == j) {
                        result_matrix[i, j+1] <- "1"
                    } else if ((i < j && UT) || (i > j && LT)) {
                        result_matrix[i, j+1] <- sprintf(paste0("%.", digits, "f"), data[i, j])
                    }
                }
                result_matrix[i, 1] <- vars[i]
            }

            # Convert to data frame and set column names
            col_names <- c("Variable", vars)
            result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
            colnames(result_df) <- col_names

            # Call cor_table with the simplified data - return invisibly
            invisible(cor_table(result_df, method = method[1], left_align_first = TRUE,
                                var_lookup = NULL, CI = FALSE, digits = digits,
                                center_table = center_table))
        } else {
            stop("The input matrix must be a correlation matrix (symmetric).")
        }
    } else {
        # If it's a data frame, process it according to the specified parameters
        if (!is.data.frame(data)) {
            stop("Input must be a correlation matrix or a data frame.")
        }

        # Determine method
        if (is.null(method)) {
            if ("method" %in% names(data)) {
                method <- unique(data$method)
            } else {
                method <- "Unknown"
            }
        }

        # Modified helper function that is strict about column matching
        find_column <- function(col_spec, standard_names, type = NULL) {
            # First priority: user-provided explicit column name
            if (!is.null(col_spec)) {
                # Check if the name matches a column in data
                if (col_spec %in% names(data)) {
                    return(col_spec)
                } else {
                    # If user explicitly specified a column that doesn't exist, return NULL
                    # This will allow the function to handle it properly
                    return(NULL)
                }
            }

            # Second priority: user-provided column mapping
            if (!is.null(col_mapping) && !is.null(type) && type %in% names(col_mapping)) {
                mapped_col <- col_mapping[[type]]
                if (mapped_col %in% names(data)) {
                    return(mapped_col)
                }
            }

            # Third priority: try standard names exactly
            for (name in standard_names) {
                if (name %in% names(data)) {
                    return(name)
                }
            }

            # No auto-detection for partial matches to avoid unexpected behavior
            return(NULL)
        }

        # Find variable columns
        var1_col <- find_column(NULL, c("var1", "variable1", "x", "X"), "variable1")
        var2_col <- find_column(NULL, c("var2", "variable2", "y", "Y"), "variable2")

        if (is.null(var1_col) || is.null(var2_col)) {
            stop("Cannot identify variable columns in the data frame.")
        }

        # Find correlation column
        cor_col <- find_column(NULL, c("cor", "r", "estimate", "correlation"), "correlation")
        if (is.null(cor_col)) {
            stop("Cannot identify correlation column in the data frame.")
        }

        # Find statistic column with stricter matching
        stat_col <- NULL
        if (!is.null(statistic_col)) {
            stat_col <- find_column(statistic_col, c("statistic", "t", "z"), "statistic")
            if (is.null(stat_col) && !is.null(statistic_col)) {
                warning("Specified statistic column '", statistic_col, "' not found in data.")
            }
        }

        # Find p-value column with stricter matching
        p_col <- NULL
        if (!is.null(pval_col)) {
            p_col <- find_column(pval_col, c("p", "p.value", "pvalue"), "pvalue")
            if (is.null(p_col) && !is.null(pval_col)) {
                warning("Specified p-value column '", pval_col, "' not found in data.")
            }
        }

        # Find confidence interval columns with stricter matching
        ci_low_col <- NULL
        ci_high_col <- NULL
        if (!is.null(ci_lc_col)) {
            ci_low_col <- find_column(ci_lc_col, c("conf.low", "ci.lower", "lower.ci"), "lower_ci")
            if (is.null(ci_low_col) && !is.null(ci_lc_col)) {
                warning("Specified lower CI column '", ci_lc_col, "' not found in data.")
            }
        }
        if (!is.null(ci_uc_col)) {
            ci_high_col <- find_column(ci_uc_col, c("conf.high", "ci.upper", "upper.ci"), "upper_ci")
            if (is.null(ci_high_col) && !is.null(ci_uc_col)) {
                warning("Specified upper CI column '", ci_uc_col, "' not found in data.")
            }
        }

        # Determine if CI should be displayed - must have at least one CI column
        has_CI <- (!is.null(ci_low_col) || !is.null(ci_high_col)) && CI

        # Show layout if requested
        if (layout_view) {
            cat("\n")
            layout_width <- 29

            # Apply centering to the layout if requested
            if (center_layout) {
                term_width <- cli::console_width()
                padding <- max(0, floor((term_width - layout_width) / 2))
                padding_str <- paste0(rep(" ", padding), collapse = "")
            } else {
                padding_str <- ""
            }

            # Fixed layout display to match the original formatting
            top_line <- paste0("┌", paste0(rep("─", layout_width - 2), collapse = ""), "┐")
            middle_line <- paste0("├", paste0(rep("─", layout_width - 2), collapse = ""), "┤")
            bottom_line <- paste0("└", paste0(rep("─", layout_width - 2), collapse = ""), "┘")

            cat(padding_str, top_line, "\n", sep = "")
            cat(padding_str, "| ", center_text("Layout for Corr. Matrix", layout_width - 4), " |", "\n", sep = "")
            cat(padding_str, middle_line, "\n", sep = "")

            # Show correlation row
            cat(padding_str, "| ", center_text("< corr >", layout_width - 4), " |", "\n", sep = "")

            # Show statistic row if requested AND available
            if (!is.null(stat_col)) {
                cat(padding_str, "| ", center_text("< statistic >", layout_width - 4), " |", "\n", sep = "")
            }

            # Show p-value row if requested AND available
            if (!is.null(p_col)) {
                cat(padding_str, "| ", center_text("< p-value >", layout_width - 4), " |", "\n", sep = "")
            }

            # Show CI row if requested and available
            if (has_CI) {
                if (!is.null(ci_low_col) && !is.null(ci_high_col)) {
                    ci_text <- "< [Lower CI, Upper CI] >"
                } else if (!is.null(ci_low_col)) {
                    ci_text <- "< [Lower CI, NA] >"
                } else if (!is.null(ci_high_col)) {
                    ci_text <- "< [NA, Upper CI] >"
                } else {
                    ci_text <- "< CI >"
                }
                cat(padding_str, "| ", center_text(ci_text, layout_width - 4), " |", "\n", sep = "")
            }

            cat(padding_str, bottom_line, "\n", sep = "")
            cat("\n")
        }

        # Get unique variables
        vars <- unique(c(data[[var1_col]], data[[var2_col]]))
        n <- length(vars)

        # Count how many rows per variable we'll have based on selected outputs
        elements_to_show <- 1  # Start with 1 for correlation
        if (!is.null(stat_col)) elements_to_show <- elements_to_show + 1
        if (!is.null(p_col)) elements_to_show <- elements_to_show + 1
        if (has_CI && (!is.null(ci_low_col) || !is.null(ci_high_col))) elements_to_show <- elements_to_show + 1

        # Initialize result matrix with appropriate size - one row per variable
        result_matrix <- matrix("", nrow = n * elements_to_show, ncol = n + 1)

        # Fill the matrix with correlation data
        for (i in 1:n) {
            row_base <- (i - 1) * elements_to_show + 1

            # Set variable name in the first column
            result_matrix[row_base, 1] <- vars[i]

            for (j in 1:n) {
                if (i == j) {
                    # Diagonal: correlation = 1
                    result_matrix[row_base, j + 1] <- "1"
                } else if ((i < j && UT) || (i > j && LT)) {
                    # Find matching row in data
                    match_row <- which(
                        (data[[var1_col]] == vars[i] & data[[var2_col]] == vars[j]) |
                            (data[[var1_col]] == vars[j] & data[[var2_col]] == vars[i])
                    )

                    if (length(match_row) > 0) {
                        row_idx <- match_row[1]  # Use first match if multiple
                        current_row <- 0

                        # Correlation
                        if (!is.null(cor_col)) {
                            result_matrix[row_base + current_row, j + 1] <- sprintf(paste0("%.", digits, "f"), as.numeric(data[row_idx, cor_col]))
                        }
                        current_row <- current_row + 1

                        # Statistic - only if explicitly specified AND found
                        if (!is.null(stat_col)) {
                            result_matrix[row_base + current_row, j + 1] <- sprintf(paste0("%.", digits, "f"), as.numeric(data[row_idx, stat_col]))
                            current_row <- current_row + 1
                        }

                        # P-value - only if explicitly specified AND found
                        if (!is.null(p_col)) {
                            p_value <- as.numeric(data[row_idx, p_col])
                            result_matrix[row_base + current_row, j + 1] <- ifelse(p_value < 0.001, "<0.001", sprintf(paste0("%.", digits, "f"), p_value))
                            current_row <- current_row + 1
                        }

                        # Confidence interval - only if CI=TRUE and at least one bound specified and found
                        if (has_CI && (!is.null(ci_low_col) || !is.null(ci_high_col))) {
                            ci_low_val <- if (!is.null(ci_low_col)) sprintf(paste0("%.", digits, "f"), as.numeric(data[row_idx, ci_low_col])) else "NA"
                            ci_high_val <- if (!is.null(ci_high_col)) sprintf(paste0("%.", digits, "f"), as.numeric(data[row_idx, ci_high_col])) else "NA"
                            result_matrix[row_base + current_row, j + 1] <- sprintf("[%s, %s]", ci_low_val, ci_high_val)
                        }
                    }
                }
            }
        }

        # Use actual variable names as column names
        col_names <- c("Variable", vars)
        result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
        colnames(result_df) <- col_names

        # Call cor_table with the prepared data - return invisibly
        invisible(cor_table(result_df, method = method[1], left_align_first = TRUE,
                            var_lookup = NULL, CI = FALSE, digits = digits,
                            center_table = center_table))
    }
}
