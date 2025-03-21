cor_table <- function(data, method, left_align_first = FALSE, var_lookup = NULL, CI = FALSE, digits = 2, center_table = FALSE, ...) {
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

    horizontal_line <- paste0(rep("─", total_width), collapse = "")

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

    # Always use rows per var = 1 for matrix type input (no CI, stats, etc.)
    rows_per_var <- 1

    # For cor_test type input, determine rows dynamically based on what columns exist
    if (CI) {
        rows_per_var <- 4  # corr, statistic, p-value, CI
    }

    # Print each variable with its separator line
    var_count <- nrow(data_chars) / rows_per_var

    for (i in 1:var_count) {
        start_idx <- (i - 1) * rows_per_var + 1
        end_idx <- min(i * rows_per_var, nrow(data_chars))

        # Print all rows for this variable group without separator lines
        for (j in start_idx:end_idx) {
            row_data <- data_chars[j, ]
            if (any(nzchar(trimws(row_data)))) {
                cat(padding_str, format_row_cm(row_data, col_widths, left_align_first), "\n", sep = "")
            }
        }

        # Print horizontal line only after each variable group
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
#' This function generates and displays a correlation matrix table in the R command line.
#' It supports various options such as displaying the lower triangle (LT), upper triangle (UT),
#' confidence intervals (CI), and custom formatting options like centering the table or layout.
#'
#' @param data A data frame or list containing the correlation data. The data should include
#'   columns `var1`, `var2`, `cor`, `statistic`, `p`, and optionally `conf.low` and `conf.high`
#'   for confidence intervals.
#' @param LT Logical. If `TRUE`, the lower triangle of the correlation matrix is displayed.
#'   Default is `TRUE`.
#' @param UT Logical. If `TRUE`, the upper triangle of the correlation matrix is displayed.
#'   Default is `TRUE`.
#' @param layout_view Logical. If `TRUE`, a layout preview of the correlation matrix is displayed
#'   in the console. Default is `FALSE`.
#' @param CI Logical. If `TRUE`, confidence intervals are included in the correlation matrix.
#'   Default is `FALSE`.
#' @param digits Integer. The number of decimal places to round the correlation coefficients,
#'   statistics, and p-values. Default is `3`.
#' @param center_table Logical. If `TRUE`, the correlation table is centered in the console.
#'   Default is `FALSE`.
#' @param center_layout Logical. If `TRUE`, the layout preview is centered in the console.
#'   Default is `FALSE`.
#' @param ... Additional arguments passed to the `cor_table` function.
#'
#' @return A formatted correlation matrix table displayed in the R console. The function
#'   does not return a value but prints the table directly.
#'
#' @examples
#' \dontrun{
#' # Example data frame with correlation results
#' corr_data <- data.frame(
#'   var1 = c("var1", "var1", "var2"),
#'   var2 = c("var2", "var3", "var3"),
#'   cor = c(0.5, 0.3, 0.7),
#'   statistic = c(2.1, 1.5, 3.2),
#'   p = c(0.04, 0.13, 0.001),
#'   conf.low = c(0.1, -0.1, 0.4),
#'   conf.high = c(0.9, 0.7, 1.0)
#' )
#'
#' # Display the correlation matrix with default settings
#' corr_matrix(corr_data)
#'
#' # Display the correlation matrix with confidence intervals and centered layout
#' corr_matrix(corr_data, CI = TRUE, center_layout = TRUE)
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
                        center_layout = FALSE, ...) {

    # Fix NSE by properly capturing symbols before evaluating
    statistic_col <- if (!is.null(substitute(statistic))) deparse(substitute(statistic))
    pval_col <- if (!is.null(substitute(pval))) deparse(substitute(pval))
    ci_lc_col <- if (!is.null(substitute(ci_lc))) deparse(substitute(ci_lc))
    ci_uc_col <- if (!is.null(substitute(ci_uc))) deparse(substitute(ci_uc))

    # Check if input is a matrix
    if (is.matrix(data)) {
        # If it's a correlation matrix, handle it differently
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

        # Helper function to find column by name or from standard options
        find_column <- function(col_spec, standard_names) {
            if (!is.null(col_spec)) {
                # Check if the name matches a column in data
                if (col_spec %in% names(data)) {
                    return(col_spec)
                }
            }

            # Try standard names
            for (name in standard_names) {
                if (name %in% names(data)) {
                    return(name)
                }
            }

            return(NULL)
        }

        # Find variable columns
        var1_col <- find_column(NULL, c("var1", "variable1", "x", "X"))
        var2_col <- find_column(NULL, c("var2", "variable2", "y", "Y"))

        if (is.null(var1_col) || is.null(var2_col)) {
            stop("Cannot identify variable columns in the data frame.")
        }

        # Find correlation column
        cor_col <- find_column(NULL, c("cor", "r", "estimate", "correlation"))

        # Find statistic column only if specified
        stat_col <- find_column(statistic_col, c("statistic", "t", "z"))

        # Find p-value column only if specified or if statistic is specified (auto-include)
        p_col <- NULL
        if (!is.null(pval_col)) {
            p_col <- find_column(pval_col, c("p", "p.value", "pvalue"))
        } else if (!is.null(stat_col)) {
            # Auto-include p-value if statistic is specified and p-value not explicitly set to NULL
            p_col <- find_column(NULL, c("p", "p.value", "pvalue"))
        }

        # Find confidence interval columns only if specified
        ci_low_col <- find_column(ci_lc_col, c("conf.low", "ci.lower", "lower.ci"))
        ci_high_col <- find_column(ci_uc_col, c("conf.high", "ci.upper", "upper.ci"))

        # Determine if CI should be displayed
        has_CI <- FALSE
        if (CI) {  # Only check for CI if explicitly requested
            if (!is.null(ci_low_col) && !is.null(ci_high_col)) {
                has_CI <- TRUE
            } else if (is.null(ci_lc_col) && is.null(ci_uc_col)) {
                # Auto-detect confidence intervals if not explicitly specified
                potential_low <- find_column(NULL, c("conf.low", "ci.lower", "lower.ci"))
                potential_high <- find_column(NULL, c("conf.high", "ci.upper", "upper.ci"))
                if (!is.null(potential_low) && !is.null(potential_high)) {
                    ci_low_col <- potential_low
                    ci_high_col <- potential_high
                    has_CI <- TRUE
                }
            }
        }

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
            cat(padding_str, "| ", center_text("< corr >", layout_width - 4), " |", "\n", sep = "")
            if (!is.null(stat_col)) {
                cat(padding_str, "| ", center_text("< statistic >", layout_width - 4), " |", "\n", sep = "")
            }
            if (!is.null(p_col)) {
                cat(padding_str, "| ", center_text("< p-value >", layout_width - 4), " |", "\n", sep = "")
            }
            if (has_CI) {
                cat(padding_str, "| ", center_text("< CI >", layout_width - 4), " |", "\n", sep = "")
            }
            cat(padding_str, bottom_line, "\n", sep = "")
            cat("\n")
        }

        # Get unique variables
        vars <- unique(c(data[[var1_col]], data[[var2_col]]))
        n <- length(vars)

        # Create output matrix - determine rows per variable
        rows_per_var <- 1  # Start with 1 (for correlation value)
        if (!is.null(stat_col)) rows_per_var <- rows_per_var + 1
        if (!is.null(p_col)) rows_per_var <- rows_per_var + 1
        if (has_CI) rows_per_var <- rows_per_var + 1

        # Initialize result matrix with appropriate structure
        total_rows <- n * rows_per_var
        result_matrix <- matrix("", nrow = total_rows, ncol = n + 1)

        # Fill the matrix with correlation data
        for (i in 1:n) {
            row_base <- (i - 1) * rows_per_var + 1

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
                            current_row <- current_row + 1
                        }

                        # Statistic - only if explicitly specified or requested in function call
                        if (!is.null(stat_col)) {
                            result_matrix[row_base + current_row, j + 1] <- sprintf(paste0("%.", digits, "f"), as.numeric(data[row_idx, stat_col]))
                            current_row <- current_row + 1
                        }

                        # P-value - only if explicitly specified or requested in function call
                        if (!is.null(p_col)) {
                            p_value <- as.numeric(data[row_idx, p_col])
                            result_matrix[row_base + current_row, j + 1] <- ifelse(p_value < 0.001, "<0.001", sprintf(paste0("%.", digits, "f"), p_value))
                            current_row <- current_row + 1
                        }

                        # Confidence interval - only if both bounds explicitly specified or requested
                        if (has_CI) {
                            ci_low <- sprintf(paste0("%.", digits, "f"), as.numeric(data[row_idx, ci_low_col]))
                            ci_high <- sprintf(paste0("%.", digits, "f"), as.numeric(data[row_idx, ci_high_col]))
                            result_matrix[row_base + current_row, j + 1] <- sprintf("[%s, %s]", ci_low, ci_high)
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
                                var_lookup = NULL, CI = has_CI, digits = digits,
                                center_table = center_table))
    }
}
