align_center <- function(text, width, pos = FALSE) {
    clean_text <- gsub("\033\\[[0-9;]*m", "", text)

    if (pos) {
        padding <- max(width - nchar(clean_text), 0)
    } else {
        padding <- width - nchar(clean_text)
    }

    if (padding < 0) {
        return(substr(text, 1, width))
    }

    left_pad <- floor(padding / 2)
    right_pad <- ceiling(padding / 2)

    paste0(
        paste(rep(" ", left_pad), collapse = ""),
        text,
        paste(rep(" ", right_pad), collapse = "")
    )
}

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
#' @param corr Column name containing correlation coefficient values. If NULL (default),
#'   tries to automatically identify using standard names like "cor", "r", "estimate", or "correlation".
#' @param var1 Column name containing first variable names. If NULL (default), tries to
#'   automatically identify using standard names like "var1", "variable1", "x", or "X".
#' @param var2 Column name containing second variable names. If NULL (default), tries to
#'   automatically identify using standard names like "var2", "variable2", "y", or "Y".
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
#' @param digits Integer specifying the number of decimal places to display.
#' @param center_table Logical. If TRUE, centers the entire table in the console.
#' @param center_layout Logical. If TRUE, centers the layout view in the console.
#' @param col_mapping Named list mapping column types to specific column names in the data.
#'   Supported types are "variable1", "variable2", "correlation", "statistic", "pvalue",
#'   "lower_ci", and "upper_ci".
#' @param border_char A character to draw border. Default is `"─"`.
#' @param style A list controlling the visual styling of table elements using ANSI formatting.
#'   Can include the following components:
#'   - `corr`: Styling for the correlation values.
#'   - `statistic`: Styling for the test statistics.
#'   - `pval`: Styling for the p-values.
#'   - `lower_ci`: Styling for the lower Confidence Interval values.
#'   - `upper_ci`: Styling for the upper Confidence Interval values.
#'   - `border_text`: Styling for the border.
#'   - `title`: Styling for the title.
#'
#'   Each style component can be either a predefined style string (e.g., "blue", "red_italic", "bold")
#'   or a function that takes a context list with/without a `value` element and returns the styled text.
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
#' \code{list(correlation = "cor_value", statistic = "t_value", pvalue = "p_adjusted")}
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
#' # With style
#'
#' mtcars |>
#'     rstatix::cor_test(disp, wt, hp) |>
#'     corr_matrix(
#'         statistic = "statistic",
#'         pval = "p",
#'         ci_lc = conf.low,
#'         ci_uc = conf.high,
#'         style = list(
#'             corr = 'blue',
#'             statistic = 'red',
#'             pval = 'green',
#'             lower_ci = 'magenta',
#'             upper_ci = 'cyan',
#'             title = "red",
#'             border_text = function(x) {
#'                 st_style <- cli::make_ansi_style("limegreen")
#'                 st_style(x)
#'             }
#'         ),
#'         layout_view = TRUE
#'     )
#'
#' @export
corr_matrix <- function(data,
                        method = NULL,
                        corr = NULL,
                        var1 = NULL,
                        var2 = NULL,
                        statistic = NULL,
                        pval = NULL,
                        ci_lc = NULL,
                        ci_uc = NULL,
                        LT = TRUE,
                        UT = TRUE,
                        layout_view = FALSE,
                        digits = 3,
                        center_table = FALSE,
                        center_layout = FALSE,
                        col_mapping = NULL,
                        border_char = "─",
                        style = list(), ...) {

    # Require cli package for styling
    if (!requireNamespace("cli", quietly = TRUE)) {
        stop("The 'cli' package is required for styling. Please install it.")
    }

    # Default styling functions
    default_style <- function(x) x

    # Enhanced style function to handle more complex styling
    getStyleFunction <- function(style_string) {
        # Split combined styles
        styles <- strsplit(style_string, "_")[[1]]

        # Map style strings to CLI functions
        style_map <- list(
            'red' = cli::col_red,
            'blue' = cli::col_blue,
            'green' = cli::col_green,
            'yellow' = cli::col_yellow,
            'magenta' = cli::col_magenta,
            'cyan' = cli::col_cyan,
            'white' = cli::col_white,
            'black' = cli::col_black,
            'bold' = cli::style_bold,
            'italic' = cli::style_italic,
            'underline' = cli::style_underline
        )

        # Compose the styling functions
        style_funcs <- lapply(styles, function(s) {
            if (s %in% names(style_map)) {
                return(style_map[[s]])
            } else {
                warning(paste("Unknown style:", s, ". Using default."))
                return(default_style)
            }
        })

        # Reduce the functions to a single composition
        Reduce(function(f, g) function(x) f(g(x)), style_funcs, right = TRUE)
    }

    # Prepare styling functions with more robust handling
    prepare_style_function <- function(style_spec, default_style_func = default_style) {
        if (is.null(style_spec)) return(default_style_func)

        if (is.character(style_spec)) {
            return(getStyleFunction(style_spec))
        } else if (is.function(style_spec)) {
            return(style_spec)
        } else {
            return(default_style_func)
        }
    }

    # Prepare all style functions
    style_corr <- prepare_style_function(style$corr)
    style_statistic <- prepare_style_function(style$statistic)
    style_pval <- prepare_style_function(style$pval)
    style_lower_ci <- prepare_style_function(style$lower_ci)
    style_upper_ci <- prepare_style_function(style$upper_ci)

    # Special handling for border and title with fallback
    style_border <- if (!is.null(style$border_text)) {
        if (is.character(style$border_text)) {
            getStyleFunction(style$border_text)
        } else if (is.function(style$border_text)) {
            style$border_text
        } else default_style
    } else default_style

    style_title <- if (!is.null(style$title)) {
        if (is.character(style$title)) {
            getStyleFunction(style$title)
        } else if (is.function(style$title)) {
            style$title
        } else default_style
    } else default_style

    corr_col <- if (!is.null(substitute(corr))) deparse(ensym(corr))
    var1_col_spec <- if (!is.null(substitute(var1))) deparse(ensym(var1))
    var2_col_spec <- if (!is.null(substitute(var2))) deparse(ensym(var2))
    statistic_col <- if (!is.null(substitute(statistic))) deparse(ensym(statistic))
    pval_col <- if (!is.null(substitute(pval))) deparse(ensym(pval))
    ci_lc_col <- if (!is.null(substitute(ci_lc))) deparse(ensym(ci_lc))
    ci_uc_col <- if (!is.null(substitute(ci_uc))) deparse(ensym(ci_uc))

    find_column <- function(col_spec, standard_names, type = NULL, position = NULL) {
        if (!is.null(col_spec)) {
            if (col_spec %in% names(data)) {
                return(col_spec)
            } else {
                return(NULL)
            }
        }

        if (!is.null(col_mapping) && !is.null(type) && type %in% names(col_mapping)) {
            mapped_col <- col_mapping[[type]]
            if (mapped_col %in% names(data)) {
                return(mapped_col)
            }
        }

        for (name in standard_names) {
            if (name %in% names(data)) {
                return(name)
            }
        }

        if (!is.null(position) && position <= ncol(data)) {
            return(names(data)[position])
        }

        return(NULL)
    }

    if (is.matrix(data)) {
        if (isSymmetric(unname(as.matrix(data)))) {
            if (is.null(method)) method <- "Unknown"

            vars <- colnames(data)
            if (is.null(vars)) vars <- paste0("Var", seq_len(ncol(data)))

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

            col_names <- c("Variable", vars)
            result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
            colnames(result_df) <- col_names
        } else {
            stop("The input matrix must be a correlation matrix (symmetric).")
        }
    } else {
        if (!is.data.frame(data)) {
            stop("Input must be a correlation matrix or a data frame.")
        }

        if (is.null(method)) {
            if ("method" %in% names(data)) {
                method <- unique(data$method)
            } else {
                method <- "Unknown"
            }
        }

        var1_col <- find_column(var1_col_spec, c("var1", "variable1", "x", "X"), "variable1", 1)
        var2_col <- find_column(var2_col_spec, c("var2", "variable2", "y", "Y"), "variable2", 2)

        if (is.null(var1_col) || is.null(var2_col)) {
            stop("Cannot identify variable columns in the data frame.")
        }

        cor_col <- find_column(corr_col, c("cor", "r", "estimate", "correlation"), "correlation")
        if (is.null(cor_col)) {
            stop("Cannot identify correlation column in the data frame.")
        }

        stat_col <- NULL
        if (!is.null(statistic_col)) {
            stat_col <- find_column(statistic_col, c("statistic", "t", "z"), "statistic")
            if (is.null(stat_col) && !is.null(statistic_col)) {
                warning("Specified statistic column '", statistic_col, "' not found in data.")
            }
        }

        p_col <- NULL
        if (!is.null(pval_col)) {
            p_col <- find_column(pval_col, c("p", "p.value", "pvalue"), "pvalue")
            if (is.null(p_col) && !is.null(pval_col)) {
                warning("Specified p-value column '", pval_col, "' not found in data.")
            }
        }

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

        show_ci <- !is.null(ci_low_col) || !is.null(ci_high_col)

        elements_to_show <- 1
        if (!is.null(stat_col)) elements_to_show <- elements_to_show + 1
        if (!is.null(p_col)) elements_to_show <- elements_to_show + 1
        if (show_ci) elements_to_show <- elements_to_show + 1

        if (layout_view) {
            cat("\n")
            layout_width <- 29

            if (center_layout) {
                term_width <- tryCatch({
                    as.numeric(system("tput cols", intern = TRUE))
                }, error = function(e) {
                    as.double(options("width"))
                })

                left_padding <- max(0, floor((term_width - layout_width) / 2))
                padding_str <- paste0(rep(" ", left_padding), collapse = "")
            } else {
                padding_str <- ""
            }

            # Apply border styling
            top_line <- paste0(
                style_border("┌"),
                style_border(paste0(rep(border_char, layout_width - 2), collapse = "")),
                style_border("┐")
            )
            middle_line <- paste0(
                style_border("├"),
                style_border(paste0(rep(border_char, layout_width - 2), collapse = "")),
                style_border("┤")
            )
            bottom_line <- paste0(
                style_border("└"),
                style_border(paste0(rep(border_char, layout_width - 2), collapse = "")),
                style_border("┘")
            )

            # Apply title styling
            cat(padding_str, top_line, "\n", sep = "")
            cat(padding_str, "| ", style_title(align_center("Layout for Corr. Matrix", layout_width - 4)), " |", "\n", sep = "")
            cat(padding_str, middle_line, "\n", sep = "")

            # Apply layout section styles using the correlation style
            cat(padding_str, "| ", style_corr(align_center("< corr >", layout_width - 4)), " |", "\n", sep = "")

            if (!is.null(stat_col)) {
                cat(padding_str, "| ", style_statistic(align_center("< statistic >", layout_width - 4)), " |", "\n", sep = "")
            }

            if (!is.null(p_col)) {
                cat(padding_str, "| ", style_pval(align_center("< p-value >", layout_width - 4)), " |", "\n", sep = "")
            }

            if (show_ci) {
                if (!is.null(ci_low_col) && !is.null(ci_high_col)) {
                    ci_text <- "< [Lower CI, Upper CI] >"
                } else if (!is.null(ci_low_col)) {
                    ci_text <- "< [Lower CI, NA] >"
                } else if (!is.null(ci_high_col)) {
                    ci_text <- "< [NA, Upper CI] >"
                } else {
                    ci_text <- "< CI >"
                }
                cat(padding_str, "| ",
                    style_lower_ci(align_center(ci_text, layout_width - 4)),
                    " |", "\n", sep = "")
            }

            cat(padding_str, bottom_line, "\n", sep = "")
            cat("\n")
        }

        vars <- unique(c(data[[var1_col]], data[[var2_col]]))
        n <- length(vars)

        result_matrix <- matrix("", nrow = n * elements_to_show, ncol = n + 1)

        for (i in 1:n) {
            row_base <- (i - 1) * elements_to_show + 1

            result_matrix[row_base, 1] <- vars[i]

            for (j in 1:n) {
                if (i == j) {
                    result_matrix[row_base, j + 1] <- "1"
                } else if ((i < j && UT) || (i > j && LT)) {
                    match_row <- which(
                        (data[[var1_col]] == vars[i] & data[[var2_col]] == vars[j]) |
                            (data[[var1_col]] == vars[j] & data[[var2_col]] == vars[i])
                    )

                    if (length(match_row) > 0) {
                        row_idx <- match_row[1]
                        current_row <- 0

                        if (!is.null(cor_col)) {
                            result_matrix[row_base + current_row, j + 1] <- sprintf(paste0("%.", digits, "f"), as.numeric(data[row_idx, cor_col]))
                        }
                        current_row <- current_row + 1

                        if (!is.null(stat_col)) {
                            result_matrix[row_base + current_row, j + 1] <- sprintf(paste0("%.", digits, "f"), as.numeric(data[row_idx, stat_col]))
                            current_row <- current_row + 1
                        }

                        if (!is.null(p_col)) {
                            p_value <- as.numeric(data[row_idx, p_col])
                            result_matrix[row_base + current_row, j + 1] <- ifelse(p_value < 0.001, "<0.001", sprintf(paste0("%.", digits, "f"), p_value))
                            current_row <- current_row + 1
                        }

                        if (show_ci) {
                            ci_low_val <- if (!is.null(ci_low_col)) sprintf(paste0("%.", digits, "f"), as.numeric(data[row_idx, ci_low_col])) else "NA"
                            ci_high_val <- if (!is.null(ci_high_col)) sprintf(paste0("%.", digits, "f"), as.numeric(data[row_idx, ci_high_col])) else "NA"
                            result_matrix[row_base + current_row, j + 1] <- sprintf("[%s, %s]", ci_low_val, ci_high_val)
                        }
                    }
                }
            }
        }

        col_names <- c("Variable", vars)
        result_df <- as.data.frame(result_matrix, stringsAsFactors = FALSE)
        colnames(result_df) <- col_names
    }

    # Helper function to calculate effective character length (stripping ANSI codes)
    effective_nchar <- function(x) {
        nchar(gsub("\033\\[[0-9;]*m", "", x))
    }

    # Modify the col_widths calculation to handle styled text
    col_widths <- sapply(1:ncol(result_df), function(col) {
        max(
            effective_nchar(colnames(result_df)[col]),
            max(sapply(1:nrow(result_df), function(row) {
                effective_nchar(result_matrix[row, col])
            }))
        )
    })

    total_width <- sum(as.numeric(col_widths)) + 3 * (length(col_widths) - 1) + 4

    horizontal_line <- style_border(paste0(rep(border_char, total_width), collapse = ""))

    title <- paste0(method, " Correlation Matrix")

    # Apply centering if requested
    prefix <- ""
    if (center_table) {
        term_width <- tryCatch({
            as.numeric(system("tput cols", intern = TRUE))
        }, error = function(e) {
            as.double(options("width"))
        })

        left_padding <- max(0, floor((term_width - total_width) / 2))
        prefix <- paste0(rep(" ", left_padding), collapse = "")
    }

    # Create a new matrix for styled output
    styled_result_matrix <- result_matrix

    # Apply styles to each element based on its position
    for (i in 1:nrow(result_matrix)) {
        for (j in 1:ncol(result_matrix)) {
            if (j == 1) {
                # Variable names - no special styling
                next
            }

            # Determine which type of value this is based on row position
            row_group <- (i - 1) %/% elements_to_show
            row_in_group <- (i - 1) %% elements_to_show

            if (result_matrix[i, j] == "1") {
                styled_result_matrix[i, j] <- style_corr(result_matrix[i, j])
            } else if (row_in_group == 0) {
                # Correlation value
                styled_result_matrix[i, j] <- style_corr(result_matrix[i, j])
            } else if (row_in_group == 1 && !is.null(stat_col)) {
                # Statistic value
                styled_result_matrix[i, j] <- style_statistic(result_matrix[i, j])
            } else if ((row_in_group == 2 || (row_in_group == 1 && is.null(stat_col))) && !is.null(p_col)) {
                # P-value
                styled_result_matrix[i, j] <- style_pval(result_matrix[i, j])
            } else if (grepl("^\\[.+,.+\\]$", result_matrix[i, j])) {
                # Confidence interval
                ci_parts <- strsplit(gsub("\\[|\\]", "", result_matrix[i, j]), ",")[[1]]
                styled_result_matrix[i, j] <- sprintf("[%s, %s]",
                                                      style_lower_ci(trimws(ci_parts[1])),
                                                      style_upper_ci(trimws(ci_parts[2])))
            }
        }
    }

    cat("\n", prefix, style_title(align_center(title, total_width)), "\n", sep = "")
    cat(prefix, horizontal_line, "\n", sep = "")
    cat(prefix, format_row_cm(colnames(result_df), col_widths, left_align_first = TRUE), "\n", sep = "")
    cat(prefix, horizontal_line, "\n", sep = "")

    for (i in 1:nrow(result_df)) {
        cat(prefix, format_row_cm(styled_result_matrix[i, ], col_widths, left_align_first = TRUE), "\n", sep = "")

        if (i %% elements_to_show == 0) {
            cat(prefix, horizontal_line, "\n", sep = "")
        }
    }

    invisible(result_df)
}
