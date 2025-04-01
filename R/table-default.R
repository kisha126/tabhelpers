#' Display a table in the command line with customizable formatting.
#'
#' This function takes a x frame or tibble and displays it in the command line with various formatting options.
#' The table can be customized in terms of alignment, number formatting, column width, and more.
#'
#' @param x A data frame or tibble to display.
#' @param justify_cols Controls column text alignment. Can be:
#'   - A single string ("left", "right", "center") to apply to all columns
#'   - A vector of strings to apply to columns by position
#'   - A named list (e.g., `list("1" = "left", "mpg" = "right")`) for specific columns
#' @param digits Number of digits to round numeric columns to. Default is `3`.
#' @param digits_by_col Named list specifying the number of digits for specific columns. Default is `NULL`.
#' @param scientific Logical. If `TRUE`, numeric values are displayed in scientific notation. Default is `FALSE`.
#' @param na_print Character string to represent missing values. Default is an empty string `""`.
#' @param min_width Minimum column width. Default is `NULL`.
#' @param border_char Character used for borders. Default is `"\u2500"`.
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
#'   Hidden Feature: This parameter can be used to mutate the existing column, as well.
#'
#' @param nrows Number of rows to display. Tables with more rows will be truncated with informative messages.
#'   Default is the value set in `options("tab_default")$nrows`, which is typically `10`.
#'
#' @param vb Default is an empty list. Stands for "vertical border" to draw a vertical in a specific position.
#'   Should be a list that contains the following:
#'   - `char`: To provide a specific character to iteratively draw a vertical border. The `"\u2502"` character is recommended.
#'   - `after`: The position of the vertical border AFTER that column. Should be a vector of integers.
#'
#'   This feature is recommended in presenting a statistical result like the coefficient table in Linear Regression.
#'
#' @param ... Additional arguments passed to specific methods.
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
#' # Set the number of rows to display
#' table_default(mtcars, nrow = 15)
#'
#' # Change the global default for number of rows
#' tabhelpers_options("nrows", 20)
#' table_default(mtcars)  # Will show up to 20 rows
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
#' # With vertical line
#' mtcars |>
#'     tibble::rownames_to_column(var = "Car Names") |>
#'     head(5) |>
#'     table_default(
#'         justify_cols = list("1" = "right", "mpg"="right"),
#'         digits = 1,
#'         center_table = TRUE,
#'         n_space = 2,
#'         style_columns = list(
#'             "mpg" = function(ctx) {
#'                 val <- as.numeric(ctx$value)
#'                 if(val > 20) cli::col_green(ctx$formatted_value)
#'                 else if(val > 15) cli::col_yellow(ctx$formatted_value)
#'                 else cli::col_red(ctx$formatted_value)
#'             }
#'         ),
#'         vb = list(char = getOption("tab_default")$vb_char, after = c(1, 10, 2))
#'     )
#'
#' # Displaying Linear Regression output
#' require(broom)
#' require(rstatix)
#'
#' mtcars |>
#'     lm(formula = mpg ~ wt + disp + hp) |>
#'     broom::tidy() |>
#'     table_default(
#'         justify_cols = list("term" = "right"),
#'         style_columns = list(
#'             p.value = function(ctx) {
#'                 val <- as.numeric(ctx$formatted_value)
#'                 if (val < 0.05 & val >= 0.001) {
#'                     cli::col_red(val)
#'                 } else if (val < 0.001) {
#'                     rep_txt <- replace(val, val < 0.001, "<0.001")
#'                     cli::style_bold(rep_txt)
#'                 } else {
#'                     cli::style_italic(val)
#'                 }
#'
#'             }
#'         ),
#'         vb = list(
#'             char = getOption("tab_default")$vb_char, after = 1
#'         ),
#'         n_space = 3,
#'         center_table = TRUE
#'     )
#'
#' ## Extend it with functional programming
#' require(dplyr)
#' require(tidyr)
#' require(glue)
#'
#' print_table = function(x) {
#'     groups = unique(x[[1]])
#'     groups = sort(groups)
#'
#'     for (i in seq_along(groups)) {
#'         group_value = groups[i]
#'         group_data = x[x[[1]] == group_value, -1]
#'
#'         col_name = toupper(substr(names(x)[1], 1, 1))
#'         col_name = paste0(col_name, substr(names(x)[1], 2, nchar(names(x)[1])))
#'
#'         cat(glue::glue("{i}. {col_name} = {group_value}"), "\n\n")
#'
#'         tabhelpers::table_default(
#'             group_data,
#'             justify_cols = list("term" = "left"),
#'             center_table = TRUE,
#'             style_columns = list(
#'                 p.value = function(ctx) {
#'                     val <- as.numeric(ctx$formatted_value)
#'                     if (val < 0.05 & val >= 0.001) {
#'                         cli::col_red(val)
#'                     } else if (val < 0.001) {
#'                         rep_txt <- replace(val, val < 0.001, "<0.001")
#'                         cli::style_bold(rep_txt)
#'                     } else {
#'                         cli::style_italic(val)
#'                     }
#'
#'                 }
#'             ),
#'             vb = list(
#'                 char = getOption("tab_default")$vb_char, after = 1
#'             )
#'         )
#'
#'         cat("\n")
#'     }
#'
#'     invisible(x)
#' }
#'
#' mtcars |>
#'     group_by(cyl) |>
#'     summarise(
#'         lm_model = list({
#'             model <- lm(mpg ~ wt)
#'             broom::tidy(model)
#'         })
#'     ) |>
#'     tidyr::unnest(lm_model) |>
#'     print_table()
#'
#' @importFrom cli console_width col_red col_blue col_green cli_alert_info cli_alert_warning
#' @importFrom utils head
#'
#' @export
table_default <- function(x,
                          justify_cols = "center",
                          digits = 3,
                          digits_by_col = NULL,
                          scientific = FALSE,
                          na_print = "",
                          min_width = NULL,
                          border_char = options('tab_default')$tab_default$border_char,
                          show_row_names = FALSE,
                          center_table = FALSE,
                          n_space = 2,
                          style_colnames = NULL,
                          style_columns = NULL,
                          nrows = getOption("tab_default")$nrows,
                          vb = list(),
                          ...) {

    if (!inherits(x, "data.frame")) {
        x <- try(as.data.frame(x), silent = TRUE)
        if (inherits(x, "try-error")) {
            stop("`x` must be a data frame or coercible to one.", call. = FALSE)
        }
    }

    # --- Input Validation and Preparation ---
    if (!is.numeric(n_space) || n_space < 0) {
        warning("`n_space` must be non-negative, using default 2.", call. = FALSE)
        n_space = 2
    }
    n_space <- floor(n_space)

    if (!is.numeric(nrows) || nrows < 0) {
        warning("`nrows` must be non-negative, using default 10.", call. = FALSE)
        nrows <- 10
    }
    nrows <- floor(nrows)
    original_row_count <- nrow(x)
    truncated <- original_row_count > nrows
    if (truncated) x <- head(x, nrows)
    original_x <- x
    x <- tibble::as_tibble(x, rownames = if (show_row_names) "row_names" else NA)

    if (show_row_names && !"row_names" %in% names(x)) {
        x <- dplyr::mutate(x, row_names = as.character(seq_len(nrow(x))), .before = 1)
        original_x <- tibble::as_tibble(original_x)
        original_x <- dplyr::mutate(original_x, row_names = as.character(seq_len(nrow(original_x))), .before = 1)

    } else if (show_row_names && "row_names" %in% names(x)) {
        x <- dplyr::relocate(x, "row_names", .before = 1)
        if("row_names" %in% names(original_x)) {
            original_x <- dplyr::relocate(tibble::as_tibble(original_x), "row_names", .before = 1)
        }
    }

    if (nrow(x) == 0 && ncol(x) == 0) {
        cat("Empty data frame (0 rows, 0 columns)\n")
        invisible(NULL)
    }

    x <- dplyr::mutate(x, dplyr::across(tidyselect::where(is.factor), as.character))
    format_num_col <- function(col, col_name) {
        digits_val <- digits_by_col[[col_name]] %||% digits

        is_int_like <- all(is.na(col) | (col %% 1 == 0))

        formatted_col <- character(length(col))
        na_idx <- is.na(col)
        formatted_col[na_idx] <- na_print

        if (is_int_like) {
            formatted_col[!na_idx] <- format(col[!na_idx], scientific = FALSE, trim = TRUE)
        } else if (scientific) {
            formatted_col[!na_idx] <- format(col[!na_idx], digits = digits_val, scientific = TRUE, trim = TRUE)
        } else {
            formatted_col[!na_idx] <- format(round(col[!na_idx], digits = digits_val), nsmall = digits_val, scientific = FALSE, trim = TRUE)
        }
        formatted_col
    }

    num_cols <- names(x)[sapply(x, is.numeric)]
    for(col_name in num_cols) {
        x[[col_name]] <- format_num_col(x[[col_name]], col_name)
    }

    x <- dplyr::mutate(x, dplyr::across(tidyselect::where(\(c) !is.numeric(c) && !is.character(c)), as.character))
    x <- dplyr::mutate(x, dplyr::across(tidyselect::where(\(c) !is.numeric(c)), ~ ifelse(is.na(.), na_print, .)))
    x <- dplyr::mutate(x, dplyr::across(dplyr::everything(), as.character))

    col_names <- colnames(x)
    if (ncol(x) == 0) {
        if(show_row_names && "row_names" %in% names(x)) {
            print(x)
        } else {
            cat("Data frame has 0 columns.\n")
        }
        return(invisible(NULL))
    }

    x_chars <- as.matrix(x)
    col_widths <- apply(rbind(nchar(col_names), nchar(x_chars)), 2, max, na.rm = TRUE)

    if (!is.null(min_width) && is.numeric(min_width)) {
        col_widths <- pmax(col_widths, floor(min_width[1]))
    }

    border_char_v <- vb$char %||% getOption("tab_default")$vb_char
    after_cols_spec <- vb$after
    after_cols_idx <- integer(0)

    if (length(after_cols_spec) > 0) {
        if (is.numeric(after_cols_spec)) {
            after_cols_idx <- after_cols_spec
        } else if (is.character(after_cols_spec)) {
            after_cols_idx <- match(after_cols_spec, col_names)
        }
        valid_indices <- !is.na(after_cols_idx) & after_cols_idx > 0 & after_cols_idx < length(col_widths)
        after_cols_idx <- sort(unique(after_cols_idx[valid_indices]))
    }
    n_borders <- length(after_cols_idx)

    # --- Total Width and Centering ---
    # Total width = Sum(widths) + NumSeparators*n_space + NumBorders*(1+nchar(border_char_v)) + 4 (side padding)
    total_width <- sum(col_widths) + (length(col_widths) - 1) * n_space + n_borders * (1 + nchar(border_char_v)) + 4

    terminal_width <- if (center_table) {
        tryCatch({
            cli::console_width()
        }, error = function(e) {
            as.integer(options("width")) %||% 80
        })
    } else {
        0
    }

    left_padding <- if (center_table && terminal_width > total_width) {
        paste0(rep(" ", floor((terminal_width - total_width) / 2)), collapse = "")
    } else {
        ""
    }

    # --- Horizontal Line Construction ---
    # Function to create ───┬───┼───┴─── style lines
    create_horizontal_line <- function(connector_char_mid, connector_left = border_char, connector_right = border_char) {
        line <- paste0(connector_left, border_char)

        for (i in seq_along(col_widths)) {
            line <- paste0(line, paste0(rep(border_char, col_widths[i]), collapse = ""))

            if (i < length(col_widths)) {
                if (i %in% after_cols_idx) {
                    line <- paste0(line,
                                   border_char,
                                   connector_char_mid,
                                   paste0(rep(border_char, n_space), collapse=""))
                } else {
                    line <- paste0(line, paste0(rep(border_char, n_space), collapse = ""))
                }
            }
        }
        line <- paste0(line, border_char, connector_right)
        line
    }

    # --- Define the specific lines using appropriate connectors
    connectors <- getOption("tab_default")
    top_line <- create_horizontal_line(connectors$vb_top)
    middle_line <- create_horizontal_line(connectors$vb_mid)
    bottom_line <- create_horizontal_line(connectors$vb_bottom)

    # --- If no vertical borders, use simple lines
    if (n_borders == 0) {
        simple_line <- paste0(rep(border_char, total_width), collapse = "")
        simple_width <- sum(col_widths) + max(0, length(col_widths) - 1) * n_space + 4
        simple_line <- paste0(rep(border_char, simple_width), collapse = "")

        top_line    <- paste0(border_char, paste0(rep(border_char, simple_width-2), collapse=""), border_char)
        middle_line <- paste0(border_char, paste0(rep(border_char, simple_width-2), collapse=""), border_char)
        bottom_line <- paste0(border_char, paste0(rep(border_char, simple_width-2), collapse=""), border_char)
        if (length(col_widths) <= 1) {
            middle_line <- simple_line
        }
    }


    # --- Print Table ---
    # --- Display truncation message before table if needed
    if (truncated) {
        cli::cli_alert_info("Showing {nrows} of {original_row_count} rows")
    }

    cat(left_padding, top_line, "\n", sep = "")

    # --- Use stats::setNames to ensure names are present for format_row lookup
    header_values <- stats::setNames(col_names, col_names)
    header_row <- format_row(header_values, col_widths,
                             justify_cols = justify_cols,
                             n_space = n_space,
                             styles = style_colnames,
                             col_data = original_x,
                             is_header = TRUE,
                             vb = vb)
    cat(left_padding, header_row, "\n", sep = "")

    cat(left_padding, middle_line, "\n", sep = "")

    if (nrow(x_chars) > 0) {
        for (i in 1:nrow(x_chars)) {
            current_row <- stats::setNames(x_chars[i,], col_names)
            cat(left_padding,
                format_row(current_row, col_widths,
                           justify_cols = justify_cols,
                           n_space = n_space,
                           styles = style_columns,
                           col_data = original_x,
                           is_header = FALSE,
                           vb = vb),
                "\n", sep = "")
        }
    }

    cat(left_padding, bottom_line, "\n", sep = "")

    # -- Display truncation message after table if needed --
    if (truncated) {
        cli::cli_alert_warning("Displayed only {nrows} of {original_row_count} rows. Set 'nrows' parameter to show more rows.")
    }

    invisible(x)
}

