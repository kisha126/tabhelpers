# Helper functions
center_text <- function(text, width, pos = FALSE) {
    if (pos) {
        padding <- pmax(width - nchar(text), 0)
    } else {
        padding <- width - nchar(text)
    }
    left_pad <- floor(padding / 2)
    right_pad <- ceiling(padding / 2)
    paste0(paste0(rep(" ", left_pad), collapse = ""), text, paste0(rep(" ", right_pad), collapse = ""))
}

justify_text <- function(text, width, justify = "center", pos = FALSE) {
    justify <- justify[1]

    if (justify == "center") {
        return(center_text(text, width, pos))
    } else if (justify == "left") {
        padding <- width - nchar(text)
        return(paste0(text, paste0(rep(" ", padding), collapse = "")))
    } else if (justify == "right") {
        padding <- width - nchar(text)
        return(paste0(paste0(rep(" ", padding), collapse = ""), text))
    }
    return(center_text(text, width, pos))
}

format_row <- function(row, col_widths, justify_cols = NULL, pos = FALSE, n_space = 2, styles = NULL,
                       col_data = NULL, is_header = FALSE) {
    formatted <- sapply(seq_along(row), function(i) {
        col_name <- names(row)[i]
        value <- row[i]
        justify_value <- "center"

        if (!is.null(justify_cols)) {
            if (is.character(justify_cols) && length(justify_cols) == 1) {
                justify_value <- justify_cols
            } else if (is.character(justify_cols) && length(justify_cols) >= i) {
                justify_value <- justify_cols[i]
            } else if (is.list(justify_cols)) {
                if (!is.null(names(justify_cols)) && as.character(i) %in% names(justify_cols)) {
                    justify_value <- justify_cols[[as.character(i)]]
                } else if (!is.null(names(justify_cols)) && !is.null(col_name) && col_name %in% names(justify_cols)) {
                    justify_value <- justify_cols[[col_name]]
                } else if (is.null(names(justify_cols)) && i <= length(justify_cols)) {
                    justify_value <- justify_cols[[i]]
                }
            }
        }

        text <- justify_text(value, col_widths[i], justify_value, pos)

        if (!is.null(styles)) {
            style_fn_or_name <- NULL
            lookup_key <- if (is_header) value else col_name

            if (!is.null(names(styles)) && !is.null(lookup_key) && lookup_key %in% names(styles)) {
                style_fn_or_name <- styles[[lookup_key]]
            } else if (!is.null(names(styles)) && as.character(i) %in% names(styles)) {
                style_fn_or_name <- styles[[as.character(i)]]
            } else if (is.null(names(styles)) && i <= length(styles)) {
                style_fn_or_name <- styles[[i]]
            }

            if (!is.null(style_fn_or_name)) {
                if (is.function(style_fn_or_name)) {
                    context <- list(
                        value = value,
                        formatted_value = text,
                        col_name = lookup_key,
                        col_index = i,
                        is_header = is_header,
                        data = col_data
                    )

                    result <- tryCatch({
                        style_fn_or_name(context)
                    }, error = function(e) {
                        text
                    })

                    if (is.character(result)) {
                        text <- result
                    }
                } else if (is.character(style_fn_or_name) && requireNamespace("cli", quietly = TRUE)) {
                    style_parts <- unlist(strsplit(style_fn_or_name, "_"))

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
                }
            }
        }

        text
    })

    paste0("  ", paste(formatted, collapse = paste0(rep(" ", n_space), collapse = "")), "  ")
}

# For cross tabulation

center_text_x2 <- function(text, width) {
    text <- as.character(text)
    if (is.na(width) || width <= nchar(text)) return(text)
    padding <- width - nchar(text)
    left_pad <- floor(padding / 2)
    right_pad <- ceiling(padding / 2)
    paste0(strrep(" ", left_pad), text, strrep(" ", right_pad))
}

format_number_x2 <- function(x) {
    sapply(x, function(n) {
        if (abs(n - round(n)) < 1e-10) {
            sprintf("%d", round(n))
        } else {
            sprintf("%.1f", n)
        }
    })
}

# For summary table

align_test <- function(text, width) {
    padding <- width - nchar(text)
    left_pad <- floor(padding / 2)
    right_pad <- ceiling(padding / 2)
    paste0(strrep(" ", left_pad), text, strrep(" ", right_pad))
}

format_row_summary <- function(left, right, left_width, right_width) {
    left_formatted <- sprintf("%-*s", left_width, left)
    right_formatted <- sprintf("%*s", right_width, right)
    paste0("  ", left_formatted, "    ", right_formatted)
}
