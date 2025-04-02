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

justify_text <- function(text, width, justify = "center") {
    justify <- rep_len(justify, length(text))

    width <- pmax(width, 0)

    side <- ifelse(justify == "left", "right",
                   ifelse(justify == "right", "left", "both"))
    stringr::str_pad(text, width, side = side)
}

format_row <- function(row, col_widths, justify_cols = NULL, n_space = 2, styles = NULL,
                       col_data = NULL, is_header = FALSE, vb = list()) {
    original_names <- names(row)
    if (is.null(original_names) && length(row) > 0) {
        original_names <- as.character(seq_along(row))
    } else if (length(original_names) != length(row)) {
        original_names <- as.character(seq_along(row))
    }

    # First pass: Apply styles to get final text content including any additions...
    styled_content <- character(length(row))
    for (i in seq_along(row)) {
        col_name_or_index <- original_names[i]
        value <- row[i]

        styled_text <- value

        if (!is.null(styles)) {
            style_fn_or_name <- NULL
            lookup_key <- if (is_header) value else col_name_or_index

            if (is.list(styles)) {
                if (!is.null(names(styles))) {
                    if (!is.null(lookup_key) && lookup_key %in% names(styles)) {
                        style_fn_or_name <- styles[[lookup_key]]
                    } else if (as.character(i) %in% names(styles)) {
                        style_fn_or_name <- styles[[as.character(i)]]
                    }
                }
                if (is.null(style_fn_or_name) && is.null(names(styles)) && i <= length(styles)) {
                    style_fn_or_name <- styles[[i]]
                }
            } else if (length(styles) >= i) {
                style_fn_or_name <- styles[[i]]
            }

            if (!is.null(style_fn_or_name) && is.function(style_fn_or_name)) {
                context <- list(
                    value = value,
                    formatted_value = value,  # Not justified yet
                    col_name = col_name_or_index,
                    col_index = i,
                    is_header = is_header,
                    data = col_data,
                    justify = "center",  # Default, will be adjusted later
                    width = col_widths[i]
                )

                result <- tryCatch({
                    style_result <- style_fn_or_name(context)
                    if (is.character(style_result) && length(style_result) == 1) {
                        # Strip ANSI codes to get the plain content
                        stripped_text <- gsub("\033\\[[0-9;]*m", "", style_result)
                        styled_text <- stripped_text
                    }
                    styled_text
                }, error = function(e) {
                    warning("Styling function failed for column '", col_name_or_index, "': ", e$message, call. = FALSE)
                    value
                })
            }
        }

        styled_content[i] <- styled_text
    }

    # Calculate actual width needed for each column based on styled content
    actual_widths <- pmax(col_widths, nchar(styled_content))

    # Second pass: Apply justification and styling with appropriate width
    pre_justified <- sapply(seq_along(row), function(i) {
        col_name_or_index <- original_names[i]
        value <- row[i]
        justify_value <- "center"

        if (!is.null(justify_cols)) {
            temp_justify <- NULL
            if (is.list(justify_cols)) {
                if (!is.null(names(justify_cols))) {
                    if (col_name_or_index %in% names(justify_cols)) {
                        temp_justify <- justify_cols[[col_name_or_index]]
                    } else if (as.character(i) %in% names(justify_cols)) {
                        temp_justify <- justify_cols[[as.character(i)]]
                    }
                }
                if (is.null(temp_justify) && is.null(names(justify_cols)) && i <= length(justify_cols)) {
                    temp_justify <- justify_cols[[i]]
                }
            } else if (is.character(justify_cols)) {
                temp_justify <- justify_cols[min(i, length(justify_cols))]
            }
            if (!is.null(temp_justify) && temp_justify %in% c("left", "right", "center")) {
                justify_value <- temp_justify
            }
        }

        # Use actual_widths here instead of col_widths
        justify_text(value, actual_widths[i], justify_value)
    })

    formatted <- sapply(seq_along(row), function(i) {
        col_name_or_index <- original_names[i]
        value <- row[i]
        text <- pre_justified[i]
        justify_value <- "center"

        if (!is.null(justify_cols)) {
            temp_justify <- NULL
            if (is.list(justify_cols)) {
                if (!is.null(names(justify_cols))) {
                    if (col_name_or_index %in% names(justify_cols)) {
                        temp_justify <- justify_cols[[col_name_or_index]]
                    } else if (as.character(i) %in% names(justify_cols)) {
                        temp_justify <- justify_cols[[as.character(i)]]
                    }
                }
                if (is.null(temp_justify) && is.null(names(justify_cols)) && i <= length(justify_cols)) {
                    temp_justify <- justify_cols[[i]]
                }
            } else if (is.character(justify_cols)) {
                temp_justify <- justify_cols[min(i, length(justify_cols))]
            }
            if (!is.null(temp_justify) && temp_justify %in% c("left", "right", "center")) {
                justify_value <- temp_justify
            }
        }

        # --- Apply Styles ---
        if (!is.null(styles)) {
            style_fn_or_name <- NULL
            lookup_key <- if (is_header) value else col_name_or_index

            if (is.list(styles)) {
                if (!is.null(names(styles))) {
                    if (!is.null(lookup_key) && lookup_key %in% names(styles)) {
                        style_fn_or_name <- styles[[lookup_key]]
                    } else if (as.character(i) %in% names(styles)) {
                        style_fn_or_name <- styles[[as.character(i)]]
                    }
                }
                if (is.null(style_fn_or_name) && is.null(names(styles)) && i <= length(styles)) {
                    style_fn_or_name <- styles[[i]]
                }
            } else if (length(styles) >= i) {
                style_fn_or_name <- styles[[i]]
            }

            if (!is.null(style_fn_or_name)) {
                if (is.function(style_fn_or_name)) {
                    context <- list(
                        value = value,
                        formatted_value = value,  # Use original value, not justified text
                        col_name = col_name_or_index,
                        col_index = i,
                        is_header = is_header,
                        data = col_data,
                        justify = justify_value,
                        width = actual_widths[i]  # Use actual width
                    )

                    result <- tryCatch({
                        styled_result <- style_fn_or_name(context)

                        if (is.character(styled_result) && length(styled_result) == 1) {
                            stripped_text <- gsub("\033\\[[0-9;]*m", "", styled_result)

                            # Use actual_widths for justification
                            new_text <- justify_text(stripped_text, actual_widths[i], justify_value)

                            # Transfer ANSI codes to the justified text
                            if (grepl("\033\\[", styled_result)) {
                                ansi_parts <- strsplit(styled_result, "\033\\[[0-9;]*m")[[1]]
                                ansi_codes <- gregexpr("\033\\[[0-9;]*m", styled_result)
                                ansi_codes_text <- regmatches(styled_result, ansi_codes)[[1]]

                                if (length(ansi_codes_text) > 0) {
                                    # Start with the first part of the text
                                    result_parts <- list()
                                    last_pos <- 1

                                    # Extract positions of ANSI codes
                                    code_positions <- ansi_codes[[1]]

                                    # Process each ANSI code
                                    for (j in seq_along(code_positions)) {
                                        # Add text before this code
                                        if (code_positions[j] > last_pos) {
                                            result_parts <- c(result_parts,
                                                              list(substr(new_text, last_pos,
                                                                          code_positions[j] - 1)))
                                        }

                                        # Add the ANSI code
                                        result_parts <- c(result_parts, list(ansi_codes_text[j]))

                                        # Update position
                                        last_pos <- code_positions[j]
                                    }

                                    # Add remaining text
                                    if (last_pos <= nchar(new_text)) {
                                        result_parts <- c(result_parts,
                                                          list(substr(new_text, last_pos,
                                                                      nchar(new_text))))
                                    }

                                    return(paste0(unlist(result_parts), collapse = ""))
                                }
                            }

                            # If we couldn't properly transfer ANSI codes, just return the styled result
                            return(justify_text(stripped_text, actual_widths[i], justify_value))
                        }

                        return(text)
                    }, error = function(e) {
                        warning("Styling function failed for column '", col_name_or_index, "': ",
                                e$message, call. = FALSE)
                        return(text)
                    })

                    if (is.character(result) && length(result) == 1) {
                        text <- result
                    } else if (!is.character(result)){
                        warning("Styling function for column '", col_name_or_index,
                                "' did not return a character string.", call. = FALSE)
                    }

                } else if (is.character(style_fn_or_name)) {
                    style_parts <- unlist(strsplit(style_fn_or_name, "_"))

                    original_text <- text
                    for (style_part in style_parts) {
                        text <- tryCatch({
                            # Try col_*, style_*, then direct name lookup in cli namespace
                            if (exists(paste0("col_", style_part), where = asNamespace("cli"))) {
                                style_fn <- get(paste0("col_", style_part), envir = asNamespace("cli"))
                                style_fn(text)
                            } else if (exists(paste0("style_", style_part), where = asNamespace("cli"))) {
                                style_fn <- get(paste0("style_", style_part), envir = asNamespace("cli"))
                                style_fn(text)
                            } else if (exists(style_part, where = asNamespace("cli"))) {
                                style_fn <- get(style_part, envir = asNamespace("cli"))
                                style_fn(text)
                            } else {
                                warning("Unknown cli style '", style_part, "' in '", style_fn_or_name,
                                        "'.", call. = FALSE)
                                text
                            }
                        }, error = function(e) {
                            warning("Applying cli style '", style_part, "' failed: ", e$message,
                                    call. = FALSE)
                            original_text
                        })
                    }
                }
            }
        }

        text
    })

    # --- Add Vertical Borders ---
    border_char <- vb$char %||% getOption("tab_default")$vb_char
    after_cols_spec <- vb$after

    if (length(after_cols_spec) > 0) {
        after_cols_idx <- integer(0)
        if (is.numeric(after_cols_spec)) {
            after_cols_idx <- after_cols_spec
        } else if (is.character(after_cols_spec)) {
            after_cols_idx <- match(after_cols_spec, original_names)
        }

        valid_indices <- !is.na(after_cols_idx) & after_cols_idx > 0 & after_cols_idx < length(formatted)
        after_cols_idx <- sort(unique(after_cols_idx[valid_indices]))
        if (length(after_cols_idx) > 0) {
            processed_formatted <- character(length(formatted) + length(after_cols_idx))
            current_pos <- 1
            border_count <- 0
            for (i in seq_along(formatted)) {
                processed_formatted[current_pos] <- formatted[i]
                current_pos <- current_pos + 1
                if (i %in% after_cols_idx) {
                    formatted[i] <- paste0(formatted[i], " ", border_char)
                    border_count <- border_count + 1
                }
            }
        }
    }

    # --- Combine Cells with Separators ---
    separator <- paste0(rep(" ", n_space), collapse = "")
    paste0("  ", paste(formatted, collapse = separator), "  ")
}

# For cross tabulation

center_text_x2 <- function(text, width) {
    text_width <- nchar(strip_ansi(text))
    padding <- max(0, width - text_width)
    left_padding <- floor(padding / 2)
    right_padding <- ceiling(padding / 2)
    paste0(strrep(" ", left_padding), text, strrep(" ", right_padding))
}

# center_text_x2 <- function(text, width) {
#     text <- as.character(text)
#     if (is.na(width) || width <= nchar(text)) return(text)
#     padding <- width - nchar(text)
#     left_pad <- floor(padding / 2)
#     right_pad <- ceiling(padding / 2)
#     paste0(strrep(" ", left_pad), text, strrep(" ", right_pad))
# }

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

format_row_summary <- function(left,
                               right,
                               left_width,
                               right_width,
                               align = NULL,
                               style = NULL) {
    # Apply alignment
    left_align <- "left"  # Default left alignment for left column
    right_align <- "right"  # Default right alignment for right column

    if (!is.null(align)) {
        if (is.character(align) && length(align) == 1) {
            left_align <- align
        } else if (is.character(align) && length(align) == 2) {
            left_align <- align[1]
            right_align <- align[2]
        } else if (is.list(align)) {
            if (!is.null(align$left_col)) left_align <- align$left_col
            if (!is.null(align$right_col)) right_align <- align$right_col
        }
    }

    # Format text based on alignment
    if (left_align == "left") {
        left_formatted <- sprintf("%-*s", left_width, left)
    } else if (left_align == "right") {
        left_formatted <- sprintf("%*s", left_width, left)
    } else if (left_align == "center") {
        left_formatted <- align_test(left, left_width)
    }

    if (right_align == "left") {
        right_formatted <- sprintf("%-*s", right_width, right)
    } else if (right_align == "right") {
        right_formatted <- sprintf("%*s", right_width, right)
    } else if (right_align == "center") {
        right_formatted <- align_test(right, right_width)
    }

    # Apply styling if provided
    if (!is.null(style)) {
        # Style left column
        if (!is.null(style$left_col)) {
            if (is.function(style$left_col)) {
                left_formatted <- style$left_col(list(value = left_formatted))
            } else if (is.character(style$left_col)) {
                # Apply predefined styles from cli
                if (style$left_col == "bold") {
                    left_formatted <- cli::style_bold(left_formatted)
                } else if (style$left_col == "italic") {
                    left_formatted <- cli::style_italic(left_formatted)
                } else if (style$left_col == "blue") {
                    left_formatted <- cli::col_blue(left_formatted)
                } else if (style$left_col == "red") {
                    left_formatted <- cli::col_red(left_formatted)
                } else if (style$left_col == "green") {
                    left_formatted <- cli::col_green(left_formatted)
                } else if (style$left_col == "yellow") {
                    left_formatted <- cli::col_yellow(left_formatted)
                } else if (style$left_col == "blue_bold") {
                    left_formatted <- cli::col_blue(cli::style_bold(left_formatted))
                } else if (style$left_col == "red_italic") {
                    left_formatted <- cli::col_red(cli::style_italic(left_formatted))
                }
                # Add more style combinations as needed
            }
        }

        # Style right column
        if (!is.null(style$right_col)) {
            if (is.function(style$right_col)) {
                right_formatted <- style$right_col(list(value = right_formatted))
            } else if (is.character(style$right_col)) {
                # Apply predefined styles from cli
                if (style$right_col == "bold") {
                    right_formatted <- cli::style_bold(right_formatted)
                } else if (style$right_col == "italic") {
                    right_formatted <- cli::style_italic(right_formatted)
                } else if (style$right_col == "blue") {
                    right_formatted <- cli::col_blue(right_formatted)
                } else if (style$right_col == "red") {
                    right_formatted <- cli::col_red(right_formatted)
                } else if (style$right_col == "green") {
                    right_formatted <- cli::col_green(right_formatted)
                } else if (style$right_col == "yellow") {
                    right_formatted <- cli::col_yellow(right_formatted)
                } else if (style$right_col == "blue_bold") {
                    right_formatted <- cli::col_blue(cli::style_bold(right_formatted))
                } else if (style$right_col == "red_italic") {
                    right_formatted <- cli::col_red(cli::style_italic(right_formatted))
                }
                # Add more style combinations as needed
            }
        }
    }

    sep_value <- "    "
    if (!is.null(style) && !is.null(style$sep)) {
        # If a custom separator is provided, add padding around it
        sep_value <- paste0(" ", style$sep, " ")
    }

    paste0("  ", left_formatted, sep_value, right_formatted)
}

# format_row_summary <- function(left, right, left_width, right_width) {
#     left_formatted <- sprintf("%-*s", left_width, left)
#     right_formatted <- sprintf("%*s", right_width, right)
#     paste0("  ", left_formatted, "    ", right_formatted)
# }

# For correlation matrix

format_row_cm <- function(row, col_widths, left_align_first = FALSE, pos = FALSE) {
    row <- as.character(row)

    formatted <- sapply(seq_along(row), function(i) {
        x <- row[i]

        if (i == 1 && left_align_first) {
            format(x, width = col_widths[i], justify = "left")
        } else {
            align_center(x, col_widths[i], pos = pos)
        }
    })

    paste0("  ", paste(formatted, collapse = "   "), "  ")
}


