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

format_row <- function(row, col_widths, left_align_first = FALSE, pos = FALSE, n_space = 2) {
    formatted <- sapply(seq_along(row), function(i) {
        if (i == 1 && left_align_first) {
            format(row[i], width = col_widths[i], justify = "left")
        } else {
            center_text(row[i], col_widths[i], pos = pos)
        }
    })

    paste0("  ", paste(formatted, collapse = paste0(rep(" ", n_space), collapse = "")), "  ")
}

# format_row <- function(row, col_widths, left_align_first = FALSE, pos = FALSE) {
#     formatted <- sapply(seq_along(row), function(i) {
#         if (i == 1 && left_align_first) {
#             format(row[i], width = col_widths[i], justify = "left")
#         } else {
#             center_text(row[i], col_widths[i], pos = pos)
#         }
#     })
#     paste0("  ", paste(formatted, collapse = "   "), "  ")
# }

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

