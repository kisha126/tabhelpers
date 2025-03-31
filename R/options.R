.onLoad <- function(libname, pkgname) {
    # For `table_default` function
    options(tab_default = list(
        vb_char = "\u2502",           # '│
        vb_top = "\u252C",            # '┬'
        vb_mid = "\u253C",            # '┼'
        vb_bottom = "\u2534",         # '┴'
        border_char = "\u2500",       # '─'
        header_underline = "\u2500",  # '─'
        truncate_message = TRUE,      # Whether to show truncation messages
        nrows = 10                    # Default number of rows to display
    ))

    # Load cli themes
    cli::cli_div(theme = list(
        span.info = list(color = "cyan"),
        span.warning = list(color = "yellow")
    ))
}

#' Manage "tab_default" options
#'
#' This function allows retrieving or modifying the "tab_default" options.
#' If called without arguments, it returns the current list of options.
#' If `o` and `value` are provided, it updates the specified option.
#'
#' @param o A character string specifying the option to retrieve or modify.
#' @param value The new value to assign to the specified option. If NULL, the function returns the current value.
#' @return If no arguments are provided, it returns the entire "tab_default" list. If `o` is provided without `value`, it returns the current value of that option. If both `o` and `value` are provided, it updates the option.
#' @examples
#' tabhelpers_options()  # Get all options
#' tabhelpers_options("vb_top")  # Get specific option
#' tabhelpers_options("border_char", "+")  # Modify an option
#'
#' @export
tabhelpers_options <- function(o = NULL, value = NULL) {
    # Retrieve current options
    tab_opts <- getOption("tab_default", default = list())

    if (is.null(o)) {
        return(tab_opts)  # Return full options list if no argument is provided
    }

    if (!o %in% names(tab_opts)) {
        stop("Invalid option name: ", o)
    }

    if (is.null(value)) {
        return(tab_opts[[o]])  # Return specific option value
    }

    # Update the option
    tab_opts[[o]] <- value
    options(tab_default = tab_opts)
}
