.onLoad <- function(libname, pkgname) {
    # Define and set all your options
    options(
        # Table display options
        tab_default = list(
            vb_char = "\u2502",           # '│
            vb_top = "\u252C",            # '┬'
            vb_mid = "\u253C",            # '┼'
            vb_bottom = "\u2534",         # '┴'
            border_char = "\u2500",       # '─'
            header_underline = "\u2500",  # '─'
            truncate_message = TRUE,      # Whether to show truncation messages
            nrows = 10                    # Default number of rows to display
        ),

        # Digits precision options
        tab_digits = list(
            ex = 1,                       # Expected values precision
            row_pct = 0,                  # Row percentages precision
            col_pct = 0,                  # Column percentages precision
            total_pct = 0                 # Total percentages precision
        )

        # --- Add more options
    )

    # Load cli themes
    cli::cli_div(theme = list(
        span.info = list(color = "cyan"),
        span.warning = list(color = "yellow")
    ))
}

#' Manage package options
#'
#' This function allows retrieving or modifying the package options across different categories.
#' If called without arguments, it returns all option categories and their values.
#' If `category` is provided alone, it returns all options in that category.
#' If `category` and `option` are provided, it returns the specific option value.
#' If all three parameters are provided, it updates the specified option.
#'
#' @param category A character string specifying the option category (e.g., "tab_default", "tab_digits").
#'                 If omitted, returns all option categories.
#' @param option A character string specifying the option to retrieve or modify within the category.
#'               For backward compatibility, you can also use a specific option name directly as the `category` parameter.
#' @param value The new value to assign to the specified option. If NULL, the function returns the current value.
#' @return If no arguments are provided, returns all option categories and their values.
#'         If only `category` is provided, returns all options in that category.
#'         If `category` and `option` are provided without `value`, returns the current value of that option.
#'         If all parameters are provided, updates the option and returns the updated option category list invisibly.
#' @examples
#' # Get all options across all categories
#' tabhelpers_options()
#'
#' # Get all options in the "tab_default" category
#' tabhelpers_options("tab_default")
#'
#' # Get all options in the "tab_digits" category
#' tabhelpers_options("tab_digits")
#'
#' # Get a specific option
#' tabhelpers_options("tab_default", "vb_top")
#' tabhelpers_options("tab_digits", "ex")
#'
#' # Using backward compatibility (system will find the right category)
#' tabhelpers_options("vb_top")
#' tabhelpers_options("ex")
#'
#' # Modify an option
#' tabhelpers_options("tab_default", "border_char", "+")
#' tabhelpers_options("tab_digits", "ex", 2)
#'
#' # Using backward compatibility for modification
#' tabhelpers_options("border_char", "+")
#' tabhelpers_options("ex", 2)
#'
#' @export
tabhelpers_options <- function(category = NULL, option = NULL, value = NULL) {
    valid_categories <- c("tab_default", "tab_digits") # --- Add more categories...


    if (is.null(category)) {
        result <- list()
        for (cat in valid_categories) {
            result[[cat]] <- getOption(cat, default = list())
        }
        return(result)
    }

    if (!category %in% valid_categories && !category %in% unlist(lapply(valid_categories, function(cat) {
        names(getOption(cat, default = list()))
    }))) {
        stop("Invalid category or option name: ", category)
    }

    is_option <- FALSE
    option_category <- NULL

    for (cat in valid_categories) {
        cat_options <- names(getOption(cat, default = list()))
        if (category %in% cat_options) {
            is_option <- TRUE
            option_category <- cat
            break
        }
    }

    if (is_option) {
        actual_option <- category
        actual_category <- option_category
        actual_value <- option

        cat_opts <- getOption(actual_category, default = list())

        if (is.null(actual_value)) {
            return(cat_opts[[actual_option]])
        }

        cat_opts[[actual_option]] <- actual_value
        options(structure(list(cat_opts), names = actual_category))
        return(invisible(cat_opts))
    }

    if (is.null(option)) {
        return(getOption(category, default = list()))
    }

    cat_opts <- getOption(category, default = list())

    if (!option %in% names(cat_opts)) {
        stop("Option '", option, "' not found in category '", category, "'")
    }

    if (is.null(value)) {
        return(cat_opts[[option]])
    }

    cat_opts[[option]] <- value
    options(structure(list(cat_opts), names = category))
    return(invisible(cat_opts))
}
