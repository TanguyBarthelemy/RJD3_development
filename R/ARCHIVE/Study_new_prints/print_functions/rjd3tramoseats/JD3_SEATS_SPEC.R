print_JD3_SEATS_SPEC <- function(x, enable_print_style = getOption("enable_print_style")) {
    if (enable_print_style) {
        style_pre_code <- "\033[4m\033[1m"
        style_post_code <- "\033[22m\033[24m"
    } else {
        style_pre_code <- style_post_code <- ""
    }

    cat(style_pre_code, "Specification SEATS", style_post_code, "\n", sep = "")


    cat("Approximation mode: ", x$approximation, "\n", sep = "")
    cat("MA unit root boundary: ", x$xl, "\n", sep = "")
    cat("Trend boundary: ", x$rmod, "\n", sep = "")
    cat("Seasonal tolerance: ", x$epsphi, "\n", sep = "")
    cat("Seasonal boundary: ", x$sbound, "\n", sep = "")
    cat("Method: ", x$algorithm, "\n", sep = "")

    return(invisible(x))
}
