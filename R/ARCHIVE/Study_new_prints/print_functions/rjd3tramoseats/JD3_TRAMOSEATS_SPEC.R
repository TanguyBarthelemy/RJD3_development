print_JD3_TRAMOSEATS_SPEC <- function(x, enable_print_style = getOption("enable_print_style")) {
    if (enable_print_style) {
        style_pre_code <- "\033[4m\033[1m"
        style_post_code <- "\033[22m\033[24m"
    } else {
        style_pre_code <- style_post_code <- ""
    }

    print_JD3_TRAMO_SPEC(x$tramo, enable_print_style = enable_print_style)
    print_JD3_SEATS_SPEC(x$seats, enable_print_style = enable_print_style)

    cat("\n", style_pre_code, "Benchmarking", style_post_code, "\n", sep = "")

    if (!x$benchmarking$enabled) {
        cat("Is enabled: No\n")
    } else {
        cat("Enabled: Yes", sep = "")
        cat("Target: ", x$benchmarking$target, "\n", sep = "")
        cat("Lambda: ", x$benchmarking$lambda, "\n", sep = "")
        cat("Rho: ", x$benchmarking$rho, "\n", sep = "")
        cat("Use forecast: ", ifelse(x$benchmarking$forecast, "Yes", "No"), "\n", sep = "")
    }

    cat("\n")
    return(invisible(x))
}
