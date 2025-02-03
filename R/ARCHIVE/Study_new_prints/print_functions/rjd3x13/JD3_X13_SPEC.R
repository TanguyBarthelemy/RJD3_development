print_JD3_X13_SPEC <- function(x, enable_print_style = getOption("enable_print_style")) {
    if (enable_print_style) {
        style_pre_code <- "\033[4m\033[1m"
        style_post_code <- "\033[22m\033[24m"
    } else {
        style_pre_code <- style_post_code <- ""
    }

    print_JD3_REGARIMA_SPEC(x$regarima, enable_print_style = enable_print_style)

    cat("\n")

    print_JD3_X11_SPEC(x$x11, enable_print_style = enable_print_style)

    cat("\n", style_pre_code, "Benchmarking", style_post_code, "\n", sep = "")

    if (x$benchmarking$enabled) {
        cat("Enabled: Yes\n", sep = "")
        cat("Target: ", x$benchmarking$target, ifelse(x$benchmarking$target == "TARGET_CALENDARADJUSTED", " (Auto)", ""), "\n", sep = "")
        cat("Lambda: ", x$benchmarking$lambda, ifelse(x$benchmarking$lambda == 1, " (Auto)", ""), "\n", sep = "")
        cat("Rho: ", x$benchmarking$rho, ifelse(x$benchmarking$rho == 1, " (Auto)", ""), "\n", sep = "")
        cat("Bias: ", x$benchmarking$bias, ifelse(x$benchmarking$bias == "BIAS_NONE", " (Auto)", ""), "\n", sep = "")
        cat("Use forecast: ", ifelse(x$benchmarking$forecast, "Yes", "No (Auto)"), "\n", sep = "")
    } else {
        cat("Is enabled: No\n")
    }

    return(invisible(x))
}
