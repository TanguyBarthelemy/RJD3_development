print_JD3_X11_SPEC <- function(
    x,
    enable_print_style = getOption("enable_print_style")
) {
    if (enable_print_style) {
        style_pre_code <- "\033[4m\033[1m"
        style_post_code <- "\033[22m\033[24m"
    } else {
        style_pre_code <- style_post_code <- ""
    }

    cat(style_pre_code, "Specification X11", style_post_code, "\n", sep = "")

    cat("Seasonal component: ", ifelse(x$seasonal, "Yes", "No"), "\n", sep = "")
    cat("Length of the Henderson filter: ", x$henderson, "\n", sep = "")
    cat("Seasonal filter: ", x$sfilters, "\n", sep = "")
    cat(
        "Boundaries used for extreme values correction :",
        "\n\t lower_sigma: ",
        x$lsig,
        "\n\t upper_sigma: ",
        x$usig
    )
    cat("\n")
    cat("Nb of forecasts: ", x$nfcasts, "\n", sep = "")
    cat("Nb of backcasts: ", x$nbcasts, "\n", sep = "")
    cat("Calendar sigma: ", x$sigma, "\n", sep = "")

    return(invisible(x))
}
