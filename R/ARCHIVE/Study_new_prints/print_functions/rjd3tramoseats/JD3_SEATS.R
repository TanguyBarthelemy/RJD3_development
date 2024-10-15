print_JD3_SEATS <- function(x, enable_print_style = getOption("enable_print_style")) {
    if (enable_print_style) {
        style_pre_code <- "\033[4m\033[1m"
        style_post_code <- "\033[22m\033[24m"
    } else {
        style_pre_code <- style_post_code <- ""
    }

    print(x$seatsmodel)
    print(x$canonicaldecomposition)

    tableau <- cbind(
        x$stochastics$series$data,
        x$stochastics$sa$data,
        x$stochastics$t$data,
        x$stochastics$sa$data,
        x$stochastics$i$data
    )
    colnames(tableau) <- c("Series", "Seasonally adjusted", "Trend", "Seasonal", "Irregular")

    cat("Last values\n")
    print(tail(.preformat.ts(tableau)))

    return(invisible(x))
}
