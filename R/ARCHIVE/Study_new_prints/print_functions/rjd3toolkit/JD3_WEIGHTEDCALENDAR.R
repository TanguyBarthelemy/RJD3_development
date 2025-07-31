print_JD3_WEIGHTEDCALENDAR <- function(
    x,
    enable_print_style = getOption("enable_print_style"),
    ...
) {
    if (enable_print_style) {
        style_pre_code <- "\033[4m\033[1m"
        style_post_code <- "\033[22m\033[24m"
    } else {
        style_pre_code <- style_post_code <- ""
    }

    for (index_cal in seq_along(x$weights)) {
        cat(
            style_pre_code,
            "Calendar n°",
            index_cal,
            style_post_code,
            "\n",
            sep = ""
        )
        cat("weight: ", x$weight[index_cal], "\n", sep = "")
        print(x$calendars[[index_cal]])
        cat("\n")
    }

    return(invisible(x))
}
