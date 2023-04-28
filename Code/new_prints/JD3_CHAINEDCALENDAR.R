print.JD3_CHAINEDCALENDAR_new <- function (x, 
                                            enable_print_style = getOption("enable_print_style"), 
                                            ...)
{
    if (enable_print_style) {
        style_pre_code <- "\033[4m\033[1m"
        style_post_code <- "\033[22m\033[24m"
    } else {
        style_pre_code <- style_post_code <- ""
    }
    
    cat(style_pre_code, "First calendar before ", x$break_date, style_post_code, "\n", sep = "")
    print(x$calendar1, enable_print_style = FALSE)
    
    cat("\n")
    
    cat(style_pre_code, "Second calendar after ", x$break_date, style_post_code, "\n", sep = "")
    print(x$calendar2, enable_print_style = FALSE)
    
    return(invisible(x))
}
