
print_JD3_REGARIMA_SPEC <- function(x, enable_print_style = getOption("enable_print_style")) {
    
    if (enable_print_style) {
        style_pre_code <- "\033[4m\033[1m"
        style_post_code <- "\033[22m\033[24m"
    } else {
        style_pre_code <- style_post_code <- ""
    }
    
    cat(style_pre_code, "Specification", style_post_code, "\n", sep = "")
    
    
    cat("\n", style_pre_code, "Series", style_post_code, "\n", sep = "")
    
    cat("Serie span: ")
    print_JD3_SPAN(x$basic$span)
    
    cat("Preliminary Check: ", ifelse(x$basic$preliminaryCheck, "Yes", "No"), "\n", sep = "")
    
    
    cat("\n", style_pre_code, "Estimate", style_post_code, "\n", sep = "")
    
    cat("Model span: ")
    print_JD3_SPAN(x$estimate$span)
    cat("\n")
    cat("Tolerance: ", x$estimate$tol, "\n", sep = "")
    
    
    cat("\n", style_pre_code, "Transformation", style_post_code, "\n", sep = "")
    
    cat("Function: ", x$transform$fn, "\n", sep = "")
    cat("AIC difference: ", x$transform$aicdiff, "\n", sep = "")
    cat("Adjust: ", x$transform$adjust, "\n", sep = "")
    
    
    cat("\n", style_pre_code, "Regression", style_post_code, "\n", sep = "")
    
    if (!is.null(x$regression$td$users) && length(x$regression$td$users) > 0) {
        cat("Calendar regressor: user-defined calendar", "\n", sep = "")
        cat("Test: ", x$regression$td$test, "\n", sep = "")
    } else if (x$regression$td$td == "TD_NONE") {
        cat("No calendar regressor", "\n", sep = "")
    } else {
        cat("Calendar regressor: ", x$regression$td$td, "\n", sep = "")
        cat("with Leap Year: ", ifelse(x$regression$td$lp == "LEAPYEAR", "Yes", "No"), "\n", sep = "")
        cat("AutoAdjust: ", x$regression$td$autoadjust, "\n", sep = "")
        cat("Test: ", x$regression$td$test, "\n", sep = "")
    }
    cat("\n")
    
    cat("Easter: ", x$regression$easter$type, "\n", sep = "")
    cat("\n")
    
    cat("Pre-specified outliers: ", length(x$regression$outliers), "\n", sep = "")
    if (!is.null(x$regression$outliers) && length(x$regression$outliers) > 0) {
        for (out in x$regression$outliers) {
            cat("\t-", out$name, "\n")
        }
    }
    cat("Ramps: ", ifelse(!is.null(x$regression$ramps) && length(x$regression$ramps) > 0, "Yes", "No"), "\n", sep = "")
    cat("User-defined variables: ", ifelse(!is.null(x$regression$users) && length(x$regression$users) > 0, "Yes", "No"), "\n", sep = "")
    
    
    cat("\n", style_pre_code, "Outliers", style_post_code, "\n", sep = "")
    
    if (is.null(x$outlier$outliers) || length(x$outlier$outliers) == 0) {
        cat("Is enabled: No\n")
    } else {
        cat("Detection span: ")
        print_JD3_SPAN(x$outlier$span)
        
        cat("Outliers type: ", paste(sapply(x$outlier$outliers, base::`[[`, "type"), collapse = ", "), "\n", sep = "")
        cat("TC rate: ", x$outlier$monthlytcrate, "\n", sep = "")
        cat("Method: ", x$outlier$method, "\n", sep = "")
    }
    
    
    cat("\n", style_pre_code, "ARIMA", style_post_code, "\n", sep = "")
    
    print(x$arima)
    
    cat("\n")
    return(invisible(x))
}
