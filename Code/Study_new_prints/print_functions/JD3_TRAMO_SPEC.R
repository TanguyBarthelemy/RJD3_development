
print_JD3_TRAMO_SPEC <- function(x, enable_print_style = getOption("enable_print_style")) {
    
    if (enable_print_style) {
        style_pre_code <- "\033[4m\033[1m"
        style_post_code <- "\033[22m\033[24m"
    } else {
        style_pre_code <- style_post_code <- ""
    }
    
    cat(style_pre_code, "Specification", style_post_code, "\n", sep = "")
    
    
    cat("\n", style_pre_code, "Series", style_post_code, "\n", sep = "")
    
    cat("Serie span: ", x$basic$span$type, "\n", sep = "")
    cat("Preliminary Check: ", ifelse(x$basic$preliminaryCheck, "Yes", "No"), "\n", sep = "")
    
    
    cat("\n", style_pre_code, "Estimate", style_post_code, "\n", sep = "")
    
    cat("Model span: ", x$estimate$span$type, "\n", sep = "")
    cat("Tolerance: ", x$estimate$tol, "\n", sep = "")
    cat("Exact ML: ", ifelse(x$estimate$ml, "Yes", "No"), "\n", sep = "")
    cat("Unit root limit: ", x$estimate$ubp, "\n", sep = "")
    
    
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
        cat("Detection span: ", x$outlier$span$type, sep = "")
        if (toupper(x$outlier$span$type) %in% c("FROM", "BETWEEN")) {
            cat(" from", x$outlier$span$d0)
        }
        if (toupper(x$outlier$span$type) %in% c("TO", "BETWEEN")) {
            cat(" to", x$outlier$span$d1)
        }
        if (x$outlier$span == "All") {
            cat("Detection span: All\n")
        }
        cat("\n")
        
        list_outliers <- c("ao", "ls", "tc", "so")
        detected_outliers <- c("ao", "ls", "tc", "so")[do.call(
            args = x$outlier[c("ao", "ls", "tc", "so")], 
            what = c)]
        
        if (length(detected_outliers) > 0) {
            cat("Outliers type: ", paste(detected_outliers, collapse = ", "), "\n", sep = "")
        }
        
        cat("Critical value: ", ifelse(x$outlier$va == 0, "0 (Auto)", x$outlier$va), "\n", sep = "")
        cat("TC rate: ", ifelse(x$outlier$tcrate == 0.7, "0,7 (Auto)", x$outlier$tcrate), "\n", sep = "")
        cat("EML estimation: ", ifelse(x$outlier$ml, "Yes", "No"), "\n", sep = "")
    }
    
    
    cat("\n", style_pre_code, "ARIMA", style_post_code, "\n", sep = "")
    
    print(x$arima)
    
    cat("\n")
    return(invisible(x))
}
