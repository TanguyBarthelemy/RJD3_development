
print_JD3_REGARIMA_SPEC <- function(x, enable_print_style = getOption("enable_print_style")) {

    if (enable_print_style) {
        style_pre_code <- "\033[4m\033[1m"
        style_post_code <- "\033[22m\033[24m"
    } else {
        style_pre_code <- style_post_code <- ""
    }

    cat(style_pre_code, "Specification", style_post_code, "\n", sep = "")


    cat("\n", style_pre_code, "Series", style_post_code, "\n", sep = "")

    cat("Series span:")
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
    } else if (x$regression$td$w > 0) {
        cat("No calendar regressor", "\n", sep = "")
    } else if (x$regression$td$td == "TD_NONE") {
        cat("No calendar regressor", "\n", sep = "")
    } else {
        if (x$regression$td$td == "TD7") {
            cat("Calendar regressor: TradingDays\n", sep = "")
        } else if (x$regression$td$td == "TD2") {
            cat("Calendar regressor: WorkingDays\n", sep = "")
        } else if (x$regression$td$td %in% c("TD3", "TD3C", "TD4")) {
            cat("Calendar regressor: ", x$regression$td$td, "\n", sep = "")
        } else {
            message("Trading days regressor unknown.")
        }
        cat("with Leap Year: ",
            ifelse(x$regression$td$lp == "LEAPYEAR", "Yes", "No"), "\n", sep = "")
        cat("AutoAdjust: ", x$regression$td$autoadjust, "\n", sep = "")
        cat("Test: ", x$regression$td$test, "\n", sep = "")
    }

    cat("\n")

    cat("Easter: ")
    if (x$regression$easter$type == "UNUSED") {
        cat("No\n")
    } else {
        cat(x$regression$easter$type, "\n")
        cat("Duration:", x$regression$easter$duration, ifelse(x$regression$easter$duration == 8, "(Auto)", ""), "\n")
        cat("Test:", x$regression$easter$test, ifelse(x$regression$easter$test == "ADD", "(Auto)", ""), "\n")

        if (!is.null(x$regression$easter$coef)) {
            cat("Coef:\n")
            cat("\t- Type:", x$regression$easter$coefficient$type,
                ifelse(x$regression$easter$coefficient$type == "FIXED", "(Auto)", ""), "\n")
            cat("\t- Value:", x$regression$easter$coefficient$value, "\n")
        }
    }

    cat("\n")

    cat("Pre-specified outliers: ", length(x$regression$outliers), "\n", sep = "")
    if (!is.null(x$regression$outliers) && length(x$regression$outliers) > 0) {
        for (out in x$regression$outliers) {
            cat("\t- ", out$name,
                ifelse(is.null(out$coef), "", paste0(", coefficient: ", out$coef$value, " (", out$coef$type, ")")),
                "\n", sep = "")
        }
    }
    cat("Ramps: ")
    if (!is.null(x$regression$ramps) && length(x$regression$ramps) > 0) {
        cat("\n")
        for (ramp in x$regression$ramps) {
            cat("\t- start: ", ramp$start, ", end : ", ramp$end,
                ifelse(is.null(ramp$coef), "", paste0(", coefficient: ", ramp$coef, " (", ramp$coef$type, ")")), sep = "")
            cat("\n")
        }
    } else {
        cat("No\n")
    }

    if (!is.null(x$regression$users) && length(x$regression$users) > 0) {
        cat("User-defined variables:\n")
        for (uv in x$regression$users) {
            cat("\t-", uv$name,
                ifelse(is.null(uv$coef), "", paste0(", coefficient: ", uv$coef)),
                ", component: ", uv$regeffect, "\n", sep = "")
        }
    }

    cat("\n", style_pre_code, "Outliers", style_post_code, "\n", sep = "")

    if (is.null(x$outlier$outliers) || length(x$outlier$outliers) == 0) {
        cat("Is enabled: No\n")
    } else {
        cat("Detection span: ")
        print_JD3_SPAN(x$outlier$span)

        cat("Outliers type: \n")
        for (out in x$outlier$outliers) {
            cat("\t- ", out$type, ", critical value : ", out$va, ifelse(out$va == 0, " (Auto)", ""), "\n", sep = "")
        }

        cat("TC rate: ", x$outlier$monthlytcrate, ifelse(x$outlier$monthlytcrate == 0.7, " (Auto)", ""), "\n", sep = "")
        cat("Method: ", x$outlier$method, ifelse(x$outlier$method == "ADDONE", " (Auto)", ""), "\n", sep = "")
    }


    cat("\n", style_pre_code, "ARIMA", style_post_code, "\n", sep = "")

    print(x$arima)

    return(invisible(x))
}
