print_JDFractionalAirlineEstimation <- function(x, digits = max(3L, getOption("digits") - 3L), 
                                                starting = as.Date("1968-01-01")) {
    
    print_vect <- function(.x) {
        s <- .x
        n_min <- nchar(s) |> min()
        n_max <- nchar(s) |> max() + 1
        s <- s |> paste0(strrep(" ", n_max - n_min + 1)) |> substr(1, n_max)
        
        dimension <- 5
        if (length(s) %% 3 == 0) dimension <- 3
        if (n_max < 11 && length(s) %% 6 == 0) dimension <- 6
        if (n_max < 16 && length(s) %% 4 == 0) dimension <- 4
        if (n_max < 13 && length(s) %% 5 == 0) dimension <- 5
        
        out <- matrix(c(s, rep("", dimension - (((length(s) - 1) %% dimension) + 1))), 
                      ncol = dimension) |> 
            apply(MARGIN = 1, paste, collapse = "\t") |> 
            paste(collapse = "\n")
        
        cat(out)
        cat("\n")
        invisible(.x)
    }
    
    nb_outliers <- sum((x$model$variables |> 
                            substr(1L, 2L) |> 
                            toupper()) %in% c("AO", "WO", "LS"))
    nb_reg_cjo <- length(x$model$variables) - nb_outliers
    
    summary_coeff <-  data.frame(
        "Variable" = x$model$variables, 
        "Coef" = x$model$b, 
        "Coef_SE" = sqrt(diag(x$model$bcov)))
    summary_coeff$Tstat <- round(summary_coeff$Coef / summary_coeff$Coef_SE, digits)
    summary_coeff$Coef <- round(summary_coeff$Coef, digits)
    summary_coeff$Coef_SE <- round(summary_coeff$Coef_SE, digits)
    
    summary_coeff$Variable[1L:nb_reg_cjo] <- c(
        "14th_july", "8th_may", "1st_jan", "Xmas", "1st_may", "asc", 
        "east_mon", "pen_mon", "15th_aug", "1st_nov", "11th_nov")
    
    if (nb_outliers > 0) {
        
        outliers_coeff <- summary_coeff[(nb_reg_cjo + 1L):nrow(summary_coeff), ]
        
        date_vect <- seq.Date(from = starting, 
                              to = starting + x$likelihood$nobs, 
                              by = "days")
        outliers_coeff$Variable <- paste0(
            substr(outliers_coeff$Variable, 1L, 3L), 
            date_vect[substr(outliers_coeff$Variable, 4L, 10L) |> as.numeric()]
        )
        outliers <- outliers_coeff$Variable
        
    }
    
    if(nb_reg_cjo > 0) {
        reg_cjo_coeff <- summary_coeff[1:nb_reg_cjo, ]
        reg_cjo <- reg_cjo_coeff$Variable
    }
    
    # Estimated MA parameters (coefs, se, student)
    nb_freq <- (x$estimation$parameters |> length()) - 1L
    est_ma_params <- data.frame(
        MA_parameter = c("Theta(1)", 
                         paste0("Theta(", paste0("period n°", 
                                                 seq_len(nb_freq)), ")")),
        Coef = x$estimation$parameters, 
        Coef_SE = sqrt(diag(x$estimation$covariance)), 
        check.names = FALSE) 
    est_ma_params$Tstat <- est_ma_params$Coef / est_ma_params$Coef_SE
    
    cat("\n")
    cat("Estimate MA parameters:")
    cat("\n")
    print(est_ma_params, row.names = FALSE)
    
    cat("\n")
    cat("Number calendar regressors:", nb_reg_cjo, ", Number outliers :", nb_outliers)
    cat("\n\n")
    
    # if(nb_reg_cjo > 0) {
    #     cat("List of regressors:")
    #     cat("\n")
    #     print_vect(reg_cjo)
    #     cat("\n")
    # }
    # 
    # if(nb_outliers > 0) {
    #     cat("List of outliers:")
    #     cat("\n")
    #     print_vect(outliers)
    #     cat("\n")
    # }
    
    if(nb_reg_cjo > 0) {
        cat("TD regressors coefficients:")
        cat("\n")
        print(reg_cjo_coeff, row.names = FALSE)
        # print(head(reg_cjo_coeff, 10), row.names = FALSE)
        # if (nb_reg_cjo > 10) cat("...\n")
        cat("\n")
    }
    
    if(nb_outliers > 0) {
        cat("Outliers coefficients:")
        cat("\n")
        print(outliers_coeff, row.names = FALSE)
        # print(head(outliers_coeff, 10), row.names = FALSE)
        # if (nb_outliers > 10) cat("...\n")
        cat("\n")
    }
    
    cat("Number of observations:", formatC(x$likelihood$nobs, digits = digits))
    cat("\n")
    
    cat("Sum of square residuals:", formatC(x$likelihood$ssq, digits = digits), 
        "on", x$likelihood$df, "degrees of freedom", 
        sep = " ")
    cat("\n")
    
    cat("Log likelihood = ", formatC(x$likelihood$ll, digits = digits), 
        ", \n\taic = ", formatC(x$likelihood$aic, digits = digits), 
        ", \n\taicc = ", formatC(x$likelihood$aicc, digits = digits), 
        ", \n\tbic(corrected for length) = ", 
        formatC(x$likelihood$bicc, digits = digits), sep = "")
    cat("\n")
    
    cat("Hannan–Quinn information criterion = ", 
        formatC(x$likelihood$hannanquinn, digits = digits), sep = "")
    
    cat("\n\n")
    invisible(x)
}

print_JDX11 <- function(x, digits = max(3L, getOption("digits") - 3L), 
                        starting = as.Date("1968-01-01")) {
    
    
    if (x$parameters$multiplicative) cat("Multiplicative model")
    if (!x$parameters$multiplicative) cat("Additive model")
    cat("\n\n")
    
    cat("Trend:", 
        "\n\tFilter:", x$parameters$trend.horizon, 
        "\n\tPolynomial order:", x$parameters$trend.degree, 
        "\n\tType:", x$parameters$trend.kernel)
    
    cat("\n\n")
    cat("Decomposition:")
    cat("\n")
    decompo_table <- do.call(cbind, x$decomposition)
    intervalle <- seq(as.Date("1968-01-01"), as.Date("1968-01-01") + decompo_table |> nrow() - 1, by = "day")
    print(zoo::zoo(decompo_table, intervalle) |> tail(n = 10))
    cat("\n")
    
    cat("Boundary used for outlier correction in irregular = [", 
        formatC(x$parameters$extreme.lsig, digits = digits), "; ",  
        formatC(x$parameters$extreme.usig, digits = digits), "]", sep = "")
    
    cat("\n\n")
    invisible(x)
}
print_JDFractionalAirlineDecomposition <- function(x, digits = max(3L, getOption("digits") - 3L), 
                                                   starting = as.Date("1968-01-01")) {
    
    # Estimated MA parameters (coefs, se, student)
    nb_freq <- (x$estimation$parameters |> length()) - 1L
    est_ma_params <- data.frame(
        MA_parameter = c("Theta(1)", 
                         paste0("Theta(", paste0("period n°", 
                                                 seq_len(nb_freq)), ")")), 
        Coef = x$estimation$parameters, 
        Coef_SE = sqrt(diag(x$estimation$covariance)), 
        check.names = FALSE) 
    est_ma_params$Tstat <- est_ma_params$Coef / est_ma_params$Coef_SE
    
    cat("\n")
    cat("Estimate MA parameters:")
    cat("\n")
    print(est_ma_params, row.names = FALSE)
    
    cat("\n\n")
    cat("Decomposition:")
    cat("\n")
    decompo_table <- do.call(cbind, x$decomposition)
    intervalle <- seq(as.Date("1968-01-01"), as.Date("1968-01-01") + decompo_table |> nrow() - 1, by = "day")
    print(zoo::zoo(decompo_table, intervalle) |> tail(n = 10))
    cat("\n")
    
    cat("Number of observations:", formatC(x$likelihood$nobs, digits = digits))
    cat("\n")
    
    cat("Sum of square residuals:", formatC(x$likelihood$ssq, digits = digits), 
        "on", x$likelihood$df, "degrees of freedom", 
        sep = " ")
    cat("\n")
    
    cat("Log likelihood = ", formatC(x$likelihood$ll, digits = digits), 
        ", \n\taic = ", formatC(x$likelihood$aic, digits = digits), 
        ", \n\taicc = ", formatC(x$likelihood$aicc, digits = digits), 
        ", \n\tbic(corrected for length) = ", 
        formatC(x$likelihood$bicc, digits = digits), sep = "")
    cat("\n")
    
    cat("Hannan–Quinn information criterion = ", 
        formatC(x$likelihood$hannanquinn, digits = digits), sep = "")
    
    cat("\n\n")
    invisible(x)
}

print_JDFractionalAirlineEstimation(pre.mult)
print_JDFractionalAirlineEstimation(pre.mult_cal)

print_JDX11(x11.dow)
print_JDX11(x11.doy)

print_JDFractionalAirlineDecomposition(amb.dow)
print_JDFractionalAirlineDecomposition(amb.doy)
print_JDFractionalAirlineDecomposition(amb.multi)
