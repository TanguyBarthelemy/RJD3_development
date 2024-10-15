fractionalAirlineDecomposition_new <- function(y, period, sn = FALSE, stde = FALSE, nbcasts = 0, nfcasts = 0) {
    checkmate::assertNumeric(y, null.ok = FALSE)
    checkmate::assertNumeric(period, len = 1, null.ok = FALSE)
    checkmate::assertLogical(sn, len = 1, null.ok = FALSE)
    jrslt <- .jcall(
        "demetra/highfreq/r/FractionalAirlineProcessor",
        "Ljdplus/highfreq/LightExtendedAirlineDecomposition;",
        "decompose", as.numeric(y), period, sn, stde, as.integer(nbcasts),
        as.integer(nfcasts)
    )
    return(jd2r_fractionalAirlineDecomposition_new(jrslt, sn, stde, period))
}

jd2r_fractionalAirlineDecomposition_new <- function(jrslt, sn = FALSE, stde = FALSE, period) {
    ncmps <- rjd3toolkit::.proc_int(jrslt, "ucarima.size")
    model <- rjd3highfreq:::arima_extract(jrslt, "ucarima_model")
    cmps <- lapply(1:ncmps, function(cmp) {
        return(rjd3highfreq:::ucm_extract(jrslt, cmp))
    })
    ucarima <- rjd3toolkit::ucarima_model(model, cmps)
    yc <- rjd3toolkit::.proc_vector(jrslt, "y")
    sa <- rjd3toolkit::.proc_vector(jrslt, "sa")
    s <- rjd3toolkit::.proc_vector(jrslt, "s")
    if (sn) {
        if (stde) {
            decomposition <- list(
                y = yc,
                sa = sa,
                s = s,
                s.stde = rjd3toolkit::.proc_vector(jrslt, "s_stde")
            )
        } else {
            decomposition <- list(y = yc, sa = sa, s = s)
        }
    } else {
        t <- rjd3toolkit::.proc_vector(jrslt, "t")
        i <- rjd3toolkit::.proc_vector(jrslt, "i")
        if (stde) {
            decomposition <- list(
                y = yc,
                t = t,
                sa = sa,
                s = s,
                i = i,
                t.stde = rjd3toolkit::.proc_vector(jrslt, "t_stde"),
                s.stde = rjd3toolkit::.proc_vector(jrslt, "s_stde"),
                i.stde = rjd3toolkit::.proc_vector(jrslt, "i_stde")
            )
        } else {
            decomposition <- list(y = yc, t = t, sa = sa, s = s, i = i)
        }
    }
    estimation <- list(
        parameters = rjd3toolkit::.proc_vector(jrslt, "parameters"),
        score = rjd3toolkit::.proc_vector(jrslt, "score"),
        covariance = rjd3toolkit::.proc_matrix(jrslt, "pcov"),
        periods = period
    )

    likelihood <- rjd3toolkit::.proc_likelihood(jrslt, "likelihood.")

    return(structure(
        list(
            ucarima = ucarima,
            decomposition = decomposition,
            estimation = estimation,
            likelihood = likelihood
        ),
        class = "JDFractionalAirlineDecomposition"
    ))
}

multiAirlineDecomposition_new <- function(y, periods, ndiff = 2, ar = FALSE, stde = FALSE, nbcasts = 0,
                                          nfcasts = 0) {
    if (length(periods) == 1) {
        return(fractionalAirlineDecomposition(y, periods,
            stde = stde,
            nbcasts = nbcasts, nfcasts = nfcasts
        ))
    }
    checkmate::assertNumeric(y, null.ok = FALSE)
    jrslt <- .jcall(
        "demetra/highfreq/r/FractionalAirlineProcessor",
        "Ljdplus/highfreq/LightExtendedAirlineDecomposition;",
        "decompose", as.numeric(y), .jarray(periods), as.integer(ndiff),
        ar, stde, as.integer(nbcasts), as.integer(nfcasts)
    )
    if (length(periods) == 1) {
        return(jd2r_fractionalAirlineDecomposition_new(
            jrslt, FALSE,
            stde, periods
        ))
    } else {
        return(jd2r_multiAirlineDecomposition_new(jrslt, stde, periods))
    }
}

jd2r_multiAirlineDecomposition_new <- function(jrslt, stde = FALSE, periods) {
    ncmps <- rjd3toolkit::.proc_int(jrslt, "ucarima.size")
    model <- rjd3highfreq:::arima_extract(jrslt, "ucarima_model")
    cmps <- lapply(1:ncmps, function(cmp) {
        return(rjd3highfreq:::ucm_extract(jrslt, cmp))
    })
    ucarima <- rjd3toolkit::ucarima_model(model, cmps)
    yc <- rjd3toolkit::.proc_vector(jrslt, "y")
    estimation <- list(
        parameters = rjd3toolkit::.proc_vector(jrslt, "parameters"),
        score = rjd3toolkit::.proc_vector(jrslt, "score"),
        covariance = rjd3toolkit::.proc_matrix(jrslt, "pcov"),
        periods = periods
    )
    likelihood <- rjd3toolkit::.proc_likelihood(jrslt, "likelihood.")
    ncmps <- rjd3toolkit::.proc_int(jrslt, "ncmps")
    if (stde) {
        decomposition <- lapply((1:ncmps), function(j) {
            return(cbind(
                rjd3toolkit::.proc_vector(jrslt, paste0("cmp(", j, ")")),
                rjd3toolkit::.proc_vector(jrslt, paste0("cmp_stde(", j, ")"))
            ))
        })
    } else {
        decomposition <- lapply((1:ncmps), function(j) {
            return(rjd3toolkit::.proc_vector(jrslt, paste0("cmp(", j, ")")))
        })
    }
    return(structure(
        list(
            ucarima = ucarima,
            decomposition = decomposition,
            estimation = estimation,
            likelihood = likelihood
        ),
        class = "JDFractionalAirlineDecomposition"
    ))
}

print_JDFractionalAirlineDecomposition <- function(x, digits = max(3L, getOption("digits") - 3L) # ,
                                                   # starting = as.Date("1968-01-01")
) {
    # Estimated MA parameters (coefs, se, student)
    nb_freq <- (x$estimation$parameters |> length()) - 1L
    est_ma_params <- data.frame(
        MA_parameter = c(
            "Theta(1)",
            paste0("Theta(", paste0(
                "period = ",
                x$estimation$periods
            ), ")")
        ),
        Coef = x$estimation$parameters,
        Coef_SE = sqrt(diag(x$estimation$covariance)),
        check.names = FALSE
    )
    est_ma_params$Tstat <- est_ma_params$Coef / est_ma_params$Coef_SE

    cat("\n")
    cat("Estimate MA parameters:")
    cat("\n")
    print(est_ma_params, row.names = FALSE)

    cat("\n")
    cat("Decomposition:")
    cat("\n")
    decompo_table <- do.call(cbind, x$decomposition)
    # intervalle <- seq(starting, starting + decompo_table |> nrow() - 1, by = "day")
    # print(zoo::zoo(decompo_table, intervalle) |> tail(n = 10))
    print(decompo_table |> tail(n = 10))
    cat("\n")

    cat("Number of observations:", formatC(x$likelihood$nobs, digits = digits))
    cat("\n")

    cat("Sum of square residuals:", formatC(x$likelihood$ssq, digits = digits),
        "on", x$likelihood$df, "degrees of freedom",
        sep = " "
    )
    cat("\n")

    cat("Log likelihood = ", formatC(x$likelihood$ll, digits = digits),
        ", \n\taic = ", formatC(x$likelihood$aic, digits = digits),
        ", \n\taicc = ", formatC(x$likelihood$aicc, digits = digits),
        ", \n\tbic(corrected for length) = ",
        formatC(x$likelihood$bicc, digits = digits),
        sep = ""
    )
    cat("\n")

    cat("Hannan–Quinn information criterion = ",
        formatC(x$likelihood$hannanquinn, digits = digits),
        sep = ""
    )

    cat("\n\n")
    return(invisible(x))
}
