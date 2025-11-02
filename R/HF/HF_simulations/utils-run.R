

run_estimation <- function(nb_years, gamma, sigma2 = 0, deterministic = TRUE, nb_series = 50) {

    ## Define parameters -------------------------------------------------------

    alpha <- 1 # paper value
    sw <- 100 # function needed input (param /100)

    #yearly
    beta <- 1 # paper value
    sy <- 33 # function needed input (param /33)

    # irregular
    si <- gamma * 150 # function needed input (param/150)
    sig2 <- sigma2 * 20 # function needed input (param/20)


    ## Simulation des séries ------------------------------------------------------

    if (deterministic) {
        series_sim <- create_n_datasets(
            N = nb_years,
            allComponents = TRUE,
            multiplicative = FALSE,
            deterministic = TRUE,
            sizeAnnualSeas = 100,
            sizeWeeklySeas = sw,
            sizeIrregularity = si,
            nb_series = nb_series
        )
    } else {
        series_sim <- create_n_datasets(
            N = nb_years,
            allComponents = TRUE,
            multiplicative = FALSE,
            deterministic = FALSE,
            sizeAnnualSeas = 100,
            sizeWeeklySeas = sw,
            sizeIrregularity = si,
            shockWeeklySeas = sig2,
            nb_series = nb_series
        )
    }

    raw_series <- get_all_raw(series_sim)

    ## Estimation ------------------------------------------------------------------

    output_X11 <- future_lapply(
        X = raw_series,
        FUN = my_x11,
        args1 = list(
            period = 7, # DOW pattern
            mul = FALSE,
            trend.horizon = 9, # 1/2 Filter length : not too long vs p
            trend.degree = 3, # Polynomial degree
            trend.kernel = "Henderson", # Kernel function
            trend.asymmetric = "CutAndNormalize", # Truncation method
            seas.s0 = "S3X9",
            seas.s1 = "S3X9", # Seasonal filters
            extreme.lsig = 1.5,
            extreme.usig = 2.5),
        args2 = list(
            period = 365.2425, # DOY pattern (try to round and see)
            mul = FALSE,
            trend.horizon = 250,
            trend.degree = 3,
            trend.kernel = "Henderson",
            trend.asymmetric = "CutAndNormalize",
            seas.s0 = "S3X1",
            seas.s1 = "S3X1",
            extreme.lsig = 1.5,
            extreme.usig = 2.5
        )
    )

    output_STL <- future_lapply(
        X = raw_series,
        FUN = my_stl,
        args1 = list(
            period = 7,
            multiplicative = FALSE,
            swindow = 3,
            twindow = 9, # 3= half, t= env p ?
            ninnerloop = 2,
            nouterloop = 0,
            # nojump = FALSE, # not used any more?
            weight.threshold = 0.001,
            weight.function = "BIWEIGHT"
        ),
        args2 = list(
            period = 365.2425,
            multiplicative = FALSE,
            swindow = 11,
            twindow = 169,
            ninnerloop = 2,
            nouterloop = 0,
            # nojump = FALSE,
            weight.threshold = 0.001,
            weight.function = "BIWEIGHT"
        )
    )

    output_AMB <- future_lapply(
        X = raw_series,
        FUN = my_amb,
        args1 = list(
            period = 7, # DOW pattern
            sn = FALSE, # Signal (SA)-noise decomposition
            stde = FALSE, # Calculate standard deviations
            nbcasts = 0,
            nfcasts = 0
        ),
        args2 = list(
            period = 365.2425, # DOY pattern
            sn = FALSE,
            stde = FALSE,
            nbcasts = 0,
            nfcasts = 0
        )
    )

    output_MAMB <- future_lapply(
        X = raw_series,
        FUN = my_Mamb,
        args1 = list(
            periods = c(7, 365.2425),
            # sn = FALSE,
            # stde = FALSE,
            nbcasts = 0,
            nfcasts = 0
        )
    )

    output_TBATS <- future_lapply(
        X = raw_series,
        FUN = my_TBATS
    )

    output_PROPHET <- future_lapply(
        X = raw_series,
        FUN = my_prophet,
        future.seed = TRUE
    )

    output_MSTL <- future_lapply(
        X = raw_series,
        FUN = my_mstl
    )

    return(list(
        gamma = gamma,
        sigma2 = sigma2,
        nb_years = nb_years,
        simulated = series_sim,
        deterministic = deterministic,
        estimated = list(
            X11 = output_X11,
            STL = output_STL,
            AMB = output_AMB,
            MAMB = output_MAMB,
            TBATS = output_TBATS,
            PROPHET = output_PROPHET,
            MSTL = output_MSTL
        )
    ))
}

compute_summary <- function(outputs) {

    ## Calcul des RMSE -------------------------------------------------------------

    RMSE_x11 <- future_lapply(
        X = seq_along(outputs$simulated),
        FUN = \(k) compute_RMSE(real_decompo = outputs$simulated[[k]][, -1], estimated_decompo = outputs$estimated$X11[[k]]$series)
    ) |> do.call(what = rbind) |> apply(MARGIN = 2, FUN = mean, na.rm = TRUE)
    RMSE_stl <- future_lapply(
        X = seq_along(outputs$simulated),
        FUN = \(k) compute_RMSE(real_decompo = outputs$simulated[[k]][, -1], estimated_decompo = outputs$estimated$STL[[k]]$series)
    ) |> do.call(what = rbind) |> apply(MARGIN = 2, FUN = mean)
    RMSE_amb <- future_lapply(
        X = seq_along(outputs$simulated),
        FUN = \(k) compute_RMSE(real_decompo = outputs$simulated[[k]][, -1], estimated_decompo = outputs$estimated$AMB[[k]]$series)
    ) |> do.call(what = rbind) |> apply(MARGIN = 2, FUN = mean)
    RMSE_Mamb <- future_lapply(
        X = seq_along(outputs$simulated),
        FUN = \(k) compute_RMSE(real_decompo = outputs$simulated[[k]][, -1], estimated_decompo = outputs$estimated$MAMB[[k]]$series)
    ) |> do.call(what = rbind) |> apply(MARGIN = 2, FUN = mean)
    RMSE_TBATS <- future_lapply(
        X = seq_along(outputs$simulated),
        FUN = \(k) compute_RMSE(real_decompo = outputs$simulated[[k]][, -1], estimated_decompo = outputs$estimated$TBATS[[k]]$series)
    ) |> do.call(what = rbind) |> apply(MARGIN = 2, FUN = mean, na.rm = TRUE)
    RMSE_prophet <- future_lapply(
        X = seq_along(outputs$simulated),
        FUN = \(k) compute_RMSE(real_decompo = outputs$simulated[[k]][, -1], estimated_decompo = outputs$estimated$PROPHET[[k]]$series)
    ) |> do.call(what = rbind) |> apply(MARGIN = 2, FUN = mean)
    RMSE_mstl <- future_lapply(
        X = seq_along(outputs$simulated),
        FUN = \(k) compute_RMSE(real_decompo = outputs$simulated[[k]][, -1], estimated_decompo = outputs$estimated$MSTL[[k]]$series)
    ) |> do.call(what = rbind) |> apply(MARGIN = 2, FUN = mean)

    computing_time <- c(
        X11 = outputs$estimated$X11 |> future_lapply("[[", "time") |> as.numeric() |> mean(na.rm = TRUE),
        STL = outputs$estimated$STL |> future_lapply("[[", "time") |> as.numeric() |> mean(),
        AMB = outputs$estimated$AMB |> future_lapply("[[", "time") |> as.numeric() |> mean(),
        MAMB = outputs$estimated$MAMB |> future_lapply("[[", "time") |> as.numeric() |> mean(),
        TBATS = outputs$estimated$TBATS |> future_lapply("[[", "time") |> as.numeric() |> mean(na.rm = TRUE),
        PROPHET = outputs$estimated$PROPHET |> future_lapply("[[", "time") |> as.numeric() |>mean(),
        MSTL = outputs$estimated$MSTL |> future_lapply("[[", "time") |> as.numeric() |> mean()
    )

    summary_table <- rbind(
        X11 = RMSE_x11,
        STL = RMSE_stl,
        AMB = RMSE_amb,
        MAMB = RMSE_Mamb,
        TBATS = RMSE_TBATS,
        PROPHET = RMSE_prophet,
        MSTL = RMSE_mstl
    ) |> as.data.frame() |> tibble::rownames_to_column(var = "method") |>
        dplyr::rename(
            trend_RMSE = t,
            weekly_RMSE = s7,
            yearly_RMSE = s365,
            remainder_RMSE = i,
            sa_RMSE = sa
        ) |>
        dplyr::mutate(
            gamma = outputs$gamma,
            sigma = outputs$sigma2,
            type = c("Stochastic GDP", "Deterministic DGP")[outputs$nb_years + 1L]
            .before = method
        ) |>
        dplyr::mutate(
            time = computing_time,
            nb_years = outputs$nb_years
        )

    return(summary_table)
}
