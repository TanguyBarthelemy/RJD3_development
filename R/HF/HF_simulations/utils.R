create_n_datasets <- function(..., nb_series) {
    return(lapply(seq_len(nb_series), \(x) {
        tsbox::ts_ts(tssim::sim_daily_mstl(...))
    }))
}

apply_x11 <- function(datasets, ...) {
    x11.dow <- apply(FUN = \(d) rjd3x11plus::x11plus(y = d[, "original"], ...))
}


# X-11 decomposition  -----------------------------------------------------

# storage

# results (element = ts or vector...)
list_X11_sa <- as.list(rep(NA, nb_series))
list_X11_s7 <- as.list(rep(NA, nb_series))
list_X11_s365 <- as.list(rep(NA, nb_series))
list_X11_t <- as.list(rep(NA, nb_series))
list_X11_i <- as.list(rep(NA, nb_series))

# RMSE for each series (element = number = RMSE)
list_RMSE_X11_sa <- as.list(rep(NA, nb_series))
list_RMSE_X11_s7 <- as.list(rep(NA, nb_series))
list_RMSE_X11_s365 <- as.list(rep(NA, nb_series))
list_RMSE_X11_t <- as.list(rep(NA, nb_series))
list_RMSE_X11_i <- as.list(rep(NA, nb_series))

# computing time for each series
list_time_X11 <- as.list(rep(NA, nb_series))

for (j in 1:nb_series) {
    res <- try({
        start <- Sys.time()
        x11.dow <- rjd3x11plus::x11plus(
            y = list_raw[[j]],
            period = 7, # DOW pattern
            mul = FALSE,
            trend.horizon = 9, # 1/2 Filter length : not too long vs p
            trend.degree = 3, # Polynomial degree
            trend.kernel = "Henderson", # Kernel function
            trend.asymmetric = "CutAndNormalize", # Truncation method
            seas.s0 = "S3X9",
            seas.s1 = "S3X9", # Seasonal filters
            extreme.lsig = 1.5,
            extreme.usig = 2.5
        ) # Sigma-limits

        # Extract DOY pattern from DOW-adjusted data : run on SA from dow step
        x11.doy <- rjd3x11plus::x11plus(
            y = x11.dow$decomposition$sa,
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
        end <- Sys.time()
    })

    if (!inherits(res, "try-error")) {
        list_time_X11[[j]] <- round(
            as.numeric(difftime(end, start, units = "secs")),
            3
        )

        #store s7
        list_X11_s7[[j]] <- x11.dow$decomposition$s
        # RMSE for X11 (vs True) S7
        list_RMSE_X11_s7[[j]] <- sqrt(mean(
            (list_X11_s7[[j]] - list_true_s7[[j]])^2
        ))

        #store s365
        list_X11_s365[[j]] <- x11.doy$decomposition$s
        # RMSE for X11 (vs True) S365
        list_RMSE_X11_s365[[j]] <- sqrt(mean(
            (list_X11_s365[[j]] - list_true_s365[[j]])^2
        ))

        #store t
        list_X11_t[[j]] <- x11.doy$decomposition$t
        # RMSE for X11 (vs True) T
        list_RMSE_X11_t[[j]] <- sqrt(mean(
            (list_X11_t[[j]] - list_true_t[[j]])^2
        ))

        #store i
        list_X11_i[[j]] <- x11.doy$decomposition$i
        # RMSE for X11 (vs True) I
        list_RMSE_X11_i[[j]] <- sqrt(mean(
            (list_X11_i[[j]] - list_true_i[[j]])^2
        ))

        #store sa
        list_X11_sa[[j]] <- x11.doy$decomposition$sa
        # RMSE for X11 (vs True) SA
        list_RMSE_X11_sa[[j]] <- sqrt(mean(
            (list_X11_sa[[j]] - list_true_sa[[j]])^2
        ))
    }
}
list_RMSE_X11_s7
list_RMSE_X11_s365
list_RMSE_X11_t
list_RMSE_X11_i
list_RMSE_X11_sa
list_time_X11
