# remotes::install_url("https://github.com/annasmyk/RJD3_development/raw/refs/heads/main/R/HF/HF_simulations/tssim_0.2.15.tar.gz")

create_n_datasets <- function(..., nb_series) {
    return(future_lapply(seq_len(nb_series), \(x) {
        tssim::sim_daily_mstl(...) |>
            setNames(nm = c("y", "sa", "s7", "s365", "t", "i")) |>
            tsbox::ts_ts()
    }, future.seed = TRUE))
}

get_all_raw <- function(series) {
    return(lapply(series, FUN = "[", i = , j = "y"))
}

compute_RMSE <- function(real_decompo, estimated_decompo = NULL) {
    if (is.null(estimated_decompo)) {
        return(c(sa = NA, s7 = NA, s365 = NA, t = NA, i = NA))
    }
    RMSE <- ((real_decompo - estimated_decompo) ** 2) |>
        apply(MARGIN = 2L, FUN = mean) |>
        sqrt() |>
        setNames(nm = c("sa", "s7", "s365", "t", "i"))
    return(RMSE)
}

my_x11 <- function(y, args1, args2) {
    starting_time <- Sys.time()
    res_try <- try({
        x11.dow <- do.call(what = rjd3x11plus::x11plus, args = c(list(y = y), args1))
        x11.doy <- do.call(what = rjd3x11plus::x11plus, args = c(list(y = x11.dow$decomposition$sa), args2))
    })
    ending_time <- Sys.time()

    if (inherits(res_try, "try-error")) {
        return(list(
            time = NA,
            series = NULL
        ))
    }

    series <- cbind(
        sa = x11.doy$decomposition$sa,
        s7 = x11.dow$decomposition$s,
        s365 = x11.doy$decomposition$s,
        t = x11.doy$decomposition$t,
        i = x11.doy$decomposition$i
    )

    return(list(
        time = ending_time - starting_time,
        series = series
    ))
}

my_stl <- function(y, args1, args2) {
    starting_time <- Sys.time()
    stl.dow <- do.call(what = rjd3stl::stlplus, args = c(list(y = y), args1))
    stl.doy <- do.call(what = rjd3stl::stlplus, args = c(list(y = stl.dow$decomposition[, "sa"]), args2))
    ending_time <- Sys.time()

    series <- cbind(
        sa = stl.doy$decomposition[, "sa"],
        s7 = stl.dow$decomposition[, "s"],
        s365 = stl.doy$decomposition[, "s"],
        t = stl.doy$decomposition[, "t"],
        i = stl.doy$decomposition[, "i"]
    )

    return(list(
        time = ending_time - starting_time,
        series = series
    ))
}

my_amb <- function(y, args1, args2) {
    starting_time <- Sys.time()
    amb.dow <- do.call(what = rjd3highfreq::fractionalAirlineDecomposition, args = c(list(y = y), args1))
    amb.doy <- do.call(what = rjd3highfreq::fractionalAirlineDecomposition, args = c(list(y = amb.dow$decomposition$sa), args2))
    ending_time <- Sys.time()

    series <- cbind(
        sa = amb.doy$decomposition$sa,
        s7 = amb.dow$decomposition$s,
        s365 = amb.doy$decomposition$s,
        t = amb.doy$decomposition$t,
        i = amb.doy$decomposition$i
    )

    return(list(
        time = ending_time - starting_time,
        series = series
    ))
}

my_Mamb <- function(y, args1) {
    starting_time <- Sys.time()
    amb.multi <- do.call(what = rjd3highfreq::multiAirlineDecomposition, args = c(list(y = y), args1))
    ending_time <- Sys.time()

    series <- cbind(
        sa = amb.multi$decomposition$sa,
        s7 = amb.multi$decomposition$s_7,
        s365 = amb.multi$decomposition$s_365.2425,
        t = amb.multi$decomposition$t,
        i = amb.multi$decomposition$y -
            amb.multi$decomposition$t -
            amb.multi$decomposition$s_365.2425 -
            amb.multi$decomposition$s_7
    )

    return(list(
        time = ending_time - starting_time,
        series = series
    ))
}

my_TBATS <- function(y) {
    z_TBATS <- forecast::msts(
        data = y,
        seasonal.periods = c(7, 365.2425),
        start = start(y)
    )
    starting_time <- Sys.time()
    res <- forecast::tbats(z_TBATS, use.box.cox = FALSE, use.damped.trend = FALSE)
    ending_time <- Sys.time()

    comps_tbats <- ts(forecast::tbats.components(res), start = start(y), frequency = 365.2425)

    if (!all(c("season1", "season2", "level") %in% colnames(comps_tbats))) {
        return(list(
            time = ending_time - starting_time,
            series = NULL
        ))
    }

    series <- cbind(
        sa = y -
            comps_tbats[, "season1"] -
            comps_tbats[, "season2"],
        s7 = comps_tbats[, "season1"],
        s365 = comps_tbats[, "season2"],
        t = comps_tbats[, "level"],
        i = y -
            comps_tbats[, "level"] -
            comps_tbats[, "season1"] -
            comps_tbats[, "season2"]
    )

    return(list(
        time = ending_time - starting_time,
        series = series
    ))
}

my_prophet <- function(y) {
    history <- data.frame(
        ds = seq.Date(from = as.Date("2020-01-01"), length.out = length(y), by = 'day'),
        y = y
    )
    colnames(history) <- c("ds", "y")
    starting_time <- Sys.time()
    model_prophet <- prophet::prophet(
        df = history,
        growth = "linear",
        # changepoints = NULL, #auto
        # n.changepoints = 25,
        # changepoint.range = 0.8,
        yearly.seasonality = "auto",
        weekly.seasonality = "auto",
        # daily.seasonality = "auto",
        holidays = NULL,
        seasonality.mode = "additive", # does multplicative imply log, check negative values
        # seasonality.prior.scale = 10,
        # holidays.prior.scale = 10,
        # changepoint.prior.scale = 0.05,
        # mcmc.samples = 0,
        # interval.width = 0.8,
        uncertainty.samples = 0,
        fit = TRUE
    )
    ending_time <- Sys.time()

    components_prophet <- ts(predict(model_prophet, history[, "ds", drop = FALSE]), start = start(y), frequency = 365.2425)

    series <- cbind(
        sa = y -
            components_prophet[, "weekly"] -
            components_prophet[, "yearly"],
        s7 = components_prophet[, "weekly"],
        s365 = components_prophet[, "yearly"],
        t = components_prophet[, "trend"],
        i = y -
            components_prophet[, "weekly"] -
            components_prophet[, "yearly"] -
            components_prophet[, "trend"]
    )

    return(list(
        time = ending_time - starting_time,
        series = series
    ))
}

my_mstl <- function(y) {
    z_MSTL <- forecast::msts(
        data = y,
        seasonal.periods = c(7, 365.2425),
        start = start(y)
    )
    starting_time <- Sys.time()
    comps_MSTL <- ts(forecast::mstl(x = z_MSTL), start = start(y), frequency = 365.2425)
    ending_time <- Sys.time()

    series <- cbind(
        sa = y -
            comps_MSTL[, "Seasonal7"] -
            comps_MSTL[, "Seasonal365.24"],
        s7 = comps_MSTL[, "Seasonal7"],
        s365 = comps_MSTL[, "Seasonal365.24"],
        t = comps_MSTL[, "Trend"],
        i = comps_MSTL[, "Remainder"]
    )

    return(list(
        time = ending_time - starting_time,
        series = series
    ))
}

