# UC on Sim Data ----------------------------------------------------------------------

library(tssim) # attention version ad hoc tar.gz
library(rjd3highfreq)
library(rjd3x11plus)
library(rjd3stl)
library(rjd3sts)
library(prophet)
library(forecast)
library(lubridate)
library(flextable)
library(tsbox)

#play on data lentgh ?
# default =12 years

# Simulation parameters
# _s2 : deterministic and...
# weekly
alpha <- 1 # paper value
sw <- 100 # function needed input (param /100)

#yearly
beta <- 1 # paper value
sy <- 33 # function needed input (param /33)

# irregular
gamma <- 0.4
si <- 0.4 * 150 # function needed input (param/150)

# shock sigma : 0 when deterministic

# Storing Sim Data  ---------------------------------------------------------

list_raw <- as.list(rep(NA, nb_series))
list_raw
list_true_sa <- as.list(rep(NA, nb_series))
list_true_s7 <- as.list(rep(NA, nb_series))
list_true_s365 <- as.list(rep(NA, nb_series))
list_true_t <- as.list(rep(NA, nb_series))
list_true_i <- as.list(rep(NA, nb_series))

for (j in 1:nb_series) {
    x <- sim_daily_mstl(
        N = nb_years,
        allComponents = TRUE,
        multiplicative = FALSE,
        deterministic = TRUE,
        sizeAnnualSeas = 100,
        sizeWeeklySeas = sw,
        sizeIrregularity = si
    )
    list_raw[[j]] <- ts_ts(x[, 1])
    list_true_sa[[j]] <- ts_ts(x[, 2])
    list_true_s7[[j]] <- ts_ts(x[, 3])
    list_true_s365[[j]] <- ts_ts(x[, 4])
    list_true_t[[j]] <- ts_ts(x[, 5])
    list_true_i[[j]] <- ts_ts(x[, 6])
}
head(x)
# class(x[,1])
# class(list_raw[[j]])
# class(ts_ts(x[,1]))
# str(x) # retrieve date
nobs <- length(x[, 1])
nobs

# class(list_raw[[j]])
# plot(list_raw[[j]])
#
# head(list_true_s7[[j]])
# plot(list_true_s7[[j]])
# head(list_true_s365[[j]])
# plot(list_true_s365[[j]])

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


# STL - decomposition  ----------------------------------------------------

# storage

# results (element = ts or vector...)
list_stl_sa <- as.list(rep(NA, nb_series))
list_stl_s7 <- as.list(rep(NA, nb_series))
list_stl_s365 <- as.list(rep(NA, nb_series))
list_stl_t <- as.list(rep(NA, nb_series))
list_stl_i <- as.list(rep(NA, nb_series))

# RMSE for each series (element = number = RMSE)
list_RMSE_stl_sa <- as.list(rep(NA, nb_series))
list_RMSE_stl_s7 <- as.list(rep(NA, nb_series))
list_RMSE_stl_s365 <- as.list(rep(NA, nb_series))
list_RMSE_stl_t <- as.list(rep(NA, nb_series))
list_RMSE_stl_i <- as.list(rep(NA, nb_series))

# computing time for each series
list_time_stl <- as.list(rep(NA, nb_series))

# start.dow = Sys.time()
for (j in 1:nb_series) {
    start <- Sys.time()
    stl.dow <- rjd3stl::stlplus(
        y = list_raw[[j]],
        period = 7,
        multiplicative = FALSE,
        swindow = 3,
        twindow = 9, # 3= half, t= env p ?
        ninnerloop = 2,
        nouterloop = 0,
        # nojump = FALSE, # not used any more?
        weight.threshold = 0.001,
        weight.function = "BIWEIGHT"
    )
    # Extract DOY pattern from DOW-adjusted data : run on SA
    stl.doy <- rjd3stl::stlplus(
        y = stl.dow$decomposition[, "sa"],
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
    end <- Sys.time()
    list_time_stl[[j]] <- round(
        as.numeric(difftime(end, start, units = "secs")),
        3
    )

    #store s7
    list_stl_s7[[j]] <- stl.dow$decomposition[, "s"]
    # RMSE for STL (vs True) S7
    list_RMSE_stl_s7[[j]] <- sqrt(mean(
        (list_stl_s7[[j]] - list_true_s7[[j]])^2
    ))

    #store s365
    list_stl_s365[[j]] <- stl.doy$decomposition[, "s"]
    # RMSE for STL (vs True) S365
    list_RMSE_stl_s365[[j]] <- sqrt(mean(
        (list_stl_s365[[j]] - list_true_s365[[j]])^2
    ))

    #store t
    list_stl_t[[j]] <- stl.doy$decomposition[, "t"]
    # RMSE for STL (vs True) T
    list_RMSE_stl_t[[j]] <- sqrt(mean((list_stl_t[[j]] - list_true_t[[j]])^2))

    #store i
    list_stl_i[[j]] <- stl.doy$decomposition[, "i"]
    # RMSE for STL (vs True) I
    list_RMSE_stl_i[[j]] <- sqrt(mean((list_stl_i[[j]] - list_true_i[[j]])^2))

    #store sa
    list_stl_sa[[j]] <- stl.doy$decomposition[, "sa"]
    # RMSE for stl (vs True) SA
    list_RMSE_stl_sa[[j]] <- sqrt(mean(
        (list_stl_sa[[j]] - list_true_sa[[j]])^2
    ))
}
list_RMSE_stl_s7
list_RMSE_stl_s365
list_RMSE_stl_t
list_RMSE_stl_i
list_RMSE_stl_sa
list_time_stl

# AMB decomposition (Iterative) -------------------------------------------

# storage

# results (element = ts or vector...)
list_amb_sa <- as.list(rep(NA, nb_series))
list_amb_s7 <- as.list(rep(NA, nb_series))
list_amb_s365 <- as.list(rep(NA, nb_series))
list_amb_t <- as.list(rep(NA, nb_series))
list_amb_i <- as.list(rep(NA, nb_series))

# RMSE for each series (element = number = RMSE)
list_RMSE_amb_sa <- as.list(rep(NA, nb_series))
list_RMSE_amb_s7 <- as.list(rep(NA, nb_series))
list_RMSE_amb_s365 <- as.list(rep(NA, nb_series))
list_RMSE_amb_t <- as.list(rep(NA, nb_series))
list_RMSE_amb_i <- as.list(rep(NA, nb_series))

# computing time for each series
list_time_amb <- as.list(rep(NA, nb_series))

for (j in 1:nb_series) {
    start = Sys.time()
    amb.dow <- rjd3highfreq::fractionalAirlineDecomposition(
        list_raw[[j]], # input time series
        period = 7, # DOW pattern
        sn = FALSE, # Signal (SA)-noise decomposition
        stde = FALSE, # Calculate standard deviations
        nbcasts = 0,
        nfcasts = 0
    ) # Numbers of back- and forecasts

    # Extract DOY pattern from DOW-adjusted linearised data
    start = Sys.time()
    amb.doy <- rjd3highfreq::fractionalAirlineDecomposition(
        amb.dow$decomposition$sa, # DOW-adjusted linearised data
        period = 365.2425, # DOY pattern
        sn = FALSE,
        stde = FALSE,
        nbcasts = 0,
        nfcasts = 0
    )
    end <- Sys.time()
    list_time_amb[[j]] <- round(
        as.numeric(difftime(end, start, units = "secs")),
        3
    )

    #store s7
    list_amb_s7[[j]] <- amb.dow$decomposition$s
    # RMSE for amb (vs True) S7
    list_RMSE_amb_s7[[j]] <- sqrt(mean(
        (list_amb_s7[[j]] - list_true_s7[[j]])^2
    ))

    #store s365
    list_amb_s365[[j]] <- amb.doy$decomposition$s
    # RMSE for amb (vs True) S365
    list_RMSE_amb_s365[[j]] <- sqrt(mean(
        (list_amb_s365[[j]] - list_true_s365[[j]])^2
    ))

    #store t
    list_amb_t[[j]] <- amb.doy$decomposition$t
    # RMSE for amb (vs True) T
    list_RMSE_amb_t[[j]] <- sqrt(mean((list_amb_t[[j]] - list_true_t[[j]])^2))

    #store i
    list_amb_i[[j]] <- amb.doy$decomposition$i
    # RMSE for amb (vs True) I
    list_RMSE_amb_i[[j]] <- sqrt(mean((list_amb_i[[j]] - list_true_i[[j]])^2))

    #store sa
    list_amb_sa[[j]] <- amb.doy$decomposition$sa
    # RMSE for amb (vs True) SA
    list_RMSE_amb_sa[[j]] <- sqrt(mean(
        (list_amb_sa[[j]] - list_true_sa[[j]])^2
    ))
}
list_RMSE_amb_s7
list_RMSE_amb_s365
list_RMSE_amb_t
list_RMSE_amb_i
list_RMSE_amb_sa
list_time_amb

# AMB decomposition (Multi) -----------------------------------------------

# storage

# results (element = ts or vector...)
list_Mamb_sa <- as.list(rep(NA, nb_series))
list_Mamb_s7 <- as.list(rep(NA, nb_series))
list_Mamb_s365 <- as.list(rep(NA, nb_series))
list_Mamb_t <- as.list(rep(NA, nb_series))
list_Mamb_i <- as.list(rep(NA, nb_series))

# RMSE for each series (element = number = RMSE)
list_RMSE_Mamb_sa <- as.list(rep(NA, nb_series))
list_RMSE_Mamb_s7 <- as.list(rep(NA, nb_series))
list_RMSE_Mamb_s365 <- as.list(rep(NA, nb_series))
list_RMSE_Mamb_t <- as.list(rep(NA, nb_series))
list_RMSE_Mamb_i <- as.list(rep(NA, nb_series))

# computing time for each series
list_time_Mamb <- as.list(rep(NA, nb_series))

for (j in 1:nb_series) {
    start = Sys.time()
    amb.multi <- rjd3highfreq::multiAirlineDecomposition(
        list_raw[[j]], # input time series in log
        periods = c(7, 365.2425),
        # sn = FALSE,
        # stde = FALSE,
        nbcasts = 0,
        nfcasts = 0
    )

    end <- Sys.time()
    list_time_Mamb[[j]] <- round(
        as.numeric(difftime(end, start, units = "secs")),
        3
    )

    # str(amb.multi)

    #store s7
    list_Mamb_s7[[j]] <- amb.multi$decomposition$s_7
    # RMSE for amb (vs True) S7
    list_RMSE_Mamb_s7[[j]] <- sqrt(mean(
        (list_Mamb_s7[[j]] - list_true_s7[[j]])^2
    ))

    #store s365
    list_Mamb_s365[[j]] <- amb.multi$decomposition$s_365.2425
    # RMSE for amb (vs True) S365
    list_RMSE_Mamb_s365[[j]] <- sqrt(mean(
        (list_Mamb_s365[[j]] - list_true_s365[[j]])^2
    ))

    #store t
    list_Mamb_t[[j]] <- amb.multi$decomposition$t
    # RMSE for amb (vs True) T
    list_RMSE_Mamb_t[[j]] <- sqrt(mean((list_Mamb_t[[j]] - list_true_t[[j]])^2))

    #store i
    list_Mamb_i[[j]] <- amb.multi$decomposition$y -
        amb.multi$decomposition$t -
        amb.multi$decomposition$s_365.2425 -
        amb.multi$decomposition$s_7
    # RMSE for amb (vs True) T
    list_RMSE_Mamb_i[[j]] <- sqrt(mean((list_Mamb_i[[j]] - list_true_i[[j]])^2))

    #store sa
    list_amb_sa[[j]] <- amb.multi$decomposition$sa
    # RMSE for amb (vs True) SA
    list_RMSE_Mamb_sa[[j]] <- sqrt(mean(
        (list_amb_sa[[j]] - list_true_sa[[j]])^2
    ))
}
list_RMSE_Mamb_s7
list_RMSE_Mamb_s365
list_RMSE_Mamb_t
list_RMSE_Mamb_i
list_RMSE_Mamb_sa
list_time_Mamb


# BSM (UC only, later ?)-----------------------------------------------------------

# STR (later ?) -------------------------------------------------------------

# TBATS -------------------------------------------------------------------

# storage

# results (element = ts or vector...)
list_Tbats_sa <- as.list(rep(NA, nb_series))
list_Tbats_s7 <- as.list(rep(NA, nb_series))
list_Tbats_s365 <- as.list(rep(NA, nb_series))
list_Tbats_t <- as.list(rep(NA, nb_series))
list_Tbats_i <- as.list(rep(NA, nb_series))

# RMSE for each series (element = number = RMSE)
list_RMSE_Tbats_sa <- as.list(rep(NA, nb_series))
list_RMSE_Tbats_s7 <- as.list(rep(NA, nb_series))
list_RMSE_Tbats_s365 <- as.list(rep(NA, nb_series))
list_RMSE_Tbats_t <- as.list(rep(NA, nb_series))
list_RMSE_Tbats_i <- as.list(rep(NA, nb_series))

# computing time for each series
list_time_Tbats <- as.list(rep(NA, nb_series))

for (j in 1:nb_series) {
    res2 <- try({
        # Seasonal periodicities (in days)
        seas_per <- c(7, 365.2425)
        z_TBATS <- msts(
            list_raw[[j]],
            seasonal.periods = seas_per,
            start = decimal_date(start_date)
        )
        #results
        start <- Sys.time()
        res <- tbats(z_TBATS, use.box.cox = FALSE, use.damped.trend = FALSE) # desp options
        end <- Sys.time()
    })

    if (!inherits(res2, "try-error")) {
        list_time_Tbats[[j]] <- round(
            as.numeric(difftime(end, start, units = "secs")),
            3
        )

        # Extracting components & residuals
        comps_tbats <- tbats.components(res)
        head(comps_tbats)
        # class(comps_tbats)
        # class(list_Tbats_s7[[j]])
        # class(list_true_s7[[j]])
        # length(list_Tbats_s7[[j]])
        # length(list_true_s7[[j]])

        #store s7
        list_Tbats_s7[[j]] <- as.numeric(comps_tbats[, 3])
        # RMSE for amb (vs True) S7
        list_RMSE_Tbats_s7[[j]] <- sqrt(mean(
            (list_Tbats_s7[[j]] - list_true_s7[[j]])^2
        ))

        #store s365
        list_Tbats_s365[[j]] <- as.numeric(comps_tbats[, 4])
        # RMSE for amb (vs True) S365
        list_RMSE_Tbats_s365[[j]] <- sqrt(mean(
            (list_Tbats_s365[[j]] - list_true_s365[[j]])^2
        ))

        #store t
        list_Tbats_t[[j]] <- as.numeric(comps_tbats[, 2])
        # RMSE for amb (vs True) T
        list_RMSE_Tbats_t[[j]] <- sqrt(mean(
            (list_Tbats_t[[j]] - list_true_t[[j]])^2
        ))

        #store i
        list_Tbats_i[[j]] <- as.numeric(comps_tbats[, 1]) -
            as.numeric(comps_tbats[, 2]) -
            as.numeric(comps_tbats[, 3]) -
            as.numeric(comps_tbats[, 4])
        # RMSE for amb (vs True) T
        list_RMSE_Tbats_i[[j]] <- sqrt(mean(
            (list_Tbats_i[[j]] - list_true_i[[j]])^2
        ))

        #store sa
        list_Tbats_sa[[j]] <- as.numeric(comps_tbats[, 1]) -
            as.numeric(comps_tbats[, 3]) -
            as.numeric(comps_tbats[, 4])
        # RMSE for X11 (vs True) SA
        list_RMSE_Tbats_sa[[j]] <- sqrt(mean(
            (list_Tbats_sa[[j]] - list_true_sa[[j]])^2
        ))
    }
}
list_RMSE_Tbats_s7
list_RMSE_Tbats_s365
list_RMSE_Tbats_t
list_RMSE_Tbats_i
list_RMSE_Tbats_sa
list_time_Tbats

# Prophet -----------------------------------------------------------------
# storage

# results (element = ts or vector...)
list_Prophet_sa <- as.list(rep(NA, nb_series))
list_Prophet_s7 <- as.list(rep(NA, nb_series))
list_Prophet_s365 <- as.list(rep(NA, nb_series))
list_Prophet_t <- as.list(rep(NA, nb_series))
list_Prophet_i <- as.list(rep(NA, nb_series))

# RMSE for each series (element = number = RMSE)
list_RMSE_Prophet_sa <- as.list(rep(NA, nb_series))
list_RMSE_Prophet_s7 <- as.list(rep(NA, nb_series))
list_RMSE_Prophet_s365 <- as.list(rep(NA, nb_series))
list_RMSE_Prophet_t <- as.list(rep(NA, nb_series))
list_RMSE_Prophet_i <- as.list(rep(NA, nb_series))

# computing time for each series
list_time_Prophet <- as.list(rep(NA, nb_series))

for (j in 1:nb_series) {
    history <- data.frame(
        ds = seq(start_date, end_date, by = 'd'),
        y = list_raw[[j]]
    )
    colnames(history) <- c("ds", "y")
    ### additive
    start <- Sys.time()
    m1 <- prophet(
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
    end <- Sys.time()
    list_time_Prophet[[j]] <- round(
        as.numeric(difftime(end, start, units = "secs")),
        3
    )

    future <- make_future_dataframe(m1, periods = 365)
    head(future)
    forecast <- predict(m1, future)
    head(forecast)
    # prophet_plot_components(m1, forecast) ## tb

    #store s7
    list_Prophet_s7[[j]] <- forecast$weekly[1:nobs]
    # RMSE for amb (vs True) S7
    list_RMSE_Prophet_s7[[j]] <- sqrt(mean(
        (list_Prophet_s7[[j]] - list_true_s7[[j]])^2
    ))

    #store s365
    list_Prophet_s365[[j]] <- forecast$yearly[1:nobs]
    # RMSE for amb (vs True) S365
    list_RMSE_Prophet_s365[[j]] <- sqrt(mean(
        (list_Prophet_s365[[j]] - list_true_s365[[j]])^2
    ))

    #store t
    list_Prophet_t[[j]] <- forecast$trend[1:nobs]
    # RMSE for amb (vs True) T
    list_RMSE_Prophet_t[[j]] <- sqrt(mean(
        (list_Prophet_t[[j]] - list_true_t[[j]])^2
    ))

    #store i
    list_Prophet_i[[j]] <- forecast$yhat[1:nobs] -
        forecast$trend[1:nobs] -
        forecast$yearly[1:nobs] -
        forecast$weekly[1:nobs]
    # RMSE for amb (vs True) I
    list_RMSE_Prophet_i[[j]] <- sqrt(mean(
        (list_Prophet_i[[j]] - list_true_i[[j]])^2
    ))

    #store sa
    list_Prophet_sa[[j]] <- forecast$yhat[1:nobs] -
        forecast$yearly[1:nobs] -
        forecast$weekly[1:nobs]
    # RMSE for X11 (vs True) SA
    list_RMSE_Prophet_sa[[j]] <- sqrt(mean(
        (list_Prophet_sa[[j]] - list_true_sa[[j]])^2
    ))
}
list_RMSE_Prophet_s7
list_RMSE_Prophet_s365
list_RMSE_Prophet_t
list_RMSE_Prophet_i
list_RMSE_Prophet_sa
list_time_Prophet


# MSTL Rob  --------------------------------------------------------------------

# storage

# results (element = ts or vector...)
list_MSTL_sa <- as.list(rep(NA, nb_series))
list_MSTL_s7 <- as.list(rep(NA, nb_series))
list_MSTL_s365 <- as.list(rep(NA, nb_series))
list_MSTL_t <- as.list(rep(NA, nb_series))
list_MSTL_i <- as.list(rep(NA, nb_series))

# RMSE for each series (element = number = RMSE)
list_RMSE_MSTL_sa <- as.list(rep(NA, nb_series))
list_RMSE_MSTL_s7 <- as.list(rep(NA, nb_series))
list_RMSE_MSTL_s365 <- as.list(rep(NA, nb_series))
list_RMSE_MSTL_t <- as.list(rep(NA, nb_series))
list_RMSE_MSTL_i <- as.list(rep(NA, nb_series))

# computing time for each series
list_time_MSTL <- as.list(rep(NA, nb_series))

# Seasonal periodicities (in days)
seas_per <- c(7, 365.2425)

for (j in 1:nb_series) {
    z_MSTL <- msts(
        list_raw[[j]],
        seasonal.periods = seas_per,
        start = decimal_date(start_date)
    )
    start <- Sys.time()
    comps_MSTL <- mstl(z_MSTL)
    end <- Sys.time()
    list_time_MSTL[[j]] <- round(
        as.numeric(difftime(end, start, units = "secs")),
        3
    )
    # str(comps_MSTL)

    #store s7
    list_MSTL_s7[[j]] <- as.numeric(comps_MSTL[, 3])
    # RMSE for amb (vs True) S7
    list_RMSE_MSTL_s7[[j]] <- sqrt(mean(
        (list_MSTL_s7[[j]] - list_true_s7[[j]])^2
    ))

    #store s365
    list_MSTL_s365[[j]] <- as.numeric(comps_MSTL[, 4])
    # RMSE for amb (vs True) S365
    list_RMSE_MSTL_s365[[j]] <- sqrt(mean(
        (list_MSTL_s365[[j]] - list_true_s365[[j]])^2
    ))

    #store t
    list_MSTL_t[[j]] <- as.numeric(comps_MSTL[, 2])
    # RMSE for amb (vs True) T
    list_RMSE_MSTL_t[[j]] <- sqrt(mean((list_MSTL_t[[j]] - list_true_t[[j]])^2))

    #store i
    list_MSTL_i[[j]] <- as.numeric(comps_MSTL[, 1]) -
        as.numeric(comps_MSTL[, 2]) -
        as.numeric(comps_MSTL[, 3]) -
        as.numeric(comps_MSTL[, 4])
    # RMSE for amb (vs True) I
    list_RMSE_MSTL_i[[j]] <- sqrt(mean((list_MSTL_i[[j]] - list_true_i[[j]])^2))

    #store sa
    list_MSTL_sa[[j]] <- as.numeric(comps_MSTL[, 1]) -
        as.numeric(comps_MSTL[, 3]) -
        as.numeric(comps_MSTL[, 4])
    # RMSE for X11 (vs True) SA
    list_RMSE_MSTL_sa[[j]] <- sqrt(mean(
        (list_MSTL_sa[[j]] - list_true_sa[[j]])^2
    ))
}


list_RMSE_MSTL_s7
list_RMSE_MSTL_s365
list_RMSE_MSTL_t
list_RMSE_MSTL_i
list_RMSE_MSTL_sa
list_time_MSTL


# Averaging  --------------------------------------------------------------

# S7
avg_x11_s7 <- mean(unlist(list_RMSE_X11_s7))
avg_x11_s7
avg_stl_s7 <- mean(unlist(list_RMSE_stl_s7))
avg_stl_s7
avg_amb_s7 <- mean(unlist(list_RMSE_amb_s7))
avg_amb_s7
avg_Mamb_s7 <- mean(unlist(list_RMSE_Mamb_s7))
avg_Mamb_s7
avg_Tbats_s7 <- mean(unlist(list_RMSE_Tbats_s7))
avg_Tbats_s7
avg_Prophet_s7 <- mean(unlist(list_RMSE_Prophet_s7))
avg_Prophet_s7
avg_MSTL_s7 <- mean(unlist(list_RMSE_MSTL_s7))
avg_MSTL_s7

# S365
avg_x11_s365 <- mean(unlist(list_RMSE_X11_s365))
avg_x11_s365
avg_stl_s365 <- mean(unlist(list_RMSE_stl_s365))
avg_stl_s365
avg_amb_s365 <- mean(unlist(list_RMSE_amb_s365))
avg_amb_s365
avg_Mamb_s365 <- mean(unlist(list_RMSE_Mamb_s365))
avg_Mamb_s365
avg_Tbats_s365 <- mean(unlist(list_RMSE_Tbats_s365))
avg_Tbats_s365
avg_Prophet_s365 <- mean(unlist(list_RMSE_Prophet_s365))
avg_Prophet_s365
avg_MSTL_s365 <- mean(unlist(list_RMSE_MSTL_s365))
avg_MSTL_s365

# T
avg_x11_t <- mean(unlist(list_RMSE_X11_t))
avg_x11_t
avg_stl_t <- mean(unlist(list_RMSE_stl_t))
avg_stl_t
avg_amb_t <- mean(unlist(list_RMSE_amb_t))
avg_amb_t
avg_Mamb_t <- mean(unlist(list_RMSE_Mamb_t))
avg_Mamb_t
avg_Tbats_t <- mean(unlist(list_RMSE_Tbats_t))
avg_Tbats_t
avg_Prophet_t <- mean(unlist(list_RMSE_Prophet_t))
avg_Prophet_t
avg_MSTL_t <- mean(unlist(list_RMSE_MSTL_t))
avg_MSTL_t

#I
avg_x11_i <- mean(unlist(list_RMSE_X11_i))
avg_x11_i
avg_stl_i <- mean(unlist(list_RMSE_stl_i))
avg_stl_i
avg_amb_i <- mean(unlist(list_RMSE_amb_i))
avg_amb_i
avg_Mamb_i <- mean(unlist(list_RMSE_Mamb_i))
avg_Mamb_i
avg_Tbats_i <- mean(unlist(list_RMSE_Tbats_i))
avg_Tbats_i
avg_Prophet_i <- mean(unlist(list_RMSE_Prophet_i))
avg_Prophet_i
avg_MSTL_i <- mean(unlist(list_RMSE_MSTL_i))
avg_MSTL_i

# sa
avg_x11_sa <- mean(unlist(list_RMSE_X11_sa))
avg_x11_sa
avg_stl_sa <- mean(unlist(list_RMSE_stl_sa))
avg_stl_sa
avg_amb_sa <- mean(unlist(list_RMSE_amb_sa))
avg_amb_sa
avg_Mamb_sa <- mean(unlist(list_RMSE_Mamb_sa))
avg_Mamb_sa
avg_Tbats_sa <- mean(unlist(list_RMSE_Tbats_sa))
avg_Tbats_sa
avg_Prophet_sa <- mean(unlist(list_RMSE_Prophet_sa))
avg_Prophet_sa
avg_MSTL_sa <- mean(unlist(list_RMSE_MSTL_sa))
avg_MSTL_sa

#time

avg_x11_time <- mean(unlist(list_time_X11))
avg_x11_time
avg_stl_time <- mean(unlist(list_time_stl))
avg_stl_time
avg_amb_time <- mean(unlist(list_time_amb))
avg_amb_time
avg_Mamb_time <- mean(unlist(list_time_Mamb))
avg_Mamb_time
avg_Tbats_time <- mean(unlist(list_time_Tbats))
avg_Tbats_time
avg_Prophet_time <- mean(unlist(list_time_Prophet))
avg_Prophet_time
avg_MSTL_time <- mean(unlist(list_time_MSTL))
avg_MSTL_time

# stockage scenario _s2
s7_s2 <- c(
    avg_x11_s7,
    avg_stl_s7,
    avg_amb_s7,
    avg_Mamb_s7,
    avg_Tbats_s7,
    avg_Prophet_s7,
    avg_MSTL_s7
)
s7_s2

s365_s2 <- c(
    avg_x11_s365,
    avg_stl_s365,
    avg_amb_s365,
    avg_Mamb_s365,
    avg_Tbats_s365,
    avg_Prophet_s365,
    avg_MSTL_s365
)
s365_s2

t_s2 <- c(
    avg_x11_t,
    avg_stl_t,
    avg_amb_t,
    avg_Mamb_t,
    avg_Tbats_t,
    avg_Prophet_t,
    avg_MSTL_t
)
t_s2

i_s2 <- c(
    avg_x11_i,
    avg_stl_i,
    avg_amb_i,
    avg_Mamb_i,
    avg_Tbats_i,
    avg_Prophet_i,
    avg_MSTL_i
)
i_s2

sa_s2 <- c(
    avg_x11_sa,
    avg_stl_sa,
    avg_amb_sa,
    avg_Mamb_sa,
    avg_Tbats_sa,
    avg_Prophet_sa,
    avg_MSTL_sa
)
sa_s2

time_s2 <- c(
    avg_x11_time,
    avg_stl_time,
    avg_amb_time,
    avg_Mamb_time,
    avg_Tbats_time,
    avg_Prophet_time,
    avg_MSTL_time
)
time_s2

# s<-rbind(s7,s365,t,i,sa)
# s
# saveRDS(s,"data/avg_by_m.rds")
# ############ HERE
# s<-readRDS("data/avg_by_m.rds")
