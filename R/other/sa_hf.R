################################################################################
#######                Démonstration de rjd3highfreq                     #######
################################################################################


# Chargement des packages ------------------------------------------------------

# La PR n'a pas été accepté donc il faut installer mon package rjd3toolkit

remotes::install_github("TanguyBarthelemy/rjd3toolkit")

library("rjd3highfreq")
library("ggplot2")
library("rjd3toolkit")


# Chargement data --------------------------------------------------------------

df_daily <- read.csv("./data/TS_daily_births_franceM_1968_2020.csv", sep = ";") |>
    dplyr::mutate(
        log_births = log(births),
        date = as.Date(date)
    )

# This dataframe contains the following variables:
# date       = from 01/01/1968 to 12/31/2000
# births     = daily number of French Births
# log_births = daily number of French Births in log
# day        = indicates the day of the week, D1=Monday...D7=Sunday
# month      = indicates the day of the month, M01=January...M12=December


# Calendar regressors ----------------------------------------------------------


# Names for calendar regressors
calendar_regressors <- c(
    "Bastille_day", "Victory_day", "NEWYEAR", "CHRISTMAS", "MAYDAY",
    "EASTERMONDAY", "ASCENSION", "WHITMONDAY", "ASSUMPTION", "ALLSAINTSDAY",
    "ARMISTICE"
)

# Define a national calendar
french_calendar <- national_calendar(
    days = list(
        Bastille_day = fixed_day(7, 14), # Bastille Day
        Victory_day = fixed_day(5, 8, validity = list(start = "1982-05-08")), # Victory Day
        NEWYEAR = special_day("NEWYEAR"),
        CHRISTMAS = special_day("CHRISTMAS"),
        MAYDAY = special_day("MAYDAY"),
        EASTERMONDAY = special_day("EASTERMONDAY"),
        ASCENSION = special_day("ASCENSION"),
        WHITMONDAY = special_day("WHITMONDAY"),
        ASSUMPTION = special_day("ASSUMPTION"),
        ALLSAINTSDAY = special_day("ALLSAINTSDAY"),
        ARMISTICE = special_day("ARMISTICE")
    )
)

# Generate calendar regressors
q <- holidays(
    calendar = french_calendar,
    start = "1968-01-01",
    length = length(df_daily$births),
    type = "All",
    nonworking = 7L
)


# Muti AMB decomposition with extended fractional Airline Model ----------------


f <- function(
        # input time series
    y,
    # Different periods
    periods,
    # q= calendar regressors matrix
    x = NULL,
    ndiff = 2,
    ar = FALSE,
    mean = FALSE,
    # type of outliers detected
    outliers = c("ao", "wo"),
    criticalValue = 0,
    precision = 1e-12,
    approximateHessian = FALSE,
    # Signal (SA)-noise decomposition
    sn = FALSE,
    # Calculate standard deviations
    stde = FALSE,
    # Numbers of back- and forecasts
    nbcasts = 0,
    nfcasts = 0,
    # To perform a multiplicative model
    log = FALSE,
    y_time = NULL
) {
    if (is.null(y_time) && !is.null(x)) {
        y_time <- rownames(x)
    }

    pre_adj <- fractionalAirlineEstimation(
        y = y,
        y_time = y_time,
        x = x,
        periods = periods,
        ndiff = ndiff,
        ar = ar,
        mean = mean,
        outliers = outliers,
        criticalValue = criticalValue,
        precision = precision,
        approximateHessian = approximateHessian,
        log = log
    )

    if (!is.null(pre_adj$model$variables)) {
        # Retrieving estimated outlier & calendar effects (coefs, SE, student)
        regs <- data.frame(
            "Variable" = pre_adj$model$variables,
            "Coef"     = pre_adj$model$b,
            "Coef_SE"  = sqrt(diag(pre_adj$model$bcov))
        )
        regs$Tstat <- round(regs$Coef / regs$Coef_SE, 2)
        pre_adj$model <- c(pre_adj$model, list(regressors_coefficients = regs))
    }

    # Retrieving estimated MA parameters (coefs, se, student)
    MA_coeffs <- data.frame(
        "MA parameter" = c("Theta(1)", paste0("Theta(period = ", pre_adj$model$periods, ")")),
        "Coef" = pre_adj$estimation$parameters,
        "Coef_SE" = sqrt(diag(pre_adj$estimation$covariance)),
        check.names = FALSE
    )
    MA_coeffs$Tstat <- MA_coeffs$Coef / MA_coeffs$Coef_SE
    pre_adj$model <- c(pre_adj$model, list(sarima_coefficients = MA_coeffs))

    sa <- pre_adj$model$linearized
    all_amb <- list()
    for (p in sort(periods)) {
        amb <- rjd3highfreq::fractionalAirlineDecomposition(
            y = sa,
            period = p,
            sn = sn,
            stde = stde,
            nbcasts = nbcasts,
            nfcasts = nfcasts,
            log = log,
            y_time = y_time
        )
        all_amb <- c(all_amb, list(amb))
        sa <- amb$decomposition$sa
    }

    # Compute final components and SA series
    #calendar component
    cal.cmp <- rep(0, length(y))
    if (!is.null(x)) {
        cal.cmp <- pre_adj$model$xreg[, seq_len(ncol(x))] %*% pre_adj$model$b[seq_len(ncol(x))]
    }

    #final s components
    amb_s <- lapply(all_amb, \(.amb) .amb$decomposition$s)
    amb_s <- as.data.frame(setNames(amb_s, paste0("s_", sort(periods))))

    #final sa
    y_trans <- y
    if (log) y_trans <- log(y)
    amb.sa <- y_trans - (cal.cmp + rowSums(amb_s))

    # group
    final_decomposition <- data.frame(
        y = y_trans,
        lin = pre_adj$model$linearized,
        cal = cal.cmp,
        amb_s,
        sa = amb.sa
    )
    rownames(final_decomposition) <- as.character(y_time)
    if (log) final_decomposition <- exp(final_decomposition)

    output <- c(
        list(preprocessing = pre_adj),
        setNames(all_amb, paste0("amb_model_period_", sort(periods))),
        list(final = final_decomposition)
    )

    return(output)
}

out <- f(
    y = df_daily$births,
    periods = c(7, 365.25),
    x = q,
    ndiff = 2,
    ar = FALSE,
    mean = FALSE,
    outliers = c("ao", "wo"),
    criticalValue = 0,
    precision = 1e-9,
    approximateHessian = TRUE,
    sn = FALSE,
    stde = FALSE,
    nbcasts = 0,
    nfcasts = 0,
    log = TRUE,
    y_time = seq.Date(from = as.Date("1968-01-01"), length.out = length(df_daily$log_births), by = "days")
)


############# TESTS
