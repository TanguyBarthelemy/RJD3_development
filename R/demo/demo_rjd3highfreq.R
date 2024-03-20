################################################################################
#######                Démonstration de rjd3highfreq                     #######
################################################################################


# Chargement des packages ------------------------------------------------------

library("rjd3highfreq")
library("ggplot2")


# Chargement data --------------------------------------------------

df_daily <- read.csv("./data/TS_daily_births_franceM_1968_2020.csv", sep = ";") |>
    dplyr::mutate(
        log_births = log(births),
        date = as.Date(date)
    ) |>
    head(8000)

# This dataframe contains the following variables:
# date       = from 01/01/1968 to 12/31/2000
# births     = daily number of French Births
# log_births = daily number of French Births in log
# day        = indicates the day of the week, D1=Monday...D7=Sunday
# month      = indicates the day of the month, M01=January...M12=December

ch.sp <- 2:367 # Seasonal periodicities to be tested for

df_ch <- data.frame(
    "sp" = ch.sp,
    "ch.raw" = rjd3toolkit::seasonality_canovahansen(
        data = df_daily$births,
        p0 = min(ch.sp), p1 = max(ch.sp),
        np = max(ch.sp) - min(ch.sp) + 1L, original = TRUE
    ),
    "ch.log" = rjd3toolkit::seasonality_canovahansen(
        data = df_daily$log_births,
        p0 = min(ch.sp), p1 = max(ch.sp),
        np = max(ch.sp) - min(ch.sp) + 1L, original = TRUE
    ),
    "ch.dlg" = rjd3toolkit::seasonality_canovahansen(
        data = diff(df_daily$log_births, lag = 1L, differences = 1L),
        p0 = min(ch.sp), p1 = max(ch.sp),
        np = max(ch.sp) - min(ch.sp) + 1L, original = TRUE
    )
)

# Significant periodicities (Harvey, 2001, Table I(b))

which(df_ch$ch.raw > .211) + 1 # 10% level of significance
which(df_ch$ch.raw > .247) + 1 #  5% level of significance
which(df_ch$ch.raw > .329) + 1 #  1% level of significance

# Barplot

ggplot(df_ch) +
    geom_col(aes(sp, ch.raw), linewidth = .25) +
    labs(x = "Periodicity (in days)", y = "") +
    ggthemes::theme_hc()

# -------------------------------------------------------------------------------------------------
# (3) Calendar regressors
# -------------------------------------------------------------------------------------------------

library("rjd3toolkit")
library("rjd3highfreq")

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

# Generrate calendar regressors
q <- holidays(
    calendar = french_calendar,
    start = "1968-01-01",
    length = length(df_daily$births),
    type = "All",
    nonworking = 7L
)


# -------------------------------------------------------------------------------------------------
# (4) Pre-adjustement with extended fractional Airline Model
# -------------------------------------------------------------------------------------------------

library("dplyr")

# Reg-Arima estimation

pre.mult_bl <- fractionalAirlineEstimation(
    y = df_daily$log_births,
    y_time = seq.Date(from = as.Date("1968-01-01"), length.out = length(df_daily$log_births), by = "days"),
    x = q, # q= calendar regressors matrix
    periods = c(7, 365.25),
    ndiff = 2,
    ar = FALSE,
    mean = FALSE,
    outliers = c("ao", "wo"), # type of outliers detected
    criticalValue = 0, # automatically set
    precision = 1e-9,
    approximateHessian = TRUE
)
pre.mult_log <- fractionalAirlineEstimation(
    y = df_daily$births,
    y_time = seq.Date(from = as.Date("1968-01-01"), length.out = length(df_daily$log_births), by = "days"),
    x = q, # q= calendar regressors matrix
    periods = c(7, 365.25),
    ndiff = 2,
    ar = FALSE,
    mean = FALSE,
    outliers = c("ao", "wo"), # type of outliers detected
    criticalValue = 0, # automatically set
    precision = 1e-9,
    approximateHessian = TRUE,
    log = TRUE
)

# Retrieving estimated outlier & calendar effects (coefs, se, student)

regs_mult <- data.frame(
    "Variable" = pre.mult$model$variables,
    "Coef"     = pre.mult$model$b,
    "Coef_SE"  = sqrt(diag(pre.mult$model$bcov))
) |> mutate(Tstat = round(Coef / Coef_SE, 2))


# Retrieving estimated MA parameters (coefs, se, student)

MA_coeffs <- data.frame(
    "MA parameter" = c("Theta(1)", paste0("Theta(period = ", pre.mult$model$periods, ")")),
    "Coef" = pre.mult$estimation$parameters,
    "Coef_SE" = sqrt(diag(pre.mult$estimation$covariance)),
    check.names = FALSE
) |>
    mutate(Tstat = Coef / Coef_SE)

# -------------------------------------------------------------------------------------------------
# (5) AMB decomposition with extended fractional Airline Model
# -------------------------------------------------------------------------------------------------
#
# Extract DOW pattern from linearised series

amb.dow <- rjd3highfreq::fractionalAirlineDecomposition(
    pre.mult$model$linearized, # input time series
    period = 7, # DOW pattern
    sn = FALSE, # Signal (SA)-noise decomposition
    stde = FALSE, # Calculate standard deviations
    nbcasts = 0, nfcasts = 0
) # Numbers of back- and forecasts

# Extract DOY pattern from DOW-adjusted linearised data
amb.doy <- rjd3highfreq::fractionalAirlineDecomposition(
    y = amb.dow$decomposition$sa, # DOW-adjusted linearised data
    period = 365.2425, # DOY pattern
    sn = FALSE,
    stde = FALSE,
    nbcasts = 0, nfcasts = 0
)


# Compute final components and SA series


# calendar component
df_daily <- df_daily |>
    mutate(cal.cmp = exp(pre.mult$model$xreg[, 1:length(calendar_regressors)] %*%
                             pre.mult$model$b[1:length(calendar_regressors)]))

# final dow, doy and sa
df_daily <- df_daily |>
    mutate(amb.dow = exp(amb.dow$decomposition$s)) |>
    mutate(amb.doy = exp(amb.doy$decomposition$s)) |>
    mutate(amb.sa = births / (cal.cmp * amb.dow * amb.doy))

head(df_daily)
head(calendar_regressors)

# Plot raw and SA series
ggplot(df_daily) +
    geom_line(aes(date, births), size = .000001) +
    geom_line(aes(date, amb.sa), size = .000001, col = "red") +
    labs(x = "", y = "") +
    theme_classic() +
    theme(panel.grid.major.y = element_line(color = "black", size = 0.5))
