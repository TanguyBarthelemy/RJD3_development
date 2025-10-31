# -------------------------------------------------------------------------------------------------
# (1) General information
# -------------------------------------------------------------------------------------------------

# Aim: illustration of model based seasonal adjustment with the {rjd3highfreq} package (version 1.0.1)

# Packages repositories: https://github.com/rjdverse/rjd3highfreq (version 1.0.1)
#
# Dependencies: {RProtoBuf}, {rJava}, {checkmate}, {rjd3toolkit}, Java 17 (or higher)

# Data: French daily births (Metropolitan France only)
# Source: https://www.insee.fr/fr/statistiques/6524900?sommaire=6524912

# -------------------------------------------------------------------------------------------------
# (2) Data input & point-wise Canova-Hansen statistics
# -------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)


df_daily <- readRDS("Births.RDS")

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
        df_daily$births,
        p0 = min(ch.sp),
        p1 = max(ch.sp),
        np = max(ch.sp) - min(ch.sp) + 1,
        original = TRUE
    ),
    "ch.log" = rjd3toolkit::seasonality_canovahansen(
        df_daily$log_births,
        p0 = min(ch.sp),
        p1 = max(ch.sp),
        np = max(ch.sp) - min(ch.sp) + 1,
        original = TRUE
    ),
    "ch.dlg" = rjd3toolkit::seasonality_canovahansen(
        diff(df_daily$log_births, lag = 1, differences = 1),
        p0 = min(ch.sp),
        p1 = max(ch.sp),
        np = max(ch.sp) - min(ch.sp) + 1,
        original = TRUE
    )
)

# Significant periodicities (Harvey, 2001, Table I(b))

which(df_ch$ch.raw > .211) + 1 # 10% level of significance
which(df_ch$ch.raw > .247) + 1 #  5% level of significance
which(df_ch$ch.raw > .329) + 1 #  1% level of significance

# Barplot

ggplot(df_ch) +
    geom_col(aes(sp, ch.raw), size = .25) +
    labs(x = "Periodicity (in days)", y = "") +
    ggthemes::theme_hc()

# -------------------------------------------------------------------------------------------------
# (3) Calendar regressors
# -------------------------------------------------------------------------------------------------

library(rjd3toolkit)
library(rjd3highfreq)

# Define a national calendar
french_calendar <- national_calendar(
    days = list(
        fixed_day(7, 14), # Bastille Day
        fixed_day(5, 8, validity = list(start = "1982-05-08")), # Victory Day
        special_day("NEWYEAR"),
        special_day("CHRISTMAS"),
        special_day("MAYDAY"),
        special_day("EASTERMONDAY"),
        special_day("ASCENSION"),
        special_day("WHITMONDAY"),
        special_day("ASSUMPTION"),
        special_day("ALLSAINTSDAY"),
        special_day("ARMISTICE")
    )
)
# Generrate calendar regressors
q <- holidays(
    french_calendar,
    "1968-01-01",
    length = length(df_daily$births),
    type = "All",
    nonworking = 7L
)


# -------------------------------------------------------------------------------------------------
# (4) Pre-adjustement with extended fractional Airline Model
# -------------------------------------------------------------------------------------------------

# Reg-Arima estimation

pre.mult <- rjd3highfreq::fractionalAirlineEstimation(
    df_daily$log_births,
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

# Retrieving estimated outlier & calendar effects (coefs, se, student)

regs_mult <- data.frame(
    "Variable" = pre.mult$model$variables,
    "Coef" = pre.mult$model$b,
    "Coef_SE" = sqrt(diag(pre.mult$model$bcov))
) %>%
    mutate(Tstat = round(Coef / Coef_SE, 2))

# adding names for calendar regressors
calendar_regressors <- c(
    "Bastille_day",
    "Victory_day",
    "NEWYEAR",
    "CHRISTMAS",
    "MAYDAY",
    "EASTERMONDAY",
    "ASCENSION",
    "WHITMONDAY",
    "ASSUMPTION",
    "ALLSAINTSDAY",
    "ARMISTICE"
)

regs_mult$Variable[1:11] <- calendar_regressors


# Formatting outliers dates
nb_outliers <- nrow(regs_mult) - length(calendar_regressors)

outliers <- regs_mult$Variable[
    (length(calendar_regressors) + 1):nrow(regs_mult)
]
outliers_names <- substr(outliers, 1, 2)
outliers_position <- substr(outliers, 4, 9)
outliers_dates <- as.character(df_daily$date[as.numeric(outliers_position)])
outliers_renamed <- paste0(outliers_names, ".", outliers_dates)
regs_mult$Variable[
    (length(calendar_regressors) + 1):nrow(regs_mult)
] <- outliers_renamed
regs_mult


# Retrieving estimated MA parameters (coefs, se, student)

MA_coeffs <- data.frame(
    "MA parameter" = c("MA1", "DOW"),
    "Coef" = pre.mult$estimation$parameters,
    "Coef_SE" = sqrt(diag(pre.mult$estimation$covariance)),
    check.names = FALSE
) %>%
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
    nbcasts = 0,
    nfcasts = 0
) # Numbers of back- and forecasts

# Extract DOY pattern from DOW-adjusted linearised data
amb.doy <- rjd3highfreq::fractionalAirlineDecomposition(
    amb.dow$decomposition$sa, # DOW-adjusted linearised data
    period = 365.2425, # DOY pattern
    sn = FALSE,
    stde = FALSE,
    nbcasts = 0,
    nfcasts = 0
)


# Compute final components and SA series

# calendar component
df_daily <- df_daily %>%
    mutate(
        cal.cmp = exp(
            pre.mult$model$xreg[, seq_along(calendar_regressors)] %*%
                pre.mult$model$b[seq_along(calendar_regressors)]
        )
    )

# final dow, doy and sa
df_daily <- df_daily %>%
    mutate(amb.dow = exp(amb.dow$decomposition$s)) %>%
    mutate(amb.doy = exp(amb.doy$decomposition$s)) %>%
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
