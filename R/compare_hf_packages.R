# Script pour tester la HF avec d'autres packages que le rjdverse

library(tidyverse)

# Import data -------------------------------------------------------------

births <- read.csv2(
    "https://raw.githubusercontent.com/TanguyBarthelemy/Tsace_RJD_Webinar_Dec22/b5fcf6b14ae47393554950547ef4788a0068a0f6/Data/TS_daily_births_franceM_1968_2020.csv"
)

df <- births %>%
    mutate(ds = as.Date(date), y = births) %>%
    select(ds, y)

# Packages à tester :

# rjd3x11plus / rjd3highfreq ---------------------------------------------------

library("rjd3highfreq")

frenchCalendar <- rjd3toolkit::national_calendar(
    days = list(
        rjd3toolkit::fixed_day(7, 14), # Bastille Day
        rjd3toolkit::fixed_day(5, 8, validity = list(start = "1982-05-08")), # End of 2nd WW
        rjd3toolkit::special_day('NEWYEAR'),
        rjd3toolkit::special_day('MAYDAY'), # 1st may
        rjd3toolkit::special_day('EASTERMONDAY'),
        rjd3toolkit::special_day('ASCENSION'),
        rjd3toolkit::special_day('WHITMONDAY'),
        rjd3toolkit::special_day('ASSUMPTION'),
        rjd3toolkit::special_day('ALLSAINTSDAY'), # Toussaint
        rjd3toolkit::special_day('ARMISTICE'), # End of 1st WW
        rjd3toolkit::special_day('CHRISTMAS')
    )
)
# Calendar regressor matrix
cal_reg <- rjd3toolkit::holidays(
    calendar = frenchCalendar,
    start = "1968-01-01",
    length = nrow(df),
    type = "All",
    nonworking = 7L
)

colnames(cal_reg) <- c(
    "14th_july",
    "8th_may",
    "1st_jan",
    "1st_may",
    "east_mon",
    "asc",
    "pen_mon",
    "15th_aug",
    "1st_nov",
    "11th_nov",
    "Xmas"
)

pre_pro <- fractionalAirlineEstimation(
    y = df$y,
    x = cal_reg,
    periods = 7, # weekly frequency
    outliers = c("ao", "wo"),
    log = FALSE,
    y_time = df$ds
)

amb.multi <- rjd3highfreq::multiAirlineDecomposition(
    y = pre_pro$model$linearized, # input time series
    periods = c(7, 365.2425), # 2 frequency
    log = FALSE,
    y_time = df$ds
)

t_rjdverse <- amb.multi$decomposition$t
sa_rjdverse <- amb.multi$decomposition$sa
s7_rjdverse <- amb.multi$decomposition$s_7
s365_rjdverse <- amb.multi$decomposition$s_365.2425
i_rjdverse <- amb.multi$decomposition$i


# prophet ----------------------------------------------------------------------

library("prophet")

df_holidays <- data.frame(
    holiday = rownames(cal_reg)[
        apply(cal_reg, MARGIN = 1, \(x) sum(x) > 0) |> which()
    ]
)

m <- prophet(
    df,
    weekly.seasonality = TRUE,
    yearly.seasonality = TRUE,
    fit = TRUE,
    holidays =
)

future <- make_future_dataframe(m, periods = 365)
head(future)
forecast <- predict(m, future)
head(forecast)
prophet_plot_components(m, forecast)

s7_prophet <- forecast$weekly
s365_prophet <- forecast$yearly
t_prophet <- forecast$trend


# forecast : tbats ------------------------------------------------------------

library("forecast")

fit2 <- tbats(
    births$births,
    seasonal.periods = c(7, 365.2425),
    use.trend = TRUE,
    use.arma.errors = TRUE
)

# Décomposition
components_tbats <- tbats.components(fit2)
components_tbats <- components_tbats |> as.data.frame()

s7_tbats <- components_tbats$season1
s365_tbats <- components_tbats$season2
t_tbats <- components_tbats$level
i_tbats <- components_tbats$slope


# forecast : mstl ----------------------------------------------------------

library("forecast")

# Créer une série ts avec saisonnalités journalière et annuelle
# On simule deux saisons : 7 et 365.25
y_msts <- msts(births$births, seasonal.periods = c(7, 365.2425))

# Décomposition
fit_mstl <- mstl(y_msts)

# Affichage
autoplot(fit_mstl)

components_mstl <- fit_mstl |> as.data.frame()

t_mstl <- components_mstl$Trend
s7_mstl <- components_mstl$Seasonal7
s365_mstl <- components_mstl$Seasonal365.24
i_mstl <- components_mstl$Remainder


# feast - STL -------------------------------------------------------

library(tidyverse)
library(tsibble)
library(feasts)
library(lubridate)

# Création d'un tsibble
df_ts <- df |>
    mutate(ds = as.Date(ds)) |>
    as_tsibble(index = ds)

# Décomposition STL automatique
fit_stl <- df_ts |>
    model(STL(y))
components_stl <- fit_stl |> components()
autoplot(components_stl)


t_stl <- components_stl$trend
s7_stl <- components_stl$season_week
s365_stl <- components_stl$season_year
i_stl <- components_stl$remainder
sa_stl <- components_stl$season_adjust


# fastTS ----------------------------------------------------
# WaveletETS ----------------------------------------------------
# dsa ----------------------------------------------------

# stats::decompose -----------------------------------------------------

# Extraire la série ts
y_ts <- ts(df_ts$y, frequency = 7)

# Décomposition classique
decomp_stl_stats_s7 <- decompose(y_ts, type = "additive")

y_s7 <- ts(df_ts$y - decomp_stl_stats_s7$seasonal, frequency = 365)
decomp_stl_stats_s365 <- decompose(y_s7, type = "additive")

t_stl_stats <- as.numeric(decomp_stl_stats_s365$trend)
s7_stl_stats <- as.numeric(decomp_stl_stats_s7$seasonal)
s365_stl_stats <- as.numeric(decomp_stl_stats_s365$seasonal)
i_stl_stats <- as.numeric(decomp_stl_stats_s365$random)
sa_stl_stats <- df_ts$y - s7_stl_stats - s365_stl_stats


# plot --------------------------------------------------------------------

# Chargement du package ggplot2
library(ggplot2)
library(plotly)

# Création d'un data.frame
df_s7 <- data.frame(
    date = df$ds,
    rjdverse = s7_rjdverse,
    prophet = s7_prophet[seq_len(19359)],
    tbats = s7_tbats,
    mstl = s7_mstl,
    stl = s7_stl,
    stl_stats = s7_stl_stats
) |>
    pivot_longer(cols = -date) |>
    mutate(type = "s7")

View(df_s7)

# Création d'un data.frame
df_s365 <- data.frame(
    date = df$ds,
    rjdverse = s365_rjdverse,
    prophet = s365_prophet[seq_len(19359)],
    tbats = s365_tbats,
    mstl = s365_mstl,
    stl = s365_stl,
    stl_stats = s365_stl_stats
) |>
    pivot_longer(cols = -date) |>
    mutate(type = "s365")

# Création d'un data.frame
df_t <- data.frame(
    date = df$ds,
    rjdverse = t_rjdverse,
    prophet = t_prophet[seq_len(19359)],
    tbats = t_tbats,
    mstl = t_mstl,
    stl = t_stl,
    stl_stats = t_stl_stats
) |>
    pivot_longer(cols = -date) |>
    mutate(type = "t")

df_final <- rbind(df_t, df_s7, df_s365)
df_final <- df_final |> filter(date > as.Date("2020-01-01"))

p <- df_final %>%
    ggplot(aes(x = date, y = value, color = name)) +
    geom_line() +
    facet_wrap(~type, scales = "free_y") +
    labs(
        title = "Décomposition temporelle",
        x = "Date",
        y = "Valeur",
        color = "Méthode"
    ) +
    theme_minimal()
ggplotly(p)
