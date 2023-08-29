
## Chargement des packages -----------------------------------------------------

# library("dplyr")
# library("ggplot2")
# library("kableExtra")
library("rjd3highfreq")


# Paramètres --------------------------------------------------------------

col_bg <- "#f5f4e7"
col_grid <-"#dadad3"
col_y <- "#f1ba1d"
col_t <- "#1e6c0b"
col_sa <- "#00488c"

col_s1 <- "#ffab78"
col_s2 <- "#9169be"


## Import des données ----------------------------------------------------------

# df_daily <- read.csv2("https://raw.githubusercontent.com/TanguyBarthelemy/Tsace_RJD_Webinar_Dec22/b5fcf6b14ae47393554950547ef4788a0068a0f6/Data/TS_daily_births_franceM_1968_2020.csv")
df_daily <- read.csv2("./data/TS_daily_births_franceM_1968_2020.csv") |> 
    dplyr::mutate(
        log_births = log(births), 
        date = as.Date(date))

zoom_1_month <- which(
    df_daily$date >= "2019-01-01" 
    & df_daily$date <= "2019-01-31")

zoom_1_year <- which(
    df_daily$date >= "2018-01-01" 
    & df_daily$date <= "2018-12-31")

zoom_3_years <- which(
    df_daily$date >= "2018-01-01" 
    & df_daily$date <= "2020-12-31")



# Plot de la série initiale ----------------------------------------------------

plot.new()
rect(xleft = par("usr")[1], xright = par("usr")[2], 
     ytop = par("usr")[4], ybottom = par("usr")[3], col = col_bg)
par(new = TRUE)

plot(y = df_daily$births, 
     x = df_daily$date, col = col_y, 
     type = "l", xlab = "Time", ylab = "Nb of french births", 
     main = "Raw data")

par(new = TRUE)
grid(nx = NULL, ny = NULL, col = col_grid)
box(col = col_grid)


## Préparation du calendrier ---------------------------------------------------

# French calendar
frenchCalendar <- rjd3toolkit::national_calendar(days = list(
    rjd3toolkit::fixed_day(7, 14), # Bastille Day
    rjd3toolkit::fixed_day(5, 8, validity = list(start = "1982-05-08")), # End of 2nd WW
    rjd3toolkit::special_day('NEWYEAR'), # Nouvelle année
    rjd3toolkit::special_day('MAYDAY'), # 1st may
    rjd3toolkit::special_day('EASTERMONDAY'), # Lundi de Pâques
    rjd3toolkit::special_day('ASCENSION'), # Jeudi ascension
    rjd3toolkit::special_day('WHITMONDAY'), # Lundi de Pentecôte
    rjd3toolkit::special_day('ASSUMPTION'), # Assomption
    rjd3toolkit::special_day('ALLSAINTSDAY'), # Toussaint
    rjd3toolkit::special_day('ARMISTICE'), # End of 1st WW
    rjd3toolkit::special_day('CHRISTMAS')) # Noël
)

# Calendar regressor matrix
cal_reg <- rjd3toolkit::holidays(
    calendar = frenchCalendar, 
    "1968-01-01", length = nrow(df_daily), type = "All", nonworking = 7L)
colnames(cal_reg) <- c("14th_july", "8th_may", "1st_jan", "1st_may", 
                 "east_mon", "asc", "pen_mon", 
                 "15th_aug", "1st_nov", "11th_nov", "Xmas")


## Pré-ajustement --------------------------------------------------------------

pre_pro <- fractionalAirlineEstimation(
    y = df_daily$births, 
    x = cal_reg, 
    periods = 7, # weekly frequency
    outliers = c("ao", "wo"), log = TRUE, y_time = df_daily$date)

print(pre_pro)

y_lin <- pre_pro$model$linearized

## Plot of linearised series ---------------------------------------------------

# Plot of the first year
# Adjusted with outliers and calendar regressors

plot.new()
rect(xleft = par("usr")[1], xright = par("usr")[2], 
     ytop = par("usr")[4], ybottom = par("usr")[3], col = col_bg)
par(new = TRUE)

plot(y = df_daily$births[zoom_1_year], 
     x = df_daily$date[zoom_1_year], col = col_y, 
     type = "l", xlab = "Time", ylab = "Nb of french births")
lines(y = exp(y_lin)[zoom_1_year], 
      x = df_daily$date[zoom_1_year], col = col_t)

par(new = TRUE)
grid(nx = NULL, ny = NULL, col = col_grid)
box(col = col_grid)

legend("bottomleft", legend = c("Raw data", "Linearised series"), 
       pch = 16, col = c(col_y, col_t), horiz = TRUE, xpd = TRUE, 
       inset = c(0, 1), bty = "n")

## Code rjd3x11plus --> x11plus() ----------------------------------------------
# Pour la décomposition x11 sous haute frequence, voir le package rjd3x11plus
## graphique de x11plus() avec ggplot2 -----------------------------------------
## graphique zoomé -------------------------------------------------------------


## Code AMB --------------------------------------------------------------------

# Decomposition with day of the week
amb.dow <- rjd3highfreq::fractionalAirlineDecomposition(
    y = y_lin, # input time series
    period = 7, # weekly decomposition
    log = TRUE, y_time = df_daily$date) 

# Extract DOY pattern from DOW-adjusted linearised data
# step 2 en log
amb.doy <- rjd3highfreq::fractionalAirlineDecomposition(
    y = amb.dow$decomposition$sa, # DOW-adjusted linearised data
    period = 365.2425, # day of year pattern
    log = TRUE, y_time = df_daily$date) 

amb.s7 <- exp(amb.dow$decomposition$s)
amb.s365 <- exp(amb.doy$decomposition$s)
amb.t <- exp(amb.doy$decomposition$t)
amb.sa <- exp(amb.doy$decomposition$sa)
amb.i <- exp(amb.doy$decomposition$i)

## Plot AMB --------------------------------------------------------------------

### Plot seasonal pattern ------------------------------------------------------

plot.new()
rect(xleft = par("usr")[1], xright = par("usr")[2], 
     ytop = par("usr")[4], ybottom = par("usr")[3], col = col_bg)
par(new = TRUE)

plot(x = df_daily$date, 
     y = amb.s7, col = col_s1, 
     type = "l", xlab = "Time", ylab = "Seasonal component")
lines(x = df_daily$date, y = amb.s365, col = col_s2)

par(new = TRUE)
grid(nx = NULL, ny = NULL, col = col_grid)
box(col = col_grid)

legend("bottomleft", legend = c("p = 7", "p = 365"), 
       pch = 16, col = c(col_s1, col_s2), horiz = TRUE, xpd = TRUE, 
       inset = c(0, 1), bty = "n")

### Plot seasonal pattern (zoom) -----------------------------------------------

plot.new()
rect(xleft = par("usr")[1], xright = par("usr")[2], 
     ytop = par("usr")[4], ybottom = par("usr")[3], col = col_bg)
par(new = TRUE)

plot(x = df_daily$date[zoom_1_year], 
     y = amb.s7[zoom_1_year], col = col_s1, 
     type = "l", xlab = "Time", ylab = "Seasonal component")
lines(x = df_daily$date[zoom_1_year], 
      y = amb.s365[zoom_1_year], col = col_s2)

par(new = TRUE)
grid(nx = NULL, ny = NULL, col = col_grid)
box(col = col_grid)

legend("bottomleft", legend = c("p = 7", "p = 365"), 
       pch = 16, col = c(col_s1, col_s2), horiz = TRUE, xpd = TRUE, 
       inset = c(0, 1), bty = "n")


### Plot decomposition ---------------------------------------------------------

plot.new()
rect(xleft = par("usr")[1], xright = par("usr")[2], 
     ytop = par("usr")[4], ybottom = par("usr")[3], col = col_bg)
par(new = TRUE)

plot(x = df_daily$date, y = df_daily$births, col = col_y, 
     type = "l", xlab = "Time", ylab = "Nb of french births", 
     main = "Decomposition")
lines(x = df_daily$date, y = amb.sa, 
      type = "l", col = col_sa)
lines(x = df_daily$date, y = amb.t, 
      type = "l", col = col_t)

par(new = TRUE)
grid(nx = NULL, ny = NULL, col = col_grid)
box(col = col_grid)

legend("bottomleft", legend = c("Raw data", "Seasonnal adjusted", "Trend"), 
       pch = 16, col = c(col_y, col_sa, col_t), horiz = TRUE, xpd = TRUE, 
       inset = c(0, 1), bty = "n")


## AMB Multi -------------------------------------------------------------------

amb.multi <- rjd3highfreq::multiAirlineDecomposition(
    y = y_lin, # input time series
    periods = c(7, 365.2425), # 2 frequency
    log = TRUE, y_time = df_daily$date)

amb.multi.t <- exp(amb.multi$decomposition$t)
amb.multi.s7 <- exp(amb.multi$decomposition$s_7)
amb.multi.s365 <- exp(amb.multi$decomposition$s_365.2425)
amb.multi.sa <- exp(amb.multi$decomposition$sa)
amb.multi.i <- exp(amb.multi$decomposition$i)


## Comparaison des composantes saisonnières ------------------------------------

# P = 365
plot.new()
rect(xleft = par("usr")[1], xright = par("usr")[2], 
     ytop = par("usr")[4], ybottom = par("usr")[3], col = col_bg)
par(new = TRUE)

plot(x = df_daily$date[zoom_3_years], 
     y = amb.multi.s365[zoom_3_years], 
     type = "l", xlab = "Time", ylab = "Seasonal component p = 365", 
     col = col_s1, main = "Annual seasonality")
lines(x = df_daily$date[zoom_3_years], 
      y = amb.s365[zoom_3_years], 
      type = "l", col = col_s2)

par(new = TRUE)
grid(nx = NULL, ny = NULL, col = col_grid)
box(col = col_grid)

legend("bottomleft", legend = c("Simple estimation", "Multi Airline"), 
       pch = 16, col = c(col_s1, col_s2), horiz = TRUE, xpd = TRUE, 
       inset = c(0, 1), bty = "n")

#P = 7

plot.new()
rect(xleft = par("usr")[1], xright = par("usr")[2], 
     ytop = par("usr")[4], ybottom = par("usr")[3], col = col_bg)
par(new = TRUE)

plot(x = df_daily$date[zoom_1_month], 
     y = amb.multi.s7[zoom_1_month], 
     type = "l", xlab = "Time", ylab = "Seasonal component p = 7", 
     col = col_s1, main = "Weekly seasonality")
lines(x = df_daily$date[zoom_1_month], 
      y = amb.s7[zoom_1_month], 
      type = "l", col = col_s2)

par(new = TRUE)
grid(nx = NULL, ny = NULL, col = col_grid)
box(col = col_grid)

legend("bottomleft", legend = c("Simple estimation", "Multi Airline"), 
       pch = 16, col = c(col_s1, col_s2), horiz = TRUE, xpd = TRUE, 
       inset = c(0, 1), bty = "n")




