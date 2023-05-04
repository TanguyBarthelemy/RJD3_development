
################################################################################
#######                   Test des prints en version 3                   ####### 
################################################################################

# Chargement des packages ------------------------------------------------------

library("rjd3toolkit")

library("rjd3tramoseats")
library("rjd3x13")
library("rjdemetra3")

library("rjd3sts")
library("rjd3highfreq")

# Options -----------------------------------------------------------------

options(enable_print_style = TRUE)

# Chargement fonctions de print ------------------------------------------------

path <- "./Code/Study_new_prints/print_functions/"
function2import <- list.files(path, full.names = TRUE)
sapply(X = function2import, FUN = source, encoding = "UTF-8") |> invisible()

# High-freq --------------------------------------------------------------------

## Import de données -----------------------------------------------------------
df_daily <- read.csv2("./data/TS_daily_births_franceM_1968_2020.csv") |> 
    dplyr::mutate(log_births = log(births))

## Création objets -------------------------------------------------------------

### PRE TREATMENT: fractional Airline model ------------------------------------

frenchCalendar <- national_calendar(days = list(
    fixed_day(7, 14), # Fete nationale
    fixed_day(5, 8, validity = list(start = "1982-05-08")), # Victoire 2nd guerre mondiale
    special_day('NEWYEAR'), # Nouvelle année
    special_day('CHRISTMAS'), # Noël
    special_day('MAYDAY'), # 1er mai
    special_day('EASTERMONDAY'), # Lundi de Pâques
    special_day('ASCENSION'), # attention +39 et pas 40 jeudi ascension
    special_day('WHITMONDAY'), # Lundi de Pentecôte (1/2 en 2005 a verif)
    special_day('ASSUMPTION'), # Assomption
    special_day('ALLSAINTSDAY'), # Toussaint
    special_day('ARMISTICE'))
)

q <- holidays(frenchCalendar, "1968-01-01", length = length(df_daily$births), type = "All", 
              nonworking = 7L)

pre.mult <-  fractionalAirlineEstimation_new(
    y = df_daily$log_births, 
    x = q, # q = regs de calendrier
    periods = 7, 
    ndiff = 2, ar = FALSE, mean = FALSE, 
    outliers = c("ao", "wo"), criticalValue = 0, 
    precision = 1e-9, approximateHessian = TRUE)

pre.mult_cal <-  fractionalAirlineEstimation_new(
    y = df_daily$log_births, 
    x = q, # q = regs de calendrier
    periods = c(7, 28), 
    ndiff = 2, ar = FALSE, mean = FALSE, 
    # outliers = c("ao", "wo"), criticalValue = 0, 
    precision = 1e-9, approximateHessian = TRUE)

print(pre.mult)
print(pre.mult_cal)
print_JDFractionalAirlineEstimation(pre.mult)
print_JDFractionalAirlineEstimation(pre.mult_cal)

### Extended X11 Decomposition -------------------------------------------------

x11.dow <- rjd3highfreq::x11(
    exp(pre.mult$model$linearized), 
    period = 7, # DOW pattern
    mul = TRUE, 
    trend.horizon = 9, # 1/2 Filter length : not too long vs p
    trend.degree = 3, # Polynomial degree
    trend.kernel = "Henderson", # Kernel function
    trend.asymmetric = "CutAndNormalize", # Truncation method
    seas.s0 = "S3X9", seas.s1 = "S3X9", # Seasonal filters
    extreme.lsig = 1.5, extreme.usig = 2.5)

x11.doy <- rjd3highfreq::x11(
    x11.dow$decomposition$sa, 
    period = 365.2425, # DOY pattern
    mul = TRUE, 
    trend.horizon = 371, 
    trend.degree = 3, 
    trend.kernel = "Henderson", 
    trend.asymmetric = "CutAndNormalize", 
    seas.s0 = "S3X3", seas.s1 = "S3X3", 
    extreme.lsig = 1.5, extreme.usig = 2.5)

print(x11.dow)
print(x11.doy)
print_JDX11(x11.dow)
print_JDX11(x11.doy)

### AMB Decomposition ----------------------------------------------------------

amb.dow <- fractionalAirlineDecomposition_new(
    df_daily$births, # input time series
    period = 7,                # DOW pattern
    sn = FALSE,                # Signal (SA)-noise decomposition 
    stde = FALSE,              # Calculate standard deviations
    nbcasts = 0, nfcasts = 0)  # Numbers of back- and forecasts

amb.doy <- fractionalAirlineDecomposition_new(
    amb.dow$decomposition$sa, # DOW-adjusted linearised data
    period = 365.2425, # DOY pattern
    sn = FALSE, 
    stde = FALSE, 
    nbcasts = 0, nfcasts = 0)

amb.multi <- multiAirlineDecomposition_new(
    pre.mult$model$linearized, # input time series
    periods = c(7, 365.2425), # DOW pattern
    ar = F, 
    stde = FALSE, # Calculate standard deviations
    nbcasts = 0, nfcasts = 0) 

print(amb.dow)
print(amb.doy)
print(amb.multi)
print_JDFractionalAirlineDecomposition(amb.dow)
print_JDFractionalAirlineDecomposition(amb.doy)
print_JDFractionalAirlineDecomposition(amb.multi)


# X13 --------------------------------------------------------------------------

# Classes de RJD3X13
# 
# JDSTS --> OK mais à actualisé depuis old repo
# 
# JD3_REGARIMA_SPEC --> fait ! (par Tanguy)
# JD3_REGARIMA_OUTPUT -- > OK
# 
# JD3X11 --> fait !
# 
# JD3_X11_SPEC --> fait !
# 
# JD3_X13_SPEC --> fait !
# JD3_X13_OUTPUT --> OK
# JD3_X13_RSLTS --> OK

## Création des objets ---------------------------------------------------------

serie_ipi <- read.csv("./data/IPI_nace4.csv", sep = ";")
y_raw <- ts(serie_ipi$RF3030, start = 1990, frequency = 12)
ud <- ts(serie_ipi$RF3512, start = 1990, frequency = 12)

## Prints ----------------------------------------------------------------------

# Classe JD3_REGARIMA_OUTPUT et JD3_REGARIMA_RSLTS
reg_v3 <- rjd3x13::regarima(y_raw, spec = "RG5C")
print(reg_v3) #JD3_REGARIMA_OUTPUT
print(reg_v3$result) # JD3_REGARIMA_RSLTS

# Classe JD3_REGARIMA_SPEC
sp <- spec_regarima("RG5C")

sp <- rjd3toolkit::add_outlier(sp, type = c("AO", "LS"), c("2015-01-01", "2010-01-01"))

sp <- set_outlier(sp, span.type = "BETWEEN", d0 = "2000-01-01", d1= "2015-01-01", n0 = 10, n1 = 20)

sp <- rjd3toolkit::set_transform(
    rjd3toolkit::set_tradingdays(
        rjd3toolkit::set_easter(sp, enabled = TRUE, duration = 450),
        option = "workingdays"
    ),
    fun = "None"
)



print(sp)
print_JD3_REGARIMA_SPEC(sp)

# Classe JD3_X11_SPEC
init_spec <- spec_x11()
new_spec <- set_x11(init_spec,
                    mode = "LogAdditive",
                    seasonal.comp = 1,
                    seasonal.filter = "S3X9",
                    henderson.filter = 7,
                    lsigma = 1.7,
                    usigma = 2.7,
                    fcasts = -1,
                    bcasts = -1,
                    calendar.sigma ="All",
                    sigma.vector = NA,
                    exclude.forecast = FALSE,
                    bias = "LEGACY")
print(init_spec)
print_JD3_X11_SPEC(init_spec)
print_JD3_X11_SPEC(new_spec)

# Classe JD3_X13_OUTPUT
sa_x13_v3 <- rjd3x13::x13(y_raw, spec = "RSA5")
print(sa_x13_v3)

# Classe JD3_X13_RSLTS
print(sa_x13_v3$result)

# Classe JD3_X13_SPEC
print(sa_x13_v3$estimation_spec)
print_JD3_X13_SPEC(sa_x13_v3$estimation_spec)

# Classe JD3X11
sa_x11_v3 <- rjd3x13::x11(ts = y_raw, spec = init_spec)
print(sa_x11_v3)
print_JD3X11(sa_x11_v3)


# Toolkit ----------------------------------------------------------------------

# Classes

# JD3_SARIMA --> ok
# JD3_ARIMA --> ok
# JD3_UCARIMA --> ok
# JD3_UCARIMA_WK --> à voir avec Anna
# 
# JD3_SARIMA_ESTIMATE --> ok
# summary.JD3_SARIMA_ESTIMATE --> à voir avec Anna
# JD3_SARIMA_ESTIMATION --> ok
# summary.JD3_SARIMA_ESTIMATION --> ok
# 
# JD3_REGARIMA_RSLTS --> ok
# summary.JD3_REGARIMA_RSLTS --> ok
# 
# JD3_CALENDAR --> ok + refait
# JD3_CALENDARDEFINITION --> à voir avec Anna
# JD3_WEIGHTEDCALENDAR --> fait
# JD3_CHAINEDCALENDAR --> fait
# 
# JD3_FIXEDDAY --> ok
# JD3_HOLIDAY --> à voir avec Anna
# JD3_FIXEDWEEKDAY --> ok
# JD3_EASTERDAY --> ok
# JD3_SINGLEDAY --> ok
# JD3_SPECIALDAY --> ok
# 
# JD3_SADECOMPOSITION --> ok
# 
# summary.JD3_LIKELIHOOD --> ok
# 
# JD3_SPAN --> ok
# logLik --> ok (stats)
# 
# JD3_TEST --> ok
# JD3 --> à voir avec Anna
#
# jd3_utilities : qu'est ce que c'est ?
# JD3_TSMONIKER
# JD3_DYNAMICTS
# 
# OBJ <- JD3_Object
# RSLT <- JD3_ProcResults
# ??? subclasses
#

# Classe JD3_UCARIMA_WK ??
model <- sarima_model(period = 12, d = 1, bd = 1, theta = -0.6, btheta = -0.5)
ucm <- sarima_decompose(model)
out <- ucarima_wk(ucm, cmp = 1)

# Classe summary.JD3_SARIMA_ESTIMATE ?? Déjà rempli par summary.JD3_REGARIMA_RSLTS
sarima1 <- sarima_estimate(y_raw, order = c(0, 1, 1), seasonal = c(0, 1, 1))
sum_sar <- sarima1 |> summary()

# Classe JD3_CALENDARDEFINITION ??
# Classe JD3_CALENDAR
frenchCalendar <- national_calendar(days = list(
    fixed_day(7, 14), # Fete nationale
    fixed_day(5, 8, validity = list(start = "1982-05-08")), # Victoire 2nd guerre mondiale
    special_day('NEWYEAR'), # Nouvelle année
    special_day('CHRISTMAS'), # Noël
    special_day('MAYDAY'), # 1er mai
    special_day('EASTERMONDAY'), # Lundi de Pâques
    special_day('ASCENSION'), # attention +39 et pas 40 jeudi ascension
    special_day('WHITMONDAY'), # Lundi de Pentecôte (1/2 en 2005 a verif)
    special_day('ASSUMPTION'), # Assomption
    special_day('ALLSAINTSDAY'), # Toussaint
    special_day('ARMISTICE'))
)
print(frenchCalendar)
print_JD3_CALENDAR(frenchCalendar)

# Classe JD3_WEIGHTEDCALENDAR
weighted_cal <- weighted_calendar(list(frenchCalendar, frenchCalendar), c(0.5, 0.5))
print(weighted_cal)
print_JD3_WEIGHTEDCALENDAR(weighted_cal)

# Classe JD3_CHAINEDCALENDAR
final_cal <- chained_calendar(frenchCalendar, weighted_cal, break_date = "2005-05-01")
print(final_cal)
print_JD3_CHAINEDCALENDAR(final_cal)


# Tramo seats ------------------------------------------------------------------

# Classe
# 
# JDSTS Ok mais pas actualisé
# JD3 --> à voir avec Anna
# JD3_TRAMO_OUTPUT OK
# JD3_SEATS
# JD3_TRAMOSEATS_OUTPUT OK
# JD3_TRAMOSEATS_RSLTS Ok
# 
# JD3_TRAMO_SPEC
# JD3_SEATS_SPEC
# JD3_TRAMOSEATS_SPEC
# 

# Classe JD3_SEATS
sa_ts_v3 <- rjd3tramoseats::tramoseats(y_raw, spec = "RSAfull")
obj_seats <- sa_ts_v3$result$decomposition
print(obj_seats)
print_JD3_SEATS(obj_seats)

# Classe JD3_TRAMO_SPEC
tramo_spec <- sa_ts_v3$estimation_spec$tramo
print(tramo_spec)
print_JD3_TRAMO_SPEC(tramo_spec)

# Classe JD3_SEATS_SPEC
seats_spec <- sa_ts_v3$estimation_spec$seats
print(seats_spec)
print_JD3_SEATS_SPEC(seats_spec)

# Classe JD3_TRAMOSEATS_SPEC
ts_spec <- sa_ts_v3$estimation_spec
print(ts_spec)
print_JD3_TRAMOSEATS_SPEC(ts_spec)
















