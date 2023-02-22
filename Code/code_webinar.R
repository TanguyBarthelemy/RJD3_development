

# Code du webinar ---------------------------------------------------------


# P1 : Introduction -------------------------------------------------------

install.packages("RJDemetra")
library("RJDemetra")
# install.packages("remotes")

remotes::install_github("palatej/rjdemetra3")

remotes::install_github("palatej/rjd3toolkit", ref = "v0.6.0")

remotes::install_github("palatej/rjd3modelling", ref = "v0.6.0")
remotes::install_github("palatej/rjd3arima", ref = "v0.6.0")
remotes::install_github("palatej/rjd3sa", ref = "v0.6.0")

remotes::install_github("palatej/rjd3tramoseats", ref = "v0.6.0")
remotes::install_github("palatej/rjd3x13", ref = "v0.6.0")
remotes::install_github("palatej/rjdemetra3", ref = "v0.6.0")

remotes::install_github("palatej/rjd3sts", ref = "v0.5.0")
remotes::install_github("palatej/rjd3highfreq", ref = "v0.5.0")
remotes::install_github("palatej/rjd3stl", ref = "v0.5.0")
remotes::install_github("palatej/rjd3bench", ref = "v0.5.0")
# options : INSTALL_opts = "--no-multiarch"

remotes::install_github("AQLT/ggdemetra3") #additional graphics 


# P2 : Seasonal adjustment in R with JD+ ----------------------------------

ipi <- read.csv2("../Data/IPI_nace4.csv")
ipi$date <- as.Date(ipi$date, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)
# creating a TS object from a data frame 
y_raw <- ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(2019, 6))
y_new <- ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(2019, 9))


# X13 v2
sa_x13_v2 <- RJDemetra::x13(y_raw, spec = "RSA5c")
# see help pages for default spec names, identical in v2 and v3
#Tramo-Seats
sa_ts_v2 <- RJDemetra::tramoseats(y_raw, spec = "RSAfull")


#X13 v3
sa_x13_v3 <- rjd3x13::x13(y_raw, spec = "RSA5")
sa_x13_v3

#Tramo seats
sa_ts_v3 <- rjd3tramoseats::tramoseats(y_raw, spec = "RSAfull")



# Reg-Arima part from X13 only (different default spec names, cf help pages)
regA_v2 <- RJDemetra::regarima_x13(y_raw, spec = "RG5c")

# Tramo only 
tramo_v2 <- RJDemetra::regarima_tramoseats(y_raw,spec = "TRfull")

#X13
sa_regarima_v3 <- rjd3x13::regarima(y_raw, spec = "RG5c")

#Tramo seats 
#sa_tramo_v3 <- rjd3tramoseats::tramo(y_raw, spec = "TRfull")

# "fast." versions...(just results, cf output structure)


# X11 (spec option)
X11_v2 <- RJDemetra::x13(y_raw, spec = "X11")

#Tramo-Seats ? you 
#sa_ts_v2<-RJDemetra::tramoseats(y_raw, spec = "RSAfull")

#X11
x11_v3 <- rjd3x13::x11(y_raw) # specific function
#Seats: you need an arima model


# v2 "output"
Model_sa <- RJDemetra::x13(y_raw, spec = "RSA5")

Model_sa$regarima
Model_sa$decomposition
#...

# Model_sa = sa_x13_v3
sa_x13_v3 <- RJDemetra::x13(y_raw, spec = "RSA5")
sa_x13_v3$result
sa_x13_v3$estimation_spec
sa_x13_v3$result_spec
sa_x13_v3$user_defined

# Version 2 : display of Main Results table (from GUI) 
sa_x13_v2$final$series #y, sa,t,s,i
sa_x13_v2$final$forecasts

# Version 3 
# final seasonally adjusted series
sa_x13_v3$result$final$d11final


# Version 2 
sa_x13_v2$regarima$model$effects #MTS object

# forecast accessible only via user defined output (cf below)

# Version 3: "x11 names" : preadjustement effets as stored in the A table
# add doc on names   
sa_x13_v3$result$preadjust$a6


# Version 3  
sa_x13_v3$result$decomposition$d5 # tables from D1 to D13


# Version 2 
print(sa_x13_v2)
sa_x13_v2$decomposition$mstats
sa_x13_v2$decomposition$s_filter
sa_x13_v2$decomposition$t_filter

# version 3 (more diagnostics available by default)
print(sa_x13_v2)
sa_x13_v3$result$diagnostics$td.ftest.i

# Version 2 
user_defined_variables("X13-ARIMA") 
user_defined_variables("TRAMO-SEATS")

# Version 3: more specific functions 
userdefined_variables_tramoseats("tramoseats")
userdefined_variables_tramoseats("tramo") # restriction

userdefined_variables_x13("regarima") #restriction
userdefined_variables_x13()


# version 3
ud <- userdefined_variables_x13()[15:17] # b series
ud
sa_x13_v3_UD <- rjd3x13::x13(y_raw, "RSA5c", userdefined = ud)
sa_x13_v3_UD$user_defined # remainder of the names 
# retrieve the object 
sa_x13_v3_UD$user_defined$decomposition.b1


# Version 2
# for class 'final' : 2 types 
plot(sa_x13_v2, type_chart = "sa-trend", first_date = c(2015, 1))
#plot(sa_x13_v2, type = "cal-seas-irr", first_date = c(2015, 1))


# regarima
layout(matrix(1:6, 3, 2))
plot(sa_x13_v2$regarima, ask = FALSE)
# Plotting SI ratios  
plot(sa_x13_v2$decomposition, first_date = c(2015, 1))

# Plotting SI ratios  
plot(sa_x13_v2$decomposition, first_date = c(2015, 1))


# version 3
# remotes::install_github("AQLT/ggdemetra3", INSTALL_opts = "--no-multiarch")
library("ggdemetra3")
ggdemetra3::siratioplot(sa_x13_v3)

# version 3
ggdemetra3::ggsiratioplot(sa_x13_v3)

# version 3
ggplot2::autoplot(sa_x13_v3)

# version 2
# changing estimation span, imposing additive model and
#adding user defined ouliers 
# first create a new spec modifying the previous one 
spec_1 <- x13_spec(sa_x13_v2)
spec_2 <- x13_spec(spec_1, estimate.from = "2004-01-01",
                   usrdef.outliersEnabled = TRUE,
                   usrdef.outliersType = c("LS", "AO"),
                   usrdef.outliersDate = c("2008-10-01", "2018-01-01"),
                   transform.function = "None") # additive model
# here the reg-arima model will be estimated from  "2004-01-01" 
# the decomposition will be run on the whole span 

# new sa processing
sa_x13_v2_2 <- RJDemetra::x13(y_raw, spec_2)
sa_x13_v2_2$final$series

# start with default spec 
spec_1 <- spec_x13_default("RSA3")
# or start with existing spec (no extraction function needed)
spec_1 <- sa_x13_v3_UD$estimation_spec

# set a new spec
## add outliers 
spec_2 <- rjd3modelling::add_outlier(spec_1,
                                     type = c("AO"), c("2015-01-01", "2010-01-01"))
## set trading days
spec_2 <- rjd3modelling::set_tradingdays(spec_2,
                                         option = "workingdays" )
# set x11 options 
spec_2 <- set_x11(spec_2, henderson.filter = 13)
# apply with `fast.x13` (results only)
fast.x13(y, spec_2)

# defining user defined trading days 
spec_td <- x13_spec(spec_1,
                    tradingdays.option = "UserDefined",
                    tradingdays.test ="None",
                    usrdef.varEnabled = TRUE,
                    # the user defined variable will be assigned to the calendar component
                    usrdef.varType="Calendar",
                    usrdef.var=td_regs ) # regressors have to be a single or multiple TS 
# new sa processing
sa_x13_v2_4 <- x13(y_raw, spec_td)
# user defined intervention variable  
spec_int <- x13_spec(spec_1,
                     usrdef.varEnabled = TRUE,
                     # the user defined variable will be assigned to the trend component
                     usrdef.varType = "Trend",
                     usrdef.var = x ) # x has to to be a single or multiple TS 
# new sa processing
sa_x13_v2_5 <- x13(y_raw, spec_int)


# define a user defined trading days regressor 
td_reg1 <- rjd3modelling::td(12, start = start(y_raw), length = length(y_raw), groups = c(1, 1, 1, 1, 1, 0, 0))

# define a context
my_context <- rjd3modelling::modelling_context(variables = list(a = xvar))

# set a new specification from a default specification
spec_td <- rjd3x13::spec_regarima_default(name = "rg3") |>
  rjd3modelling::add_usrdefvar(id = "r.a")

# new reg-arima estimation
reg_a_estimation <- rjd3x13::regarima(window(ts, start = 1985, end = 2013), spec_td, context = my_context)


sa_x13_v3$estimation_spec$regarima$arima
sa_x13_v3$result_spec$regarima$arima


current_result_spec <- sa_x13_v3$result_spec
current_domain_spec <- sa_x13_v3$estimation_spec

# generate NEW spec for refresh 
refreshed_spec <- x13.refresh(current_result_spec, # point spec to be refreshed
                              current_domain_spec, #domain spec (set of constraints)
                              policy = "Outliers",
                              period = 12, # monthly series
                              start = "2017-01-01",
                              end = NULL)

# apply the new spec on new data : y_new= y_raw + 1 month

sa_x13_v3_refresh <- x13(y_new, refreshed_spec)


rjd3highfreq::fractionalAirlineEstimation
(df_daily$log_births, # here series in log
  x = q, # q= calendar
  periods = 7, # approx  c(7,365.25)
  ndiff = 2, ar = FALSE, mean = FALSE,
  outliers = c("ao","wo","LS"), 
  # WO compensation
  criticalValue = 0, # computed in the algorithm
  precision = 1e-9, approximateHessian = TRUE)

# calendar regressors can be defined with the rjd3modelling package 

#step 1: p=7
x11.dow <- rjd3highfreq::x11(exp(pre.mult$model$linearized),
                             period = 7,                 # DOW pattern
                             mul = TRUE,                              
                             trend.horizon = 9,  # 1/2 Filter length : not too long vs p
                             trend.degree = 3,                         # Polynomial degree
                             trend.kernel = "Henderson",               # Kernel function
                             trend.asymmetric = "CutAndNormalize",     # Truncation method
                             seas.s0 = "S3X9", seas.s1 = "S3X9",       # Seasonal filters
                             extreme.lsig = 1.5, extreme.usig = 2.5)   # Sigma-limits
#step 2: p=365.25
x11.doy <- rjd3highfreq::x11(x11.dow$decomposition$sa,  # previous sa
                             period = 365.2425,         # DOY pattern
                             mul = TRUE) #other parameters skipped here


#step 1: p=7
#step 2: p=365.25
amb.doy <- rjd3highfreq::fractionalAirlineDecomposition(
  amb.dow$decomposition$sa,  # DOW-adjusted linearised data
  period = 365.2425,         # DOY pattern
  sn = FALSE,                # Signal (SA)-noise decomposition 
  stde = FALSE,              # Compute standard deviations
  nbcasts = 0, nfcasts = 0)  # Numbers of back- and forecasts

library("rjd3modelling")
# French
fr_cal <- calendar.new()
calendar.holiday(fr_cal, "NEWYEAR")
calendar.holiday(fr_cal, "EASTERMONDAY")
calendar.holiday(fr_cal, "MAYDAY")
calendar.fixedday(fr_cal, month = 5, day = 8,
                  start = "1982-01-01")
# calendar.holiday(fr_cal, "WHITMONDAY") # Equivalent to:
calendar.easter(fr_cal, offset = 61)

calendar.fixedday(fr_cal, month = 7, day = 14)
# calendar.holiday(fr_cal, "ASSUMPTION")
calendar.easter(fr_cal, offset = 61)
calendar.holiday(fr_cal, "ALLSAINTSDAY")
calendar.holiday(fr_cal, "ARMISTICE")
calendar.holiday(fr_cal, "CHRISTMAS")


holidays(fr_cal, "2020-12-24", 10,single = T)
s <- ts(0, start = 2020, end = c(2020, 11), frequency = 12)
# Trading-days regressors (each day has a different effect, sunday as contrasts)
td_reg <- htd(fr_cal, s = s, groups = c(1, 2, 3, 4, 5, 6, 0))
# Working-days regressors (Monday = ... = Friday; Saturday = Sunday = contrasts)
wd_reg <- htd(fr_cal, s = s, groups = c(1, 1, 1, 1, 1, 0, 0))
# Monday = ... = Friday; Saturday; Sunday = contrasts
wd_reg <- htd(fr_cal, s = s, groups = c(1, 1, 1, 1, 1, 2, 0))
wd_reg
# Monday = ... = Wednesday; Thursday; Friday = contrasts
wd_reg2 <- htd(fr_cal, s = s, groups = c(1, 1, 1, 2, 0, 1, 1))
wd_reg2

s <- ts(0, start = 2000, end = 2005, frequency = 12)
ao <- ao.variable(s = s, date = "2001-03-01")
ls <- ls.variable(s = s, date = "2001-01-01")
tc <- tc.variable(s = s, date = "2001-01-01", rate = 0.7) # Customizable rate
so <- so.variable(s = s, date = "2003-05-01")
ramp <- ramp.variable(s = s, range = c("2001-01-01","2001-12-01"))
plot(ts.union(ao, ls, tc, so, ramp), plot.type = "single",
     col = c("red","lightgreen","orange","blue","black"))

# JD+
print(system.time(
  for (i in 1:1000) {  
    j <- rjd3modelling::sarima.estimate(
      data = log(rjd3toolkit::ABS$X0.2.09.10.M), 
      order = c(2, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
  }))
#       user    system        elapsed (in seconds) 
#      4.98        0.37        4.63 

#R-native
print(system.time(
  for (i in 1:1000) {  
    r <- arima(
      x = log(rjd3toolkit::ABS$X0.2.09.10.M), 
      order = c(2, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
  }))
#       user    system        elapsed (in seconds) 
#     158.74        0.23      160.49 

print(j$likelihood )
print(r)



# P3 : Wrangling workspaces ----------------------------------------------------

# If devtools is not installed
# install.packages("devtools")
library("devtools")

install.packages("RJDemetra")
install_github("https://github.com/palatej/rjdemetra3")
install_github("https://github.com/InseeFrLab/rjdworkspace")

ws <- RJDemetra::load_workspace("./WS_input/WS_simple.xml")
RJDemetra::compute(ws)

mp_1 <- RJDemetra::get_object(ws, pos = 1)
sa_item_1 <- RJDemetra::get_object(mp_1, pos = 1)
model_sa_1 <- RJDemetra::get_model(sa_item_1, workspace = ws)

ws <- rjdemetra3::load_workspace("./WS_input/WS_simple.xml")

# Data preparation
raw_data <- read.csv2("./data/raw_data.csv", dec = ".") |> 
  ts(start = 1990, frequency = 12)

# Create WS
ws <- RJDemetra::new_workspace()
mp_1 <- RJDemetra::new_multiprocessing(workspace = ws, 
                                       name = "SAProcessing-1")
spec_x13 <- RJDemetra::x13_spec(spec = "RSA3")
model_sa_1 <- RJDemetra::x13(raw_data, spec = spec_x13)



# P4 : Production in R ----------------------------------------------------

# choose the demetra_m.csv file generated by the cruncher
QR <- extract_QR("../Output/SA")
QR

?compute_score # to see how the score is calculated (formula)
QR <- compute_score(QR, n_contrib_score = 3)

QR

QR <- sort(QR, decreasing = TRUE, sort_variables = "score")
export_xlsx(QR, file_name = "U:/quality_report.xls")


# oos_mse weight reduced to 1 when the other 
# indicators are "Bad" ou "Severe"
condition1 <- list(indicator = "oos_mse",
                   conditions = c("residuals_independency",
                                  "residuals_homoskedasticity",
                                  "residuals_normality"),
                   conditions_modalities = c("Bad","Severe"))

BQ <- compute_score(BQ, n_contrib_score = 5,
                    conditional_indicator = list(condition1),
                    na.rm = TRUE)
