library(rjd3toolkit)
library(rjd3x13)
library(rjd3tramoseats)
library(RJDemetra)

# Code du webinar ---------------------------------------------------------

# P2 : Seasonal adjustment in R with JD+ ----------------------------------

ipi <- read.csv2("C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Data/IPI_nace4.csv")
ipi$date <- as.Date(ipi$date, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)
# creating a TS object from a data frame 
y_raw <- ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(2019, 6))
y_new <- ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(2019, 9))


# X13 v2
sa_x13_v2 <- RJDemetra::x13(y_raw, spec = "X11")
sa_x13_v2 # Tout est implémenté (jusqu'à un certain niveau)

print(sa_x13_v2)
plot(sa_x13_v2)

# see help pages for default spec names, identical in v2 and v3
#Tramo-Seats
sa_ts_v2 <- RJDemetra::tramoseats(y_raw, spec = "RSAfull")
sa_ts_v2 # Tout est implémenté (jusqu'à un certain niveau)

#X13 v3
sa_x13_v3 <- rjd3x13::x13(y_raw, spec = "RSA5")
sa_x13_v3 # print à compléter ?

sa_x13_v3$user_defined

# sa_x13_v3$result
# 
# sa_x13_v3$result$preprocessing
# sa_x13_v3$result$preadjust # Pas de class
# sa_x13_v3$result$decomposition # Pas de print
# sa_x13_v3$result$final # Pas de class
# sa_x13_v3$result$mstats # Pas de class
# sa_x13_v3$result$diagnostics # Pas de class
# 
# sa_x13_v3$estimation_spec # Pas de print
# 
# sa_x13_v3$estimation_spec$regarima # Pas de print
# # Pour la partie regarima, il existe plusieurs class :
# # - JD3_REGARIMA_SPEC # Pas de print
# # - JD3_REGARIMA_RSLTS # Pas de print
# # - JD3_regarima_output --> JD3_REGARIMA_OUTPUT
# 
# # Voir l'intérieur si il faut pas trop re-définir
# 
# 
# sa_x13_v3$estimation_spec$x11 # Pas de print
# sa_x13_v3$estimation_spec$benchmarking # Pas de class
# 
# sa_x13_v3$result_spec # Pareil que pour estimation_spec (recursivement)
# 
# sa_x13_v3$user_defined
# 
# 
# #Tramo seats
# sa_ts_v3 <- rjd3tramoseats::tramoseats(y_raw, spec = "RSAfull")
# 
# sa_ts_v3$result
# 
# sa_ts_v3$result$preprocessing
# sa_ts_v3$result$decomposition # Pas de print
# sa_ts_v3$result$final # Pas de class
# sa_ts_v3$result$diagnostics # Pas de class
# 
# sa_ts_v3$estimation_spec # Pas de print
# 
# sa_ts_v3$estimation_spec$tramo # Pas de print
# sa_ts_v3$estimation_spec$seats # Pas de print
# sa_ts_v3$estimation_spec$benchmarking # Pas de class
# 
# sa_ts_v3$result_spec # Pareil que pour estimation_spec (recursivement)
# 
# sa_ts_v3$user_defined
# 
# 
# 
# # Reg-Arima part from X13 only (different default spec names, cf help pages)
# regA_v2 <- RJDemetra::regarima_x13(y_raw, spec = "RG5c")
# regA_v2
# 
# # Tramo only 
# tramo_v2 <- RJDemetra::regarima_tramoseats(y_raw,spec = "TRfull")
# tramo_v2
# 
# #X13
# sa_regarima_v3 <- rjd3x13::regarima(y_raw, spec = "RG5c")
# # Tout est inclus dans sa_x13_v3
# 
# 
# #Tramo seats 
# #sa_tramo_v3 <- rjd3tramoseats::tramo(y_raw, spec = "TRfull")
# 
# # "fast." versions...(just results, cf output structure)
# 
# 
# # X11 (spec option)
# X11_v2 <- RJDemetra::x13(y_raw, spec = "X11")
# X11_v2
# 
# #Tramo-Seats ? you 
# #sa_ts_v2<-RJDemetra::tramoseats(y_raw, spec = "RSAfull")
# 
# #X11
# x11_v3 <- rjd3x13::x11(y_raw) # specific function
# #Seats: you need an arima model
# x11_v3
# 
# # v2 "output"
# Model_sa <- RJDemetra::x13(y_raw, spec = "RSA5")
# 
# Model_sa$regarima
# Model_sa$decomposition
# #...
# 
# # Model_sa = sa_x13_v3
# sa_x13_v3 <- RJDemetra::x13(y_raw, spec = "RSA5")
# sa_x13_v3$result
# sa_x13_v3$estimation_spec
# sa_x13_v3$result_spec
# sa_x13_v3$user_defined
# 
# # Version 2 : display of Main Results table (from GUI) 
# sa_x13_v2$final$series #y, sa,t,s,i
# sa_x13_v2$final$forecasts
# 
# # Version 3 
# # final seasonally adjusted series
# sa_x13_v3$result$final$d11final
# 
# 
# # Version 2 
# sa_x13_v2$regarima$model$effects #MTS object
# 
# # forecast accessible only via user defined output (cf below)
# 
# # Version 3: "x11 names" : preadjustement effets as stored in the A table
# # add doc on names   
# sa_x13_v3$result$preadjust$a6
# 
# 
# # Version 3  
# sa_x13_v3$result$decomposition$d5 # tables from D1 to D13
# 
# 
# # Version 2 
# print(sa_x13_v2)
# sa_x13_v2$decomposition$mstats
# sa_x13_v2$decomposition$s_filter
# sa_x13_v2$decomposition$t_filter
# 
# # version 3 (more diagnostics available by default)
# print(sa_x13_v2)
# sa_x13_v3$result$diagnostics$td.ftest.i
# 
# # Version 2 
# user_defined_variables("X13-ARIMA") 
# user_defined_variables("TRAMO-SEATS")
# 
# # Version 3: more specific functions 
# userdefined_variables_tramoseats("tramoseats")
# userdefined_variables_tramoseats("tramo") # restriction
# 
# userdefined_variables_x13("regarima") #restriction
# userdefined_variables_x13()
# 
# 
# # version 3
# ud <- userdefined_variables_x13()[15:17] # b series
# ud
# sa_x13_v3_UD <- rjd3x13::x13(y_raw, "RSA5c", userdefined = ud)
# sa_x13_v3_UD$user_defined # remainder of the names 
# # retrieve the object 
# sa_x13_v3_UD$user_defined$decomposition.b1
# 
# 
# # Version 2
# # for class 'final' : 2 types 
# plot(sa_x13_v2, type_chart = "sa-trend", first_date = c(2015, 1))
# #plot(sa_x13_v2, type = "cal-seas-irr", first_date = c(2015, 1))
# 
# 
# # regarima
# layout(matrix(1:6, 3, 2))
# plot(sa_x13_v2$regarima, ask = FALSE)
# # Plotting SI ratios  
# plot(sa_x13_v2$decomposition, first_date = c(2015, 1))
# 
# # Plotting SI ratios  
# plot(sa_x13_v2$decomposition, first_date = c(2015, 1))
# 
# 
# # version 3
# # remotes::install_github("AQLT/ggdemetra3", INSTALL_opts = "--no-multiarch")
# library("ggdemetra3")
# ggdemetra3::siratioplot(sa_x13_v3)
# 
# # version 3
# ggdemetra3::ggsiratioplot(sa_x13_v3)
# 
# # version 3
# ggplot2::autoplot(sa_x13_v3)
# 
# # version 2
# # changing estimation span, imposing additive model and
# #adding user defined outliers 
# # first create a new spec modifying the previous one 
# spec_1 <- x13_spec(sa_x13_v2)
# spec_2 <- x13_spec(spec_1, estimate.from = "2004-01-01",
#                    usrdef.outliersEnabled = TRUE,
#                    usrdef.outliersType = c("LS", "AO"),
#                    usrdef.outliersDate = c("2008-10-01", "2018-01-01"),
#                    transform.function = "None") # additive model
# # here the reg-arima model will be estimated from  "2004-01-01" 
# # the decomposition will be run on the whole span 
# 
# # new sa processing
# sa_x13_v2_2 <- RJDemetra::x13(y_raw, spec_2)
# sa_x13_v2_2$final$series
# 
# # start with default spec 
# spec_1 <- spec_x13_default("RSA3")
# # or start with existing spec (no extraction function needed)
# spec_1 <- sa_x13_v3_UD$estimation_spec
# 
# # set a new spec
# ## add outliers 
# spec_2 <- rjd3toolkit::add_outlier(spec_1,
#                                      type = c("AO"), c("2015-01-01", "2010-01-01"))
# ## set trading days
# spec_2 <- rjd3toolkit::set_tradingdays(spec_2,
#                                          option = "workingdays" )
# # set x11 options 
# spec_2 <- set_x11(spec_2, henderson.filter = 13)
# # apply with `fast.x13` (results only)
# fast.x13(y, spec_2)
# 
# # defining user defined trading days 
# spec_td <- x13_spec(spec_1,
#                     tradingdays.option = "UserDefined",
#                     tradingdays.test ="None",
#                     usrdef.varEnabled = TRUE,
#                     # the user defined variable will be assigned to the calendar component
#                     usrdef.varType="Calendar",
#                     usrdef.var=td_regs ) # regressors have to be a single or multiple TS 
# # new sa processing
# sa_x13_v2_4 <- x13(y_raw, spec_td)
# # user defined intervention variable  
# spec_int <- x13_spec(spec_1,
#                      usrdef.varEnabled = TRUE,
#                      # the user defined variable will be assigned to the trend component
#                      usrdef.varType = "Trend",
#                      usrdef.var = x ) # x has to to be a single or multiple TS 
# # new sa processing
# sa_x13_v2_5 <- x13(y_raw, spec_int)
# 
#### CALENDAR
# define a user defined trading days regressor 
td_reg1 <- rjd3toolkit::td(12, start = start(y_raw), length = length(y_raw), 
                           groups = c(1, 1, 1, 1, 1, 0, 0))

# add a user defined trading day regressor
# build new specification 
spec<-rjd3x13::spec_x13("RSA3")
# set a new specification from a default specification
spec_ud_TD<- set_tradingdays(spec,
                             option ="UserDefined",
                             uservariable = "regs.td_reg1")

# define a context
vars<-list(regs=list(td_reg1 = td_reg1))
my_context <- rjd3toolkit::modelling_context(variables=vars)

# New X13 estimation with user defined spec and corresponding context 
sa_x13_v3_td <- rjd3x13::x13(y_raw, spec_ud_TD, context = my_context)
sa_x13_v3_td$result$preprocessing
sa_x13_v3_td$result$decomposition


###### REGRESSOR TO TREND

# step 1: define a regressor, for example
x<-rjd3toolkit::intervention_variable(12, start(y_raw), length(y_raw),
                                      starts = "2001-01-01", ends = "2001-12-01")
# step 2: build new specification to customize or take an existing one 
spec<-rjd3x13::spec_x13("RSA3")
# step 3: customize default specification
spec_T<- add_usrdefvar(spec,id = "regs.x", regeffect="Trend")

# "regs.x": "group_name.variable_name: has to be the same as in context below

# NEW in V3: define a context (to add regressors)
vars<-list(regs=list(x = x))
vars$regs_cal$variables
## step 2: create context
my_context_2 <- rjd3toolkit::modelling_context(variables=vars)

# New X13 estimation with user defined spec and corresponding context 
sa_x13_v3_t <- rjd3x13::x13(y_raw, spec_T, context = my_context_2)
# to check results 
sa_x13_v3_td$result$preprocessing


sa_x13_v3$estimation_spec$regarima$arima
sa_x13_v3$result_spec$regarima$arima

###### Adding user defined variables 
## 1 create var 

## create context



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
