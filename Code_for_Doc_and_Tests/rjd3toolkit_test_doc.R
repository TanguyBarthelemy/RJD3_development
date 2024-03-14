### Testing rjd3toolkit
library("rjd3toolkit")
######## Part 1
# a calendar

# question : when and what for add a calendar to a modelling context  ?
# I change the current example in modelling context function and add calendar back
# when I fully get it...
## default options available with any calendar in modelling context ?.

# b outliers
# c other aux variables
### stop here
####### Part 2
library("rjd3toolkit")
library(rjd3)
### data in case, but better use built in data for examples
ipi <- read.csv2("C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Data/IPI_nace4.csv")
ipi$date <- as.Date(ipi$date, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)
# creating a TS object from a data frame
y_raw <- ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(2019, 6))
y_new <- ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(2019, 9))


# ########### ISSUE 1 : posted : OK att type and NOT Type
# # ### First 60 obs
spec_x13_d<-rjd3x13::spec_x13("rsa5c")
spec_x13_d<-set_basic(spec_x13_d,type="First", n0 = 60,
                      preliminary.check = TRUE,
                      preprocessing= TRUE)


###### ISSUE 2:  preprocessing= FALSE : IMPOSSIBLE: posted
# spec_x13_d<-rjd3x13::spec_x13("rsa5c")
# spec_x13_d<-set_basic(spec_x13_d, preprocessing= FALSE)
#
# sa_x13_d<- rjd3x13::x13(y_raw, spec_x13_d)


#######  ISSUE 3: check out weighted calendars  too check
## cf tanguy, pb class

######## Issue 3: pb default in set transform ? not an issue
# ##### set  transform
spec_x13_d<-rjd3x13::spec_x13("rsa5c")

spec_x13_d<- set_transform(spec_x13_d, fun = "Log")
                           # adjust="LengthOfPeriod",
                           # outliers = TRUE)
sa_x13_d<- rjd3x13::x13(y_raw, spec_x13_d)

spec_x13_d$regarima$transform$fn
sa_x13_d$estimation_spec$regarima$transform$fn
sa_x13_d$result_spec$regarima$transform$fn

#
# ## here pb de adjust and leap year : not clear
spec_x13_d$regarima$transform$adjust
sa_x13_d$estimation_spec$regarima$transform$adjust
sa_x13_d$result_spec$regarima$transform$adjust



## Intervention variables
### file variables.R

#' #Outliers in February 2002
ao <- ao_variable(4, c(2000,1), length = 12*4, date = "2002-02-01")
ao <- ao_variable(12, c(2000,1), length = 12*4, date = "2002-02-01")
ao
ls <- ls_variable(12, c(2000,1), length = 12*4, date = "2002-02-01", zeroended=FALSE)
ls
ls <- ls_variable(12, c(2000,1), length = 12*4, date = "2002-02-01", zeroended=TRUE)
ls
tc <- tc_variable(12, c(2000,1), length = 12*4, date = "2002-02-01")
tc
so <- so_variable(12, c(2000,1), length = 12*4, date = "2002-02-01",zeroended=TRUE)
so
so <- so_variable(12, c(2000,1), length = 12*4, date = "2002-02-01", zeroended=FALSE)
so
#' plot.ts(ts.union(ao, ls, tc, so), plot.type = "single",
#'         col = c("black", "orange", "green", "gray"))
#'



# Ramp variable from January 2001 to September 2001
rp<-ramp_variable(12, c(2000,1), length = 12*4, range = c(13, 21))
class(ramp_variable)
class(r)
plot.ts(rp)
# Or equivalently
ramp_variable(12, c(2000,1), length = 12*4, range = c("2001-01-01", "2001-12-02"))
class(ramp_variable)
class(r)
plot.ts(rp)
## intervention variables (comp avec gui ttu)

iv<-intervention_variable(frequency=12, start=c(2000, 1), length=60,
                      starts = "2001-01-01", ends = "2001-12-01")
iv
plot(iv)

iv<-intervention_variable(12, c(2000, 1), 60,
                      starts = "2001-01-01", ends = "2001-12-01", delta = 1)
iv
plot(iv)

iv<-intervention_variable(12, c(2000, 1), 60,
                          starts = "2001-01-01", ends = "2001-12-01",
                          delta =0, seasonaldelta=1)
iv
plot(iv)

iv<-intervention_variable(12, c(2000, 1), 60,
                          starts = "2001-01-01", ends = "2001-12-01",
                          delta =1, seasonaldelta=1)
iv
plot(iv)


## periodic dummies : add explanations and examples
p<-periodic.dummies(4, c(2000,1), 60)
head(p)
class(p)
q<-periodic.contrasts(4, c(2000,1), 60) # erreur to be checked ?
q[1:9,]

#' Trigonometric variables: check doc and test
#'

#####################################################################
#' Aggregation of time series
# File = timeseries.R

#' Residual Trading Days Test
#' file test_td.R
#'
#' Adding regressors
# file spec_regarima.R



############# CALENDAR REGRESSORS
# file calendars.R

day <- fixed_day(7, 21, .9)
class(day)
day  # 21 July, with weight=0.9
day <- fixed_day(12, 25, .5, validity = list(start = "2010-01-01"))
day # 21 July, with weight=0.5 from Jan 2010
day <- fixed_day(12, 25, .5, validity = list(end = "2010-01-01"))
day # 21 July, with weight=0.9 until Jan 2010

day <- fixed_day(12, 25, .5, validity = list(start="1968-02-01", end = "2010-01-01"))
day
#' day # December 25th, with weight=0.9, from February 1968 until January 2010

# fixed_week_day

day <- fixed_week_day(7, 2, 3) # second Wednesday of July
day
class(day)
## pb here ? try out in a calendar


day<-easter_day(offset=60,julian=TRUE, weight=0.5, validity = list(start="2000-01-01", end = "2020-12-01"))
day


#' single_day("1999-03-19")
day<-single_day("1999-03-19")
day

#special_day: drawing from a list
day<-special_day("CHRISTMAS", offset = 1, weight = 0.8, validity = list(start="2000-01-01", end = "2020-12-01"))

### Creating calendars
## National calendar
frenchCalendar <- national_calendar(days = list(
  fixed_day(7, 14), # Bastille Day
  fixed_day(5, 8, validity = list(start = "1982-05-08")), # End of 2nd WW
  special_day('NEWYEAR'),
  special_day('CHRISTMAS'),
  special_day('MAYDAY'),
  special_day('EASTERMONDAY'),
  special_day('ASCENSION'), #
  special_day('WHITMONDAY'),
  special_day('ASSUMPTION'),
  special_day('ALLSAINTSDAY'),
  special_day('ARMISTICE'))
)
frenchCalendar
class(frenchCalendar)

MyCalendar <- national_calendar(list(
    fixed_day(7,21),
     special_day('NEWYEAR'),
     special_day('CHRISTMAS'),
     fixed_week_day(7, 2, 3), # second Wednesday of July
     special_day('MAYDAY'),
     easter_day(1), # Easter Monday
     easter_day(-2), # Good Friday
     fixed_day(5, 8, validity = list(start = "1982-05-08")), # End of 2nd WW
     single_day("2001-09-11"), # appearing once
     special_day('ASCENSION'),
     easter_day(offset=60,julian=FALSE, weight=0.5, validity = list(start="2000-01-01", end = "2020-12-01")),  # Corpus Christi
     special_day('WHITMONDAY'),
     special_day('ASSUMPTION'),
     special_day('ALLSAINTSDAY'),
     special_day('ARMISTICE')))

MyCalendar
class(MyCalendar)
# frCal_2005 <- weighted_calendar(list(frenchCalendar), 0.5)
# final_cal <- chained_calendar(frenchCalendar, frCal_2005, break_date = "2005-05-01")


### Composite calendar
###### how are the weights defined !!
Belgium <- national_calendar(list(special_day('NEWYEAR'),fixed_day(7,21)))
Belgium
France <- national_calendar(list(special_day('NEWYEAR'),fixed_day(7,14)))
France
composite_calendar<- weighted_calendar(list(France,Belgium), weights = c(1,2))

composite_calendar
class(composite_calendar)


### chained calendar
chained_cal<-chained_calendar(France, Belgium, "2000-01-01")
chained_cal

## additional functions
easter_dates(2022, 2023)
e<-easter_dates(2000, 2023)
e
class(e)
names(e)<-"a"
e
################# REGRESSORS
# not taking into account national calendar
# Monthly regressors for Trading Days: each type of day is different, contrasts to Sundays (6 series)
regs_td<- td(12,c(2020,1),60, groups = c(1, 2, 3, 4, 5, 6, 0),
    contrasts = TRUE)
# Quarterly regressors for Working Days: week days are similar, contrasts to week-end days (1 series)
regs_wd<- td(4,c(2020,1),60, groups = c(1, 1, 1, 1, 1, 0, 0),
          contrasts = TRUE)
regs_wd
class(regs_td)

## taking holidays into account
BE <- national_calendar(list(
     fixed_day(7,21),
     special_day('NEWYEAR'),
     special_day('CHRISTMAS'),
     special_day('MAYDAY'),
     special_day('EASTERMONDAY'),
     special_day('ASCENSION'),
     special_day('WHITMONDAY'),
     special_day('ASSUMPTION'),
    special_day('ALLSAINTSDAY'),
     special_day('ARMISTICE')))
calendar_td(BE, 12, c(1980,1), 240, holiday=7, groups=c(1,1,1,2,2,3,0), contrasts = FALSE)


### Long-term means of a calendar
BE <- national_calendar(list(
    fixed_day(7,21),
    special_day('NEWYEAR'),
    special_day('CHRISTMAS'),
    special_day('MAYDAY'),
    special_day('EASTERMONDAY'),
    special_day('ASCENSION'),
    special_day('WHITMONDAY'),
    special_day('ASSUMPTION'),
    special_day('ALLSAINTSDAY'),
    special_day('ARMISTICE')))
class(BE)
lt<-long_term_mean(BE,12,
    groups = c(1,1,1,1,1,0,0),
    holiday = 7)

lt
class(lt)
### For daily data
## dummies corresponding to holidays
q<-holidays(BE, "2020-01-01",365.25, type="All")
tail(q)
# ISSUE : does it work with weighted and composite calendars
## sens de 365.25///partie decimale ?

#### pb "type" and non working days in daily data ?
q <- rjd3toolkit::holidays(frenchCalendar, "1968-01-01", length = 365*4, type = "All",
              nonworking = 7L)


q<-holidays(BE, "2021-01-01", 365.25, type="All")
plot(apply(q,1, max))

## easter regressor
# in variable.R file

# HOLE
## values of position ? endpos=-1

## mean
#correction=c("Simple", "PreComputed", "Theoretical", "None")


ee<-easter_variable(
    12,
    c(2020,1),
    36, endpos=1)

ee
class(ee)

## Leap Year
# files variables.R

# lp_variable(
#     frequency,
#     start,
#     length,
#     s,
#     type = c("LeapYear", "LengthOfPeriod")
# )

lper<-lp_variable(12,c(2000,1),length=10*12,type ="LengthOfPeriod")
lper
class(lper)

## stock td (notes ? recurring question)
st<-stock_td(12, c(2012,1), length=5*60, w = 31)
st
class(st)
########### VIGNETTE to fully update

##################################################################################################
### SPECS, especially for calendar, outliers and external regressors
## FILE spec_regarima.R


## add outliers and ramps
library("rjd3toolkit")
library("rjd3x13")
init_spec <- rjd3x13::spec_x13("RSA5c")
init_spec$regarima$basic$span$type

new_spec<-rjd3toolkit::add_outlier(init_spec, type="AO", date="2012-01-01")
new_spec
# ramp on year 2012
new_spec<-rjd3toolkit::add_ramp(init_spec,start="2012-01-01",end="2012-12-01")
new_spec

## estimation span

## pre proc only ?


spec_2<-set_basic(
    init_spec,
    type = "Between",
    d0 = "2014-01-01",
    d1 = "2019-01-01",
    preliminary.check = TRUE,
    preprocessing = TRUE
)
spec_2

sa1<-rjd3x13::x13(y_new,spec_2)
sa1$result$preprocessing$estimation$y


library("rjd3tramoseats")
init_spec <- rjd3tramoseats::spec_tramoseats("rsafull")
new_spec<-set_estimate(init_spec, type= "From", d0 = "2012-01-01", tol = 0.0000002,
    exact.ml = FALSE, unit.root.limit = 0.98)

## outlier detection: set_outlier
init_spec <- rjd3tramoseats::spec_tramoseats("rsafull")
new_spec<-set_outlier(init_spec, span.type= "From", d0 = "2012-01-01",
    outliers.type = c("LS", "AO"),
    critical.value = 5,
    tc.rate =0.85)


# Set Automodel Specification : examples missing

# PB def and roots thresholds
# PB def mixed and fct
# default values might not be the same for tramo and x13.

init_spec <- rjd3tramoseats::spec_tramoseats("rsafull")
# ISSUe with tramo seats as init ?
init_spec <- rjd3x13::spec_x13("RSA5c")
new_spec<-set_automodel(init_spec,
    enabled = FALSE,
    acceptdefault = TRUE)

# Set ARIMA Specification: examples ?
# missing parameter: automdl enabled
# explain if has to be set first

init_spec <- rjd3tramoseats::spec_tramoseats("rsafull")
# ISSUe with tramo seats as init ?
init_spec <- rjd3x13::spec_x13("RSA5c")
new_spec<-set_automodel(init_spec,
                        enabled = FALSE)

## ISSUE In set_arima
new_spec<-set_arima(init_spec, ## ISSUE ???
    mean = 0.8,
    mean.type = "Initial", p = 1, d = 1, q = 1,
    bp = 0, bd = 1, bq = 1,
    coef = c(0.7,0.6,0.8),
    coef.type = "Fixed")
########################################

# Set Benchmarking Specification
library("rjd3x13")
init_spec <- rjd3x13::x13_spec("RSA5c")
new_spec<- set_benchmarking(init_spec,
    enabled = TRUE,
    target = "CALENDARADJUSTED", # normal NOT original
    rho = 0.8,
    lambda = 0.5,
    forecast = FALSE,
    bias = "None")

### Transform

### Trading days
# all the details in options : cf x12 and new GUI 3.0

# - pb regresseurs auto : quels contrasts ?
# pb leap year auto adjust

#' @param autoadjust a logical indicating if the program corrects automatically the raw series for
#' the leap year effect if the leap year regressor is significant. Only used when the data is log transformed.
#'
#' @param leapyear a \code{character} to specify whether or not to include the leap-year effect in the model:
#' \code{"LeapYear"} = leap year effect; \code{"LengthOfPeriod"} = length of period (REGARIMA/X-13 specific),
#' \code{"None"} = no effect included. Default: a leap year effect regressor is included with any built-in set of trad

library("rjd3x13")
library("rjd3toolkit")
init_spec <- rjd3x13::spec_x13("RSA5c")
new_spec<-set_tradingdays(init_spec,
    option = "TD4",
    test =  "None")
sa<-rjd3x13::x13(y_raw,new_spec)
sa$result$preprocessing$estimation$X
sa$result$preprocessing$estimation$parameters$description
set_tradingdays(
    x,
    option = c(NA, "TradingDays", "WorkingDays", "TD3", "TD3c", "TD4", "None",
               "UserDefined"),
    uservariable = NA,
    stocktd = NA,
    test = c(NA, "None", "Remove", "Add", "Separate_T", "Joint_F"),
    coef = NA,
    coef.type = c(NA, "Fixed", "Estimated"),
    automatic = c(NA, "Unused", "FTest", "WaldTest", "Aic", "Bic"),
    pftd = NA,
    autoadjust = NA,
    leapyear = c(NA, "LeapYear", "LengthOfPeriod", "None"),
    leapyear.coef = NA,
    leapyear.coef.type = c(NA, "Fixed", "Estimated")
)

## set easter
library("rjd3x13")
library("rjd3toolkit")
init_spec <- rjd3x13::spec_x13("RSA5c")
new_spec<-set_easter(init_spec,
    enabled = TRUE,
    julian = NA,
    duration = 12,
    test = "None",
    type = "IncludeEasterMonday")
sa<-rjd3x13::x13(y_raw,new_spec)
sa$result$preprocessing$estimation$X
sa$result$preprocessing$estimation$parameters$description



set_easter(
    x,
    enabled = NA,
    julian = NA,
    duration = NA,
    test = c(NA, "Add", "Remove", "None"),
    coef = NA,
    coef.type = c(NA, "Estimated", "Fixed"),
    type = c(NA, "Unused", "Standard", "IncludeEaster", "IncludeEasterMonday")
)
## Set transform

#' Set Log-level Transformation and Decomposition scheme in Pre-Processing Specification
#'
#'
#'ISSUE : adjust !
#'
#'set_transform
#'
library("rjd3x13")
library("rjd3toolkit")
init_spec <- rjd3x13::spec_x13("RSA5c")
new_spec<- set_transform(init_spec,
    fun = "Log",
    adjust = "LengthOfPeriod", # ISSUE : fait un lp classique malgré tout ?
    outliers = TRUE)

sa<-rjd3x13::x13(y_raw,new_spec)
sa$result$preprocessing$description$variables
sa$result$preprocessing$estimation$parameters$description

#' Add User-Defined Variable to Pre-Processing Specification.
#'
#' add_usrdefvar(
# x,
# id,
# name = NULL,
# lag = 0,
# coef = NULL,
# regeffect = c("Undefined", "Trend", "Seasonal", "Irregular", "Series",
#               "SeasonallyAdjusted")


intervention_variable(
    frequency,
    start,
    length,
    s,
    starts,
    ends,
    delta = 0,
    seasonaldelta = 0
)


############### Context

# create or import regressors
## create a list of regressors with group names
### to be used in spec custo with add_usrdef
### to be used when creating context to add to estimation parameters

 # creating one or several external regressors (TS objects), which will be gathered in one or several groups
 iv1<-intervention_variable(12, c(2000, 1), 60,
    starts = "2001-01-01", ends = "2001-12-01")
 iv2<- intervention_variable(12, c(2000, 1), 60,
    starts = "2001-01-01", ends = "2001-12-01", delta = 1)
 # regressors as a list of two groups reg1 and reg2
 vars<-list(reg1=list(x = iv1),reg2=list(x = iv2) )
 # creating the modelling context
 my_context<-modelling_context(variables=vars)
 # customize a default specification
 init_spec <- rjd3x13::spec_x13("RSA5c")
 new_spec<- add_usrdefvar(init_spec,id = "reg1.iv1", regeffect="Trend") # att GROUP NAME
 # modelling context in needed for the estimation phase
 sa_x13<- rjd3x13::x13(ABS$X0.2.09.10.M, new_spec, context = my_context)
sa_x13$result$preprocessing$description$preadjustment


### using directly calendars in calendar correction
### TEST adding a calendar into holidays (just like in gui: no need to generate external regressors)
# can we retrieve them

# ISSUE: ("HOLIDAYS" Parameter: specifying User defined calendar for built in regressors
spec_x13_d<-rjd3x13::spec_x13("rsa5c")
### create a calendar
BE <- national_calendar(list(
    fixed_day(7,21),
    special_day('NEWYEAR'),
    special_day('CHRISTMAS'),
    special_day('MAYDAY'),
    special_day('EASTERMONDAY'),
    special_day('ASCENSION'),
    special_day('WHITMONDAY'),
    special_day('ASSUMPTION'),
    special_day('ALLSAINTSDAY'),
    special_day('ARMISTICE')))
## put into a context
my_context<-modelling_context(calendars = list(cal=BE))
class(my_context)
my_context$calendars$cal$days
rjd3toolkit::.r2jd_modellingcontext(my_context)$getCalendars()$getNames()

spec_x13_d$regarima$regression$td$holidays<-BE
spec_x13_d$regarima$regression$td$holida

sa_x13<- rjd3x13::x13(ABS$X0.2.09.10.M,spec_x13_d, context = my_context)

# Pre-defined regressors based on user-defined calendar
library("rjd3toolkit")
y_raw<-ABS$X0.2.09.10.M
### create a calendar
BE <- national_calendar(list(
    fixed_day(7,21),
    special_day('NEWYEAR'),
    special_day('CHRISTMAS'),
    special_day('MAYDAY'),
    special_day('EASTERMONDAY'),
    special_day('ASCENSION'),
    special_day('WHITMONDAY'),
    special_day('ASSUMPTION'),
    special_day('ALLSAINTSDAY'),
    special_day('ARMISTICE')))
## put into a context
my_context<-modelling_context(calendars = list(cal=BE))
## create a specification
# init_spec <- rjd3x13::spec_x13("RSA5c")
## modify the specification
# new_spec<-set_tradingdays(init_spec,
#                          option = "TradingDays", calendar.name="cal")
## estimate with context
# sa<-rjd3x13::x13(y_raw,new_spec, context=my_context)
# sa$result$preprocessing


x13()

# built in regressors with specific holidays (like gui)

spec_x13_d<-rjd3x13::spec_x13("rsa5c")
spec_x13_d<- set_tradingdays(spec_x13_d,
    option = "TRADINGDAYS",
    test = "None")

# QUESTION : where to see the created (not user def) regressors
# in spec rien
spec_x13_d$regarima$
# in results
sa_x13_d<- rjd3x13::x13(y_raw, spec_x13_d)
sa_x13_d$result$preprocessing$description

# $preadjustment ?
# [1] "NONE

### trying to add holidays
spec_x13_d$regarima$regression$td$holidays<-BE

sa_x13_d<- rjd3x13::x13(y_raw, spec_x13_d, context = my_context)
sa_x13_d$result$preprocessing

a<-rjd3x13::x13(y_raw, spec_x13_d, userdefined="bla")
a$user_defined$bla
