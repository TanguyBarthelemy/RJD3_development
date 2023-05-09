##### RJD3x13 : creating Enriched spec and output for testing 
### 1 PRINTS

#####################
library(rjd3toolkit)
library(rjd3x13)
# Data  :
ipi <- read.csv2("C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Data/IPI_nace4.csv")
ipi$date <- as.Date(ipi$date, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)
# creating a TS object from a data frame 
y_raw <- ts(ipi[, "RF0812"], frequency = 12, start = c(1990, 1), end = c(2021,12))
y_new <- ts(ipi[, "RF0812"], frequency = 12, start = c(1990, 1), end = c(2022,9))

################# LAYERS 

## Layer 1: customized spec before estimation
## Layer 2: post estimation : estimation spec 
## Layer 3: post estimation : result spec 
## Layer 4: post spec refresh : refreshed spec 
## Layer 5: post estimation with refreshed spec: refreshed estimation spec
## Layer 6: post estimation with refreshed spec: refreshed result spec

spec_ts_d<-rjd3tramoseats::spec_tramoseats("rsa5") 

############# CUSTOMIZATION by parts
# ##### set basic 
spec_ts_d<-set_basic(spec_ts_d,type = "From",d0 = "2000-01-01",
                     preliminary.check = TRUE,
                     preprocessing= TRUE)
### PRINT HOLE in  preprocessing
# series span 
# model span 

### quick check if estimation works 
sa_ts_d<- tramoseats(y_raw, spec_ts_d)
sa_ts_d$result$preprocessing


# ## test from : ok includes d0
# # spec_ts_d<-set_basic(spec_ts_d,type = "From",d0 = "2000-01-01",
# #                       preliminary.check = TRUE,
# #                       preprocessing= TRUE)
# 
# # ## test to : ok includes d1 ?
# # spec_ts_d<-set_basic(spec_ts_d,type = "To",d1 = "2000-01-01",
# #                       preliminary.check = TRUE,
# #                       preprocessing= TRUE)

# # ### Last 90 obs 
# spec_ts_d<-rjd3tramoseats::spec_tramoseats("rsa5")
# spec_ts_d<-set_basic(spec_ts_d,Type="Last", n1 = 90,
#                       preliminary.check = TRUE,
#                       preprocessing= TRUE)
# # ### Excluding : first 60 and Last 60 obs 
# # spec_ts_d<-set_basic(spec_ts_d,Type="EXcluding", n0= 60, n1 = 60,
# #                       preliminary.check = TRUE,
# #                       preprocessing= TRUE)
# 
# ##### set estimate 
spec_ts_d<-set_estimate(spec_ts_d,type = "From",d0 = "2010-01-01")

sa_ts_d<- tramoseats(y_raw, spec_ts_d)
sa_ts_d$result$preprocessing

# # # ### Last 60 obs SAME issue
# spec_ts_d<-rjd3x13::spec_x13("rsa5c")
# spec_ts_d<-set_estimate(spec_ts_d,Type="Last", n1 = 60)
#

### quick check if estimation works 

# ##### set  transform 
# spec_ts_d<-rjd3x13::spec_x13("rsa5c")

spec_ts_d<- set_transform(spec_ts_d,
                          fun = "Log",
                          adjust="LengthOfPeriod",
                          outliers = TRUE)

# spec_ts_d<- set_transform(spec_ts_d,
#                            fun = "Log",
#                            outliers = TRUE)
# 
# ##### set  outlier (see pb in refresh not copied to spec ?)
# # spec_ts_d<-set_outlier(spec_ts_d, 
# #                     span.type= "From", d0 = "2012-01-01",
# #                       outliers.type = c("LS", "TC"),
# #                       critical.value = 5,
# #                       tc.rate =0.85)

spec_ts_d<-set_outlier(spec_ts_d,
                       span.type= "Last", n1 = 60,
                       outliers.type = c("LS", "TC"),
                       critical.value = 5,
                       tc.rate =0.85)
### quick check if estimation works 
sa_ts_d<- tramoseats(y_raw, spec_ts_d)
sa_ts_d$result$preprocessing

# #### set automodel 
### parametres a revoir (vauleurs admissibles differentes de x13)
# spec_ts_d<-set_automodel(spec_ts_d,
#                          enabled = FALSE,
#                          cancel=0.06,
#                          ub1=1.05,
#                          ub2=1.15)
### quick check if estimation works

sa_ts_d<- tramoseats(y_raw, spec_ts_d)
sa_ts_d$result$preprocessing


# ### set benchmarking 
spec_ts_d<-spec_tramoseats("rsa5")
spec_ts_d<-set_benchmarking(spec_ts_d,
                            enabled = TRUE,
                            target = "CALENDARADJUSTED",
                            rho = 0.8,
                            lambda = 0.5,
                            forecast = FALSE,
                            bias = "None")


### quick check if estimation works 
sa_ts_d<- tramoseats(y_raw, spec_ts_d)
sa_ts_d$result$preprocessing
sa_ts_d$result # no bencch marking results, define output in user defined


### PRINT HOLE
#### sa_ts_d$result$final : pas bon : faire un table / main results (GUI)


#####  set arima 
spec_ts_d<-set_automodel(spec_ts_d,
                         enabled = FALSE)
spec_ts_d<-set_arima(spec_ts_d,mean = 0.2,
                     mean.type = "Fixed",
                     p = 1,
                     d = 2,
                     q = 0,
                     bp = 1,
                     bd = 1,
                     bq = 0,
                     coef = c(0.6,0.7),
                     coef.type = c("Initial","Fixed"))
###
sa_ts_d<- tramoseats(y_raw, spec_ts_d)
sa_ts_d$result$preprocessing


# # not fixed
# spec_ts_d<-set_arima(spec_ts_d,
#                       p = 1,
#                       d = 0,
#                       q = 1,
#                       bp = 1,
#                       bd = 1,
#                       bq = 0)

# ### set_tradingdays (NOT USER DEF, for user def see below)
spec_ts_d<-spec_tramoseats("rsa5")
spec_ts_d<- set_tradingdays(spec_ts_d,
                            option = "TD4", test = "None",
                            coef=c(0.7,NA,0.5),
                            coef.type=c("Fixed","Estimated","Fixed"),
                            leapyear="LengthOfPeriod",
                            leapyear.coef=0.6
)

sa_ts_d<- tramoseats(y_raw, spec_ts_d)
sa_ts_d$result$preprocessing


## tester affichage en cas de stock td 
# # spec_ts_d<- set_tradingdays(spec_ts_d,stocktd=28)
# 
# 
# 
# ### set_easter
# spec_ts_d<-rjd3x13::spec_x13("rsa5c")
spec_ts_d<-set_easter(spec_ts_d,
                      enabled = TRUE,
                      duration = 12,
                      coef=0.6,
                      coef.type="Fixed",
                      test="None")
# # type = "Unused" : TRAMO specific
# # "Unused", "Standard", "IncludeEaster", "IncludeEasterMonday"
# spec_ts_d

sa_ts_d<- tramoseats(y_raw, spec_ts_d)
sa_ts_d$result$preprocessing

### HOLE SEATS params


### Adding user defined variables 
### add outliers
# spec_ts_d<-rjd3x13::spec_x13("rsa5c")
spec_ts_d<-rjd3toolkit::add_outlier(spec_ts_d, type="AO", date="2020-03-01", coef=12)
spec_ts_d<-rjd3toolkit::add_outlier(spec_ts_d, type="LS", date="2020-04-01")
spec_ts_d

## quick estimation check 
sa_ts_d<- tramoseats(y_raw, spec_ts_d)
sa_ts_d$result$preprocessing

### add ramp 
# ramp on year 2021
spec_ts_d<-rjd3toolkit::add_ramp(spec_ts_d,start="2021-01-01",end="2021-12-01")
## quick check 
sa_ts_d<- tramoseats(y_raw, spec_ts_d)
sa_ts_d$result$preprocessing


###################################################################
#### add user defined regressors
# (if user defined calendar: dont use set_trading days before )

# ## add intervention variables (with add_usrdefvar) 
# y_raw<-rjd3toolkit::ABS$X0.2.08.10.M 
iv1<-intervention_variable(12, c(2000, 1), 60,
                           starts = "2000-01-01", ends = "2001-12-01")
iv2<- intervention_variable(12, c(2000, 1), 60,
                            starts = "2010-01-01", ends = "2010-12-01", delta = 1)

### calendar regressors (to be added with set_trading days)
regs_td<- td(s=y_raw, groups = c(1, 2, 3, 4, 5, 6, 0),
             contrasts = TRUE)

#### Creating context for all external regressors   
variables<-list(Monday=regs_td[,1],Tuesday=regs_td[,2], Wednesday=regs_td[,3],
                Thursday=regs_td[,4],Friday= regs_td[,5], Saturday=regs_td[,6],
                reg1=iv1,reg2=iv2)
my_context<-modelling_context(variables=variables)
rjd3toolkit::.r2jd_modellingcontext(my_context)$getTsVariableDictionary()

### add calendar regressors to spec 
# spec_ts_d<-rjd3x13::spec_x13("rsa5c")
spec_ts_d<- set_tradingdays(spec_ts_d,
                            option = "UserDefined", 
                            uservariable=c("r.Monday","r.Tuesday","r.Wednesday","r.Thursday","r.Friday","r.Saturday"), # forcement en caracteres dans un vecteur 
                            test = "None")
### ISSUE ?
spec_ts_d # indicates TD_NONE...
spec_ts_d$regarima$regression$td$users

## adding other external regressors : WAIT change underway
# spec_ts_d<- add_usrdefvar(spec_ts_d,id = "r.reg1",name="iv1" , regeffect="Trend")
# spec_ts_d<- add_usrdefvar(spec_ts_d,id = "r.reg2", regeffect="Trend", coef=0.7)


## estimation with context and user def output 
userdefined_variables_tramoseats()
sa_ts_d<- tramoseats(y_raw, spec_ts_d, context=my_context, userdefined= c("ycal","reg_t"))
sa_ts_d$result$preprocessing
sa_ts_d$user_defined$ycal

### refresh
current_result_spec <- sa_ts_d$result_spec
current_domain_spec <- sa_ts_d$estimation_spec
spec_tramoseats_ref <- tramoseats_refresh(current_result_spec, # point spec to be refreshed
                                          current_domain_spec, #domain spec (set of constraints)
                                          policy = "FixedParameters")

############## specification details  ###
## basic 
spec_ts_d$tramo$basic$span$type
sa_ts_d$estimation_spec$tramo$basic$span$type
sa_ts_d$result_spec$tramo$basic$span$type
spec_tramoseats_ref$tramo$basic$span$type
sa_ts_ref$estimation_spec$tramo$basic$span$type
sa_ts_ref$result_spec$tramo$basic$span$type

spec_ts_d$tramo$basic$span$d0
sa_ts_d$estimation_spec$tramo$basic$span$d0
sa_ts_d$result_spec$tramo$basic$span$d0
spec_tramoseats_ref$tramo$basic$span$d0
sa_ts_ref$estimation_spec$tramo$basic$span$d0
sa_ts_ref$result_spec$tramo$basic$span$d0

spec_ts_d$tramo$basic$span$d1
sa_ts_d$estimation_spec$tramo$basic$span$d1
sa_ts_d$result_spec$tramo$basic$span$d1
spec_tramoseats_ref$tramo$basic$span$d1
sa_ts_ref$estimation_spec$tramo$basic$span$d1
sa_ts_ref$result_spec$tramo$basic$span$d1

spec_ts_d$tramo$basic$span$n0
sa_ts_d$estimation_spec$tramo$basic$span$n0
sa_ts_d$result_spec$tramo$basic$span$n0
spec_tramoseats_ref$tramo$basic$span$n0
sa_ts_ref$estimation_spec$tramo$basic$span$n0
sa_ts_ref$result_spec$tramo$basic$span$n0

spec_ts_d$tramo$basic$span$n1
sa_ts_d$estimation_spec$tramo$basic$span$n1
sa_ts_d$result_spec$tramo$basic$span$n1
spec_tramoseats_ref$tramo$basic$span$n1
sa_ts_ref$estimation_spec$tramo$basic$span$n1
sa_ts_ref$result_spec$tramo$basic$span$n1

spec_ts_d$tramo$basic$preprocessing
sa_ts_d$estimation_spec$tramo$basic$preprocessing
sa_ts_d$result_spec$tramo$basic$preprocessing
spec_tramoseats_ref$tramo$basic$preprocessing
sa_ts_ref$estimation_spec$tramo$basic$preprocessing
sa_ts_ref$result_spec$tramo$basic$preprocessing

spec_ts_d$tramo$basic$preliminaryCheck
sa_ts_d$estimation_spec$tramo$basic$preliminaryCheck
sa_ts_d$result_spec$tramo$basic$preliminaryCheck
spec_tramoseats_ref$tramo$basic$preliminaryCheck
sa_ts_ref$estimation_spec$tramo$basic$preliminaryCheck
sa_ts_ref$result_spec$tramo$basic$preliminaryCheck

## transform
## refresh : is transported, never touched (exception = complete)
### PB with refresh seems to reestimate schema
spec_ts_d$tramo$transform$fn
sa_ts_d$estimation_spec$tramo$transform$fn
sa_ts_d$result_spec$tramo$transform$fn
spec_tramoseats_ref$tramo$transform$fn
sa_ts_ref$estimation_spec$tramo$transform$fn
sa_ts_ref$result_spec$tramo$transform$fn

## here pb de adjust and leap year : not clear
spec_ts_d$tramo$transform$adjust
sa_ts_d$estimation_spec$tramo$transform$adjust
sa_ts_d$result_spec$tramo$transform$adjust
spec_tramoseats_ref$tramo$transform$adjust
sa_ts_ref$estimation_spec$tramo$transform$adjust
sa_ts_ref$result_spec$tramo$transform$adjust

## outlier (auto detection params) mieux ecrit que ds x12
spec_ts_d$tramo$outlier$enabled
sa_ts_d$estimation_spec$tramo$outlier$enabled
sa_ts_d$result_spec$tramo$outlier$enabled
spec_tramoseats_ref$tramo$outlier$enabled
sa_ts_ref$estimation_spec$tramo$outlier$enabled
sa_ts_ref$result_spec$tramo$outlier$enabled

spec_ts_d$tramo$outlier$span$type
sa_ts_d$estimation_spec$tramo$outlier$span$type
sa_ts_d$result_spec$tramo$outlier$span$type
spec_tramoseats_ref$tramo$outlier$span$type
sa_ts_ref$estimation_spec$tramo$outlier$span$type
sa_ts_ref$result_spec$tramo$outlier$span$type


spec_ts_d$tramo$outlier$span$d0
sa_ts_d$estimation_spec$tramo$outlier$span$d0
sa_ts_d$result_spec$tramo$outlier$span$d0
spec_tramoseats_ref$tramo$outlier$span$d0
sa_ts_ref$estimation_spec$tramo$outlierspan$d0
sa_ts_ref$result_spec$tramo$outlier$span$d0

spec_ts_d$tramo$outlier$span$d1
sa_ts_d$estimation_spec$tramo$outlier$span$d1
sa_ts_d$result_spec$tramo$outlier$span$d1
spec_tramoseats_ref$tramo$outlier$span$d1
sa_ts_ref$estimation_spec$tramo$outlierspan$d1
sa_ts_ref$result_spec$tramo$outlier$span$d1


spec_ts_d$tramo$outlier$span$n0
sa_ts_d$estimation_spec$tramo$outlier$span$n0
sa_ts_d$result_spec$tramo$outlier$span$n0
spec_tramoseats_ref$tramo$outlier$span$n0
sa_ts_ref$estimation_spec$tramo$outlier$span$n0
sa_ts_ref$result_spec$tramo$outlier$span$n0

spec_ts_d$tramo$outlier$span$n1
sa_ts_d$estimation_spec$tramo$outlier$span$n1
sa_ts_d$result_spec$tramo$outlier$span$n1
spec_tramoseats_ref$tramo$outlier$span$n1
sa_ts_ref$estimation_spec$tramo$outlier$span$n1
sa_ts_ref$result_spec$tramo$outlier$span$n1

spec_ts_d$tramo$outlier$va
sa_ts_d$estimation_spec$tramo$outlier$va
sa_ts_d$result_spec$tramo$outlier$va
spec_tramoseats_ref$tramo$outlier$va
sa_ts_ref$estimation_spec$tramo$outlier$va
sa_ts_ref$result_spec$tramo$outlier$va

## check ml 
spec_ts_d$tramo$outlier$ml
sa_ts_d$estimation_spec$tramo$outlier$ml
sa_ts_d$result_spec$tramo$outlier$ml
spec_tramoseats_ref$tramo$outlier$ml
sa_ts_ref$estimation_spec$tramo$outlier$ml
sa_ts_ref$result_spec$tramo$outlier$ml

spec_ts_d$tramo$outlier$tcrate
sa_ts_d$estimation_spec$tramo$outlier$tcrate
sa_ts_d$result_spec$tramo$outlier$tcrate
spec_tramoseats_ref$tramo$outlier$tcrate
sa_ts_ref$estimation_spec$tramo$outlier$tcrate
sa_ts_ref$result_spec$tramo$outlier$tcrate

spec_ts_d$tramo$outlier$ao
sa_ts_d$estimation_spec$tramo$outlier$ao
sa_ts_d$result_spec$tramo$outlier$ao
spec_tramoseats_ref$tramo$outlier$ao
sa_ts_ref$estimation_spec$tramo$outlier$ao
sa_ts_ref$result_spec$tramo$outlier$ao

spec_ts_d$tramo$outlier$ls
sa_ts_d$estimation_spec$tramo$outlier$ls
sa_ts_d$result_spec$tramo$outlier$ls
spec_tramoseats_ref$tramo$outlier$ls
sa_ts_ref$estimation_spec$tramo$outlier$ls
sa_ts_ref$result_spec$tramo$outlier$ls

spec_ts_d$tramo$outlier$tc
sa_ts_d$estimation_spec$tramo$outlier$tc
sa_ts_d$result_spec$tramo$outlier$tc
spec_tramoseats_ref$tramo$outlier$tc
sa_ts_ref$estimation_spec$tramo$outlier$tc
sa_ts_ref$result_spec$tramo$outlier$tc

spec_ts_d$tramo$outlier$so
sa_ts_d$estimation_spec$tramo$outlier$so
sa_ts_d$result_spec$tramo$outlier$so
spec_tramoseats_ref$tramo$outlier$so
sa_ts_ref$estimation_spec$tramo$outlier$so
sa_ts_ref$result_spec$tramo$outlier$so

## arima
spec_ts_d$tramo$arima$period
sa_ts_d$estimation_spec$tramo$arima$period
sa_ts_d$result_spec$tramo$arima$period
spec_tramoseats_ref$tramo$arima$period
sa_ts_ref$estimation_spec$tramo$arima$period
sa_ts_ref$result_spec$tramo$arima$period


spec_ts_d$tramo$arima$d
sa_ts_d$estimation_spec$tramo$arima$d
sa_ts_d$result_spec$tramo$arima$d
spec_tramoseats_ref$tramo$arima$d
sa_ts_ref$estimation_spec$tramo$arima$d
sa_ts_ref$result_spec$tramo$arima$d

spec_ts_d$tramo$arima$bd
sa_ts_d$estimation_spec$tramo$arima$bd
sa_ts_d$result_spec$tramo$arima$bd
spec_tramoseats_ref$tramo$arima$bd
sa_ts_ref$estimation_spec$tramo$arima$bd
sa_ts_ref$result_spec$tramo$arima$bd

spec_ts_d$tramo$arima$phi
sa_ts_d$estimation_spec$tramo$arima$phi
sa_ts_d$result_spec$tramo$arima$phi
spec_tramoseats_ref$tramo$arima$phi
sa_ts_ref$estimation_spec$tramo$arima$phi
sa_ts_ref$result_spec$tramo$arima$phi

spec_ts_d$tramo$arima$theta
sa_ts_d$estimation_spec$tramo$arima$theta
sa_ts_d$result_spec$tramo$arima$theta
spec_tramoseats_ref$tramo$arima$theta
sa_ts_ref$estimation_spec$tramo$arima$theta
sa_ts_ref$result_spec$tramo$arima$theta

spec_ts_d$tramo$arima$bphi
sa_ts_d$estimation_spec$tramo$arima$bphi
sa_ts_d$result_spec$tramo$arima$bphi
spec_tramoseats_ref$tramo$arima$bphi
sa_ts_ref$estimation_spec$tramo$arima$bphi
sa_ts_ref$result_spec$tramo$arima$bphi

spec_ts_d$tramo$arima$btheta
sa_ts_d$estimation_spec$tramo$arima$btheta
sa_ts_d$result_spec$tramo$arima$btheta
spec_tramoseats_ref$tramo$arima$btheta
sa_ts_ref$estimation_spec$tramo$arima$btheta
sa_ts_ref$result_spec$tramo$arima$btheta

# ## automodel
spec_ts_d$tramo$automodel$enabled
sa_ts_d$estimation_spec$tramo$automodel$enabled
sa_ts_d$result_spec$tramo$automodel$enabled
spec_tramoseats_ref$tramo$automodel$enabled
sa_ts_ref$estimation_spec$tramo$automodel$enabled
sa_ts_ref$result_spec$tramo$automodel$enabled

#pcr to check
spec_ts_d$tramo$automodel$pcr
sa_ts_d$estimation_spec$tramo$automodel$pcr
sa_ts_d$result_spec$tramo$automodel$pcr
spec_tramoseats_ref$tramo$automodel$pcr
sa_ts_ref$estimation_spec$tramo$automodel$pcr
sa_ts_ref$result_spec$tramo$automodel$pcr

spec_ts_d$tramo$automodel$pc
sa_ts_d$estimation_spec$tramo$automodel$pc
sa_ts_d$result_spec$tramo$automodel$pc
spec_tramoseats_ref$tramo$automodel$pc
sa_ts_ref$estimation_spec$tramo$automodel$pc
sa_ts_ref$result_spec$tramo$automodel$pc


spec_ts_d$tramo$automodel$tsig
sa_ts_d$estimation_spec$tramo$automodel$tsig
sa_ts_d$result_spec$tramo$automodel$tsig
spec_tramoseats_ref$tramo$automodel$tsig
sa_ts_ref$estimation_spec$tramo$automodel$tsig
sa_ts_ref$result_spec$tramo$automodel$tsig

spec_ts_d$tramo$automodel$amicompare
sa_ts_d$estimation_spec$tramo$automodel$amicompare
sa_ts_d$result_spec$tramo$automodel$amicompare
spec_tramoseats_ref$tramo$automodel$amicompare
sa_ts_ref$estimation_spec$tramo$automodel$amicompare
sa_ts_ref$result_spec$tramo$automodel$amicompare

spec_ts_d$tramo$automodel$ub1
sa_ts_d$estimation_spec$tramo$automodel$ub1
sa_ts_d$result_spec$tramo$automodel$ub1
spec_tramoseats_ref$tramo$automodel$ub1
sa_ts_ref$estimation_spec$tramo$automodel$ub1
sa_ts_ref$result_spec$tramo$automodel$ub1

spec_ts_d$tramo$automodel$ub2
sa_ts_d$estimation_spec$tramo$automodel$ub2
sa_ts_d$result_spec$tramo$automodel$ub2
spec_tramoseats_ref$tramo$automodel$ub2
sa_ts_ref$estimation_spec$tramo$automodel$ub2
sa_ts_ref$result_spec$tramo$automodel$ub2


spec_ts_d$tramo$automodel$cancel
sa_ts_d$estimation_spec$tramo$automodel$cancel
sa_ts_d$result_spec$tramo$automodel$cancel
spec_tramoseats_ref$tramo$automodel$cancel
sa_ts_ref$estimation_spec$tramo$automodel$cancel
sa_ts_ref$result_spec$tramo$automodel$cancel


spec_ts_d$tramo$automodel$acceptdef
sa_ts_d$estimation_spec$tramo$automodel$acceptdef
sa_ts_d$result_spec$tramo$automodel$acceptdef
spec_tramoseats_ref$tramo$automodel$acceptdef
sa_ts_ref$estimation_spec$tramo$automodel$acceptdef
sa_ts_ref$result_spec$tramo$automodel$acceptdef

# 
# ## regression
spec_ts_d$tramo$regression$mean
sa_ts_d$estimation_spec$tramo$regression$mean
sa_ts_d$result_spec$tramo$regression$mean
spec_tramoseats_ref$tramo$regression$mean # nothing in spec
sa_ts_ref$estimation_spec$tramo$regression$mean
sa_ts_ref$result_spec$tramo$regression$mean # estimated value only here

spec_ts_d$tramo$regression$check_mean
sa_ts_d$estimation_spec$tramo$regression$check_mean
sa_ts_d$result_spec$tramo$regression$check_mean
spec_tramoseats_ref$tramo$regression$check_mean
sa_ts_ref$estimation_spec$tramo$regression$check_mean
sa_ts_ref$result_spec$tramo$regression$check_mean


## regression$td
### what is this
### how to change it
spec_ts_d$tramo$regression$td$td
sa_ts_d$estimation_spec$tramo$regression$td$td
sa_ts_d$result_spec$tramo$regression$td$td
spec_tramoseats_ref$tramo$regression$td$td
sa_ts_ref$estimation_spec$tramo$regression$td$td
sa_ts_ref$result_spec$tramo$regression$td$td
# 
spec_ts_d$tramo$regression$td$lp
sa_ts_d$estimation_spec$tramo$regression$td$lp
sa_ts_d$result_spec$tramo$regression$td$lp
spec_tramoseats_ref$tramo$regression$td$lp
sa_ts_ref$estimation_spec$tramo$regression$td$lp
sa_ts_ref$result_spec$tramo$regression$td$lp

spec_ts_d$tramo$regression$td$holidays
sa_ts_d$estimation_spec$tramo$regression$td$holidays
sa_ts_d$result_spec$tramo$regression$td$holidays
spec_tramoseats_ref$tramo$regression$td$holidays
sa_ts_ref$estimation_spec$tramo$regression$td$holidays
sa_ts_ref$result_spec$tramo$regression$td$holidays
# 
spec_ts_d$tramo$regression$td$users
sa_ts_d$estimation_spec$tramo$regression$td$users
sa_ts_d$result_spec$tramo$regression$td$users
spec_tramoseats_ref$tramo$regression$td$users
sa_ts_ref$estimation_spec$tramo$regression$td$users
sa_ts_ref$result_spec$tramo$regression$td$users

spec_ts_d$tramo$regression$td$w
sa_ts_d$estimation_spec$tramo$regression$td$w
sa_ts_d$result_spec$tramo$regression$td$w
spec_tramoseats_ref$tramo$regression$td$w
sa_ts_ref$estimation_spec$tramo$regression$td$w
sa_ts_ref$result_spec$tramo$regression$td$w

spec_ts_d$tramo$regression$td$test
sa_ts_d$estimation_spec$tramo$regression$td$test
sa_ts_d$result_spec$tramo$regression$td$test
spec_tramoseats_ref$tramo$regression$td$test
sa_ts_ref$estimation_spec$tramo$regression$td$test
sa_ts_ref$result_spec$tramo$regression$td$test

spec_ts_d$tramo$regression$td$auto
sa_ts_d$estimation_spec$tramo$regression$td$auto
sa_ts_d$result_spec$tramo$regression$td$auto
spec_tramoseats_ref$tramo$regression$td$auto
sa_ts_ref$estimation_spec$tramo$regression$td$auto
sa_ts_ref$result_spec$tramo$regression$td$auto

spec_ts_d$tramo$regression$td$autoadjust
sa_ts_d$estimation_spec$tramo$regression$td$autoadjust
sa_ts_d$result_spec$tramo$regression$td$autoadjust
spec_tramoseats_ref$tramo$regression$td$autoadjust
sa_ts_ref$estimation_spec$tramo$regression$td$autoadjust
sa_ts_ref$result_spec$tramo$regression$td$autoadjust

spec_ts_d$tramo$regression$td$tdcoefficients
sa_ts_d$estimation_spec$tramo$regression$td$tdcoefficients
sa_ts_d$result_spec$tramo$regression$td$tdcoefficients
spec_tramoseats_ref$tramo$regression$td$tdcoefficients
sa_ts_ref$estimation_spec$tramo$regression$td$tdcoefficients
sa_ts_ref$result_spec$tramo$regression$td$tdcoefficients

spec_ts_d$tramo$regression$td$lpcoefficient
sa_ts_d$estimation_spec$tramo$regression$td$lpcoefficient
sa_ts_d$result_spec$tramo$regression$td$lpcoefficient
spec_tramoseats_ref$tramo$regression$td$lpcoefficient
sa_ts_ref$estimation_spec$tramo$regression$td$lpcoefficient
sa_ts_ref$result_spec$tramo$regression$td$lpcoefficient
# 
# ## regression$easter
spec_ts_d$tramo$regression$easter$type
sa_ts_d$estimation_spec$tramo$regression$easter$type
sa_ts_d$result_spec$tramo$regression$easter$type
spec_tramoseats_ref$tramo$regression$easter$type
sa_ts_ref$estimation_spec$tramo$regression$easter$type
sa_ts_ref$result_spec$tramo$regression$easter$type

spec_ts_d$tramo$regression$easter$duration
sa_ts_d$estimation_spec$tramo$regression$easter$duration
sa_ts_d$result_spec$tramo$regression$easter$duration
spec_tramoseats_ref$tramo$regression$easter$duration
sa_ts_ref$estimation_spec$tramo$regression$easter$duration
sa_ts_ref$result_spec$tramo$regression$easter$duration

spec_ts_d$tramo$regression$easter$test
sa_ts_d$estimation_spec$tramo$regression$easter$test
sa_ts_d$result_spec$tramo$regression$easter$test
spec_tramoseats_ref$tramo$regression$easter$test
sa_ts_ref$estimation_spec$tramo$regression$easter$test
sa_ts_ref$result_spec$tramo$regression$easter$test

spec_ts_d$tramo$regression$easter$coefficient
sa_ts_d$estimation_spec$tramo$regression$easter$coefficient
sa_ts_d$result_spec$tramo$regression$easter$coefficient
spec_tramoseats_ref$tramo$regression$easter$coefficient
sa_ts_ref$estimation_spec$tramo$regression$easter$coefficient
sa_ts_ref$result_spec$tramo$regression$easter$coefficient
# 
## outliers / ramps / user def vars
spec_ts_d$tramo$regression$outliers
sa_ts_d$estimation_spec$tramo$regression$outliers
sa_ts_d$result_spec$tramo$regression$outliers
spec_tramoseats_ref$tramo$regression$outliers
sa_ts_ref$estimation_spec$tramo$regression$outliers
sa_ts_ref$result_spec$tramo$regression$outliers

spec_ts_d$tramo$regression$ramps
sa_ts_d$estimation_spec$tramo$regression$ramps
sa_ts_d$result_spec$tramo$regression$ramps
spec_tramoseats_ref$tramo$regression$ramps
sa_ts_ref$estimation_spec$tramo$regression$ramps
sa_ts_ref$result_spec$tramo$regression$ramps

spec_ts_d$tramo$regression$users
sa_ts_d$estimation_spec$tramo$regression$users
sa_ts_d$result_spec$tramo$regression$users
spec_tramoseats_ref$tramo$regression$users
sa_ts_ref$estimation_spec$tramo$regression$users
sa_ts_ref$result_spec$tramo$regression$users


## estimate
spec_ts_d$tramo$estimate$span$type
sa_ts_d$estimation_spec$tramo$estimate$span$type
sa_ts_d$result_spec$tramo$estimate$span$type
spec_tramoseats_ref$tramo$estimate$span$type
sa_ts_ref$estimation_spec$tramo$estimate$span$type
sa_ts_ref$result_spec$tramo$estimate$span$type


spec_ts_d$tramo$estimate$span$d0
sa_ts_d$estimation_spec$tramo$estimate$span$d0
sa_ts_d$result_spec$tramo$estimate$span$d0
spec_tramoseats_ref$tramo$estimate$span$d0
sa_ts_ref$estimation_spec$tramo$estimate$span$d0
sa_ts_ref$result_spec$tramo$estimate$span$d0


spec_ts_d$tramo$estimate$span$d1
sa_ts_d$estimation_spec$tramo$estimate$span$d1
sa_ts_d$result_spec$tramo$estimate$span$d1
spec_tramoseats_ref$tramo$estimate$span$d1
sa_ts_ref$estimation_spec$tramo$estimate$span$d1
sa_ts_ref$result_spec$tramo$estimate$span$d1


spec_ts_d$tramo$estimate$span$n0
sa_ts_d$estimation_spec$tramo$estimate$span$n0
sa_ts_d$result_spec$tramo$estimate$span$n0
spec_tramoseats_ref$tramo$estimate$span$n0
sa_ts_ref$estimation_spec$tramo$estimate$span$n0
sa_ts_ref$result_spec$tramo$estimate$span$n0

spec_ts_d$tramo$estimate$span$n1
sa_ts_d$estimation_spec$tramo$estimate$span$n1
sa_ts_d$result_spec$tramo$estimate$span$n1
spec_tramoseats_ref$tramo$estimate$span$n1
sa_ts_ref$estimation_spec$tramo$estimate$span$n1
sa_ts_ref$result_spec$tramo$estimate$span$n1

spec_ts_d$tramo$estimate$tol
sa_ts_d$estimation_spec$tramo$estimate$tol
sa_ts_d$result_spec$tramo$estimate$tol
spec_tramoseats_ref$tramo$estimate$tol
sa_ts_ref$estimation_spec$tramo$estimate$tol
sa_ts_ref$result_spec$tramo$estimate$tol
# 
# ### decomp avec SEATS
spec_ts_d$seats$xl
sa_ts_d$estimation_spec$seats$xl
sa_ts_d$result_spec$seats$xl
spec_tramoseats_ref$seats$xl
sa_ts_ref$estimation_spec$seats$xl
sa_ts_ref$result_spec$seats$xl

spec_ts_d$seats$approximation
sa_ts_d$estimation_spec$seats$approximation
sa_ts_d$result_spec$seats$approximation
spec_tramoseats_ref$seats$approximation
sa_ts_ref$estimation_spec$seats$approximation
sa_ts_ref$result_spec$seats$approximation

spec_ts_d$seats$epsphi
sa_ts_d$estimation_spec$seats$epsphi
sa_ts_d$result_spec$seats$epsphi
spec_tramoseats_ref$seats$epsphi
sa_ts_ref$estimation_spec$seats$epsphi
sa_ts_ref$result_spec$seats$epsphi

spec_ts_d$seats$rmod
sa_ts_d$estimation_spec$seats$rmod
sa_ts_d$result_spec$seats$rmod
spec_tramoseats_ref$seats$rmod
sa_ts_ref$estimation_spec$seats$rmod
sa_ts_ref$result_spec$seats$rmod

spec_ts_d$seats$sbound
sa_ts_d$estimation_spec$seats$sbound
sa_ts_d$result_spec$seats$sbound
spec_tramoseats_ref$seats$sbound
sa_ts_ref$estimation_spec$seats$sbound
sa_ts_ref$result_spec$seats$sbound

spec_ts_d$seats$sboundatpi
sa_ts_d$estimation_spec$seats$sboundatpi
sa_ts_d$result_spec$seats$sboundatpi
spec_tramoseats_ref$seats$sboundatpi
sa_ts_ref$estimation_spec$seats$sboundatpi
sa_ts_ref$result_spec$seats$sboundatpi

spec_ts_d$seats$nfcasts
sa_ts_d$estimation_spec$seats$nfcasts
sa_ts_d$result_spec$seats$nfcasts
spec_tramoseats_ref$seats$nfcasts
sa_ts_ref$estimation_spec$seats$nfcasts
sa_ts_ref$result_spec$seats$nfcasts

spec_ts_d$seats$nbcasts
sa_ts_d$estimation_spec$seats$nbcasts
sa_ts_d$result_spec$seats$nbcasts
spec_tramoseats_ref$seats$nbcasts
sa_ts_ref$estimation_spec$seats$nbcasts
sa_ts_ref$result_spec$seats$nbcasts


spec_ts_d$seats$algorithm
sa_ts_d$estimation_spec$seats$algorithm
sa_ts_d$result_spec$seats$algorithm
spec_tramoseats_ref$seats$algorithm
sa_ts_ref$estimation_spec$seats$algorithm
sa_ts_ref$result_spec$seats$algorithm

spec_ts_d$seats$bias
sa_ts_d$estimation_spec$seats$bias
sa_ts_d$result_spec$seats$bias
spec_tramoseats_ref$seats$bias
sa_ts_ref$estimation_spec$seats$bias
sa_ts_ref$result_spec$seats$bias

# 
### benchmarking

spec_ts_d$benchmarking$enabled
sa_ts_d$estimation_spec$benchmarking$enabled
sa_ts_d$result_spec$benchmarking$enabled
spec_tramoseats_ref$benchmarking$enabled
sa_ts_ref$estimation_spec$benchmarking$enabled
sa_ts_ref$result_spec$benchmarking$enabled

spec_ts_d$benchmarking$target
sa_ts_d$estimation_spec$benchmarking$target
sa_ts_d$result_spec$benchmarking$target
spec_tramoseats_ref$benchmarking$target
sa_ts_ref$estimation_spec$benchmarking$target
sa_ts_ref$result_spec$benchmarking$target

spec_ts_d$benchmarking$lambda
sa_ts_d$estimation_spec$benchmarking$lambda
sa_ts_d$result_spec$benchmarking$lambda
spec_tramoseats_ref$benchmarking$lambda
sa_ts_ref$estimation_spec$benchmarking$lambda
sa_ts_ref$result_spec$benchmarking$lambda

spec_ts_d$benchmarking$bias
sa_ts_d$estimation_spec$benchmarking$bias
sa_ts_d$result_spec$benchmarking$bias
spec_tramoseats_ref$benchmarking$bias
sa_ts_ref$estimation_spec$benchmarking$bias
sa_ts_ref$result_spec$benchmarking$bias

spec_ts_d$benchmarking$forecast
sa_ts_d$estimation_spec$benchmarking$forecast
sa_ts_d$result_spec$benchmarking$forecast
spec_tramoseats_ref$benchmarking$forecast
sa_ts_ref$estimation_spec$benchmarking$forecast
sa_ts_ref$result_spec$benchmarking$forecast
# 

