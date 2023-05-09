
################################################################################
#####        RJD3x13 : creating Enriched spec and output for testing       #####
################################################################################


# Chargement packages ----------------------------------------------------------

library("rjd3toolkit")
library("rjd3x13")


# Import données ---------------------------------------------------------------

ipi <- read.csv2("./Data/IPI_nace4.csv")
ipi$date <- as.Date(ipi$date, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)

# creating a TS object from a data frame 
y_raw <- ts(ipi[, "RF0812"], frequency = 12, start = c(1990, 1), end = c(2021,12))
y_new <- ts(ipi[, "RF0812"], frequency = 12, start = c(1990, 1), end = c(2022,9))


# LAYERS -----------------------------------------------------------------------

## Layer 1: customized spec before estimation
## Layer 2: post estimation : estimation spec 
## Layer 3: post estimation : result spec 
## Layer 4: post spec refresh : refreshed spec 
## Layer 5: post estimation with refreshed spec: refreshed estimation spec
## Layer 6: post estimation with refreshed spec: refreshed result spec

# y_raw <- rjd3toolkit::ABS$X0.2.08.10.M
# spec_x13(name = c("rsa4", "rsa0", "rsa1", "rsa2c", "rsa3", "rsa5c"))
spec_x13_d <- rjd3x13::spec_x13("rsa5c") #### HERE PB !!! issue : rsa4 et pas rsa4c
print_JD3_X13_SPEC(spec_x13_d)


## CUSTOMIZATION by parts ------------------------------------------------------

### set basic ------------------------------------------------------------------

spec_x13_d <- set_basic(
    spec_x13_d,type = "From", 
    d0 = "2000-01-01",
    preliminary.check = TRUE,
    preprocessing = TRUE)
print_JD3_X13_SPEC(spec_x13_d)
print_JD3_REGARIMA_SPEC(spec_x13_d$regarima)





### quick check if estimation works 
sa_x13_d <- rjd3x13::x13(y_raw, spec_x13_d)
sa_x13_d$result$preprocessing
print_JD3X11(sa_x13_d$result$decomposition)

# ## test from : ok includes d0
# # spec_x13_d<-set_basic(spec_x13_d,type = "From",d0 = "2000-01-01",
# #                       preliminary.check = TRUE,
# #                       preprocessing= TRUE)
# 
# # ## test to : ok includes d1 ?
# # spec_x13_d<-set_basic(spec_x13_d,type = "To",d1 = "2000-01-01",
# #                       preliminary.check = TRUE,
# #                       preprocessing= TRUE)

# # ### Last 90 obs 
# # spec_x13_d<-set_basic(spec_x13_d,Type="Last", n1 = 90,
# #                       preliminary.check = TRUE,
# #                       preprocessing= TRUE)
# # ### Excluding : first 60 and Last 60 obs 
# # spec_x13_d<-set_basic(spec_x13_d,Type="EXcluding", n0= 60, n1 = 60,
# #                       preliminary.check = TRUE,
# #                       preprocessing= TRUE)
# 
# ##### set estimate 
spec_x13_d<-set_estimate(spec_x13_d,type = "From",d0 = "2010-01-01")
# 
# # # ### Last 60 obs SAME issue
# spec_x13_d<-rjd3x13::spec_x13("rsa5c")
# spec_x13_d<-set_estimate(spec_x13_d,Type="Last", n1 = 60)
#

### quick check if estimation works 
sa_x13_d<- rjd3x13::x13(y_raw, spec_x13_d)
sa_x13_d$result$preprocessing
sa_x13_d$result$final

# ##### set  transform 
# spec_x13_d<-rjd3x13::spec_x13("rsa5c")

spec_x13_d<- set_transform(spec_x13_d,
                           fun = "Log",
                           adjust="LengthOfPeriod",
                           outliers = TRUE)

# spec_x13_d<- set_transform(spec_x13_d,
#                            fun = "Log",
#                            outliers = TRUE)
# 
# ##### set  outlier (see pb in refresh not copied to spec ?)
# # spec_x13_d<-set_outlier(spec_x13_d, 
# #                     span.type= "From", d0 = "2012-01-01",
# #                       outliers.type = c("LS", "TC"),
# #                       critical.value = 5,
# #                       tc.rate =0.85)

spec_x13_d<-set_outlier(spec_x13_d,
                        span.type= "Last", n1 = 60,
                        outliers.type = c("LS", "TC"),
                        critical.value = 5,
                        tc.rate =0.85)
### quick check if estimation works 
sa_x13_d<- rjd3x13::x13(y_raw, spec_x13_d)
sa_x13_d$result$preprocessing
sa_x13_d$result$final

# #### set automodel 
spec_x13_d<-set_automodel(spec_x13_d,
                          enabled = FALSE,
                          cancel=0.06,
                          ub1=1.05,
                          ub2=1.15,
                          reducecv=0.15,
                          ljungboxlimit=0.96,
                          tsig=1.5,
                          ubfinal=1.06,
                          checkmu=FALSE,
                          balanced= TRUE)
### quick check if estimation works 
sa_x13_d<- rjd3x13::x13(y_raw, spec_x13_d)
sa_x13_d$result$preprocessing
sa_x13_d$result$final


# ### set benchmarking 
# 
spec_x13_d<-set_benchmarking(spec_x13_d,
                             enabled = TRUE,
                             target = "CALENDARADJUSTED",
                             rho = 0.8,
                             lambda = 0.5,
                             forecast = FALSE,
                             bias = "None")
# spec_x13_d<-rjd3x13::spec_x13("rsa5c")  

### quick check if estimation works 
sa_x13_d<- rjd3x13::x13(y_raw, spec_x13_d)
sa_x13_d$result$preprocessing
sa_x13_d$result$final
sa_x13_d$result$decomposition$
    ## ISSUE :where are benchmarking results     
    
    ### PRINT HOLE
    #### sa_x13_d$result$final : pas bon : faire un table / main results (GUI)
    
    
    #####  set arima 
    spec_x13_d<-set_automodel(spec_x13_d,
                              enabled = FALSE)
# all fixed
spec_x13_d<-set_arima(spec_x13_d,mean = 0.2,
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
sa_x13_d<- rjd3x13::x13(y_raw, spec_x13_d)
### ISSUE verifier resultats ici 
sa_x13_d$result$preprocessing
sa_x13_d$result$final


# # not fixed
# spec_x13_d<-set_arima(spec_x13_d,
#                       p = 1,
#                       d = 0,
#                       q = 1,
#                       bp = 1,
#                       bd = 1,
#                       bq = 0)

# ### set_tradingdays (NOT USER DEF, for user def see below)
# spec_x13_d<-rjd3x13::spec_x13("rsa5c")
# spec_x13_d<- set_tradingdays(spec_x13_d,
#     option = "TD4", test = "None",
#     coef=c(0.7,NA,0.5),
#     coef.type=c("Fixed","Estimated","Fixed"),
#     leapyear="LengthOfPeriod",
#     leapyear.coef=0.6
#     )

sa_x13_d<- rjd3x13::x13(y_raw, spec_x13_d)
sa_x13_d$result$preprocessing
sa_x13_d$result$final


## tester affichage en cas de stock td 
# # spec_x13_d<- set_tradingdays(spec_x13_d,stocktd=28)
# 
# 
# 
# ### set_easter
# spec_x13_d<-rjd3x13::spec_x13("rsa5c")
spec_x13_d<-set_easter(spec_x13_d,
                       enabled = TRUE,
                       duration = 12,
                       coef=0.6,
                       coef.type="Fixed",
                       test="None")
# # type = "Unused" : TRAMO specific
# # "Unused", "Standard", "IncludeEaster", "IncludeEasterMonday"
# spec_x13_d

sa_x13_d<- rjd3x13::x13(y_raw, spec_x13_d)
sa_x13_d$result$preprocessing
sa_x13_d$result$final



### Adding user defined variables 
### add outliers
# spec_x13_d<-rjd3x13::spec_x13("rsa5c")
spec_x13_d<-rjd3toolkit::add_outlier(spec_x13_d, type="AO", date="2020-03-01", coef=12)
spec_x13_d<-rjd3toolkit::add_outlier(spec_x13_d, type="LS", date="2020-04-01")
spec_x13_d

## quick estimation check 
sa_x13_d<- rjd3x13::x13(y_raw, spec_x13_d)
sa_x13_d$result$preprocessing
sa_x13_d$result$final

### add ramp 
# ramp on year 2021
spec_x13_d<-rjd3toolkit::add_ramp(spec_x13_d,start="2021-01-01",end="2021-12-01")
## quick check 
sa_x13_d<- rjd3x13::x13(y_raw, spec_x13_d)
sa_x13_d$result$preprocessing
sa_x13_d$result$final


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
# spec_x13_d<-rjd3x13::spec_x13("rsa5c")
spec_x13_d<- set_tradingdays(spec_x13_d,
                             option = "UserDefined", 
                             uservariable=c("r.Monday","r.Tuesday","r.Wednesday","r.Thursday","r.Friday","r.Saturday"), # forcement en caracteres dans un vecteur 
                             test = "None")

spec_x13_d # indicates TD_NONE...
spec_x13_d$regarima$regression$td$users

## adding other external regressors
spec_x13_d<- add_usrdefvar(spec_x13_d,id = "r.reg1",name="iv1" , regeffect="Trend")
spec_x13_d<- add_usrdefvar(spec_x13_d,id = "r.reg2", regeffect="Trend", coef=0.7)


## estimation with context and user def output : ISSUE posted : user def output doesn't work
# sa_x13_d<- rjd3x13::x13(y_raw, spec_x13_d, context = my_context, 
#                         userdefined = c("ycal","reg_t"))
sa_x13_d<- rjd3x13::x13(y_raw, spec_x13_d, context = my_context)
sa_x13_d$result$preprocessing

#################### REFRESH 

## Layer 4: refreshed spec
current_result_spec <- sa_x13_d$result_spec
current_domain_spec <- sa_x13_d$estimation_spec
spec_x13_ref <- x13_refresh(current_result_spec, # point spec to be refreshed
                            current_domain_spec, #domain spec (set of constraints)
                            policy = "FixedParameters")

## Layer 5: estimation with spec from refresh
### refreshed estimation : keep contexte
sa_x13_ref <- x13(y_new, spec_x13_ref, context=my_context)
sa_x13_ref$result$preprocessing
sa_x13_ref$result$preprocessing$description$variables 

######################################### basic
spec_x13_d$regarima$basic$span$type
sa_x13_d$estimation_spec$regarima$basic$span$type
sa_x13_d$result_spec$regarima$basic$span$type
spec_x13_ref$regarima$basic$span$type
sa_x13_ref$estimation_spec$regarima$basic$span$type
sa_x13_ref$result_spec$regarima$basic$span$type
#
spec_x13_d$regarima$basic$span$d0
sa_x13_d$estimation_spec$regarima$basic$span$d0
sa_x13_d$result_spec$regarima$basic$span$d0
spec_x13_ref$regarima$basic$span$d0
sa_x13_ref$estimation_spec$regarima$basic$span$d0
sa_x13_ref$result_spec$regarima$basic$span$d0

spec_x13_d$regarima$basic$span$d1
sa_x13_d$estimation_spec$regarima$basic$span$d1
sa_x13_d$result_spec$regarima$basic$span$d1
spec_x13_ref$regarima$basic$span$d1
sa_x13_ref$estimation_spec$regarima$basic$span$d1
sa_x13_ref$result_spec$regarima$basic$span$d1

spec_x13_d$regarima$basic$span$n0
sa_x13_d$estimation_spec$regarima$basic$span$n0
sa_x13_d$result_spec$regarima$basic$span$n0
spec_x13_ref$regarima$basic$span$n0
sa_x13_ref$estimation_spec$regarima$basic$span$n0
sa_x13_ref$result_spec$regarima$basic$span$n0

spec_x13_d$regarima$basic$span$n1
sa_x13_d$estimation_spec$regarima$basic$span$n1
sa_x13_d$result_spec$regarima$basic$span$n1
spec_x13_ref$regarima$basic$span$n1
sa_x13_ref$estimation_spec$regarima$basic$span$n1
sa_x13_ref$result_spec$regarima$basic$span$n1

spec_x13_d$regarima$basic$preprocessing
sa_x13_d$estimation_spec$regarima$basic$preprocessing
sa_x13_d$result_spec$regarima$basic$preprocessing
spec_x13_ref$regarima$basic$preprocessing
sa_x13_ref$estimation_spec$regarima$basic$preprocessing
sa_x13_ref$result_spec$regarima$basic$preprocessing

spec_x13_d$regarima$basic$preliminaryCheck
sa_x13_d$estimation_spec$regarima$basic$preliminaryCheck
sa_x13_d$result_spec$regarima$basic$preliminaryCheck
spec_x13_ref$regarima$basic$preliminaryCheck
sa_x13_ref$estimation_spec$regarima$basic$preliminaryCheck
sa_x13_ref$result_spec$regarima$basic$preliminaryCheck

# # #################### transform
spec_x13_d$regarima$transform$fn
sa_x13_d$estimation_spec$regarima$transform$fn
sa_x13_d$result_spec$regarima$transform$fn
spec_x13_ref$regarima$transform$fn
sa_x13_ref$estimation_spec$regarima$transform$fn
sa_x13_ref$result_spec$regarima$transform$fn
spec_x13_d$regarima$transform$adjust
sa_x13_d$estimation_spec$regarima$transform$adjust
sa_x13_d$result_spec$regarima$transform$adjust
spec_x13_ref$regarima$transform$adjust
sa_x13_ref$estimation_spec$regarima$transform$adjust
sa_x13_ref$result_spec$regarima$transform$adjust

spec_x13_d$regarima$transform$aicdiff
sa_x13_d$estimation_spec$regarima$transform$aicdiff
sa_x13_d$result_spec$regarima$transform$aicdiff
spec_x13_ref$regarima$transform$aicdiff
sa_x13_ref$estimation_spec$regarima$transform$aicdiff
sa_x13_ref$result_spec$regarima$transform$aicdiff

# ## outlier (auto detection params)
# ## pb $ outlier$outlier
# ### $type : Attention ici copy du span du modelling ? outlier span !!
spec_x13_d$regarima$outlier$outlier$type
spec_x13_d$regarima$outlier$outlier$d0
spec_x13_d$regarima$outlier$outlier$d1
spec_x13_d$regarima$outlier$outlier$n0
spec_x13_d$regarima$outlier$outlier$n1
#
# ### sa_x13_d$estimation_spec$regarima$outlier$ n'existe plus dans l'estimation spec et apres
#
# #
spec_x13_d$regarima$outlier$outliers
sa_x13_d$estimation_spec$regarima$outlier$outliers
sa_x13_d$result_spec$regarima$outlier$outliers # infos sur type et va critique perdues
spec_x13_ref$regarima$outlier$outliers
sa_x13_ref$estimation_spec$regarima$outlier$outliers
sa_x13_ref$result_spec$regarima$outlier$outliers
#
# ###
# ### deuxieme span marqué dans spec apres spec_x13_d$regarima$outlier$outlier$type ..etc
# ###### pas rempli
spec_x13_d$regarima$outlier$span$type
sa_x13_d$estimation_spec$regarima$outlier$span$type
sa_x13_d$result_spec$regarima$outlier$span$type
spec_x13_ref$regarima$outlier$span$type
sa_x13_ref$estimation_spec$regarima$outlier$span$type
sa_x13_ref$result_spec$regarima$outlier$span$type
# #
# # spec_x13_d$regarima$outlier$outlier$type
# #
# # spec_x13_d$regarima$outlier$span$d0
# # sa_x13_d$estimation_spec$regarima$outlier$span$d0
# # sa_x13_d$result_spec$regarima$outlier$span$d0
# # spec_x13_ref$regarima$outlier$span$d0
# # sa_x13_ref$estimation_spec$regarima$outlierspan$d0
# # sa_x13_ref$result_spec$regarima$outlier$span$d0
# #
# # spec_x13_d$regarima$outlier$span$d1
# # sa_x13_d$estimation_spec$regarima$outlier$span$d1
# # sa_x13_d$result_spec$regarima$outlier$span$d1
# # spec_x13_ref$regarima$outlier$span$d1
# # sa_x13_ref$estimation_spec$regarima$outlierspan$d1
# # sa_x13_ref$result_spec$regarima$outlier$span$d1
# #
# #
# # spec_x13_d$regarima$outlier$span$n0
# # sa_x13_d$estimation_spec$regarima$outlier$span$n0
# # sa_x13_d$result_spec$regarima$outlier$span$n0
# # spec_x13_ref$regarima$outlier$span$n0
# # sa_x13_ref$estimation_spec$regarima$outlier$span$n0
# # sa_x13_ref$result_spec$regarima$outlier$span$n0
# #
# spec_x13_d$regarima$outlier$span$n1
# sa_x13_d$estimation_spec$regarima$outlier$span$n1
# sa_x13_d$result_spec$regarima$outlier$span$n1
# spec_x13_ref$regarima$outlier$span$n1
# sa_x13_ref$estimation_spec$regarima$outlier$span$n1
# sa_x13_ref$result_spec$regarima$outlier$span$n1
# #
# spec_x13_d$regarima$outlier$defva
# spec_x13_d$regarima$outlier$defva
# sa_x13_d$estimation_spec$regarima$outlier$defva
# sa_x13_d$result_spec$regarima$outlier$defva
# spec_x13_ref$regarima$outlier$defva
# sa_x13_ref$estimation_spec$regarima$outlier$defva
# sa_x13_ref$result_spec$regarima$outlier$defva
# #
# #
# spec_x13_d$regarima$outlier$method
# sa_x13_d$estimation_spec$regarima$outlier$method
# sa_x13_d$result_spec$regarima$outlier$method
# spec_x13_ref$regarima$outlier$method
# sa_x13_ref$estimation_spec$regarima$outlier$method
# sa_x13_ref$result_spec$regarima$outlier$method
# #
# # # default value doesn't appear ?
# spec_x13_d$regarima$outlier$monthlytcrate
# sa_x13_d$estimation_spec$regarima$outlier$monthlytcrate
# sa_x13_d$result_spec$regarima$outlier$monthlytcrate
# spec_x13_ref$regarima$outlier$monthlytcrate
# sa_x13_ref$estimation_spec$regarima$outlier$monthlytcrate
# sa_x13_ref$result_spec$regarima$outlier$monthlytcrate
# #
# # spec_x13_d$regarima$outlier$maxiter
# # sa_x13_d$estimation_spec$regarima$outlier$maxiter
# # sa_x13_d$result_spec$regarima$outlier$maxiter
# # spec_x13_ref$regarima$outlier$maxiter
# # sa_x13_ref$estimation_spec$regarima$outlier$maxiter
# # sa_x13_ref$result_spec$regarima$outlier$maxiter
# #
# # spec_x13_d$regarima$outlier$lsrun
# # sa_x13_d$estimation_spec$regarima$outlier$lsrun
# # sa_x13_d$result_spec$regarima$outlier$lsrun
# # spec_x13_ref$regarima$outlier$lsrun
# # sa_x13_ref$estimation_spec$regarima$outlier$lsrun
# # sa_x13_ref$result_spec$regarima$outlier$lsrun
# #
# # ## arima
# spec_x13_d$regarima$arima$period
# sa_x13_d$estimation_spec$regarima$arima$period
# sa_x13_d$result_spec$regarima$arima$period
# spec_x13_ref$regarima$arima$period
# sa_x13_ref$estimation_spec$regarima$arima$period
# sa_x13_ref$result_spec$regarima$arima$period
#
# #### set arima
# ## pre-condition
# # ## automodel
# spec_x13_d$regarima$automodel$enabled
# sa_x13_d$estimation_spec$regarima$automodel$enabled
# sa_x13_d$result_spec$regarima$automodel$enabled
# spec_x13_ref$regarima$automodel$enabled
# sa_x13_ref$estimation_spec$regarima$automodel$enabled
# sa_x13_ref$result_spec$regarima$automodel$enabled
# # #
# spec_x13_d$regarima$arima$d
# sa_x13_d$estimation_spec$regarima$arima$d
# sa_x13_d$result_spec$regarima$arima$d
# spec_x13_ref$regarima$arima$d
# sa_x13_ref$estimation_spec$regarima$arima$d
# sa_x13_ref$result_spec$regarima$arima$d
# #
# spec_x13_d$regarima$arima$bd
# sa_x13_d$estimation_spec$regarima$arima$bd
# sa_x13_d$result_spec$regarima$arima$bd
# spec_x13_ref$regarima$arima$bd
# sa_x13_ref$estimation_spec$regarima$arima$bd
# sa_x13_ref$result_spec$regarima$arima$bd
#
# spec_x13_d$regarima$arima$phi
# sa_x13_d$estimation_spec$regarima$arima$phi
# sa_x13_d$result_spec$regarima$arima$phi
# spec_x13_ref$regarima$arima$phi
# sa_x13_ref$estimation_spec$regarima$arima$phi
# sa_x13_ref$result_spec$regarima$arima$phi
#
# spec_x13_d$regarima$arima$theta
# sa_x13_d$estimation_spec$regarima$arima$theta
# sa_x13_d$result_spec$regarima$arima$theta
# spec_x13_ref$regarima$arima$theta
# sa_x13_ref$estimation_spec$regarima$arima$theta
# sa_x13_ref$result_spec$regarima$arima$theta
#
# spec_x13_d$regarima$arima$bphi
# sa_x13_d$estimation_spec$regarima$arima$bphi
# sa_x13_d$result_spec$regarima$arima$bphi
# spec_x13_ref$regarima$arima$bphi
# sa_x13_ref$estimation_spec$regarima$arima$bphi
# sa_x13_ref$result_spec$regarima$arima$bphi
#
# spec_x13_d$regarima$arima$btheta
# sa_x13_d$estimation_spec$regarima$arima$btheta
# sa_x13_d$result_spec$regarima$arima$btheta
# spec_x13_ref$regarima$arima$btheta
# sa_x13_ref$estimation_spec$regarima$arima$btheta
# sa_x13_ref$result_spec$regarima$arima$btheta
#
# # ## automodel
# # spec_x13_d$regarima$automodel$enabled
# # sa_x13_d$estimation_spec$regarima$automodel$enabled
# # sa_x13_d$result_spec$regarima$automodel$enabled
# # spec_x13_ref$regarima$automodel$enabled
# # sa_x13_ref$estimation_spec$regarima$automodel$enabled
# # sa_x13_ref$result_spec$regarima$automodel$enabled
# #
# # spec_x13_d$regarima$automodel$ljungbox
# # sa_x13_d$estimation_spec$regarima$automodel$ljungbox
# # sa_x13_d$result_spec$regarima$automodel$ljungbox
# # spec_x13_ref$regarima$automodel$ljungbox
# # sa_x13_ref$estimation_spec$regarima$automodel$ljungbox
# # sa_x13_ref$result_spec$regarima$automodel$ljungbox
# #
# # spec_x13_d$regarima$automodel$tsig
# # sa_x13_d$estimation_spec$regarima$automodel$tsig
# # sa_x13_d$result_spec$regarima$automodel$tsig
# # spec_x13_ref$regarima$automodel$tsig
# # sa_x13_ref$estimation_spec$regarima$automodel$tsig
# # sa_x13_ref$result_spec$regarima$automodel$tsig
# #
# # spec_x13_d$regarima$automodel$predcv
# # sa_x13_d$estimation_spec$regarima$automodel$predcv
# # sa_x13_d$result_spec$regarima$automodel$predcv
# # spec_x13_ref$regarima$automodel$predcv
# # sa_x13_ref$estimation_spec$regarima$automodel$predcv
# # sa_x13_ref$result_spec$regarima$automodel$predcv
# #
# # spec_x13_d$regarima$automodel$ubfinal
# # sa_x13_d$estimation_spec$regarima$automodel$ubfinal
# # sa_x13_d$result_spec$regarima$automodel$ubfinal
# # spec_x13_ref$regarima$automodel$ubfinal
# # sa_x13_ref$estimation_spec$regarima$automodel$ubfinal
# # sa_x13_ref$result_spec$regarima$automodel$ubfinal
# #
# # spec_x13_d$regarima$automodel$ub1
# # sa_x13_d$estimation_spec$regarima$automodel$ub1
# # sa_x13_d$result_spec$regarima$automodel$ub1
# # spec_x13_ref$regarima$automodel$ub1
# # sa_x13_ref$estimation_spec$regarima$automodel$ub1
# # sa_x13_ref$result_spec$regarima$automodel$ub1
# # #
# # spec_x13_d$regarima$automodel$ub2
# # sa_x13_d$estimation_spec$regarima$automodel$ub2
# # sa_x13_d$result_spec$regarima$automodel$ub2
# # spec_x13_ref$regarima$automodel$ub2
# # sa_x13_ref$estimation_spec$regarima$automodel$ub2
# # sa_x13_ref$result_spec$regarima$automodel$ub2
# #
# #
# # spec_x13_d$regarima$automodel$cancel
# # sa_x13_d$estimation_spec$regarima$automodel$cancel
# # sa_x13_d$result_spec$regarima$automodel$cancel
# # spec_x13_ref$regarima$automodel$cancel
# # sa_x13_ref$estimation_spec$regarima$automodel$cancel
# # sa_x13_ref$result_spec$regarima$automodel$cancel
# # #
# # spec_x13_d$regarima$automodel$fct
# # sa_x13_d$estimation_spec$regarima$automodel$fct
# # sa_x13_d$result_spec$regarima$automodel$fct
# # spec_x13_ref$regarima$automodel$fct
# # sa_x13_ref$estimation_spec$regarima$automodel$fct
# # sa_x13_ref$result_spec$regarima$automodel$fct
# #
# # spec_x13_d$regarima$automodel$acceptdef
# # sa_x13_d$estimation_spec$regarima$automodel$acceptdef
# # sa_x13_d$result_spec$regarima$automodel$acceptdef
# # spec_x13_ref$regarima$automodel$acceptdef
# # sa_x13_ref$estimation_spec$regarima$automodel$acceptdef
# # sa_x13_ref$result_spec$regarima$automodel$acceptdef
# #
# # spec_x13_d$regarima$automodel$mixed
# # sa_x13_d$estimation_spec$regarima$automodel$mixed
# # sa_x13_d$result_spec$regarima$automodel$mixed
# # spec_x13_ref$regarima$automodel$mixed
# # sa_x13_ref$estimation_spec$regarima$automodel$mixed
# # sa_x13_ref$result_spec$regarima$automodel$mixed
# #
# #
# # spec_x13_d$regarima$automodel$balanced
# # sa_x13_d$estimation_spec$regarima$automodel$balanced
# # sa_x13_d$result_spec$regarima$automodel$balanced
# # spec_x13_ref$regarima$automodel$balanced
# # sa_x13_ref$estimation_spec$regarima$automodel$balanced
# # sa_x13_ref$result_spec$regarima$automodel$balanced
# #
# # ## regression
# spec_x13_d$regarima$regression$mean
# sa_x13_d$estimation_spec$regarima$regression$mean
# sa_x13_d$result_spec$regarima$regression$mean
# spec_x13_ref$regarima$regression$mean # nothing in spec
# sa_x13_ref$estimation_spec$regarima$regression$mean
# sa_x13_ref$result_spec$regarima$regression$mean # estimated value only here
# #
# spec_x13_d$regarima$regression$check_mean
# sa_x13_d$estimation_spec$regarima$regression$check_mean
# sa_x13_d$result_spec$regarima$regression$check_mean
# spec_x13_ref$regarima$regression$check_mean
# sa_x13_ref$estimation_spec$regarima$regression$check_mean
# sa_x13_ref$result_spec$regarima$regression$check_mean
# #
#
# ## regression$td
# ### what is this
# ### how to change it
# spec_x13_d$regarima$regression$td$td
# sa_x13_d$estimation_spec$regarima$regression$td$td
# sa_x13_d$result_spec$regarima$regression$td$td
# spec_x13_ref$regarima$regression$td$td
# sa_x13_ref$estimation_spec$regarima$regression$td$td
# sa_x13_ref$result_spec$regarima$regression$td$td
# #
# spec_x13_d$regarima$regression$td$lp
# sa_x13_d$estimation_spec$regarima$regression$td$lp
# sa_x13_d$result_spec$regarima$regression$td$lp
# spec_x13_ref$regarima$regression$td$lp
# sa_x13_ref$estimation_spec$regarima$regression$td$lp
# sa_x13_ref$result_spec$regarima$regression$td$lp
# #
# spec_x13_d$regarima$regression$td$holidays
# sa_x13_d$estimation_spec$regarima$regression$td$holidays
# sa_x13_d$result_spec$regarima$regression$td$holidays
# spec_x13_ref$regarima$regression$td$holidays
# sa_x13_ref$estimation_spec$regarima$regression$td$holidays
# sa_x13_ref$result_spec$regarima$regression$td$holidays
# #
# spec_x13_d$regarima$regression$td$users
# sa_x13_d$estimation_spec$regarima$regression$td$users
# sa_x13_d$result_spec$regarima$regression$td$users
# spec_x13_ref$regarima$regression$td$users
# sa_x13_ref$estimation_spec$regarima$regression$td$users
# sa_x13_ref$result_spec$regarima$regression$td$users
# #
# spec_x13_d$regarima$regression$td$w
# sa_x13_d$estimation_spec$regarima$regression$td$w
# sa_x13_d$result_spec$regarima$regression$td$w
# spec_x13_ref$regarima$regression$td$w
# sa_x13_ref$estimation_spec$regarima$regression$td$w
# sa_x13_ref$result_spec$regarima$regression$td$w
# #
# spec_x13_d$regarima$regression$td$test
# sa_x13_d$estimation_spec$regarima$regression$td$test
# sa_x13_d$result_spec$regarima$regression$td$test
# spec_x13_ref$regarima$regression$td$test
# sa_x13_ref$estimation_spec$regarima$regression$td$test
# sa_x13_ref$result_spec$regarima$regression$td$test
# #
# spec_x13_d$regarima$regression$td$auto
# sa_x13_d$estimation_spec$regarima$regression$td$auto
# sa_x13_d$result_spec$regarima$regression$td$auto
# spec_x13_ref$regarima$regression$td$auto
# sa_x13_ref$estimation_spec$regarima$regression$td$auto
# sa_x13_ref$result_spec$regarima$regression$td$auto
# #
# spec_x13_d$regarima$regression$td$autoadjust
# sa_x13_d$estimation_spec$regarima$regression$td$autoadjust
# sa_x13_d$result_spec$regarima$regression$td$autoadjust
# spec_x13_ref$regarima$regression$td$autoadjust
# sa_x13_ref$estimation_spec$regarima$regression$td$autoadjust
# sa_x13_ref$result_spec$regarima$regression$td$autoadjust
# #
# spec_x13_d$regarima$regression$td$tdcoefficients
# sa_x13_d$estimation_spec$regarima$regression$td$tdcoefficients
# sa_x13_d$result_spec$regarima$regression$td$tdcoefficients
# spec_x13_ref$regarima$regression$td$tdcoefficients
# sa_x13_ref$estimation_spec$regarima$regression$td$tdcoefficients
# sa_x13_ref$result_spec$regarima$regression$td$tdcoefficients
# #
# spec_x13_d$regarima$regression$td$lpcoefficient
# sa_x13_d$estimation_spec$regarima$regression$td$lpcoefficient
# sa_x13_d$result_spec$regarima$regression$td$lpcoefficient
# spec_x13_ref$regarima$regression$td$lpcoefficient
# sa_x13_ref$estimation_spec$regarima$regression$td$lpcoefficient
# sa_x13_ref$result_spec$regarima$regression$td$lpcoefficient
#
# ## regression$easter
spec_x13_d$regarima$regression$easter$type
sa_x13_d$estimation_spec$regarima$regression$easter$type
sa_x13_d$result_spec$regarima$regression$easter$type
spec_x13_ref$regarima$regression$easter$type
sa_x13_ref$estimation_spec$regarima$regression$easter$type
sa_x13_ref$result_spec$regarima$regression$easter$type
#
#
spec_x13_d$regarima$regression$easter$test
sa_x13_d$estimation_spec$regarima$regression$easter$test
sa_x13_d$result_spec$regarima$regression$easter$test
spec_x13_ref$regarima$regression$easter$test
sa_x13_ref$estimation_spec$regarima$regression$easter$test
sa_x13_ref$result_spec$regarima$regression$easter$test
#
spec_x13_d$regarima$regression$easter$coefficient
sa_x13_d$estimation_spec$regarima$regression$easter$coefficient
sa_x13_d$result_spec$regarima$regression$easter$coefficient
spec_x13_ref$regarima$regression$easter$coefficient
sa_x13_ref$estimation_spec$regarima$regression$easter$coefficient
sa_x13_ref$result_spec$regarima$regression$easter$coefficient

# # ## outliers / ramps / user def vars
spec_x13_d$regarima$regression$outliers
sa_x13_d$estimation_spec$regarima$regression$outliers
sa_x13_d$result_spec$regarima$regression$outliers
spec_x13_ref$regarima$regression$outliers
sa_x13_ref$estimation_spec$regarima$regression$outliers
sa_x13_ref$result_spec$regarima$regression$outliers
#
spec_x13_d$regarima$regression$ramps
sa_x13_d$estimation_spec$regarima$regression$ramps
sa_x13_d$result_spec$regarima$regression$ramps
spec_x13_ref$regarima$regression$ramps
sa_x13_ref$estimation_spec$regarima$regression$ramps
sa_x13_ref$result_spec$regarima$regression$ramps
# #
spec_x13_d$regarima$regression$users
sa_x13_d$estimation_spec$regarima$regression$users
sa_x13_d$result_spec$regarima$regression$users
spec_x13_ref$regarima$regression$users
sa_x13_ref$estimation_spec$regarima$regression$users
sa_x13_ref$result_spec$regarima$regression$users

### estimate
spec_x13_d$regarima$estimate$span$type
sa_x13_d$estimation_spec$regarima$estimate$span$type
sa_x13_d$result_spec$regarima$estimate$span$type
spec_x13_ref$regarima$estimate$span$type
sa_x13_ref$estimation_spec$regarima$estimate$span$type
sa_x13_ref$result_spec$regarima$estimate$span$type
# # #
# # #
spec_x13_d$regarima$estimate$span$d0
sa_x13_d$estimation_spec$regarima$estimate$span$d0
sa_x13_d$result_spec$regarima$estimate$span$d0
spec_x13_ref$regarima$estimate$span$d0
sa_x13_ref$estimation_spec$regarima$estimate$span$d0
sa_x13_ref$result_spec$regarima$estimate$span$d0
# #
# #
spec_x13_d$regarima$estimate$span$d1
sa_x13_d$estimation_spec$regarima$estimate$span$d1
sa_x13_d$result_spec$regarima$estimate$span$d1
spec_x13_ref$regarima$estimate$span$d1
sa_x13_ref$estimation_spec$regarima$estimate$span$d1
sa_x13_ref$result_spec$regarima$estimate$span$d1


spec_x13_d$regarima$estimate$span$n0
sa_x13_d$estimation_spec$regarima$estimate$span$n0
sa_x13_d$result_spec$regarima$estimate$span$n0
spec_x13_ref$regarima$estimate$span$n0
sa_x13_ref$estimation_spec$regarima$estimate$span$n0
sa_x13_ref$result_spec$regarima$estimate$span$n0
#
spec_x13_d$regarima$estimate$span$n1
sa_x13_d$estimation_spec$regarima$estimate$span$n1
sa_x13_d$result_spec$regarima$estimate$span$n1
spec_x13_ref$regarima$estimate$span$n1
sa_x13_ref$estimation_spec$regarima$estimate$span$n1
sa_x13_ref$result_spec$regarima$estimate$span$n1
# #
spec_x13_d$regarima$estimate$tol
sa_x13_d$estimation_spec$regarima$estimate$tol
sa_x13_d$result_spec$regarima$estimate$tol
spec_x13_ref$regarima$estimate$tol
sa_x13_ref$estimation_spec$regarima$estimate$tol
sa_x13_ref$result_spec$regarima$estimate$tol

# ### decomp avec X11
spec_x13_d$x11$mode
sa_x13_d$estimation_spec$x11$mode
sa_x13_d$result_spec$x11$mode
spec_x13_ref$x11$mode
sa_x13_ref$estimation_spec$x11$mode
sa_x13_ref$result_spec$x11$mode

spec_x13_d$x11$seasonal
sa_x13_d$estimation_spec$x11$seasonal
sa_x13_d$result_spec$x11$seasonal
spec_x13_ref$x11$seasonal
sa_x13_ref$estimation_spec$x11$seasonal
sa_x13_ref$result_spec$x11$seasonal

spec_x13_d$x11$henderson
sa_x13_d$estimation_spec$x11$henderson
sa_x13_d$result_spec$x11$henderson
spec_x13_ref$x11$henderson
sa_x13_ref$estimation_spec$x11$henderson
sa_x13_ref$result_spec$x11$henderson

spec_x13_d$x11$sfilters
sa_x13_d$estimation_spec$x11$sfilters
sa_x13_d$result_spec$x11$sfilters
spec_x13_ref$x11$sfilters
sa_x13_ref$estimation_spec$x11$sfilters
sa_x13_ref$result_spec$x11$sfilters


spec_x13_d$x11$lsig
sa_x13_d$estimation_spec$x11$lsig
sa_x13_d$result_spec$x11$lsig
spec_x13_ref$x11$lsig
sa_x13_ref$estimation_spec$x11$lsig
sa_x13_ref$result_spec$x11$lsig

spec_x13_d$x11$usig
sa_x13_d$estimation_spec$x11$usig
sa_x13_d$result_spec$x11$usig
spec_x13_ref$x11$usig
sa_x13_ref$estimation_spec$x11$usig
sa_x13_ref$result_spec$x11$usig

spec_x13_d$x11$nfcasts
sa_x13_d$estimation_spec$x11$nfcasts
sa_x13_d$result_spec$x11$nfcasts
spec_x13_ref$x11$nfcasts
sa_x13_ref$estimation_spec$x11$nfcasts
sa_x13_ref$result_spec$x11$nfcasts

spec_x13_d$x11$nbcasts
sa_x13_d$estimation_spec$x11$nbcasts
sa_x13_d$result_spec$x11$nbcasts
spec_x13_ref$x11$nbcasts
sa_x13_ref$estimation_spec$x11$nbcasts
sa_x13_ref$result_spec$x11$nbcasts


spec_x13_d$x11$vsigmas
sa_x13_d$estimation_spec$x11$vsigmas
sa_x13_d$result_spec$x11$vsigmas
spec_x13_ref$x11$vsigmas
sa_x13_ref$estimation_spec$x11$vsigmas
sa_x13_ref$result_spec$x11$vsigmas

spec_x13_d$x11$excludefcasts
sa_x13_d$estimation_spec$x11$excludefcasts
sa_x13_d$result_spec$x11$excludefcasts
spec_x13_ref$x11$excludefcasts
sa_x13_ref$estimation_spec$x11$excludefcasts
sa_x13_ref$result_spec$x11$excludefcasts


spec_x13_d$x11$bias
sa_x13_d$estimation_spec$x11$bias
sa_x13_d$result_spec$x11$bias
spec_x13_ref$x11$bias
sa_x13_ref$estimation_spec$x11$bias
sa_x13_ref$result_spec$x11$bias


# ### benchmarking

spec_x13_d$benchmarking$enabled
sa_x13_d$estimation_spec$benchmarking$enabled
sa_x13_d$result_spec$benchmarking$enabled
spec_x13_ref$benchmarking$enabled
sa_x13_ref$estimation_spec$benchmarking$enabled
sa_x13_ref$result_spec$benchmarking$enabled

spec_x13_d$benchmarking$target
sa_x13_d$estimation_spec$benchmarking$target
sa_x13_d$result_spec$benchmarking$target
spec_x13_ref$benchmarking$target
sa_x13_ref$estimation_spec$benchmarking$target
sa_x13_ref$result_spec$benchmarking$target

spec_x13_d$benchmarking$lambda
sa_x13_d$estimation_spec$benchmarking$lambda
sa_x13_d$result_spec$benchmarking$lambda
spec_x13_ref$benchmarking$lambda
sa_x13_ref$estimation_spec$benchmarking$lambda
sa_x13_ref$result_spec$benchmarking$lambda

spec_x13_d$benchmarking$bias
sa_x13_d$estimation_spec$benchmarking$bias
sa_x13_d$result_spec$benchmarking$bias
spec_x13_ref$benchmarking$bias
sa_x13_ref$estimation_spec$benchmarking$bias
sa_x13_ref$result_spec$benchmarking$bias

spec_x13_d$benchmarking$forecast
sa_x13_d$estimation_spec$benchmarking$forecast
sa_x13_d$result_spec$benchmarking$forecast
spec_x13_ref$benchmarking$forecast
sa_x13_ref$estimation_spec$benchmarking$forecast
sa_x13_ref$result_spec$benchmarking$forecast

