##### List of scripts on refresh

#### Focusing on spec writing
# here customized spec
# with refresh (several versions) indications:
#### what has just to be copied
#### what is to be re-estimated
#### what is fixed

#####################
library("rjd3toolkit")
library("rjd3x13")

# x13_refresh<-function(spec,
#                       refspec=NULL,
#                       policy=c("FreeParameters", "Complete",
#                                "Outliers_StochasticComponent",
#                                "Outliers", "FixedParameters",
#                                "FixedAutoRegressiveParameters",
#                                "Fixed", "Current"),
#                       period=0,
#                       start=NULL,
#                       end=NULL){
#     policy <- match.arg(policy)
#     if (!inherits(spec, "JD3_X13_SPEC"))
#         stop("Invalid specification type")
#     jspec<-.r2jd_spec_x13(spec)
#     if (is.null(refspec)){
#         jrefspec<-.jcall("jdplus/x13/base/api/x13/X13Spec",
#                          "Ljdplus/x13/base/api/x13/X13Spec;",
#                          "fromString", "rsa4")
#
#     }else{
#         if (!inherits(refspec, "JD3_X13_SPEC"))
#             stop("Invalid specification type")
#         jrefspec<-.r2jd_spec_x13(refspec)
#     }
#     if (policy == 'Current'){
#         if (end[2] == period) end<-c(end[1]+1, 1) else end<-c(end[1], end[2]+1)
#         jdom<-rjd3toolkit::.jdomain(period, start, end)
#     }
#     else if (policy %in% c("Outliers","Outliers_StochasticComponent"))
#         jdom<-rjd3toolkit::.jdomain(period, NULL, start)
#     else
#         jdom<-jdom<-rjd3toolkit::.jdomain(0, NULL, NULL)
#     jnspec<-.jcall("jdplus/x13/base/r/X13",
#                    "Ljdplus/x13/base/api/x13/X13Spec;",
#                    "refreshSpec", jspec, jrefspec, jdom, policy)
#     return(.jd2r_spec_x13(jnspec))
# }

# Data  :
ipi <- read.csv2(
    "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Data/IPI_nace4.csv"
)
ipi$date <- as.Date(ipi$date, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)
# creating a TS object from a data frame
y_raw <- ts(
    ipi[, "RF0812"],
    frequency = 12,
    start = c(1990, 1),
    end = c(2021, 12)
)
y_new <- ts(
    ipi[, "RF0812"],
    frequency = 12,
    start = c(1990, 1),
    end = c(2022, 9)
)

# start(y_raw)
end(y_raw)
# start(y_new)
end(y_new)


## make refresh period long too see re-estimations

#### Part 1: refresh a default spec with all policies
## STEP 1a refresh default spec
# spec
x13_spec_d <- rjd3x13::x13_spec("rsa5c")
x13_spec_d # 3 parts, no concept of estimation or result
# estimation : model and spec inside the model
sa_x13_d <- rjd3x13::x13(y_raw, x13_spec_d)
sa_x13_d$estimation_spec # copy of the used spec = x13_spec_d

## building a refreshed spec (show results)
### question on X11 part: everything will be redone unless undefined params
current_result_spec <- sa_x13_d$result_spec
current_result_spec
current_domain_spec <- sa_x13_d$estimation_spec
sa_x13_d$estimation_spec


############################ TESTING POLICIES
# question : why declare period ? is start compulsory ?
# issue: declaring period doesn't work ?
start(y_new)
# v1
x13_spec_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "Fixed"
)

x13_spec_ref

x13_spec_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "FixedParameters"
)

x13_spec_ref


x13_spec_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "FixedAutoRegressiveParameters"
)

x13_spec_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "FreeParameters"
)

############################## policies relying on span !!!

### 1 Outliers
# period : has to be specified
# defining start from where outliers will be re detected (till the end of the series anyway)

x13_spec_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "Outliers",
    start = c(2011, 5),
    period = 12
)

### update 08/2/24 : start needed, period needed, end unused

### 2  "Outliers_StochasticComponent"
# period : has to be specified
# defining start from where outliers will be re detected (till the end of the series anyway)
x13_spec_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "Outliers_StochasticComponent",
    start = c(2011, 5),
    period = 12
)

x13_spec_ref$regarima$outlier$span

### update 80/2/24 : start needed, period needed, end unused  : ok

# current_domain_spec
# current_result_spec
x13_spec_ref # outlier detection span shouldn't be all

# estimation with spec from refresh
sa_x13_ref <- x13(y_new, x13_spec_ref)
sa_x13_ref$estimation_spec
sa_x13_ref$result_spec
sa_x13_ref$result$preprocessing
sa_x13_ref$result$preprocessing$description$variables ## ?
# sa_x13_ref$result$preprocessing$description$variables
# sa_x13_ref$result$preprocessing$estimation$res

# mettre 1 veleur extreme avant le start pour voir si AO detecte
# need modif 1 valeur dans une s temp, extraire
y_new[15] <- 200
y_new

# estimation with spec from refresh
sa_x13_ref <- x13(y_new, x13_spec_ref)
sa_x13_ref$estimation_spec
sa_x13_ref$result_spec
sa_x13_ref$result$preprocessing
sa_x13_ref$result$preprocessing$description$variables ## ?
# sa_x13_ref$result$preprocessing$description$variables
# sa_x13_ref$result$preprocessing$estimation$res

####################################

### 3 Current
# defining span were on which AO (as intervention variables) will be added
x13_spec_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "Current",
    period = 12, # needed
    start = c(2012, 1),
    end = c(2012, 5)
)

# update 08/04 : start AND end needed, AO can be introduced not only at the end
## where are the AO as intervention variables in the spec
x13_spec_ref$regarima$regression$interventions


# can you identify current in the spec  ? see AO's ?

# ## current should behave like "FreeParameters: NO in fact fixed to be checked !!
x13_spec_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "FreeParameters"
)

x13_spec_ref
# here outliers not enabled

# estimation with spec from refresh
sa_x13_ref <- x13(y_new, x13_spec_ref)
sa_x13_ref$estimation_spec
sa_x13_ref$result_spec
sa_x13_ref$result$preprocessing
## où sont les AOs as intervention variables
sa_x13_ref$result$preprocessing$description$variables
sa_x13_ref$result$preprocessing$description$variables ## ?
# sa_x13_ref$result$preprocessing$description$variables
# sa_x13_ref$result$preprocessing$estimation$res

#################################### end default test spec ####################
# example for git issue : CURRENT

y <- rjd3toolkit::ABS$X0.2.08.10.M
# raw series for first estimation
y_raw <- window(y, end = 2009)
# raw series for second (refreshed) estimation
y_new <- window(y, end = 2010)
# specification for first estimation
spec_x13_1 <- x13_spec("rsa5c")
# first estimation
sa_x13 <- x13(y_raw, spec_x13_1) # AO (200,6) and TD (2000,7)
# refreshing the specification
current_result_spec <- sa_x13$result_spec
current_domain_spec <- sa_x13$estimation_spec

### test current
spec_x13_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "Current",
    period = 12,
    start = c(2022, 1)
)

# end=c(2022,1)) # should put every new point in AO from there
spec_x13_ref # all parameters are fixed
# 2nd estimation with refreshed specification
sa_x13_ref <- x13(y_new, spec_x13_ref)

sa_x13_ref$result$preprocessing # no new AOs
sa_x13_ref$result$preprocessing$description$variables # no new AOs

window(sa_x13_ref$result$final$d11final, start = 2009, end = 2010)

### refresh with "Fixed"
spec_x13_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "Fixed"
)
sa_x13_ref <- x13(y_new, spec_x13_ref)
window(sa_x13_ref$result$final$d11final, start = 2009, end = 2010) # same result as current


#######################################################
#################################### end default test spec ####################
# example for git issue : outliers

y <- rjd3toolkit::ABS$X0.2.08.10.M
# raw series for first estimation
y_raw <- window(y, end = 2009)
# raw series for second (refreshed) estimation
y_new <- window(y, end = 2010)
# specification for first estimation
spec_x13_1 <- x13_spec("rsa5c")
# first estimation
sa_x13 <- x13(y_raw, spec_x13_1) # AO (200,6) and TD (2000,7)

# refreshing the specification
current_result_spec <- sa_x13$result_spec
current_domain_spec <- sa_x13$estimation_spec
spec_x13_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "Outliers",
    period = 12,
    start = c(2001, 1), # outliers should be  identified before 2001
    end = c(2008, 12)
) # outliers should be identified from 2009
spec_x13_ref # outliers will be identified from 2009
# 2nd estimation with refreshed specification
sa_x13_ref <- x13(y_new, spec_x13_ref)

sa_x13_ref$result$preprocessing # oultliers identified from 2009 but NOT before 2001
sa_x13_ref$result$preprocessing$description$variables


### refresh with "Fixed"
spec_x13_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "Fixed"
)
sa_x13_ref <- x13(y_new, spec_x13_ref)
window(sa_x13_ref$result$final$d11final, start = 2009, end = 2010) # same result as current


#######################################################

current_result_spec <- sa_x13_d$result_spec
current_domain_spec <- sa_x13_d$estimation_spec
x13_spec_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "Current"
)
# period=12, # nb of annual observations
# start=c(2012,1), # why this
# end=c(2021,1))
s_ref <- x13(y_new, x13_spec_ref)

# policy = c("FreeParameters",
# "Complete", "Outliers_StochasticComponent",
# "Outliers",
#            "FixedParameters", "FixedAutoRegressiveParameters", "Fixed")
####
# "FixedAutoRegressiveParameters" : works with x13

########################################
## Layer 5: estimation with spec from refresh
sa_x13_ref <- x13(y_new, x13_spec_ref, context = my_context)
sa_x13_ref$result$preprocessing
sa_x13_ref$result$preprocessing$description$variables ## ?
# sa_x13_ref$result$preprocessing$description$variables
# sa_x13_ref$result$preprocessing$estimation$res

sa_x13_d$result$preprocessing
sa_x13_ref$result$preprocessing

sa_x13_ref$result$preprocessing$description$variables


# ############ CUSTOMIZATION by parts
# ### basic: ATT : won't take part to rer
# ### model or estimation ? make everything clear including vs gui and doc(s)
# ## test between (excludes last and not first)
# # x13_spec_d<-rjd3x13::x13_spec("rsa5c") #### HERE PB !!! issue : rsa4 et pas rsa4c
# ##### set basic
# x13_spec_d<-set_basic(x13_spec_d,type = "From",d0 = "2000-01-01",
#                       preliminary.check = TRUE,
#                       preprocessing= TRUE)

# # ## test from : ok includes d0
# # x13_spec_d<-set_basic(x13_spec_d,type = "From",d0 = "2000-01-01",
# #                       preliminary.check = TRUE,
# #                       preprocessing= TRUE)
#
# # ## test to : ok includes d1 ?
# # x13_spec_d<-set_basic(x13_spec_d,type = "To",d1 = "2000-01-01",
# #                       preliminary.check = TRUE,
# #                       preprocessing= TRUE)
#
# ## test n0 : to be used without "Type" ?

# #
# # ### Last 60 obs
# # x13_spec_d<-set_basic(x13_spec_d,type="Last", n1 = 60,
# #                       preliminary.check = TRUE,
# #                       preprocessing= TRUE)

# # ### Excluding : fist 60 and Last 60 obs
x13_spec_d <- set_basic(
    x13_spec_d,
    type = "Excluding",
    n0 = 60,
    n1 = 60,
    preliminary.check = TRUE,
    preprocessing = TRUE
)

sa_x13_d <- rjd3x13::x13(y_raw, x13_spec_d)
sa_x13_d$result$preprocessing
#
# ### operations on TS objects
# end(y_raw)
# end(y_new)
#
# ##### set estimate
# x13_spec_d<-set_estimate(x13_spec_d,type = "From",d0 = "2010-01-01")
# ## pre processing in estimate ? same tests with n0, n1
#
# # # ### Last 60 obs SAME issue
x13_spec_d <- rjd3x13::x13_spec("rsa5c")
x13_spec_d <- set_estimate(x13_spec_d, type = "Last", n1 = 60)

sa_x13_d <- rjd3x13::x13(y_raw, x13_spec_d)
sa_x13_d$result$preprocessing

# ##### set  transform
# x13_spec_d<-rjd3x13::x13_spec("rsa5c")

# # x13_spec_d<- set_transform(x13_spec_d,
# #                          fun = "Log",
# #                          adjust="LengthOfPeriod",
# #                          outliers = TRUE)
#  where in SPEC
# x13_spec_d<- set_transform(x13_spec_d,
#                            fun = "Log",
#                            outliers = TRUE)
#
# ##### set  outlier (see pb in refresh not copied to spec)
# # x13_spec_d<-set_outlier(x13_spec_d,
# #                     span.type= "From", d0 = "2012-01-01",
# #                       outliers.type = c("LS", "TC"),
# #                       critical.value = 5,
# #                       tc.rate =0.85)
# x13_spec_d<-set_outlier(x13_spec_d,
#                         span.type= "Last", n1 = 60,
#                         outliers.type = c("LS", "TC"),
#                         critical.value = 5,
#                         tc.rate =0.85)

# #### set automodel
# x13_spec_d<-set_automodel(x13_spec_d,
#                         enabled = FALSE,
#                         cancel=0.06,
#                         ub1=1.05,
#                         ub2=1.15,
#                         reducecv=0.15,
#                         ljungboxlimit=0.96,
#                         tsig=1.5,
#                         ubfinal=1.06,
#                         checkmu=FALSE,
#                         balanced= TRUE)
#
# ### set benchmarking (ok)
x13_spec_d <- rjd3x13::x13_spec("rsa5c")
x13_spec_d <- set_benchmarking(
    x13_spec_d,
    enabled = TRUE,
    target = "ORIGINAL",
    rho = 0.8,
    lambda = 0.5,
    forecast = FALSE,
    bias = "None"
)
sa_x13_d <- rjd3x13::x13(y_raw, x13_spec_d, userdefined = "adjust")
userdefined_variables_x13() ## should understand every item
sa_x13_d$user_defined$adjust
# # x13_spec_d<-rjd3x13::x13_spec("rsa5c")
# x13_spec_d<-set_automodel(x13_spec_d,
#                       enabled = FALSE)
#####  set arima
# all fixed
# x13_spec_d<-set_arima(x13_spec_d,mean = 0.2,
# mean.type = "Fixed",
# p = 1,
# d = 2,
# q = 0,
# bp = 1,
# bd = 1,
# bq = 0,
# coef = c(0.6,0.7),
# coef.type = c("Initial","Fixed"))
# # not fixed
# x13_spec_d<-set_arima(x13_spec_d,
#                       p = 1,
#                       d = 0,
#                       q = 1,
#                       bp = 1,
#                       bd = 1,
#                       bq = 0)

# ### set_tradingdays
x13_spec_d <- rjd3x13::x13_spec("rsa5c")

x13_spec_d <- rjd3x13::x13_spec("rsa3")
x13_spec_d <- set_tradingdays(
    x13_spec_d,
    option = "TD4",
    test = "None",
    coef = c(0.7, NA, 0.5),
    coef.type = c("Fixed", "Estimated", "Fixed"),
    leapyear = "LengthOfPeriod",
    leapyear.coef = 0.6
)
x13_spec_d$regarima$regression$td
# #
y_raw <- rjd3toolkit::ABS$X0.2.08.10.M
# ISSUE pb estimation with stocktdif rsa3 (not if rsa5c)
x13_spec_d <- rjd3x13::x13_spec("rsa3")
x13_spec_d <- set_tradingdays(x13_spec_d, stocktd = 28)
x13_spec_d$regarima$regression$td
sa_x13_d <- rjd3x13::x13(y_raw, x13_spec_d)
#
x13_spec_d <- rjd3x13::x13_spec("rsa1")
x13_spec_d$regarima$regression$td
x13_spec_d <- set_tradingdays(x13_spec_d, stocktd = 28)
x13_spec_d$regarima$regression$td
sa_x13_d <- rjd3x13::x13(y_raw, x13_spec_d)


#
# ### set_easter
# x13_spec_d<-rjd3x13::x13_spec("rsa5c")
# x13_spec_d<-set_easter(x13_spec_d,
#                        enabled = TRUE,
#                        duration = 12,
#                        coef=0.6,
#                        coef.type="Fixed",
#                        test="None")
# # type = "Unused" : TRAMO specific
# # "Unused", "Standard", "IncludeEaster", "IncludeEasterMonday"
# x13_spec_d

### Adding user defined variables
# key = localize new params in spec !!!!!!!!!!!
# ## add outlier
# x13_spec_d<-rjd3x13::x13_spec("rsa5c")
# x13_spec_d<-rjd3toolkit::add_outlier(x13_spec_d, type="AO", date="2020-03-01", coef=12)
# x13_spec_d<-rjd3toolkit::add_outlier(x13_spec_d, type="LS", date="2020-04-01")
# x13_spec_d
#
# ramp on year 2021
# x13_spec_d<-rjd3toolkit::add_ramp(x13_spec_d,start="2021-01-01",end="2021-12-01")

#### add user def regressors
# ## add interv variables
# y_raw<-rjd3toolkit::ABS$X0.2.08.10.M
iv1 <- intervention_variable(
    12,
    c(2000, 1),
    60,
    starts = "2000-01-01",
    ends = "2001-12-01"
)
iv2 <- intervention_variable(
    12,
    c(2000, 1),
    60,
    starts = "2010-01-01",
    ends = "2010-12-01",
    delta = 1
)
### calendar regressors (to be added with set_trading days)
regs_td <- td(
    s = y_raw,
    groups = c(1, 2, 3, 4, 5, 6, 0),
    contrasts = TRUE
)

#### Creating context for all external regressors
variables <- list(
    Monday = regs_td[, 1],
    Tuesday = regs_td[, 2],
    Wednesday = regs_td[, 3],
    Thursday = regs_td[, 4],
    Friday = regs_td[, 5],
    Saturday = regs_td[, 6],
    arf = list(reg1 = iv1)
)
my_context <- modelling_context(variables = variables)
rjd3toolkit::.r2jd_modellingcontext(my_context)$getTsVariableDictionary()

### add calendar regressors to spec
x13_spec_d <- rjd3x13::x13_spec("rsa5c")
x13_spec_d <- set_tradingdays(
    x13_spec_d,
    option = "UserDefined",
    uservariable = c(
        "r.Monday",
        "r.Tuesday",
        "r.Wednesday",
        "r.Thursday",
        "r.Friday",
        "r.Saturday"
    ),
    test = "None"
)

x13_spec_d <- add_usrdefvar(
    x13_spec_d,
    group = "arf",
    name = "reg1",
    label = "iv1",
    regeffect = "Trend"
)
x13_spec_d$regarima$regression$users
sa_x13_d <- rjd3x13::x13(y_raw, x13_spec_d, context = my_context)
sa_x13_d$result$preprocessing

## ISSUE
y <- rjd3toolkit::ABS$X0.2.09.10.M
m <- x13(y, "rsa5c", userdefined = c("ycal"))
m <- x13(
    y,
    "rsa5c",
    userdefined = c(
        "variancedecomposition.total",
        "y",
        "y_b(?)",
        "y_eb(?)",
        "y_ef(?)",
        "y_f(?)"
    )
)

m$user_defined$variancedecomposition.total
## adding other external regressors
# x13_spec_d<- add_usrdefvar(x13_spec_d,group = "arf", name="reg1",label="iv1", regeffect="Trend")

### old way keep as example
# x13_spec_d<- add_usrdefvar(x13_spec_d,id = "r.reg2", regeffect="Trend", coef=0.7)

## estimation with context and user def output
# sa_x13_d<- rjd3x13::x13(y_raw, x13_spec_d, context = my_context,
#                      userdefined = c("ycal","reg_t"))
# sa_x13_d<- rjd3x13::x13(y_raw, x13_spec_d, context = my_context)
# sa_x13_d$result$preprocessing

#########################################################################
## Layer 4: refreshed spec
spec <- x13_spec("rsa3")
sa_x13_d <- x13(y_raw, spec)
current_result_spec <- sa_x13_d$result_spec
current_domain_spec <- sa_x13_d$estimation_spec
x13_spec_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "FreeParameters"
)


current_result_spec <- sa_x13_d$result_spec
current_domain_spec <- sa_x13_d$estimation_spec
x13_spec_ref <- x13_refresh(
    current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "Current"
)
# period=12, # nb of annual observations
# start=c(2012,1), # why this
# end=c(2021,1))
s_ref <- x13(y_new, x13_spec_ref)

# policy = c("FreeParameters",
# "Complete", "Outliers_StochasticComponent",
# "Outliers",
#            "FixedParameters", "FixedAutoRegressiveParameters", "Fixed")
####
# "FixedAutoRegressiveParameters" : works with x13

## Layer 5: estimation with spec from refresh
sa_x13_ref <- x13(y_new, x13_spec_ref, context = my_context)
sa_x13_ref$result$preprocessing
sa_x13_ref$result$preprocessing$description$variables ## ?
# sa_x13_ref$result$preprocessing$description$variables
# sa_x13_ref$result$preprocessing$estimation$res

sa_x13_d$result$preprocessing
sa_x13_ref$result$preprocessing

sa_x13_ref$result$preprocessing$description$variables

# sa_x13_ref$estimation_spec
## layer 6: result spec from
# sa_x13_ref$result_spec

## Layer 1 ESTIMATION // RESULT // SPEC
### REGARIMA part (first part)
########## NOT useful for refresh (might have to check that stays the same ?)
## comment 1 = what should be touched by which policy and what not
## comment 2: understanding of all param purpose (DETAIL HOLES ONLY)
## .... + how to change them here, in v2 and in GUI

############# STEP 3:  REFRESHED SPEC : check changes
### Basic
# if refreshed: always untouched  / simple copy
# x13_spec_d$regarima$basic$span$type
# sa_x13_d$estimation_spec$regarima$basic$span$type
# sa_x13_d$result_spec$regarima$basic$span$type
# x13_spec_ref$regarima$basic$span$type
# sa_x13_ref$estimation_spec$regarima$basic$span$type
# sa_x13_ref$result_spec$regarima$basic$span$type
# #
# x13_spec_d$regarima$basic$span$d0
# sa_x13_d$estimation_spec$regarima$basic$span$d0
# sa_x13_d$result_spec$regarima$basic$span$d0
# x13_spec_ref$regarima$basic$span$d0
# sa_x13_ref$estimation_spec$regarima$basic$span$d0
# sa_x13_ref$result_spec$regarima$basic$span$d0
#
# x13_spec_d$regarima$basic$span$d1
# sa_x13_d$estimation_spec$regarima$basic$span$d1
# sa_x13_d$result_spec$regarima$basic$span$d1
# x13_spec_ref$regarima$basic$span$d1
# sa_x13_ref$estimation_spec$regarima$basic$span$d1
# sa_x13_ref$result_spec$regarima$basic$span$d1
#
# x13_spec_d$regarima$basic$span$n0
# sa_x13_d$estimation_spec$regarima$basic$span$n0
# sa_x13_d$result_spec$regarima$basic$span$n0
# x13_spec_ref$regarima$basic$span$n0
# sa_x13_ref$estimation_spec$regarima$basic$span$n0
# sa_x13_ref$result_spec$regarima$basic$span$n0
#
x13_spec_d$regarima$basic$span$n1
sa_x13_d$estimation_spec$regarima$basic$span$n1
# sa_x13_d$result_spec$regarima$basic$span$n1
# x13_spec_ref$regarima$basic$span$n1
# sa_x13_ref$estimation_spec$regarima$basic$span$n1
# sa_x13_ref$result_spec$regarima$basic$span$n1
#
# x13_spec_d$regarima$basic$preprocessing
# sa_x13_d$estimation_spec$regarima$basic$preprocessing
# sa_x13_d$result_spec$regarima$basic$preprocessing
# x13_spec_ref$regarima$basic$preprocessing
# sa_x13_ref$estimation_spec$regarima$basic$preprocessing
# sa_x13_ref$result_spec$regarima$basic$preprocessing
#
# x13_spec_d$regarima$basic$preliminaryCheck
# sa_x13_d$estimation_spec$regarima$basic$preliminaryCheck
# sa_x13_d$result_spec$regarima$basic$preliminaryCheck
# x13_spec_ref$regarima$basic$preliminaryCheck
# sa_x13_ref$estimation_spec$regarima$basic$preliminaryCheck
# sa_x13_ref$result_spec$regarima$basic$preliminaryCheck

# # ## estimate
# # ### question = estimate vs span: c doc ? clear ?
# x13_spec_d$regarima$estimate$span$type
# sa_x13_d$estimation_spec$regarima$estimate$span$type
# sa_x13_d$result_spec$regarima$estimate$span$type
# x13_spec_ref$regarima$estimate$span$type
# sa_x13_ref$estimation_spec$regarima$estimate$span$type
# sa_x13_ref$result_spec$regarima$estimate$span$type
# # #
# # #
# x13_spec_d$regarima$estimate$span$d0
# sa_x13_d$estimation_spec$regarima$estimate$span$d0
# sa_x13_d$result_spec$regarima$estimate$span$d0
# x13_spec_ref$regarima$estimate$span$d0
# sa_x13_ref$estimation_spec$regarima$estimate$span$d0
# sa_x13_ref$result_spec$regarima$estimate$span$d0
# #
# #
# x13_spec_d$regarima$estimate$span$d1
# sa_x13_d$estimation_spec$regarima$estimate$span$d1
# sa_x13_d$result_spec$regarima$estimate$span$d1
# x13_spec_ref$regarima$estimate$span$d1
# sa_x13_ref$estimation_spec$regarima$estimate$span$d1
# sa_x13_ref$result_spec$regarima$estimate$span$d1
# #
# #
# x13_spec_d$regarima$estimate$span$n0
# sa_x13_d$estimation_spec$regarima$estimate$span$n0
# sa_x13_d$result_spec$regarima$estimate$span$n0
# x13_spec_ref$regarima$estimate$span$n0
# sa_x13_ref$estimation_spec$regarima$estimate$span$n0
# sa_x13_ref$result_spec$regarima$estimate$span$n0
# # #
# x13_spec_d$regarima$estimate$span$n1
# sa_x13_d$estimation_spec$regarima$estimate$span$n1
# sa_x13_d$result_spec$regarima$estimate$span$n1
# x13_spec_ref$regarima$estimate$span$n1
# sa_x13_ref$estimation_spec$regarima$estimate$span$n1
# sa_x13_ref$result_spec$regarima$estimate$span$n1
# #
# x13_spec_d$regarima$estimate$tol
# sa_x13_d$estimation_spec$regarima$estimate$tol
# sa_x13_d$result_spec$regarima$estimate$tol
# x13_spec_ref$regarima$estimate$tol
# sa_x13_ref$estimation_spec$regarima$estimate$tol
# sa_x13_ref$result_spec$regarima$estimate$tol

#
# ### check is basic span from estimation or pre-p
# sa_x13_d$result$final$d11final
# end(y_raw)
# end(sa_x13_d$result$final$d11final)
# start(y_raw)
# start(sa_x13_d$result$final$d11final)
# # #################### transform
# # ## refresh : is transported, never touched (exception = complete)
# # ### PB with refresh seems to re-estimate schema
# x13_spec_d$regarima$transform$fn
# sa_x13_d$estimation_spec$regarima$transform$fn
# sa_x13_d$result_spec$regarima$transform$fn
# x13_spec_ref$regarima$transform$fn
# sa_x13_ref$estimation_spec$regarima$transform$fn
# sa_x13_ref$result_spec$regarima$transform$fn
# # #
# # # ## here pb de adjust and leap year : not clear
# x13_spec_d$regarima$transform$adjust
# sa_x13_d$estimation_spec$regarima$transform$adjust
# sa_x13_d$result_spec$regarima$transform$adjust
# x13_spec_ref$regarima$transform$adjust
# sa_x13_ref$estimation_spec$regarima$transform$adjust
# sa_x13_ref$result_spec$regarima$transform$adjust
#
# x13_spec_d$regarima$transform$aicdiff
# sa_x13_d$estimation_spec$regarima$transform$aicdiff
# sa_x13_d$result_spec$regarima$transform$aicdiff
# x13_spec_ref$regarima$transform$aicdiff
# sa_x13_ref$estimation_spec$regarima$transform$aicdiff
# sa_x13_ref$result_spec$regarima$transform$aicdiff
# #
# ## outlier (auto detection params)
# ## pb $ outlier$outlier
# ### $type : Attention ici copy du span du modelling ? outlier span !!
x13_spec_d$regarima$outlier$outlier$type
x13_spec_d$regarima$outlier$outlier$d0
x13_spec_d$regarima$outlier$outlier$d1
x13_spec_d$regarima$outlier$outlier$n0
x13_spec_d$regarima$outlier$outlier$n1

### sa_x13_d$estimation_spec$regarima$outlier$ n'existe plus dans l'estimation spec et apres

#
x13_spec_d$regarima$outlier$outliers
sa_x13_d$estimation_spec$regarima$outlier$outliers
sa_x13_d$result_spec$regarima$outlier$outliers # infos sur type et va critique perdues
x13_spec_ref$regarima$outlier$outliers
sa_x13_ref$estimation_spec$regarima$outlier$outliers
sa_x13_ref$result_spec$regarima$outlier$outliers

###
### deuxieme span marqué dans spec apres x13_spec_d$regarima$outlier$outlier$type ..etc
###### pas rempli
x13_spec_d$regarima$outlier$span$type
sa_x13_d$estimation_spec$regarima$outlier$span$type
sa_x13_d$result_spec$regarima$outlier$span$type
x13_spec_ref$regarima$outlier$span$type
sa_x13_ref$estimation_spec$regarima$outlier$span$type
sa_x13_ref$result_spec$regarima$outlier$span$type
#
x13_spec_d$regarima$outlier$outlier$type

x13_spec_d$regarima$outlier$span$d0
sa_x13_d$estimation_spec$regarima$outlier$span$d0
sa_x13_d$result_spec$regarima$outlier$span$d0
x13_spec_ref$regarima$outlier$span$d0
sa_x13_ref$estimation_spec$regarima$outlierspan$d0
sa_x13_ref$result_spec$regarima$outlier$span$d0

x13_spec_d$regarima$outlier$span$d1
sa_x13_d$estimation_spec$regarima$outlier$span$d1
sa_x13_d$result_spec$regarima$outlier$span$d1
x13_spec_ref$regarima$outlier$span$d1
sa_x13_ref$estimation_spec$regarima$outlierspan$d1
sa_x13_ref$result_spec$regarima$outlier$span$d1


x13_spec_d$regarima$outlier$span$n0
sa_x13_d$estimation_spec$regarima$outlier$span$n0
sa_x13_d$result_spec$regarima$outlier$span$n0
x13_spec_ref$regarima$outlier$span$n0
sa_x13_ref$estimation_spec$regarima$outlier$span$n0
sa_x13_ref$result_spec$regarima$outlier$span$n0
#
x13_spec_d$regarima$outlier$span$n1
sa_x13_d$estimation_spec$regarima$outlier$span$n1
sa_x13_d$result_spec$regarima$outlier$span$n1
x13_spec_ref$regarima$outlier$span$n1
sa_x13_ref$estimation_spec$regarima$outlier$span$n1
sa_x13_ref$result_spec$regarima$outlier$span$n1
# #
# x13_spec_d$regarima$outlier$defva
# x13_spec_d$regarima$outlier$defva
# sa_x13_d$estimation_spec$regarima$outlier$defva
# sa_x13_d$result_spec$regarima$outlier$defva
# x13_spec_ref$regarima$outlier$defva
# sa_x13_ref$estimation_spec$regarima$outlier$defva
# sa_x13_ref$result_spec$regarima$outlier$defva
# #
# #
# x13_spec_d$regarima$outlier$method
# sa_x13_d$estimation_spec$regarima$outlier$method
# sa_x13_d$result_spec$regarima$outlier$method
# x13_spec_ref$regarima$outlier$method
# sa_x13_ref$estimation_spec$regarima$outlier$method
# sa_x13_ref$result_spec$regarima$outlier$method
# #
# # # default value doesn't appear ?
# x13_spec_d$regarima$outlier$monthlytcrate
# sa_x13_d$estimation_spec$regarima$outlier$monthlytcrate
# sa_x13_d$result_spec$regarima$outlier$monthlytcrate
# x13_spec_ref$regarima$outlier$monthlytcrate
# sa_x13_ref$estimation_spec$regarima$outlier$monthlytcrate
# sa_x13_ref$result_spec$regarima$outlier$monthlytcrate
# #
# # x13_spec_d$regarima$outlier$maxiter
# # sa_x13_d$estimation_spec$regarima$outlier$maxiter
# # sa_x13_d$result_spec$regarima$outlier$maxiter
# # x13_spec_ref$regarima$outlier$maxiter
# # sa_x13_ref$estimation_spec$regarima$outlier$maxiter
# # sa_x13_ref$result_spec$regarima$outlier$maxiter
# #
# # x13_spec_d$regarima$outlier$lsrun
# # sa_x13_d$estimation_spec$regarima$outlier$lsrun
# # sa_x13_d$result_spec$regarima$outlier$lsrun
# # x13_spec_ref$regarima$outlier$lsrun
# # sa_x13_ref$estimation_spec$regarima$outlier$lsrun
# # sa_x13_ref$result_spec$regarima$outlier$lsrun
# #
# # ## arima
# x13_spec_d$regarima$arima$period
# sa_x13_d$estimation_spec$regarima$arima$period
# sa_x13_d$result_spec$regarima$arima$period
# x13_spec_ref$regarima$arima$period
# sa_x13_ref$estimation_spec$regarima$arima$period
# sa_x13_ref$result_spec$regarima$arima$period
#
# #### set arima
# ## pre-condition
# # ## automodel
# x13_spec_d$regarima$automodel$enabled
# sa_x13_d$estimation_spec$regarima$automodel$enabled
# sa_x13_d$result_spec$regarima$automodel$enabled
# x13_spec_ref$regarima$automodel$enabled
# sa_x13_ref$estimation_spec$regarima$automodel$enabled
# sa_x13_ref$result_spec$regarima$automodel$enabled
# # #
# x13_spec_d$regarima$arima$d
# sa_x13_d$estimation_spec$regarima$arima$d
# sa_x13_d$result_spec$regarima$arima$d
# x13_spec_ref$regarima$arima$d
# sa_x13_ref$estimation_spec$regarima$arima$d
# sa_x13_ref$result_spec$regarima$arima$d
# #
# x13_spec_d$regarima$arima$bd
# sa_x13_d$estimation_spec$regarima$arima$bd
# sa_x13_d$result_spec$regarima$arima$bd
# x13_spec_ref$regarima$arima$bd
# sa_x13_ref$estimation_spec$regarima$arima$bd
# sa_x13_ref$result_spec$regarima$arima$bd
#
# x13_spec_d$regarima$arima$phi
# sa_x13_d$estimation_spec$regarima$arima$phi
# sa_x13_d$result_spec$regarima$arima$phi
# x13_spec_ref$regarima$arima$phi
# sa_x13_ref$estimation_spec$regarima$arima$phi
# sa_x13_ref$result_spec$regarima$arima$phi
#
# x13_spec_d$regarima$arima$theta
# sa_x13_d$estimation_spec$regarima$arima$theta
# sa_x13_d$result_spec$regarima$arima$theta
# x13_spec_ref$regarima$arima$theta
# sa_x13_ref$estimation_spec$regarima$arima$theta
# sa_x13_ref$result_spec$regarima$arima$theta
#
# x13_spec_d$regarima$arima$bphi
# sa_x13_d$estimation_spec$regarima$arima$bphi
# sa_x13_d$result_spec$regarima$arima$bphi
# x13_spec_ref$regarima$arima$bphi
# sa_x13_ref$estimation_spec$regarima$arima$bphi
# sa_x13_ref$result_spec$regarima$arima$bphi
#
# x13_spec_d$regarima$arima$btheta
# sa_x13_d$estimation_spec$regarima$arima$btheta
# sa_x13_d$result_spec$regarima$arima$btheta
# x13_spec_ref$regarima$arima$btheta
# sa_x13_ref$estimation_spec$regarima$arima$btheta
# sa_x13_ref$result_spec$regarima$arima$btheta
#
# # ## automodel
# # x13_spec_d$regarima$automodel$enabled
# # sa_x13_d$estimation_spec$regarima$automodel$enabled
# # sa_x13_d$result_spec$regarima$automodel$enabled
# # x13_spec_ref$regarima$automodel$enabled
# # sa_x13_ref$estimation_spec$regarima$automodel$enabled
# # sa_x13_ref$result_spec$regarima$automodel$enabled
# #
# # x13_spec_d$regarima$automodel$ljungbox
# # sa_x13_d$estimation_spec$regarima$automodel$ljungbox
# # sa_x13_d$result_spec$regarima$automodel$ljungbox
# # x13_spec_ref$regarima$automodel$ljungbox
# # sa_x13_ref$estimation_spec$regarima$automodel$ljungbox
# # sa_x13_ref$result_spec$regarima$automodel$ljungbox
# #
# # x13_spec_d$regarima$automodel$tsig
# # sa_x13_d$estimation_spec$regarima$automodel$tsig
# # sa_x13_d$result_spec$regarima$automodel$tsig
# # x13_spec_ref$regarima$automodel$tsig
# # sa_x13_ref$estimation_spec$regarima$automodel$tsig
# # sa_x13_ref$result_spec$regarima$automodel$tsig
# #
# # x13_spec_d$regarima$automodel$predcv
# # sa_x13_d$estimation_spec$regarima$automodel$predcv
# # sa_x13_d$result_spec$regarima$automodel$predcv
# # x13_spec_ref$regarima$automodel$predcv
# # sa_x13_ref$estimation_spec$regarima$automodel$predcv
# # sa_x13_ref$result_spec$regarima$automodel$predcv
# #
# # x13_spec_d$regarima$automodel$ubfinal
# # sa_x13_d$estimation_spec$regarima$automodel$ubfinal
# # sa_x13_d$result_spec$regarima$automodel$ubfinal
# # x13_spec_ref$regarima$automodel$ubfinal
# # sa_x13_ref$estimation_spec$regarima$automodel$ubfinal
# # sa_x13_ref$result_spec$regarima$automodel$ubfinal
# #
# # x13_spec_d$regarima$automodel$ub1
# # sa_x13_d$estimation_spec$regarima$automodel$ub1
# # sa_x13_d$result_spec$regarima$automodel$ub1
# # x13_spec_ref$regarima$automodel$ub1
# # sa_x13_ref$estimation_spec$regarima$automodel$ub1
# # sa_x13_ref$result_spec$regarima$automodel$ub1
# # #
# # x13_spec_d$regarima$automodel$ub2
# # sa_x13_d$estimation_spec$regarima$automodel$ub2
# # sa_x13_d$result_spec$regarima$automodel$ub2
# # x13_spec_ref$regarima$automodel$ub2
# # sa_x13_ref$estimation_spec$regarima$automodel$ub2
# # sa_x13_ref$result_spec$regarima$automodel$ub2
# #
# #
# # x13_spec_d$regarima$automodel$cancel
# # sa_x13_d$estimation_spec$regarima$automodel$cancel
# # sa_x13_d$result_spec$regarima$automodel$cancel
# # x13_spec_ref$regarima$automodel$cancel
# # sa_x13_ref$estimation_spec$regarima$automodel$cancel
# # sa_x13_ref$result_spec$regarima$automodel$cancel
# # #
# # x13_spec_d$regarima$automodel$fct
# # sa_x13_d$estimation_spec$regarima$automodel$fct
# # sa_x13_d$result_spec$regarima$automodel$fct
# # x13_spec_ref$regarima$automodel$fct
# # sa_x13_ref$estimation_spec$regarima$automodel$fct
# # sa_x13_ref$result_spec$regarima$automodel$fct
# #
# # x13_spec_d$regarima$automodel$acceptdef
# # sa_x13_d$estimation_spec$regarima$automodel$acceptdef
# # sa_x13_d$result_spec$regarima$automodel$acceptdef
# # x13_spec_ref$regarima$automodel$acceptdef
# # sa_x13_ref$estimation_spec$regarima$automodel$acceptdef
# # sa_x13_ref$result_spec$regarima$automodel$acceptdef
# #
# # x13_spec_d$regarima$automodel$mixed
# # sa_x13_d$estimation_spec$regarima$automodel$mixed
# # sa_x13_d$result_spec$regarima$automodel$mixed
# # x13_spec_ref$regarima$automodel$mixed
# # sa_x13_ref$estimation_spec$regarima$automodel$mixed
# # sa_x13_ref$result_spec$regarima$automodel$mixed
# #
# #
# # x13_spec_d$regarima$automodel$balanced
# # sa_x13_d$estimation_spec$regarima$automodel$balanced
# # sa_x13_d$result_spec$regarima$automodel$balanced
# # x13_spec_ref$regarima$automodel$balanced
# # sa_x13_ref$estimation_spec$regarima$automodel$balanced
# # sa_x13_ref$result_spec$regarima$automodel$balanced
# #
# # ## regression
# x13_spec_d$regarima$regression$mean
# sa_x13_d$estimation_spec$regarima$regression$mean
# sa_x13_d$result_spec$regarima$regression$mean
# x13_spec_ref$regarima$regression$mean # nothing in spec
# sa_x13_ref$estimation_spec$regarima$regression$mean
# sa_x13_ref$result_spec$regarima$regression$mean # estimated value only here
# #
# x13_spec_d$regarima$regression$check_mean
# sa_x13_d$estimation_spec$regarima$regression$check_mean
# sa_x13_d$result_spec$regarima$regression$check_mean
# x13_spec_ref$regarima$regression$check_mean
# sa_x13_ref$estimation_spec$regarima$regression$check_mean
# sa_x13_ref$result_spec$regarima$regression$check_mean
# #
#
# ## regression$td
# ### what is this
# ### how to change it
# x13_spec_d$regarima$regression$td$td
# sa_x13_d$estimation_spec$regarima$regression$td$td
# sa_x13_d$result_spec$regarima$regression$td$td
# x13_spec_ref$regarima$regression$td$td
# sa_x13_ref$estimation_spec$regarima$regression$td$td
# sa_x13_ref$result_spec$regarima$regression$td$td
# #
# x13_spec_d$regarima$regression$td$lp
# sa_x13_d$estimation_spec$regarima$regression$td$lp
# sa_x13_d$result_spec$regarima$regression$td$lp
# x13_spec_ref$regarima$regression$td$lp
# sa_x13_ref$estimation_spec$regarima$regression$td$lp
# sa_x13_ref$result_spec$regarima$regression$td$lp
# #
# x13_spec_d$regarima$regression$td$holidays
# sa_x13_d$estimation_spec$regarima$regression$td$holidays
# sa_x13_d$result_spec$regarima$regression$td$holidays
# x13_spec_ref$regarima$regression$td$holidays
# sa_x13_ref$estimation_spec$regarima$regression$td$holidays
# sa_x13_ref$result_spec$regarima$regression$td$holidays
# #
# x13_spec_d$regarima$regression$td$users
# sa_x13_d$estimation_spec$regarima$regression$td$users
# sa_x13_d$result_spec$regarima$regression$td$users
# x13_spec_ref$regarima$regression$td$users
# sa_x13_ref$estimation_spec$regarima$regression$td$users
# sa_x13_ref$result_spec$regarima$regression$td$users
# #
# x13_spec_d$regarima$regression$td$w
# sa_x13_d$estimation_spec$regarima$regression$td$w
# sa_x13_d$result_spec$regarima$regression$td$w
# x13_spec_ref$regarima$regression$td$w
# sa_x13_ref$estimation_spec$regarima$regression$td$w
# sa_x13_ref$result_spec$regarima$regression$td$w
# #
# x13_spec_d$regarima$regression$td$test
# sa_x13_d$estimation_spec$regarima$regression$td$test
# sa_x13_d$result_spec$regarima$regression$td$test
# x13_spec_ref$regarima$regression$td$test
# sa_x13_ref$estimation_spec$regarima$regression$td$test
# sa_x13_ref$result_spec$regarima$regression$td$test
# #
# x13_spec_d$regarima$regression$td$auto
# sa_x13_d$estimation_spec$regarima$regression$td$auto
# sa_x13_d$result_spec$regarima$regression$td$auto
# x13_spec_ref$regarima$regression$td$auto
# sa_x13_ref$estimation_spec$regarima$regression$td$auto
# sa_x13_ref$result_spec$regarima$regression$td$auto
# #
# x13_spec_d$regarima$regression$td$autoadjust
# sa_x13_d$estimation_spec$regarima$regression$td$autoadjust
# sa_x13_d$result_spec$regarima$regression$td$autoadjust
# x13_spec_ref$regarima$regression$td$autoadjust
# sa_x13_ref$estimation_spec$regarima$regression$td$autoadjust
# sa_x13_ref$result_spec$regarima$regression$td$autoadjust
# #
# x13_spec_d$regarima$regression$td$tdcoefficients
# sa_x13_d$estimation_spec$regarima$regression$td$tdcoefficients
# sa_x13_d$result_spec$regarima$regression$td$tdcoefficients
# x13_spec_ref$regarima$regression$td$tdcoefficients
# sa_x13_ref$estimation_spec$regarima$regression$td$tdcoefficients
# sa_x13_ref$result_spec$regarima$regression$td$tdcoefficients
# #
# x13_spec_d$regarima$regression$td$lpcoefficient
# sa_x13_d$estimation_spec$regarima$regression$td$lpcoefficient
# sa_x13_d$result_spec$regarima$regression$td$lpcoefficient
# x13_spec_ref$regarima$regression$td$lpcoefficient
# sa_x13_ref$estimation_spec$regarima$regression$td$lpcoefficient
# sa_x13_ref$result_spec$regarima$regression$td$lpcoefficient
#
# ## regression$easter
x13_spec_d$regarima$regression$easter$type
sa_x13_d$estimation_spec$regarima$regression$easter$type
sa_x13_d$result_spec$regarima$regression$easter$type
x13_spec_ref$regarima$regression$easter$type
sa_x13_ref$estimation_spec$regarima$regression$easter$type
sa_x13_ref$result_spec$regarima$regression$easter$type
#
#
x13_spec_d$regarima$regression$easter$test
sa_x13_d$estimation_spec$regarima$regression$easter$test
sa_x13_d$result_spec$regarima$regression$easter$test
x13_spec_ref$regarima$regression$easter$test
sa_x13_ref$estimation_spec$regarima$regression$easter$test
sa_x13_ref$result_spec$regarima$regression$easter$test
#
x13_spec_d$regarima$regression$easter$coefficient
sa_x13_d$estimation_spec$regarima$regression$easter$coefficient
sa_x13_d$result_spec$regarima$regression$easter$coefficient
x13_spec_ref$regarima$regression$easter$coefficient
sa_x13_ref$estimation_spec$regarima$regression$easter$coefficient
sa_x13_ref$result_spec$regarima$regression$easter$coefficient
#
# # ## outliers / ramps / user def vars
x13_spec_d$regarima$regression$outliers
sa_x13_d$estimation_spec$regarima$regression$outliers
sa_x13_d$result_spec$regarima$regression$outliers
x13_spec_ref$regarima$regression$outliers
sa_x13_ref$estimation_spec$regarima$regression$outliers
sa_x13_ref$result_spec$regarima$regression$outliers
# #
# x13_spec_d$regarima$regression$ramps
# sa_x13_d$estimation_spec$regarima$regression$ramps
# sa_x13_d$result_spec$regarima$regression$ramps
# x13_spec_ref$regarima$regression$ramps
# sa_x13_ref$estimation_spec$regarima$regression$ramps
# sa_x13_ref$result_spec$regarima$regression$ramps
# #
x13_spec_d$regarima$regression$users
sa_x13_d$estimation_spec$regarima$regression$users
sa_x13_d$result_spec$regarima$regression$users
x13_spec_ref$regarima$regression$users
sa_x13_ref$estimation_spec$regarima$regression$users
sa_x13_ref$result_spec$regarima$regression$users
# #
# #

# ### decomp avec X11
# x13_spec_d$x11$mode
# sa_x13_d$estimation_spec$x11$mode
# sa_x13_d$result_spec$x11$mode
# x13_spec_ref$x11$mode
# sa_x13_ref$estimation_spec$x11$mode
# sa_x13_ref$result_spec$x11$mode
#
# x13_spec_d$x11$seasonal
# sa_x13_d$estimation_spec$x11$seasonal
# sa_x13_d$result_spec$x11$seasonal
# x13_spec_ref$x11$seasonal
# sa_x13_ref$estimation_spec$x11$seasonal
# sa_x13_ref$result_spec$x11$seasonal
#
# x13_spec_d$x11$henderson
# sa_x13_d$estimation_spec$x11$henderson
# sa_x13_d$result_spec$x11$henderson
# x13_spec_ref$x11$henderson
# sa_x13_ref$estimation_spec$x11$henderson
# sa_x13_ref$result_spec$x11$henderson
#
# x13_spec_d$x11$sfilters
# sa_x13_d$estimation_spec$x11$sfilters
# sa_x13_d$result_spec$x11$sfilters
# x13_spec_ref$x11$sfilters
# sa_x13_ref$estimation_spec$x11$sfilters
# sa_x13_ref$result_spec$x11$sfilters
#
#
# x13_spec_d$x11$lsig
# sa_x13_d$estimation_spec$x11$lsig
# sa_x13_d$result_spec$x11$lsig
# x13_spec_ref$x11$lsig
# sa_x13_ref$estimation_spec$x11$lsig
# sa_x13_ref$result_spec$x11$lsig
#
# x13_spec_d$x11$usig
# sa_x13_d$estimation_spec$x11$usig
# sa_x13_d$result_spec$x11$usig
# x13_spec_ref$x11$usig
# sa_x13_ref$estimation_spec$x11$usig
# sa_x13_ref$result_spec$x11$usig
#
# x13_spec_d$x11$nfcasts
# sa_x13_d$estimation_spec$x11$nfcasts
# sa_x13_d$result_spec$x11$nfcasts
# x13_spec_ref$x11$nfcasts
# sa_x13_ref$estimation_spec$x11$nfcasts
# sa_x13_ref$result_spec$x11$nfcasts
#
# x13_spec_d$x11$nbcasts
# sa_x13_d$estimation_spec$x11$nbcasts
# sa_x13_d$result_spec$x11$nbcasts
# x13_spec_ref$x11$nbcasts
# sa_x13_ref$estimation_spec$x11$nbcasts
# sa_x13_ref$result_spec$x11$nbcasts
#
#
# x13_spec_d$x11$vsigmas
# sa_x13_d$estimation_spec$x11$vsigmas
# sa_x13_d$result_spec$x11$vsigmas
# x13_spec_ref$x11$vsigmas
# sa_x13_ref$estimation_spec$x11$vsigmas
# sa_x13_ref$result_spec$x11$vsigmas
#
# x13_spec_d$x11$excludefcasts
# sa_x13_d$estimation_spec$x11$excludefcasts
# sa_x13_d$result_spec$x11$excludefcasts
# x13_spec_ref$x11$excludefcasts
# sa_x13_ref$estimation_spec$x11$excludefcasts
# sa_x13_ref$result_spec$x11$excludefcasts
#
#
# x13_spec_d$x11$bias
# sa_x13_d$estimation_spec$x11$bias
# sa_x13_d$result_spec$x11$bias
# x13_spec_ref$x11$bias
# sa_x13_ref$estimation_spec$x11$bias
# sa_x13_ref$result_spec$x11$bias
#
#
# ### benchmarking
#
# x13_spec_d$benchmarking$enabled
# sa_x13_d$estimation_spec$benchmarking$enabled
# sa_x13_d$result_spec$benchmarking$enabled
# x13_spec_ref$benchmarking$enabled
# sa_x13_ref$estimation_spec$benchmarking$enabled
# sa_x13_ref$result_spec$benchmarking$enabled
#
# x13_spec_d$benchmarking$target
# sa_x13_d$estimation_spec$benchmarking$target
# sa_x13_d$result_spec$benchmarking$target
# x13_spec_ref$benchmarking$target
# sa_x13_ref$estimation_spec$benchmarking$target
# sa_x13_ref$result_spec$benchmarking$target
#
# x13_spec_d$benchmarking$lambda
# sa_x13_d$estimation_spec$benchmarking$lambda
# sa_x13_d$result_spec$benchmarking$lambda
# x13_spec_ref$benchmarking$lambda
# sa_x13_ref$estimation_spec$benchmarking$lambda
# sa_x13_ref$result_spec$benchmarking$lambda
#
# x13_spec_d$benchmarking$bias
# sa_x13_d$estimation_spec$benchmarking$bias
# sa_x13_d$result_spec$benchmarking$bias
# x13_spec_ref$benchmarking$bias
# sa_x13_ref$estimation_spec$benchmarking$bias
# sa_x13_ref$result_spec$benchmarking$bias
#
# x13_spec_d$benchmarking$forecast
# sa_x13_d$estimation_spec$benchmarking$forecast
# sa_x13_d$result_spec$benchmarking$forecast
# x13_spec_ref$benchmarking$forecast
# sa_x13_ref$estimation_spec$benchmarking$forecast
# sa_x13_ref$result_spec$benchmarking$forecast
