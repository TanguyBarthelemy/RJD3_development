# Pending issues

## to declare

# print arima model in created spec (0,1,1)(0,1,1) : misleading deja vu
# pb user defined output ???? tres louche car marchait à verifier (T le nom apparait)
# surement nom pb...: FAIRE message erreur vs argument das liste (voir T)
# in user defined variables list x13 : tramo stuff
# where is benchmarking output
# check mode !! + formulas + self verif + hd+ online doc
# check composite seasonal filters
# check sigma vector and illustrate it

## declared

################################# To test + doc packs + doc Jd+ to enrich

##### Calendar: all options
## leap year (comp gui), auto adjust
## stock td
## length of period

##### Missing value correction (vs GUI)

##### 1 Output (vs GUI + check preadj effects)
# default output
# user defined out put

######
###
library("rjd3toolkit")
library("rjd3x13")


# Data

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
    end = c(2022, 9)
)
y_new <- ts(
    ipi[, "RF0812"],
    frequency = 12,
    start = c(1990, 1),
    end = c(2022, 9)
)
y_raw

## Package Doc remarks

### issue 00 residus =

userdefined_variables_x13()

### series span model span

### issue 0 (voir ds rjd3toolkit): stuck on airline ? misleading presentation
x13_spec_d <- rjd3x13::x13_spec("rsa3")
sa_x13_d <- rjd3x13::x13(y_raw, x13_spec_d, userdefined = "decomposition.10")
sa_x13_d$user_defined$decomposition.10


### issue 1: modes x11 yc rjdemetra

### issue 2: v sigmas yc rjdemetra

### ISSUE benchmarking enabled : ok works
x13_spec_d <- rjd3x13::x13_spec("rsa5c")
x13_spec_d <- rjd3toolkit::set_benchmarking(
    x13_spec_d,
    enabled = TRUE,
    target = "original",
    rho = 0.8,
    lambda = 0.5,
    forecast = FALSE,
    bias = "None"
)
## user defined output list
rjd3x13::userdefined_variables_x13()
## benchmarking output
y <- rjd3toolkit::ABS$X0.2.09.10.M
sa_x13_d <- rjd3x13::x13(y, x13_spec_d, userdefined = c("y_b"))
sa_x13_d$user_defined$y_b # NULL !!

################ outlier detection
# regarima_outliers.R
# no print : do it my self! (new ability)
# modif doc

rjd3toolkit::ABS$X0.2.09.10.M
regarima_outliers(
    rjd3toolkit::ABS$X0.2.09.10.M,
    order = c(1, 1, 1),
    seasonal = c(0, 1, 1),
    mean = FALSE,
    X = NULL,
    X.td = NULL,
    ao = TRUE,
    ls = FALSE,
    tc = TRUE,
    so = TRUE,
    cv = 4
)

## pb avec declaration modeles arima order=c(1,1,1)? declared as integer later
## order =
## seasonal=
## has to be an airline model ?

#### REJECTED EXAMPLE
#' regarima_outliers(rjd3toolkit::ABS$X0.2.09.10.M, order=c(0,1,1), seasonal=c(0,1,1),
#' mean=F,
#' X=NULL, X.td=NULL,
#' ao=T, ls=F, tc=T, so=T, cv=4)
#'
#'

################ set X11 spec: how to create one
# FILE : set_X11_spec.R
#' Set X-11 Specification

# need 1 create a sheer X11 customized spec, default spec = spec_x11() : OK ?
# TO DO :make clearer the difference of "X11" and other predifined x13 specs "RSA5"

# need 2 : customize the x11 part of an X13 spec: MET ?
# spec de depart = "RSA5c" pex

## pb pas le choix "X11" dans default spec : voir RegARIMA/X-13 Default Specification

#' @param bias TODO.: voir code JP pour tramo
#'
# example to be added (add spec creation code )

# modif 1

## param : seasonal.filter pas terrible, car ça peut etre le meme pour toute la série mais pas Msr

# modif 2

#' @param sigma.vector a vector to specify one of the two groups of periods for which standard errors used for extreme values
#' detection and adjustment will be computed separately.

# Modif 3
#' @param x the specification to be modified, default X11 spec can be be obtained as 'x=spec_x11()'

### Test
# set_x11 <- function(x,
#                     mode = c(NA, "Undefined", "Additive", "Multiplicative", "LogAdditive", "PseudoAdditive"),
#                     seasonal.comp = NA,
#                     seasonal.filter = NA,
#                     henderson.filter = NA,
#                     lsigma = NA,
#                     usigma = NA,
#                     fcasts = NA,
#                     bcasts = NA,
#                     calendar.sigma = c(NA, "None", "Signif", "All", "Select"),
#                     sigma.vector = NA,
#                     exclude.forecast = NA,
#                     bias = c(NA, "LEGACY"))

init_spec <- spec_x11()
# issue
new_spec <- set_x11(
    init_spec,
    mode = "LogAdditive",
    seasonal.comp = 1,
    seasonal.filter = "S3X9",

    # c("S3X3","S3X3","S3X3","S3X3","S3X3","S3X3",
    #                 "S3X3","S3X3","S3X3","S3X3","S3X5","S3X9"),
    # issue si vecteur length 4 or 12
    henderson.filter = 0,
    lsigma = 1.7,
    usigma = 2.7,
    fcasts = -1,
    bcasts = -1,
    calendar.sigma = "Select",
    sigma.vector = c(1, 2, 2, 1),
    exclude.forecast = FALSE,
    bias = "LEGACY"
)
new_spec

# TEST need 2 : customize the x11 part of an X13 spec: MET ?
# spec de depart = "RSA5c" pex
# issue
# ok ça marche, mais par exemple va modifier e
### obj ; explique ce qui n' pas

init_spec <- x13_spec()
init_spec
# issue : seasonal filter and sigma vector
new_spec <- set_x11(
    init_spec,
    mode = "LogAdditive",
    seasonal.comp = 1,
    seasonal.filter = "S3X9",

    # c("S3X3","S3X3","S3X3","S3X3","S3X3","S3X3",
    #                 "S3X3","S3X3","S3X3","S3X3","S3X5","S3X9"),
    # issue si vecteur length 4 or 12
    henderson.filter = 7,
    lsigma = 1.7,
    usigma = 2.7,
    fcasts = -1,
    bcasts = -1,
    calendar.sigma = "All",
    sigma.vector = NA,
    exclude.forecast = FALSE,
    bias = "LEGACY"
)
new_spec

# modif X11 params dans une spec X13 (ds rjd3toolkit) ?

################ set X13 spec: how to create one
# FILE n°3 : X13_spec.R
## homogeneisation noms spec
#' @rdname x13_spec @rdname x13_spec
#'
#' quels sont les specs par defaut

s <- spec_x11()
s
s1 <- spec_regarima()
s1 # RG2c ??

s2 <- x13_spec()
s2 # RSA2c ??

### issue faut il un nom de spec ou un spec object amibgu
# nom ambigu des examples ente specs regarima et specs X13 (même si tout marche)
## spec object useful for mofis
sp <- x13_spec("rg5c")
y <- rjd3toolkit::ABS$X0.2.09.10.M
fast_x13(y, spec = "rsa5c") # works
x13(y, spec = "rsa5c") # works ok but issue = no print ? or no automatic print ?
sp <- rjd3toolkit::add_outlier(
    sp,
    type = c("AO"),
    c("2015-01-01", "2010-01-01")
)
sp <- rjd3toolkit::set_transform(
    rjd3toolkit::set_tradingdays(
        rjd3toolkit::set_easter(sp, enabled = FALSE),
        option = "workingdays"
    ),
    fun = "None"
)
sp <- set_x11(sp, henderson.filter = 13)
fast_x13(y, spec = sp)

### pb =  modif de la spec et notamment de la partie x11

# In the estimation functions you can diectly use a specification name (string)
y <- rjd3toolkit::ABS$X0.2.09.10.M
fast_x13(y, "rsa3")
x13(y, "rsa5c") # issue: no print
fast_regarima(y, "rg0") # print exists
regarima(y, "rg3") # issue: no print
#'
#' If you want to customize a specification you have to create a specification object first
sp <- x13_spec("rsa5c")
sp <- rjd3toolkit::add_outlier(
    sp,
    type = c("AO"),
    c("2015-01-01", "2010-01-01")
)
# sp <-  rjd3toolkit::set_transform(
#'    rjd3toolkit::set_tradingdays(
#'      rjd3toolkit::set_easter(sp, enabled = FALSE),
#'     option = "workingdays"
#'   ),
#'   fun = "None"
#' )
x13(y, spec = sp)
sp <- set_x11(sp, henderson.filter = 13)
fast_x13(y, spec = sp)

################ set X13 spec: how to create one
# FILE n°3 : udvar.R

# retrieve names = bof : redefine explanation

userdefined_variables_x13("x13")
#' userdefined_variables_x13("regarima")
#' userdefined_variables_x13("x11")
################ ISSUE ?
y <- rjd3toolkit::ABS$X0.2.09.10.M
m <- x13(y, "rsa5c", userdefined = c("decomposition.b20", "ycal"))
m$user_defined$decomposition.b20
# Error in .jcall(jx, out_class, "getResult") :
#     method getResult with signature ()Ljdplus/x13/X13Results; not found

m$user_defined$b20
m$user_defined$cal
m$user_defined$residuals.kurtosis


m <- x13(y, "rsa3", userdefined = c("b20"))
m
m$user_defined$b20

### mode specific issues

#### set arima
## pre-condition
# ## automodel
x13_spec_d$regarima$automodel$enabled
sa_x13_d$estimation_spec$regarima$automodel$enabled
sa_x13_d$result_spec$regarima$automodel$enabled


# #
x13_spec_d$regarima$arima$d
sa_x13_d$estimation_spec$regarima$arima$d
sa_x13_d$result_spec$regarima$arima$d

#
x13_spec_d$regarima$arima$bd
sa_x13_d$estimation_spec$regarima$arima$bd
sa_x13_d$result_spec$regarima$arima$bd


x13_spec_d$regarima$arima$phi
sa_x13_d$estimation_spec$regarima$arima$phi
sa_x13_d$result_spec$regarima$arima$phi


x13_spec_d$regarima$arima$theta
sa_x13_d$estimation_spec$regarima$arima$theta
sa_x13_d$result_spec$regarima$arima$theta

x13_spec_d$regarima$arima$bphi
sa_x13_d$estimation_spec$regarima$arima$bphi
sa_x13_d$result_spec$regarima$arima$bphi


x13_spec_d$regarima$arima$btheta
sa_x13_d$estimation_spec$regarima$arima$btheta
sa_x13_d$result_spec$regarima$arima$btheta

##################### OUTPUT TEST
## To test vs GUI + check preadj effects: read vs and compare TS ?
# default output
# user defined out put
