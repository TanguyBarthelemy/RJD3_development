# Data

library("rjd3toolkit")
library("rjd3tramoseats")

ipi <- read.csv2("C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Data/IPI_nace4.csv")
ipi$date <- as.Date(ipi$date, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)
# creating a TS object from a data frame
y_raw <- ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(2019, 6))
y_new <- ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(2019, 9))

## Package Doc remarks

### Modifs faites
# - adding value ranges and default values


## bien chercher issues (word in scattered throughout the code)

############# rjd3tramoseats
## layer 0 : repasser doc RJDemetra V2 ?

## LAYER 1
# 1 basic tests for doc, clarification, adding examples

# 2 extended test with esoteric configs


################ outlier detection
# FILE 1 tramo_outliers.R
# no print : do it my self! (new ability)
# modif doc, harmonization avec X-13

#### issue : REJECTED EXAMPLE

####### adapt to tramo here + solve issue ?
#' regarima_outliers(rjd3toolkit::ABS$X0.2.09.10.M, order=c(0,1,1), seasonal=c(0,1,1),
#' mean=F,
#' X=NULL, X.td=NULL,
#' ao=T, ls=F, tc=T, so=T, cv=4)
#'
#'

################ set seats spec: how to create one
# FILE 2 : seats_spec.R
# equivalent of wet X11, but it's just to set seats alone

#' Set seats Specification
# x = the spec can be a character or MUST be spec object

# inverse sure ?
#' @param trend.boundary numeric: the trend boundary. The boundary beyond which an AR root is integrated in the trend component.
#' If the modulus of the inverse real root is greater than the trend boundary, the AR root is integrated in the trend component.


#' CHECK
#' ' @param seas.tolerance numeric: the seasonal tolerance. The tolerance (measured in degrees) to allocate the AR non-real roots
#' to the seasonal component (if the modulus of the inverse complex AR root is greater than the trend boundary
#' and the frequency of this root differs from one of the seasonal frequencies by less than Seasonal tolerance)
#' or the transitory component (otherwise).
#
# add example, test x possible value

# add default values in doc !! idem for x13 (cf tableaux andreas) + tech names like "esphi"

# like set x11 in rjd3 x13 but..
# key seats cannot work on its own: why is seats spec useful ??
`spec_tramoseats()`
# c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")
# x has to be a specification objet
x <- spec_tramo("rsafull")

# works with "wrong names" cf alain
# pb avec full semble donner un modele airline ??


##### ISSUE bcasts dont work
# 18

init_spec <- spec_tramoseats("rsafull")
init_spec$seats$xl
init_spec$seats$approximation
"APP_LEGACY"
init_spec$seats$epsphi
init_spec$seats$rmod
init_spec$seats$sbound
init_spec$seats$sboundatpi
init_spec$seats$bias
init_spec$seats$nfcasts
init_spec$seats$nbcasts
init_spec$seats$algorithm
"ALG_BURMAN"

init_spec <- tramoseats_spec("rsafull")
new_spec <- set_seats(init_spec,
    approximation = "Legacy",
    trend.boundary = 0.8,
    seas.boundary = 0.5,
    fcasts = 18,
    bcasts = 11,
    algorithm = "KalmanSmoother",
    bias = TRUE
)

y <- rjd3toolkit::ABS$X0.2.09.10.M

sa <- rjd3tramoseats::tramoseats(y, spec = new_spec)

sa$result$final$sa

sa$result_spec$seats$nfcasts

library("RJDemetra")
sa_model <- RJDemetra::tramoseats(y, "RSAfull")
str(sa_model$final$series)
sa_model$final$forecasts

init_spec <- spec_tramoseats("rsa4")
init_spec$seats$
    new_spec <- set_seats(init_spec,
    approximation = "Legacy",
    trend.boundary = 0.95,
    seas.boundary = 0.5,
    fcasts = -2,
    bcasts = 18,
    algorithm = "KalmanSmoother"
)
y <- rjd3toolkit::ABS$X0.2.09.10.M
sa <- rjd3tramoseats::tramoseats(y, spec = new_spec)



sa <- rjd3tramoseats::tramoseats(y_raw, spec = new_spec)
sa <- rjd3tramoseats::tramoseats(y_raw, "rsa4")

#### Issue on backcasts
init_spec <- spec_tramoseats("rsafull")
new_spec <- set_seats(init_spec, bcasts = 18)
y <- rjd3toolkit::ABS$X0.2.09.10.M
sa <- rjd3tramoseats::tramoseats(y, spec = new_spec)







## ISSUE les arguments ex seas boundary n'ont pas le meme nom en sortie
s
####### ISSUE: here no tramo estimation with a modified spec ???
y <- rjd3toolkit::ABS$X0.2.09.10.M
u <- rjd3tramoseats::tramoseats(y)
u$result_spec$seats$


    #' @param bias TODO.: voir code JP pour tramo
    #'
    ###################################### ####################################
    ################ set X13 spec: how to create one
    # FILE n°3 : tramoseats_spec.R
    # lack examples
    # like in X13: a spec can be a string in the estimation functions

    # rsafull, rsa5

    # test with customized specs
    s <- spec_seats()
s
s1 <- spec_regarima()
s1 # RG2c ??

s2 <- spec_x13()
s2 # RSA2c ??

### issue faut il un nom de spec ou un spec object amibgu
# nom ambigu des examples ente specs regarima et specs X13 (même si tout marche)
## spec object useful for mofis
sp <- spec_x13("rg5c")
y <- rjd3toolkit::ABS$X0.2.09.10.M
fast_x13(y, spec = "rsa5c") # works
x13(y, spec = "rsa5c") # works ok but issue = no print ? or no automatic print ?
sp <- rjd3toolkit::add_outlier(sp,
    type = c("AO"), c("2015-01-01", "2010-01-01")
)
sp <- rjd3toolkit::set_transform(
    rjd3toolkit::set_tradingdays(
        rjd3toolkit::set_easter(sp, enabled = FALSE),
        option = "workingdays"
    ),
    fun = "None"
)
sp <- set_seats(sp,
    henderson.filter = 13
)
fast_x13(y, spec = sp)

### pb =  modif de la spec et notamment de la partie seats

# In the estimation functions you can diectly use a specification name (string)
y <- rjd3toolkit::ABS$X0.2.09.10.M
fast_x13(y, "rsa3")
x13(y, "rsa5c") # issue: no print
fast_regarima(y, "rg0") # print exists
regarima(y, "rg3") # issue: no print
#'
#' If you want to customize a specification you have to create a specification object first
sp <- spec_x13("rsa5c")
sp <- rjd3toolkit::add_outlier(sp,
    type = c("AO"), c("2015-01-01", "2010-01-01")
)
# sp =  rjd3toolkit::set_transform(
#'    rjd3toolkit::set_tradingdays(
#'      rjd3toolkit::set_easter(sp, enabled = FALSE),
#'     option = "workingdays"
#'   ),
#'   fun = "None"
#' )
x13(y, spec = sp)
sp <- set_seats(sp, henderson.filter = 13)
fast_x13(y, spec = sp)

################ user def spec
# FILE n°3 : ud_var.R

# harmo avec x13 et x13 a resoumettre

# retrieve names = bof : redefine explanation : ok
# add meaningful example
# link to doc where output names are

userdefined_variables_tramoseats("tramoseats")
userdefined_variables_tramoseats("tramo")
y <- rjd3toolkit::ABS$X0.2.09.10.M
m <- tramoseats(y, "rsafull", userdefined = c(
    "ycal",
    "variancedecomposition.seasonality"
))

m$user_defined$variancedecomposition.seasonality
# m$user_defined$b20
# m$user_defined$ycal
# m$user_defined$residuals.kurtosis

userdefined_variables_x13("x13")
#' userdefined_variables_x13("regarima")
#' userdefined_variables_x13("seats")

y <- rjd3toolkit::ABS$X0.2.09.10.M
m <- x13(y, "rsa5c", userdefined = c("b20", "ycal", "residuals.kurtosis"))
m$user_defined$b20
m$user_defined$cal
m$user_defined$residuals.kurtosis


m <- x13(y, "rsa3", userdefined = c("b20"))
m
m$user_defined$b20

################ tramoseats spec: ok
# FILE n°4 : tramoseats_spec.R
u <- spec_tramo()
u

v <- spec_tramoseats()
v

################ tramo spec
# FILE n°5 :


################ user def spec
# FILE n°6 : tramoseats.R

# pb of defining context : stting links
# - external
# - across help pages

### add examples, beef up seats part


## REFRESH
# improve doc
# test refres and comp specs (how)

## IMPROVE READ ME00
