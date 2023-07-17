# Data  
library("RJDemetra")
ipi <- read.csv2("C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Data/IPI_nace4.csv")
ipi$date <- as.Date(ipi$date, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)
# creating a TS object from a data frame 
y_raw <- ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(2019, 6))
y_new <- ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(2019, 9))

## Package Doc remarks 

##
s<-
u<- RJDemetra::tramoseats(y_new)
u

## span pb 
### accepted values and default in reg arima spec
myseries <- y_raw
myspec1 <- x13_spec(spec = "RSA5c", estimate.from = "2000-01-01")
x13(y_raw,myspec1)


### accepted values and default in reg arima spec: useful to add them ?

myseries <- ipi_c_eu[, "FR"]
myspec1 <- regarima_spec_x13(spec = "RG5c")
user_defined_variables("X13-ARIMA")



## test on X11 mode and acceptable values 
myseries <- y_raw
myspec1 <- x13_spec(spec = "RSA5c")
myspec1
myspec2 <- x13_spec(spec = "X11", x11.mode="LogAdditive",estimate.from = "2000-01-01")
myspec2
### try to see if defined mode really used 
m<- x13(y_raw,myspec2,userdefined =c("decomposition.b1","decomposition.b20","decomposition.d1","decomposition.d12"))
m$decomposition$specification
m$final$series
m$regarima$model$spec_rslt
m$user_defined$decomposition.d1

#### tests on seasonal different seasonal factors by period 
myseries <- y_raw
myspec1 <- x13_spec(spec = "RSA5c")
spec_seaso<-x13_spec(myspec1,seasonalma=c("S3X3","Msr","S3X3","Msr"))
# ne marche pas 
# la spec est elle "marquée" trim ou mens ? à partir de qd ?
# si pas bonne longueur ?

#### tests on sigma vector (ne marche pas en v3 ?)
myseries <- y_raw
myspec1 <- x13_spec(spec = "RSA5c")
spec_sigma<-x13_spec(myspec1, x11.calendarSigma = "Select", 
                     x11.sigmaVector = c("Group1","Group1","Group1","Group1",
                                         "Group2","Group2","Group1","Group1",
                                         "Group1","Group1","Group1","Group1"))

spec_sigma
u<-x13(y_raw, spec_sigma)
u$final


# x11.mode = c(NA, "Undefined", "Additive", "Multiplicative", "LogAdditive", "PseudoAdditive"),
# x11.seasonalComp = NA,
# x11.lsigma = NA_integer_,
# x11.usigma = NA_integer_,
# x11.trendAuto = NA,
# x11.trendma = NA_integer_,
# x11.seasonalma = NA_character_,
# x11.fcasts = NA_integer_,
# x11.bcasts = NA_integer_,
# x11.calendarSigma = NA,
# x11.sigmaVector = NA,
# x11.excludeFcasts = NA)

