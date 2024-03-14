## pending issues
### declared

### to declare
# model span not printed out ? voir T (+ app process) + clarify doc in X13 ?
# mode : to be tested
# seasonal filters by period (meaning of "X11Default") : ok
# sigma vectors : ok (maybe test it on smth, see impact ?)
# benchmarking : no bench access (cf ons question)


# Data
library("RJDemetra")
ipi <- read.csv2("C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Data/IPI_nace4.csv")
ipi$date <- as.Date(ipi$date, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)
# creating a TS object from a data frame
y_raw <- ts(ipi[, "RF0811"], frequency = 12, start = c(1990, 1), end = c(2021, 2))
y_new <- ts(ipi[, "RF0811"], frequency = 12, start = c(1990, 1), end = c(2021, 4))

## Package Doc remarks

# decomposition.sa_cmp_e (cf ONS dialogue in tech doc)
user_defined_variables("TRAMO-SEATS")

m <- RJDemetra::tramoseats(y_raw,"RSAfull",
                userdefined=c("decomposition.sa_cmp_e","decomposition.sa_cmp_e_f"))

m$user_defined$decomposition.sa_cmp_e



## span pb
### series span vs model span (pb vu en prod cf series span, model span)
m1<-RJDemetra::x13(y_raw,"RSA5c")

myspec1 <- RJDemetra::x13_spec(spec = "RSA5c", estimate.from = "2000-01-01")


m2<-RJDemetra::x13(y_raw,myspec1) # modele span pas affiché

m2$regarima$specification$estimate$span

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
# estimate = model span, works just in pre processing

### try to see if defined mode really used
## mode will always follow pre-adj, unless
### 1 x11 only: everthing should work

m<- x13(y_raw,myspec2,userdefined =c("decomposition.b1","decomposition.b20","decomposition.d1","decomposition.d12"))
m$decomposition$specification
m$final$series
m$regarima$model$spec_rslt
m$user_defined$decomposition.d1


# multplicative pre adj: x11 can be pseudo additive

#### tests on seasonal different seasonal factors by period
myseries <- y_raw
myspec1 <- RJDemetra::x13_spec(spec = "RSA5c")
# one filter for all periods
spec_seaso<-RJDemetra::x13_spec(myspec1,x11.seasonalma=c("S3X9"))
ms<-RJDemetra::x13(y_raw,spec_seaso)
ms
spec_seaso<-RJDemetra::x13_spec(myspec1,x11.seasonalma=c("Msr"))
ms<-RJDemetra::x13(y_raw,spec_seaso)
ms
## one factor per period
spec_seaso<-RJDemetra::x13_spec(myspec1,x11.seasonalma=
                                    c("S3X3","Msr","S3X3","Msr","S3X3","Msr","S3X3","Msr",
                                      "S3X3","Msr","S3X3","Msr"))

ms<-RJDemetra::x13(y_raw,spec_seaso)
ms


#### tests on sigma vector
myseries <- y_raw
myspec1 <- RJDemetra::x13_spec(spec = "RSA5c")
spec_sigma<-RJDemetra::x13_spec(myspec1, x11.calendarSigma = "Select",
                     x11.sigmaVector = c("Group1","Group1","Group1","Group1",
                                         "Group2","Group2","Group1","Group1",
                                         "Group1","Group1","Group1","Group1"))
spec_sigma

ms<-RJDemetra::x13(y_raw,spec_sigma)
ms

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
