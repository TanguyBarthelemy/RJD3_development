##### Customizing a specification

## refresh issues are in a separated file 


#####################
library("rjd3toolkit")
library("rjd3x13")
# Data  :
ipi <- read.csv2("C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Data/IPI_nace4.csv")
ipi$date <- as.Date(ipi$date, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)
# creating a TS object from a data frame 
y_raw <- ts(ipi[, "RF0812"], frequency = 12, start = c(1990, 1), end = c(2021,12))
y_new <- ts(ipi[, "RF0812"], frequency = 12, start = c(1990, 1), end = c(2022,9))


# creating a spec from default 
x13_spec_d<-rjd3x13::x13_spec("rsa3") 

# first estimation 
m<-rjd3x13::x13(y_raw,x13_spec_d)

# customization functions are in rjd3toolkit

# ##### set basic : series span for the estimation 
x13_spec_d<-rjd3toolkit::set_basic(x13_spec_d,type = "From",d0 = "2000-01-01",
                      preliminary.check = TRUE,
                      preprocessing= TRUE)

# print the spec and see changes 
print(x13_spec_d) 
# check results 
m<-rjd3x13::x13(y_raw,x13_spec_d)
start(m$result$final$d11final)


## define span until d1, excluded
x13_spec_d<-set_basic(x13_spec_d,type = "To",d1 = "2000-01-01",
                       preliminary.check = TRUE,
                       preprocessing= TRUE)

# print the spec and see changes 
print(x13_spec_d) 
# check results 
m<-rjd3x13::x13(y_raw,x13_spec_d)
start(m$result$final$d11final)
end(m$result$final$d11final)

## Last observation (dynamic choice)
x13_spec_d<-set_basic(x13_spec_d,type="Last", n1 = 60,
                       preliminary.check = TRUE,
                       preprocessing= TRUE)
# print the spec and see changes 
print(x13_spec_d) 
# check results 
m<-rjd3x13::x13(y_raw,x13_spec_d)
start(m$result$final$d11final)
end(m$result$final$d11final)

# Excluding : N first and P Last 60 obs 
x13_spec_d<-set_basic(x13_spec_d,type="Excluding", n0= 60, n1 = 80,
                      preliminary.check = TRUE,
                      preprocessing= TRUE)

# print the spec and see changes 
print(x13_spec_d) 
# check results 
m<-rjd3x13::x13(y_raw,x13_spec_d)
start(m$result$final$d11final)
end(m$result$final$d11final)


# ##### set estimate : length for the arima model only, can be combined with series span
x13_spec_d<-rjd3x13::x13_spec("rsa3") # re init 
x13_spec_d<-rjd3toolkit::set_estimate(x13_spec_d,"From",d0 = "2007-01-01")

# print the spec and see changes 
print(x13_spec_d) 
# check results 
m<-rjd3x13::x13(y_raw,x13_spec_d)
start(m$result$final$d11final)
end(m$result$final$d11final)


## set  transform : log or not
##
x13_spec_d<- rjd3toolkit::set_transform(x13_spec_d,
                           fun = "Log",
                           outliers = TRUE) # big outlier detection for test: new v3 feature

# print the spec and see changes 
print(x13_spec_d) 
# check results 
m<-rjd3x13::x13(y_raw,x13_spec_d)
start(m$result$final$d11final)
end(m$result$final$d11final)
              
## Modify automatic outlier detection parameters
x13_spec_d<-rjd3toolkit::set_outlier(x13_spec_d, 
                       span.type= "From", d0 = "2012-01-01", 
                       outliers.type = c("TC", "AO"), # LS are excluded
                       critical.value = 5,
                       tc.rate =0.85)

# print the spec and see changes 
print(x13_spec_d) 
# check results 
m<-rjd3x13::x13(y_raw,x13_spec_d)
start(m$result$final$d11final)
end(m$result$final$d11final)

# Modify automatic arima model estimation parameters (not advised to tweak this)
x13_spec_d<-set_automodel(x13_spec_d,
                        enabled = TRUE,  # automatic detection
                        cancel=0.06,
                        ub1=1.05,
                        ub2=1.15,
                        reducecv=0.15,
                        ljungboxlimit=0.96,
                        tsig=1.5,
                        ubfinal=1.06,
                        checkmu=FALSE,
                        balanced= TRUE)

# Customized arima model specification
x13_spec_d<-rjd3x13::x13_spec("rsa3") # re init
# disable automatic arima modelling
x13_spec_d<-set_automodel(x13_spec_d, enabled = FALSE)
# customize arima model
x13_spec_d <-set_arima(x13_spec_d,mean = 0.2,
                     mean.type = "Fixed",
                     p = 1, d = 2, q = 0,
                     bp = 1, bd = 1, bq = 0,
                     coef = c(0.6,0.7),
                     coef.type = c("Initial","Fixed"))

# print the spec and see changes 
print(x13_spec_d) 
# check results 
m<-rjd3x13::x13(y_raw,x13_spec_d)

# ### set benchmarking
x13_spec_d<-rjd3x13::x13_spec("rsa3")
x13_spec_d<-rjd3toolkit::set_benchmarking(x13_spec_d,
                            enabled = TRUE,
                            target = "ORIGINAL",
                            rho = 0.8,
                            lambda = 0.5,
                            forecast = FALSE,
                            bias = "None")
# output will have to be retrieved in user defined output 
userdefined_variables_x13() # list of items
sa_x13_d<- rjd3x13::x13(y_raw, x13_spec_d,
                        userdefined = c("benchmarking.result",   
                                        "benchmarking.original", #input 
                                        "benchmarking.target")) 

sa_x13_d$user_defined$benchmarking.result
sa_x13_d$user_defined$benchmarking.original
sa_x13_d$user_defined$benchmarking.target

### set_tradingdays 
# JD+ built in regressors, no national calendar unless defined)
x13_spec_d<- rjd3toolkit::set_tradingdays(x13_spec_d,
    option = "TD4", test = "None",
    coef=c(0.7,NA,0.5),
    coef.type=c("Fixed","Estimated","Fixed"),
    leapyear="LengthOfPeriod",
    leapyear.coef=0.6
    )
# print the spec and see changes 
print(x13_spec_d) 
# check results 
m<-rjd3x13::x13(y_raw,x13_spec_d)

# ### set_easter
x13_spec_d<-rjd3x13::x13_spec("rsa3") # re init
x13_spec_d<-set_easter(x13_spec_d,
                       enabled = TRUE,
                       duration = 12)

# print the spec and see changes 
print(x13_spec_d) 
# check results 
m<-rjd3x13::x13(y_raw,x13_spec_d)
m$result$preprocessing$description

### Adding user defined variables 
x13_spec_d<-rjd3x13::x13_spec("rsa3") # re init
# Pre-specified outliers 
x13_spec_d<-rjd3toolkit::add_outlier(x13_spec_d, type=c("AO","LS"), date=c("2020-03-01","2020-04-01"))

# print the spec and see changes 
print(x13_spec_d) 
# check results 
m<-rjd3x13::x13(y_raw,x13_spec_d)

 
# Adding a ramp 
x13_spec_d<-rjd3toolkit::add_ramp(x13_spec_d,start="2021-01-01",end="2021-12-01")
# print the spec and see changes 
print(x13_spec_d) 
# check results 
m<-rjd3x13::x13(y_raw,x13_spec_d)


########################### PART 2

#### Add user def regressors (which are not pre defined in JD+ like outliers or ramps)

####### STEP 1: create (or import) the regressors

## here regressors = 1 intervention variable + 6 calendar regressors 

# create intervention variables (see doc in rjd3toolkit)
iv1<-intervention_variable(s=y_raw,
                           starts = "2015-01-01", ends = "2015-12-01")
# s=y_raw : formats directly your regressor like your raw series (length, frequency..)

iv1


### calendar regressors (to be added with `set_trading days`)
# set of 6 regressors every day is different, contrast with Sunday, no national calendar
regs_td<- rjd3toolkit::td(s=y_raw, groups = c(1, 2, 3, 4, 5, 6, 0),
             contrasts = TRUE)

str(regs_td)
class(regs_td)
#### TEST adding an MTS object (Alain ?)

####### STEP 2: create a modelling context

#### Creating a modelling context for external regressors (all together)

my_regressors<-list(Monday=regs_td[,1],Tuesday=regs_td[,2], Wednesday=regs_td[,3],
                Thursday=regs_td[,4],Friday= regs_td[,5], Saturday=regs_td[,6],
                reg1=iv1)

my_context<-modelling_context(variables=my_regressors)
# check your variables 
rjd3toolkit::.r2jd_modellingcontext(my_context)$getTsVariableDictionary()

### here show how to create groups + GUI comp 

####### STEP 3: add regressors  to specification 


### add calendar regressors to spec 
x13_spec_d<-rjd3x13::x13_spec("rsa3")
x13_spec_d<- rjd3toolkit::set_tradingdays(x13_spec_d,
                             option = "UserDefined", 
                             uservariable=c("r.Monday","r.Tuesday","r.Wednesday",
                                            "r.Thursday","r.Friday","r.Saturday"),  
                             test = "None")

# print the spec and see changes 
print(x13_spec_d) 

### Alain : is it possible to add directly an MTS of external regressors 

### add intervention variables to spec, choosing the component to allocate the effects to TREND
x13_spec_d<- add_usrdefvar(x13_spec_d,group = "r", name="reg1",label="iv1", regeffect="Trend")
x13_spec_d$regarima$regression$users

####### STEP 4: estimate WITH context  

sa_x13_d<- rjd3x13::x13(y_raw, x13_spec_d, context = my_context)
sa_x13_d$result$preprocessing



########################## to investigate 


# print the spec and see changes 
print(x13_spec_d) 
# check results 
m<-rjd3x13::x13(y_raw,x13_spec_d)

## ISSUE 
y <- rjd3toolkit::ABS$X0.2.09.10.M
m <- x13(y,"rsa5c", userdefined=c("ycal"))
m <- x13(y,"rsa5c", userdefined=c("variancedecomposition.total",         
             "y", "y_b(?)", "y_eb(?)", "y_ef(?)","y_f(?)"))
         
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
