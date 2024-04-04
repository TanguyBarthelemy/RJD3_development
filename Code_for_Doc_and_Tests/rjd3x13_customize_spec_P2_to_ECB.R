##### Customizing a specification with external regressors

### Refreshing data

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



########################### PART 2

#### Add user def regressors (which are not pre defined in JD+ like outliers or ramps)

####### STEP 1: create (or import) the regressors

## here regressors = 1 intervention variable + 6 calendar regressors

# create intervention variables (see doc in rjd3toolkit)
iv1<-rjd3toolkit::intervention_variable(s=y_raw,
                           starts = "2015-01-01", ends = "2015-12-01")
# s=y_raw : formats directly your regressor like your raw series (length, frequency..)

iv1


### calendar regressors (to be added with `set_trading days`)
# set of 6 regressors every day is different, contrast with Sunday, no national calendar
regs_td<- rjd3toolkit::td(s=y_raw, groups = c(1, 2, 0, 4, 5, 6, 3),
             contrasts = TRUE)



####### STEP 2: create a modelling context

#### Creating a modelling context for external regressors (all together)

my_regressors<-list(Monday=regs_td[,1],Tuesday=regs_td[,2], Wednesday=regs_td[,3],
                Thursday=regs_td[,4],Friday= regs_td[,5], Saturday=regs_td[,6],
                reg1=iv1)

my_context<-rjd3toolkit::modelling_context(variables=my_regressors)
# check your variables
rjd3toolkit::.r2jd_modellingcontext(my_context)$getTsVariableDictionary()

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


### add intervention variables to spec, choosing the component to allocate the effects to TREND
x13_spec_d<- add_usrdefvar(x13_spec_d,group = "r", name="reg1",label="iv1", regeffect="Trend")
x13_spec_d$regarima$regression$users

####### STEP 4: estimate WITH context
sa_x13_d<- rjd3x13::x13(y_raw, x13_spec_d, context = my_context)
sa_x13_d$result$preprocessing
sa_x13_d

################################### Refreshing data

## STEP 1 refreshing the specification resulting from previous estimation

### result from previous estimation: sa_x13_d
current_result_spec <- sa_x13_d$result_spec
current_result_spec
current_domain_spec <- sa_x13_d$estimation_spec
# the domain spec contains previously defined user parameters: calendar regressors
current_domain_spec$regarima$regression$users

## Example 1: refresh = reestimating all regression coefficients

x13_spec_ref <- x13_refresh(current_result_spec, # point spec to be refreshed
                            current_domain_spec, #domain spec (set of constraints)
                            policy = "FreeParameters") # for policies see  x13_refresh doc
# Free parameters: all regression coefficients (incl arima model coeffs are restimated
x13_spec_ref

# estimation with refreshed spec
## need to re use context to be able to call user defined variables !
sa_x13_ref <- x13(y_new, x13_spec_ref,  context = my_context)
sa_x13_ref$estimation_spec
sa_x13_ref$result_spec
sa_x13_ref$result$preprocessing$description$variables


## Example 2: partial concurrent last outliers (on defined span)
## re-identifying outliers on the last year (here) and re-estimating all regression coefficients
### if span not specified, outliers will be re-identified on the whole series

x13_spec_ref <- x13_refresh(current_result_spec, # point spec to be refreshed
                            current_domain_spec, #domain spec (set of constraints)
                            policy = "Outliers",
                            period=12, # periodicity of the series to be refreshed
                            start=c(1990,1), # start of the series to be refreshed,
                            end=c(2021,9)) # end of the period on which outlier are NOT re-identified
current_domain_spec
current_result_spec
x13_spec_ref # outlier detection span should be from 2021-09-01

# estimation with refreshed spec
sa_x13_ref <- x13(y_new, x13_spec_ref,context = my_context)
sa_x13_ref$estimation_spec
sa_x13_ref$result_spec
sa_x13_ref$result$preprocessing
sa_x13_ref$result$preprocessing$description$variables
