library("rjd3toolkit")
library("rjd3workspace")
library("rjd3providers")

## add vars cf T
# integrate code

## meta data cf code T
# demo_manage_path.R : integrate

## Look up below: BUG, HOLE, LATER...


# install.packages("remotes")
# remotes::install_github("rjdverse/rjd3workspace@*release")

## aux
#library(help=rjd3workspace)


# Load Test Data ----------------------------------------------------------


file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
rws <- read_workspace(jws, compute = TRUE)
rws$processing$`SAProcessing-1`$RF0812$results
#str(rws) # use for doc

# pb noms: issue si noms excel
# rws$processing$`SAProcessing-2`$`Exports
# European Union`

rws <- read_workspace(jws)
# rws$processing$`SAProcessing-1`$RF0812$results
summary(rws$processing$`SAProcessing-1`$RF0812$results)


# Creating and filling ws from scratch ------------------------------------


# Create an empty 'JDemetra+' Workspace
jws <- jws_new()
jws
# Add an empty SA-Processing
jsap <- jws_sap_new(jws, "sap1")
jsap
jws_compute(jws)


# Reading WS SAP SA-Item -----------------------------------------------------------

## Reading WS           -----------------------------------------------------------
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)

jread_workspace(jws,FALSE)
rws <- read_workspace(jws, FALSE)

## Reading SAP           -----------------------------------------------------------
sap<-jws_sap(jws,1)
jread_sap(sap)
read_sap(sap)
m<- read_sap(sap)
#str(m) # as overall base for doc

## Reading SA-Item          -----------------------------------------------------------

### read SA-item

# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
# Select SAProcessing
sap1<- jws_sap(jws,1)
# Select SA-item
sai1<-jsap_sai(sap1,3) # java object sai
# Read SA-item
r<-read_sai(sai1)
str(r)

class(r)
summary(sai1) # enlever phrase "For more detailed...)
summary(r)

# Compute the workspace to enable accessing its components
jws_compute(jws)

# Copy save (write) a WS ------------------------------------------------------


####### COPY
# Create an empty 'JDemetra+' Workspace
jws <- jws_new()
# Add an empty SA-Processing
jsap <- jws_sap_new(jws, "sap1")
# Make a copy of the workspace
jws2 <- jws_make_copy(jws)
# Make a copy of sap1 in jws2
jsap2 <- jsap_make_copy(jsap)
jread_workspace(jws2, FALSE)

# SAVE
dir <- tempdir()
jws <- jws_new()
jsap1 <- jws_sap_new(jws, "sap1")
y <- rjd3toolkit::ABS$X0.2.15.10.M
add_sa_item(jsap1, name = "serie_2", x = y, rjd3x13::x13_spec())
save_workspace(jws, file.path(dir, "workspace.xml"))
save_workspace(jws, file.path(dir, "workspace.xml"), replace=TRUE)


# Check info in a WS ------------------------------------------------------

# # Check if the SA-Item 3 in the SA-Processing 1 exists
rjd3workspace:::check_information(jws = jws, idx_sap = 1, idx_sai = 3)

# # Check if the SA-Items 1, 2 and 5 in the SA-Processing 1 exist
rjd3workspace:::check_information(jws = jws, idx_sap = 1, idx_sai = c(1, 2, 5))


# Count objects -----------------------------------------------------------

## COUNT
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
# Count the SA-Processings
ws_sap_count(jws)
# Count the SA-Items
# In SAP 1
sap1<-jws_sap(jws,1)
sap_sai_count(sap1)


# Extract SAP and Sa-Item -------------------------------------------------

# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
# Compute the workspace to enable accessing its components
jws_compute(jws)
# Extract 2nd SA-Processing
jsap2 <- jws_sap(jws,2)
# Extract 3rd SA-item
jsai3 <- jsap_sai(jsap2,3)


# Add SA-Item -------------------------------------------------------------
### Add_sa_Item (better explanations !)
## what happens to meta data ?

dir <- tempdir()
# raw series
y <- rjd3toolkit::ABS$X0.2.09.10.M
# creating empty workspace and SAProcessing
jws <- jws_new()
jsap1 <- jws_sap_new(jws, "sap1")

# WAY 1 : adding SA-Item as estimation result
# estimation with rjd313
add_sa_item(jsap1, name = "series_1", x = rjd3x13::x13(y))
# estimation with rjd3tramoseats
add_sa_item(jsap1, name = "series_2", x = rjd3tramoseats::tramoseats(y))
# WAY 2 adding SA-Item as raw series + specification
add_sa_item(jsap1, name = "series_3", x = y, rjd3x13::x13_spec("RSA3"))
add_sa_item(jsap1, name = "series_4", x = y, rjd3tramoseats::tramoseats_spec("RSAFull"))
rws<-read_workspace(jws)
rws$processing$sap1$series_1
rws$processing$sap1$series_2
rws$processing$sap1$series_3
rws$processing$sap1$series_4
sap_sai_names(jsap1)

# Writing the workspace
save_workspace(jws, file.path(dir, "workspace.xml"), replace=TRUE)


# Get names ---------------------------------------------------------------

## get names
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
# Extract 2nd SA-Processing
jsap_2 <- jws_sap(jws,2)
# Retrieve the name
sap_name(jsap_2)
# Retrieve all the SA-Items names
sap_sai_names(jsap_2)

# Get Results -----------------------------------------------------------------


### Results: PBS / BUG :LATER

.jsai_results(sai1, items = "decomposition.y_lin" )
.jsai_jresults(sai1)


# Set (domain) Spec ----------------------------------------------------------------

library(rjd3x13)

spec <- rjd3x13::x13_spec("rsa3") |>
    rjd3toolkit::set_basic(type = "From", d0 = "2012-01-01")

# Load a Workspace to modify
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)

# Select SAProcessing with the target SA-item
sap1 <- jws_sap(jws, 1)

# Set specification in targeted SA-item
set_specification(sap1, 2, spec)

# Set domain specification in selected SA-item
set_domain_specification(sap1, 3, spec)



# Set and Get TS ----------------------------------------------------------



# Set TS get TS: difference vs rw data = full jd+ TS ?
### explain in doc: and complete Doc LATER
get_ts(sai1) # BUG
set_ts(sap1,3,)

### SA-items...Meta data
########## meta data vs ts meta data
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
# Select SAProcessing
sap1<- jws_sap(jws,1)
# Select SA-item
sai1<-jsap_sai(sap1,3) # java object sai
# java
.jsai_metadata(sai1) # key
.jsai_ts_metadata(sai1) # key

# set get comment
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
# Select SAProcessing
sap1<- jws_sap(jws,1)
# Select SA-item
sai1<-jsap_sai(sap1,3) # java object sai
set_comment(sap1,2,"data collection changed in 2012")
sai1<-jsap_sai(sap1,2) # java object sai
get_comment(sai1)
# Writing the workspace
dir<- "C:\\Users\\YWYD5I\\AppData\\Local\\Temp\\RtmpMDX2Zy"
save_workspace(jws, file.path(dir, "workspace_test_rewritten.xml"))

sai_name(sai1)


## set NAME
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
# Select SAProcessing
sap1<- jws_sap(jws,1)
# Select SA-item
sai1<-jsap_sai(sap1,3) # java object sai
set_name(sap1,3,"RF1011_1")
# check
sai1<-jsap_sai(sap1,3) # reload sai
sai_name(sai1) #get name

## set / get priority : LATER

## get set Raw data / TS = full JD+ ts: explain: LATER

# Get/ Set Raw data

# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
# Select SAProcessing
sap1<- jws_sap(jws,1)
# Select SA-item
sai1<-jsap_sai(sap1,3) # java object sai
tail(get_raw_data(sai1))
new_raw_data <-rjd3toolkit::ABS$X0.2.15.10.M
tail(new_raw_data)
set_raw_data(sap1,3,new_raw_data) # BUG here ?
sai1<-jsap_sai(sap1,3) # reload sai
tail(get_raw_data(sai1))

## get TS : LATER
get_ts(sai1)

## set TS meta data : LATER

### set specification, domain_specification
library("rjd3toolkit")
library("rjd3workspace")
library("rjd3providers")

# Création de la spec
library(rjd3x13)
spec <- rjd3x13::x13_spec("rsa3") |>
    rjd3toolkit::set_basic(type = "From", d0 = "2012-01-01")

# Load a Workspace to modify
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
read_workspace(jws)
# Select SAProcessing
sap1<- jws_sap(jws,1)
sai2<-jsap_sai(sap1,2)
read_sai(sai2)

# Set specification in selected SA-item
set_specification(sap1, 2, spec)
# Set domain specification in selected SA-item
set_domain_specification(sap1, 3, spec)

### PB avec read si on touche la spec et pas le domain spec
sai2<-jsap_sai(sap1,2) # reload sai
sai3<-jsap_sai(sap1,3)
read_sai(sai2)
read_sai(sai3)

# ## add sa item to review
# add_sa_item(
#         jsap = jsap_auto,
#         name = series_name,
#         spec = spec,
#         x = ts(ipi_2025[, series_name], start = 1990L, frequency = 12L)
# )

set_domain_specification(jsap = jsap_ref, idx = k, spec = spec)



############ Calendars
# read
file <- system.file("workspaces", "workspace_test","Calendars","Calendars.xml",package = "rjd3workspace")
file
cal<-read_calendars(file)
class(cal)

cal$Test_Calendar$days

# write
library(rjd3toolkit)
    BE <- national_calendar(list(
        fixed_day(7, 21),
        special_day("NEWYEAR"),
        special_day("CHRISTMAS"),
        special_day("MAYDAY"),
        special_day("EASTERMONDAY"),
        special_day("ASCENSION"),
        special_day("WHITMONDAY"),
        special_day("ASSUMPTION"),
        special_day("ALLSAINTSDAY"),
        special_day("ARMISTICE")
    ))
write_calendars(list(BEL_cal = BE), file = normalizePath("~/tmp.xml", mustWork = FALSE))
normalizePath("~/tmp.xml", mustWork = FALSE)

# French calendar
french_calendar <- national_calendar(
    days = list(
        fixed_day(7, 14), # Bastille Day
        fixed_day(5, 8, validity = list(start = "1982-05-08")), # End of 2nd WW
        special_day("NEWYEAR"),
        special_day("CHRISTMAS"),
        special_day("MAYDAY"),
        special_day("EASTERMONDAY"),
        special_day("ASCENSION"),
        special_day("WHITMONDAY"),
        special_day("ASSUMPTION"),
        special_day("ALLSAINTSDAY"),
        special_day("ARMISTICE")
    )
)

# add calendar to ws
# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
read_workspace(jws)
add_calendar(jws, "French Calendar", french_calendar)
get_context(jws)


## si on reecrit le ws
# Writing the workspace: CHECK This

dir<- "C:\\Users\\YWYD5I\\AppData\\Local\\Temp\\RtmpMDX2Zy"
save_workspace(jws, file.path(dir, "workspace_test_rewritten.xml"))

### Add variable = regressor: BUG => issue : LATER

# Load a Workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
jws <- jws_open(file)
y<-ts(rnorm(100))
add_variables(jws,group = "r", "reg", y)
add_variables(jws,group = "UD_regs_m", "reg", y)
get_context(jws) |> str()
my_context<- get_context(jws)
my_context$


### Read variable = regressor: OK
file <- system.file("workspaces", "workspace_test","Variables","Vars-1.xml", package = "rjd3workspace")
file
my_regresors<- read_variables(file)
class(my_regresors)
str(my_regresors)



### Write variable = regressor: BUG => issue : LATER
file <- system.file("workspaces", "workspace_test","Variables","Vars-1.xml", package = "rjd3workspace")
file
my_regressors<- read_variables(file)
class(my_regressors)
str(my_regressors)


# writing the specification in a xml file
write_variables(my_regressors, file = normalizePath("~/tmp.xml", mustWork = FALSE))

### Read Spec file
library(rjd3workspace)
### X13
file <- system.file("workspaces", "workspace_test","X13Spec","X13Spec-1.xml", package = "rjd3workspace")
file
f<- x13_read_spec(file)
class(f)
str(f)

### reg arima
file <- system.file("workspaces", "workspace_test","RegArimaSpec","RegArimaSpec-1.xml", package = "rjd3workspace")
my_spec<-regarima_read_spec(file)
class(my_spec)
str(my_spec)

# tramo seats

file <- system.file("workspaces", "workspace_test","TramoSeatsSpec","TramoSeatsSpec-1.xml", package = "rjd3workspace")
file
my_spec<- tramoseats_read_spec(file)
class(my_spec)
str(my_spec)

# tramo
file <- system.file("workspaces", "workspace_test","TramoSpec","TramoSpec-1.xml", package = "rjd3workspace")
file
my_spec<- tramo_read_spec(file)
class(my_spec)
str(my_spec)


### Write Spec file:
############## x13
library(rjd3toolkit)
library(rjd3x13)


# creating a spec from default
x13_spec<- rjd3x13::x13_spec("rsa3")
# forcing multiplicative model
x13_spec_d <- rjd3toolkit::set_transform(x13_spec,
                                         fun = "Log",
                                         outliers = TRUE)
# writing the specification in a xml file
x13_write_spec(x13_spec_d, file = normalizePath("~/tmp.xml", mustWork = FALSE))


############### regarima
# creating a spec from default
reg_arima_spec <- rjd3x13::regarima_spec("rg3")
# forcing multiplicative model
reg_arima_spec_d <- rjd3toolkit::set_transform(reg_arima_spec ,
                                         fun = "Log",
                                         outliers = TRUE)
# writing the specification in a xml file
regarima_write_spec(reg_arima_spec_d, file = normalizePath("~/tmp.xml", mustWork = FALSE))

############## Tramo

# creating a spec from default
tramo_spec <- rjd3tramoseats::tramo_spec("tr3")
# forcing multiplicative model
tramo_spec_d <- rjd3toolkit::set_transform(tramo_spec ,
                                              fun = "Log",
                                              outliers = TRUE)
# writing the specification in a xml file
tramo_write_spec(tramo_spec_d, file = normalizePath("~/tmp.xml", mustWork = FALSE))

############## Tramo Seats

# creating a spec from default
tramoseats_spec <- rjd3tramoseats::tramoseats_spec("rsa3")
# forcing multiplicative model
tramoseats_spec_d <- rjd3toolkit::set_transform(tramoseats_spec ,
                                           fun = "Log",
                                           outliers = TRUE)
# writing the specification in a xml file
tramoseats_write_spec(tramoseats_spec_d, file = normalizePath("~/tmp.xml", mustWork = FALSE))


## REFRESH

jwsr <- jws_refresh(jws,policy = "Complete", info="All")
str(jwsr)
class(jwsr)
read_workspace(jwsr,FALSE)

library("rjd3toolkit")
library("rjd3workspace")
library("rjd3providers")

## UPDATE PATH TXT
# Load a workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
my_ws <- jws_open(file)
ws_sap_count(my_ws)
# Update the entire second SA-Processing of the `my_ws` workspace with a new path to raw data
txt_update_path(
   jws = my_ws,
   new_path = system.file("data", "IPI_nace4.csv", package = "rjd3workspace"),
   idx_sap = 2)
# select one (the 2nd) SA-item from second SA-Processing
sap2<- jws_sap(my_ws,2)
sai2<-jsap_sai(sap2,2)
# check path
sai2_list<-read_sai(sai2)
sai2_list$ts$metadata$`@id`
#' or read
## rewrite to see

## UPDATE PATH XLSX
# Load a workspace
file <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
my_ws <- jws_open(file)
ws_sap_count(my_ws)
# Update the entire second SA-Processing of the `my_ws` workspace with a new path to raw data
txt_update_path(
    jws = my_ws,
    new_path = system.file("data", "IPI_nace4.xlsx", package = "rjd3workspace"),
    idx_sap = 2)
# select one (the 2nd) SA-item from second SA-Processing
sap2<- jws_sap(my_ws,2)
sai2<-jsap_sai(sap2,2)
# check path
sai2_list<-read_sai(sai2)
sai2_list$ts$metadata$`@id`
#' or read
## rewrite to see


## CONTEXT

context_ref <- get_context(jws_ref) # see also
class(context_ref)
context_ref$variables$r$REG2_Semaine |> class()

jws_auto <- .jws_new(modelling_context = context_ref)
.jws_sap_new(jws_auto, "industrie")

jsap_auto <- .jws_sap(jws_auto, idx = 1L)
jsap_ref <- .jws_sap(jws_ref, idx = 1L)
context_auto <- get_context(jws_auto)
context_auto$variables$r$LY


# Test2: adding a modelling context from scratch
## variables
# creating one or several external regressors (TS objects), which will
# be gathered in one or several groups
iv1 <- intervention_variable(12, c(2000, 1), 60,
                             starts = "2001-01-01", ends = "2001-12-01"
)
iv2 <- intervention_variable(12, c(2000, 1), 60,
                             starts = "2001-01-01", ends = "2001-12-01", delta = 1
)
# regressors as a list of two groups reg1 and reg2
vars <- list(reg1 = list(x = iv1), reg2 = list(x = iv2))
# creating the modelling context
my_context <- modelling_context(variables = vars)
class(my_context)
my_context$variables
jws_auto <- .jws_new(modelling_context = my_context)
c<-get_context(jws_auto)
c$variables$


# Issues to check  --------------------------------------------------------


###### connex issues to check

### .tsmoniker de toolkit

### question ? info dupliquée ? + la source est dans l'id ?

jws_ref<- .jws_open(file = ws)
ws1 <- read_workspace(jws_ref, compute = FALSE)
s<-ws1$processing$industrie$RF0610$ts$metadata$`@source`
id<-ws1$processing$industrie$RF0610$ts$metadata$`@id`
ws1$processing$industrie$RF0610$ts$moniker$source
id
s
moniker <- .tsmoniker(s, id)
class(moniker)


