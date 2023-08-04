
library("RJDemetra")

r <- "RJDemetra" |> 
    asNamespace() |> 
    lsf.str(envir = _, all = TRUE) |> 
    unclass()
for(name in r) eval(parse(text=paste0(name, '<-RJDemetra:::', name)))


library("rJava")
library("rjdworkspace")

# Fct to import ----------------------------------------------------------------

path_fct <- "./R/rjd3workspace/utility/"
function2import <- list.files(path_fct, full.names = TRUE, recursive = TRUE)
sapply(X = function2import, FUN = source, encoding = "UTF-8") |> invisible()

# Déclaration des variables ----------------------------------------------------

regs_cjo <- read.csv("./data/regs_cjo.csv", sep = ";", dec = ".")
reg1 <- ts(subset(regs_cjo, select = REG1_AC1), start = 1990, frequency = 12)
reg2 <- ts(subset(regs_cjo, select = c(REG2_AC1, REG2_AC2)), 
           start = 1990, frequency = 12)
reg3 <- ts(subset(regs_cjo, select = c(REG3_AC1, REG3_AC2, REG3_AC3)), 
           start = 1990, frequency = 12)
reg5 <- ts(subset(regs_cjo, select = c(REG5_AC1, REG5_AC2, REG5_AC3, 
                                       REG5_AC4, REG5_AC5)), 
           start = 1990, frequency = 12)
reg6 <- ts(subset(regs_cjo, select = c(REG6_AC1, REG6_AC2, REG6_AC3, 
                                       REG6_AC4, REG6_AC5, REG6_AC6)), 
           start = 1990, frequency = 12)

# Code to set spec -------------------------------------------------------------

id1 <- pull_out_fire("ws_affect_reg_cjo")

ws <- RJDemetra::load_workspace("WS/ws_affect_reg_cjo.xml")

RJDemetra::compute(ws)


context <- .jcall(ws, "Lec/tstoolkit/algorithm/ProcessingContext;", 
                  "getContext")
ts_dico <- context$getTsVariableDictionary()

ts_dico$indexOf("Reg6.Lundi")
ts_dico$indexOf("Reg6")
ts_dico$get(1L)
ts_dico$size()

ts_manager <- context$getTsVariableManagers()
ts_manager$variables()
.jmethods(context, "getTsVariables")

var_reg1 <- context$getTsVariables("Reg1")
var_reg2 <- context$getTsVariables("Reg2")
var_reg6 <- .jcall(context, "Lec/tstoolkit/timeseries/regression/TsVariables;", 
                      "getTsVariables", "Reg6")
var_reg1$read()
.jcall(var_reg1, "Lec.tstoolkit.timeseries.regression.TsVariables.read;", 
       "read")
.jmethods(var_reg1, "read")

dico <- .jcall(ws, "Ljdr/spec/ts/Utility$Dictionary;", 
                  "dictionary")



predef.variables <- spec_preVar(var = usrdef.var, vartype = usrdef.varType, 
                                varcoef = usrdef.varCoef, tradingdays.option = tradingdays.option)

sa_item1 <- ws |> 
    RJDemetra::get_object(1) |> 
    RJDemetra::get_object(1) |> 
    RJDeemetra

spec1 <- ws |> 
    RJDemetra::get_object(1) |> 
    RJDemetra::get_object(2) |> 
    RJDemetra::get_jspec(workspace = ws)

spec2 <- ws |> 
    RJDemetra::get_object(1) |> 
    RJDemetra::get_object(5) |> 
    RJDemetra::get_model(workspace = ws)

RJDemetra::x13_spec(spec = spec1, usrdef.varEnabled = TRUE)



new_sa_item <- set_spec(sa_item = sa_item, spec = sa_item_input)
replace_sa_item(mp = ws |> RJDemetra::get_object(3), 
                pos = 5, sa_item = new_sa_item)

RJDemetra::save_workspace(ws, "./WS/ws_output.xml")

bring_all_back()


