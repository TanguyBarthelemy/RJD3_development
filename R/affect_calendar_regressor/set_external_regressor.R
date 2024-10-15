library("RJDemetra")

# Fct to import ----------------------------------------------------------------

path_fct <- "./R/demo/utility/"
function2import <- list.files(path_fct, full.names = TRUE, recursive = TRUE)
sapply(X = function2import, FUN = source, encoding = "UTF-8") |> invisible()


# Déclaration des variables ----------------------------------------------------

regs_cjo <- read.csv("./data/regs_cjo.csv", sep = ";", dec = ".")
reg1 <- ts(subset(regs_cjo, select = REG1_AC1),
    start = 1990, frequency = 12
)
reg2 <- ts(subset(regs_cjo, select = c(REG2_AC1, REG2_AC2)),
    start = 1990, frequency = 12
)
reg3 <- ts(subset(regs_cjo, select = c(REG3_AC1, REG3_AC2, REG3_AC3)),
    start = 1990, frequency = 12
)
reg5 <- ts(
    subset(regs_cjo, select = c(
        REG5_AC1, REG5_AC2, REG5_AC3,
        REG5_AC4, REG5_AC5
    )),
    start = 1990, frequency = 12
)
reg6 <- ts(
    subset(regs_cjo, select = c(
        REG6_AC1, REG6_AC2, REG6_AC3,
        REG6_AC4, REG6_AC5, REG6_AC6
    )),
    start = 1990, frequency = 12
)


# Add a usr variable for calendar regressor ------------------------------------

id1 <- pull_out_fire("ws_affect_reg_cjo")

ws <- RJDemetra::load_workspace("WS/ws_affect_reg_cjo.xml")

RJDemetra::compute(ws)


sa_item1 <- ws |>
    RJDemetra::get_object(1) |>
    RJDemetra::get_object(1)

spec <- sa_item1 |>
    RJDemetra::get_model(workspace = ws)

new_spec <- RJDemetra::x13_spec(
    spec = spec,
    tradingdays.option = "UserDefined",
    usrdef.varEnabled = TRUE,
    usrdef.var = reg3,
    usrdef.varType = c("Calendar", "Calendar", "Calendar")
)

new_sa_item <- set_spec(sa_item = sa_item1, spec = new_spec)

replace_sa_item(
    mp = ws |> RJDemetra::get_object(1),
    pos = 1, sa_item = new_sa_item
)

RJDemetra::save_workspace(ws, "WS/ws_affect_reg_cjo.xml")

bring_all_back()


# Rename variable in var = error -----------------------------------------------

id1 <- pull_out_fire("ws_affect_reg_cjo")

ws <- RJDemetra::load_workspace("WS/ws_affect_reg_cjo.xml")
RJDemetra::save_workspace(ws, "WS/ws_affect_reg_cjo.xml")

bring_all_back()
