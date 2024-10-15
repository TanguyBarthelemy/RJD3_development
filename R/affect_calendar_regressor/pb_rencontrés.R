raw_series_ipi <- read.csv("./data/IPI_nace4.csv",
    sep = ";", dec = "."
)
series_ipi_ts <- raw_series_ipi |>
    ts(start = 1990L, frequency = 12L)

regs_cjo_ts <- read.csv("./data/regs_cjo.csv", sep = ";", dec = ".") |>
    ts(start = 1990L, frequency = 12L)


#-----------------------------------------------------
spec <- RJDemetra::x13_spec(
    spec = "RSA3",
    tradingdays.option = "UserDefined",
    usrdef.varEnabled = TRUE,
    usrdef.var = regs_cjo_sets[[1]],
    usrdef.varType = c("Calendar", "Calendar", "Calendar")
)

mod <- RJDemetra::x13(
    series = series_ipi_ts[, 3],
    spec = spec
)

# Ici il ne faut pas formatter en ts
RJDemetra::x13(ts(raw_series_ipi[, 1], start = 1990, frequency = 12), spec = spec) # erreur à cause du format date
#-----------------------------------------------------
