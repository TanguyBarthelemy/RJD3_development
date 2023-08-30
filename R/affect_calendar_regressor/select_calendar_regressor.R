
series <- read.csv("./data/IPI_nace4.csv", sep = ";", dec = ".") |> 
    ts(start = 1990L, frequency = 12L)

regs_cjo <- read.csv("./data/regs_cjo.csv", sep = ";", dec = ".")

reg1 <- ts(subset(regs_cjo, select = REG1_AC1), 
           start = 1990, frequency = 12)
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

regs_cjo_set <- list(
    reg1, reg2, reg3, reg5, reg6
)

output <- data.frame()

spec <- RJDemetra::x13_spec(
    spec = "RSA1", 
    tradingdays.option = "UserDefined",
    usrdef.varEnabled = TRUE,
    usrdef.var = reg3,
    usrdef.varType = c("Calendar", "Calendar", "Calendar"))
RJDemetra::x13(series[, 1], spec = spec) # erreur à cause du format date
mod <- RJDemetra::x13(series[1:40, 2] |> 
                          ts(start = 1990L, frequency = 12L), spec = spec)
aicc <- mod$regarima$loglik["aicc", ]



