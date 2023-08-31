
raw_series_ipi <- read.csv("./data/IPI_nace4.csv", 
                           sep = ";", dec = ".")
series_ipi_ts <- raw_series_ipi |> 
    ts(start = 1990L, frequency = 12L)

regs_cjo_ts <- read.csv("./data/regs_cjo.csv", sep = ";", dec = ".") |> 
    ts(start = 1990L, frequency = 12L)

create_reg_cjo_sets <- function(regs_cjo) {
    sets <- list(
        NO_CJO = NULL, 
        REG1 = regs_cjo[, "REG1_AC1"], 
        REG2 = regs_cjo[, c("REG2_AC1", "REG2_AC2")], 
        REG3 = regs_cjo[, c("REG3_AC1", "REG3_AC2", "REG3_AC3")], 
        REG5 = regs_cjo[, c("REG5_AC1", "REG5_AC2", "REG5_AC3", 
                            "REG5_AC4", "REG5_AC5")], 
        REG6 = regs_cjo[, c("REG6_AC1", "REG6_AC2", "REG6_AC3", 
                            "REG6_AC4", "REG6_AC5", "REG6_AC6")], 
        NO_CJO_LY = regs_cjo[, "LY"], 
        REG1_LY = regs_cjo[, c("REG1_AC1", "LY")], 
        REG2_LY = regs_cjo[, c("REG2_AC1", "REG2_AC2", "LY")], 
        REG3_LY = regs_cjo[, c("REG3_AC1", "REG3_AC2", "REG3_AC3", "LY")], 
        REG5_LY = regs_cjo[, c("REG5_AC1", "REG5_AC2", "REG5_AC3", 
                               "REG5_AC4", "REG5_AC5", "LY")], 
        REG6_LY = regs_cjo[, c("REG6_AC1", "REG6_AC2", "REG6_AC3", 
                               "REG6_AC4", "REG6_AC5", "REG6_AC6", "LY")]
    )
    
    return(sets)
}

regs_cjo_sets <- create_reg_cjo_sets(regs_cjo_ts)

spec <- RJDemetra::x13_spec(
    spec = "RSA3", 
    tradingdays.option = "UserDefined",
    usrdef.varEnabled = TRUE,
    usrdef.var = regs_cjo_sets[[1]],
    usrdef.varType = c("Calendar", "Calendar", "Calendar"))
mod <- RJDemetra::x13(
    series = series_ipi_ts[, 3], 
    spec = spec)

# Ici il ne faut pas formatter en ts
RJDemetra::x13(ts(raw_series_ipi[, 1], start = 1990, frequency = 12), spec = spec) # erreur à cause du format date

one_diagnostic <- function(serie, regs_set) {
    
    if (is.null(regs_set)) {
        spec <- RJDemetra::x13_spec(spec = "RSA3")
    } else {
        spec <- RJDemetra::x13_spec(
            spec = "RSA3", 
            tradingdays.option = "UserDefined",
            usrdef.varEnabled = TRUE,
            usrdef.var = regs_set,
            usrdef.varType = c("Calendar", "Calendar", "Calendar"))
    }
    
    mod <- RJDemetra::x13(series = serie, spec = spec)
    
    res_td <- mod$diagnostics$residuals_test[c("f-test on sa (td)", 
                                               "f-test on i (td)"), "P.value"]
    
    note <- sum((res_td < .05) * 2:1)
    aicc <- mod$regarima$loglik["aicc", ]
    
    return(c(note = note, aicc = aicc))
}

all_diagnostics <- function(serie) {
    
    output <- lapply(X = regs_cjo_sets, FUN = one_diagnostic, serie = serie) |> do.call(what = rbind)
    output <- cbind(regs = rownames(output), 
                    data.frame(output))
    
    return(output)
}

z <- all_diagnostics(series_ipi_ts[, 4])

select_reg_one_serie <- function(serie, name = "") {
    diag <- all_diagnostics(serie)
    diag_wo_na <- diag |> 
        subset(!is.na(note) & !is.na(aicc))
    
    if (any(is.na(diag$note))) {
        stop("ON a trouvé ", name)
    }
    
    if (nrow(diag_wo_na) == 0) {
        stop("Erreur lors du calcul de l'aicc et des p-value.\nAucun jeu de regresseur n'a pu être sélectionné.")
    } else if (all(diag_wo_na$note == 0)) {
        warning("Aucun jeu de regresseur n'est significatif.")
    }
    
    best_regs <- diag_wo_na |> 
        subset(note == max(note, na.rm = TRUE)) |> 
        subset(aicc == min(aicc, na.rm = TRUE))
    
    return(best_regs[1, 1])
}

select_regs <- function(series) {
    
    if (is.null(ncol(series))) {
        return(select_reg_one_serie(series))
    }
    
    output <- sapply(X = seq_len(ncol(series)), FUN = function(k) {
        name_serie = colnames(series)[k]
        cat(paste0("Série ", name_serie, " en cours... ", k, "/", ncol(series)), "\n")
        return(select_reg_one_serie(series[, k], name = name_serie))
    })
    output <- cbind(serie = colnames(series), reg_selected = output)
    return(output)
}

a <- select_regs(series_ipi_ts[, 2:5])

