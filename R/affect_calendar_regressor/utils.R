create_reg_cjo_sets <- function(regs_cjo) {
    REG1 <- regs_cjo[, "REG1_AC1", drop = FALSE]
    attr(REG1, "class") <- c("mts", "ts", "matrix", "array")

    LY <- regs_cjo_ts[, "LY", drop = FALSE]
    attr(LY, "class") <- c("mts", "ts", "matrix", "array")

    sets <- list(
        NO_CJO = NULL,
        REG1 = REG1,
        REG2 = regs_cjo[, c("REG2_AC1", "REG2_AC2")],
        REG3 = regs_cjo[, c("REG3_AC1", "REG3_AC2", "REG3_AC3")],
        REG5 = regs_cjo[, c(
            "REG5_AC1", "REG5_AC2", "REG5_AC3",
            "REG5_AC4", "REG5_AC5"
        )],
        REG6 = regs_cjo[, c(
            "REG6_AC1", "REG6_AC2", "REG6_AC3",
            "REG6_AC4", "REG6_AC5", "REG6_AC6"
        )],
        NO_CJO_LY = LY,
        REG1_LY = regs_cjo[, c("REG1_AC1", "LY")],
        REG2_LY = regs_cjo[, c("REG2_AC1", "REG2_AC2", "LY")],
        REG3_LY = regs_cjo[, c("REG3_AC1", "REG3_AC2", "REG3_AC3", "LY")],
        REG5_LY = regs_cjo[, c(
            "REG5_AC1", "REG5_AC2", "REG5_AC3",
            "REG5_AC4", "REG5_AC5", "LY"
        )],
        REG6_LY = regs_cjo[, c(
            "REG6_AC1", "REG6_AC2", "REG6_AC3",
            "REG6_AC4", "REG6_AC5", "REG6_AC6", "LY"
        )]
    )

    return(sets)
}

create_spec_sets <- function() {
    regs_cjo_sets <- create_reg_cjo_sets(regs_cjo_ts)

    spec_0 <- RJDemetra::x13_spec(
        spec = "RSA3",
        usrdef.outliersEnabled = TRUE,
        usrdef.outliersDate = seq(
            from = as.Date("2020-03-01"),
            to = as.Date("2022-02-01"),
            by = "month"
        ) |> as.character(),
        usrdef.outliersType = rep("AO", times = 2 * 12),
        estimate.from = "2013-01-01"
    )

    spec_sets <- lapply(X = regs_cjo_sets, FUN = function(regs_set) {
        spec <- spec_0
        if (!is.null(regs_set)) {
            nb_regs <- ifelse(is.null(ncol(regs_set)), 1, ncol(regs_set))
            spec <- RJDemetra::x13_spec(
                spec = spec,
                tradingdays.option = "UserDefined",
                usrdef.varEnabled = TRUE,
                usrdef.var = regs_set,
                usrdef.varType = rep("Calendar", nb_regs)
            )
        }
        return(spec)
    })

    return(spec_sets)
}

one_diagnostic <- function(serie, spec) {
    mod <- RJDemetra::x13(series = serie, spec = spec)

    res_td <- mod$diagnostics$residuals_test[c(
        "f-test on sa (td)",
        "f-test on i (td)"
    ), "P.value"]

    note <- sum((res_td < .05) * 2:1)
    aicc <- mod$regarima$loglik["aicc", ]

    return(c(note = note, aicc = aicc))
}

all_diagnostics <- function(serie, spec_sets) {
    if (missing(spec_sets)) {
        spec_sets <- create_spec_sets()
    }

    output <- lapply(X = seq_along(spec_sets), FUN = function(k) {
        spec <- spec_sets[[k]]
        cat("Computing spec", names(spec_sets)[k], "...")
        output <- one_diagnostic(spec = spec, serie = serie)
        cat("Done !\n")
        return(output)
    }) |> do.call(what = rbind)

    output <- cbind(
        regs = names(spec_sets),
        data.frame(output)
    )

    return(output)
}

select_reg_one_serie <- function(serie, name = "", spec_sets) {
    if (missing(spec_sets)) {
        spec_sets <- create_spec_sets()
    }

    diag <- all_diagnostics(serie, spec_sets = spec_sets)
    diag_wo_na <- diag |>
        subset(!is.na(note) & !is.na(aicc))

    if (nrow(diag_wo_na) == 0) {
        stop(
            "Erreur lors du calcul de l'aicc et des p-value.
             Aucun jeu de regresseur n'a pu être sélectionné. ",
            ifelse(name == "", "", paste0("(Série ", name, ")"))
        )
    } else if (all(diag_wo_na$note == 0)) {
        warning(
            "Aucun jeu de regresseur n'est significatif. ",
            ifelse(name == "", "", paste0("(Série ", name, ")"))
        )
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
        name_serie <- colnames(series)[k]
        cat(paste0("\nSérie ", name_serie, " en cours... ", k, "/", ncol(series)), "\n")
        return(select_reg_one_serie(series[, k], name = name_serie))
    })
    output <- as.data.frame(cbind(
        serie = colnames(series),
        reg_selected = output
    ))
    return(output)
}
