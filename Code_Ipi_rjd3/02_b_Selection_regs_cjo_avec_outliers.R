################################################################################
#                                                                              #
#                 Selection regresseurs cjo                                    #
#                                                                              #
################################################################################


# Chargement des packages R ----------------------------------------------------

library("RJDemetra")

### selection AVEC et SANS recup des outliers precedents


# Import séries brutes ---------------------------------------------------------
## attention ici
raw_series_ipi <- read.csv("./donnees/IPI_nace4.csv",
    sep = ";", dec = "."
)
series_ipi_ts <- raw_series_ipi |>
    ts(start = 1990L, frequency = 12L)

regs_cjo_ts <- read.csv("./REGS_CJO/regs_mens_JD.csv",
    sep = ";", dec = "."
) |>
    ts(start = 1990L, frequency = 12L)

create_reg_cjo_sets <- function(regs_cjo) {
    REG1 <- regs_cjo[, "REG1_Semaine", drop = FALSE] # drop = false, arg
    attr(REG1, "class") <- c("mts", "ts", "matrix", "array") # si ts  de colonne, cas où un seul regresseur

    LY <- regs_cjo_ts[, "LY", drop = FALSE]
    attr(LY, "class") <- c("mts", "ts", "matrix", "array")

    sets <- list(
        Pas_CJO = NULL,
        REG1 = REG1,
        # REG2 = regs_cjo[, c("REG2_Semaine", "REG2_Samedi")],
        # REG3 = regs_cjo[, c("REG3_Lundi", "REG3_Semaine", "REG3_Samedi")],
        REG5 = regs_cjo[, c(
            "REG5_Lundi", "REG5_Mardi", "REG5_Mercredi",
            "REG5_Jeudi", "REG5_Vendredi"
        )],
        REG6 = regs_cjo[, c(
            "REG6_Lundi", "REG6_Mardi", "REG6_Mercredi",
            "REG6_Jeudi", "REG6_Vendredi", "REG6_Samedi"
        )],
        LY = LY,
        REG1_LY = regs_cjo[, c("REG1_Semaine", "LY")],
        # REG2_LY = regs_cjo[, c("REG2_Semaine", "REG2_Samedi", "LY")],
        # REG3_LY = regs_cjo[, c("REG3_Lundi", "REG3_Semaine", "REG3_Samedi", "LY")],
        REG5_LY = regs_cjo[, c(
            "REG5_Lundi", "REG5_Mardi", "REG5_Mercredi",
            "REG5_Jeudi", "REG5_Vendredi", "LY"
        )],
        REG6_LY = regs_cjo[, c(
            "REG6_Lundi", "REG6_Mardi", "REG6_Mercredi",
            "REG6_Jeudi", "REG6_Vendredi", "REG6_Samedi", "LY"
        )]
    )

    return(sets)
}

create_spec_sets <- function() {
    regs_cjo_sets <- create_reg_cjo_sets(regs_cjo_ts)

    spec_0 <- RJDemetra::x13_spec(
        spec = "RSA3",
        estimate.from = "2012-01-01"
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


all_diagnostics <- function(serie, spec_sets, outliers = NULL) {
    if (missing(spec_sets)) {
        spec_sets <- create_spec_sets()
    }

    output <- lapply(X = seq_along(spec_sets), FUN = function(k) {
        spec_out <- spec_sets[[k]]
        if (!is.null(outliers)) {
            spec_out <- x13_spec(
                spec = spec_out,
                usrdef.outliersEnabled = TRUE,
                usrdef.outliersType = outliers$type,
                usrdef.outliersDate = outliers$date |> as.character()
            )
        }
        cat("Computing spec", names(spec_sets)[k], "...")
        output <- one_diagnostic(spec = spec_out, serie = serie)
        cat("Done !\n")
        return(output)
    })

    output <- output |> do.call(what = rbind)
    output <- cbind(
        regs = names(spec_sets),
        data.frame(output)
    )

    return(output)
}


select_reg_one_serie <- function(serie, name = "", outliers = NULL, spec_sets) {
    if (missing(spec_sets)) {
        spec_sets <- create_spec_sets()
    }

    diag <- all_diagnostics(serie, outliers = outliers, spec_sets = spec_sets)
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

select_regs <- function(series, with_outliers = FALSE) {
    if (is.null(ncol(series))) {
        return(select_reg_one_serie(series))
    }

    output <- sapply(X = seq_len(ncol(series)), FUN = function(k) {
        series_name <- colnames(series)[k]
        outliers <- NULL

        if (with_outliers) {
            # On récupère les outliers
            sai_ref <- sap_ref |> get_object(which(series_name_ref == series_name))
            sai_mod <- sai_ref |> get_model(workspace = ws_ref)
            regressors <- sai_mod$regarima$regression.coefficients |> rownames()
            regressors <- regressors[substr(regressors, 1, 2) %in% c("AO", "TC", "LS", "SO")]


            if (length(regressors) > 0) {
                outliers_type <- regressors |> substr(start = 1, stop = 2)
                outliers_date <- regressors |>
                    substr(start = 5, stop = nchar(regressors) - 1) |>
                    paste0("01-", ... = _) |>
                    as.Date(format = "%d-%m-%Y")

                outliers_type <- outliers_type[outliers_date >= as.Date("2012-01-01")]
                outliers_date <- outliers_date[outliers_date >= as.Date("2012-01-01")]

                if (length(outliers_date) > 0) outliers <- list(type = outliers_type, date = outliers_date)
            }
        }

        cat(paste0("\nSérie ", series_name, " en cours... ", k, "/", ncol(series)), "\n")
        return(select_reg_one_serie(serie = series[, k], name = series_name, outliers = outliers))
    })

    output <- cbind(series = colnames(series), reg_selected = output)
    return(output)
}

# Avec outliers --> récupération sur le WS ref
ws_ref <- load_workspace(file = "./workspace_ref/industrie.xml")
compute(ws_ref)
series_name_ref <- ws_ref |>
    get_object() |>
    get_all_names()
sap_ref <- ws_ref |> get_object()

selected_cjo_N_outliers <- select_regs(series_ipi_ts[, -1], with_outliers = TRUE)
colnames(selected_cjo_N_outliers) <- c("Series", "selected_N_outliers")
write.table(selected_cjo_N_outliers, file = "./selected_cjo_N_outliers.csv", sep = ";", row.names = FALSE)

selected_cjo_N <- select_regs(series_ipi_ts[, -1], with_outliers = FALSE)
colnames(selected_cjo_N_outliers) <- c("Series", "selected_N")
write.table(selected_cjo_N, file = "./selected_cjo_N.csv", sep = ";", row.names = FALSE)
