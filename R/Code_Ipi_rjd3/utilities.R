################################################################################
#                                                                              #
#                                   Utilités                                   #
#                                                                              #
################################################################################

# Fonction d'affichage ----------------------------------------------------

## objectif affiche le nom

printt <- function(obj) {
    obj_name <- deparse(substitute(obj)) # recuperer le nom ds l'appel
    cat(obj_name, ":", obj, "\n") # cat = print  + \n retour à la ligne
    return(invisible(NULL)) # forcer un retour
}

v <- 1:10
v
printt(v)

# Indicateurs statistiques -----------------------------------------------------

# Calcul d'un taux de croissance
### si vecteur
tx_cr <- function(v) {
    if ("ts" %in% class(v)) {
        w <- (v - stats::lag(v, -1)) / stats::lag(v, -1) * 100
    } else {
        w <- (v[-1] - dplyr::lag(v)[-1]) / dplyr::lag(v)[-1] * 100
    }
    return(w)
}

# Calcul d'un glissement annuel
glissement_annuel <- function(v) {
    if ("ts" %in% class(v)) {
        w <- v - stats::lag(v, -1)
    } else {
        w <- v[-1] - dplyr::lag(v)[-1]
    }
    return(w)
}


# Fonctions de complément à rjdworkspace ---------------------------------------

# récupérer le jeu de régresseurs associé à un SA-ITEM

get_cjo_regressor <- function(specification) {
    regressors <- specification$regarima$regression.coefficients |> rownames()
    regressors <- regressors[
        !substr(regressors, 1, 2) %in% c("AO", "TC", "LS", "SO", "Me")
    ]

    regs_cjo <- "Pas_CJO"
    if (any(grepl("REG1", regressors))) {
        regs_cjo <- "REG1"
    }
    if (any(grepl("REG5", regressors))) {
        regs_cjo <- "REG5"
    }
    if (any(grepl("REG6", regressors))) {
        regs_cjo <- "REG6"
    }
    if (any(grepl("LeapYear", regressors)) || any(grepl("LY", regressors))) {
        regs_cjo <- paste0(regs_cjo, "_LY")
    }

    if (regs_cjo == "Pas_CJO_LY") {
        regs_cjo <- "LY"
    }

    return(regs_cjo)
}

# récupérer les outliers associé à un SA-ITEM

# after = apres une certaine date
# sorted

get_outliers <- function(specification, after = TRUE, sorted = TRUE) {
    regressors <- specification$regarima$regression.coefficients |> rownames()
    regressors <- regressors[
        substr(regressors, 1, 2) %in% c("AO", "TC", "LS", "SO")
    ]

    if (length(regressors) > 0) {
        outliers_type <- regressors |> substr(start = 1, stop = 2)
        outliers_date <- regressors |>
            substr(start = 5, stop = nchar(regressors) - 1) |>
            paste0("01-", ... = _) |>
            as.Date(format = "%d-%m-%Y")

        if (after) {
            outliers_type <- outliers_type[
                outliers_date >= as.Date("2012-01-01")
            ]
            outliers_date <- outliers_date[
                outliers_date >= as.Date("2012-01-01")
            ]

            if (length(outliers_date) == 0) {
                return(NULL)
            }
        }

        if (sorted) {
            outliers_type <- outliers_type[order(outliers_date)]
            outliers_date <- outliers_date[order(outliers_date)]
        }

        return(list(
            type = outliers_type,
            date = outliers_date
        ))
    }

    return(NULL)
}

# Changer le domainSpec

set_domain_spec <- function(sa_item, spec) {
    sa_def <- .jcall(
        obj = sa_item,
        returnSig = "Ljd2/datatypes/sa/SaItemType;",
        method = "getSaDefinition"
    )

    sa_item <- rjdworkspace:::builder_from_sa(
        sa_def = sa_def,
        domainSpec = RJDemetra::get_jspec(spec)
    )
    new_sa_item <- .jnew("ec/tstoolkit/jdr/ws/SaItem", sa_item)
    new_sa_item
}
