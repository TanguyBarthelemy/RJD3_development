################################################################################
##############    Récupérer une série dans plusieurs workspaces    #############
################################################################################

# Présentation -----------------------------------------------------------------

# But : Comparer les composantes d'UNE serie dans plusieurs workspaces
# --> voir l'impact de differents choix de parametres
# --> voir les revisions avec 3 modes de refresh différents

# Explications :
#   - Chaque WS a été créé avec des refresh policy differents
#   - On va donc extraire une série de chaque WS (la même à chaque fois
#   pour comparer son ajustement saisonniers par les différents refreshment)

# Plan du programme :
#   1) Initialisation de paramètres généraux et import des packages
#   2) Préciser les chemins des WS et les variables spécifiques user-defined
#   3) Importer la série (à expertiser) de chaque WS, traiter les données et les
#       enregistrer dans une liste
#   4) Merger les listes
#   5) Ecrire les tables dans des fichiers excel
#   6) Affichage de graphiques

# Pendant chaque étape, on travaillera avec des data.frame car :
#   - subset pour conserver ou enlever certaines colonnes --> difficile en ts
#   - cbind change les colnames

# Import des packages ----------------------------------------------------------

# Si RJDemetra n'est pas installé sur votre poste,
# faire tourner la ligne suivante :

# install.packages("RJDemetra", type = "source",
#                  INSTALL_opts = "--no-multiarch" )

library("RJDemetra")
library("ggplot2")

# library("dplyr")
# library("xlsx")
# library("zoo")


# Lecture fonctions utiles ------------------------------------------------

source("R/utilities.R")


# Personnalisation des chemins et des séries -----------------------------------

# Série à expertiser (même nom que dans les workspaces)
serie_a_exp <- "RF3511"

# ws_work : WS de travail
ch_1 <- "./workspace_automatique/industrie.xml"
# ws_auto : WS automatique
ch_2 <- "./workspace_de_travail/industrie.xml"

# Création d'une table pour les chemins et noms des workspaces
# name : méthode de refreshment policy
# path : liste chemins ws
# id : liste noms WS permettant d'identifier les WS
#   --> pour suffixer les variables et identifier le WS
# Mettre les noms / id dans le même ordre que les ws
summary_ws <- data.frame(
    path = c(ch_1, ch_2),
    id = c("work", "auto"),
    name = c("Travail", "Automatique")
) |>
    dplyr::mutate(num_ws = seq_len(dplyr::n()))

summary_ws


# Paramètres de X13 ------------------------------------------------------------

# Ici on définit le "user-defined" : c'est un ensemble de variable qui ne sont
# pas incluses de base mais que l'on peut paramétrer au besoin pour récupérer
# des infos supplémentaires :
# On ajoute :
#   - "cal" + "full residuals" = residus du modele REG-Arima
#   - "d10" = coefficient saisonnier hors effet calendrier
#       (d10a pour la prevision)
#   - "b1" = ylin,
#   - "c20" = poids finaux X11,
#   - "d1" brute corrigee finale dans X11
#   - et les series prevues ("y_f", "s_f", "d10a", ...)

# Liste choix series et diagnostics possibles
user_defined_variables("X13-ARIMA")

# Indices des variables choisies
num_out <- c(2, 4, 6, 8, 10, 28, 35, 36, 67, 134, 173, 174, 183, 184)
v_out <- user_defined_variables("X13-ARIMA")[num_out]
v_out


# Recherches dans les WS -------------------------------------------------------
# Recuperation des composantes d'UNE SERIE dans chacun des WS de la liste

list_ws <- list()

for (i in summary_ws$num_ws) {
    print(paste("WS n°", i))
    # chemins WS
    ch <- summary_ws[i, "path"]
    print(paste("Le chemin est ", ch))


    ## Chargement du workspace -------------------------------------------------

    ws <- load_workspace(ch)
    RJDemetra::compute(ws)
    serie_ipi <- get_object(ws, 1)
    series <- get_all_objects(serie_ipi)
    # On choisit une série (ici serie_a_exp)
    sa <- get_object(serie_ipi, which(get_all_names(serie_ipi) == serie_a_exp))
    model_sa <- get_model(sa, ws, userdefined = v_out)


    ## Séries principales ------------------------------------------------------

    print(paste("Séries principales du workspace", summary_ws[i, "name"]))
    # Récupération des séries principales sous forme de ts
    main_series_ts <- model_sa$final$series
    head(main_series_ts)

    # Passage en data.frame
    main_series_df <- cbind(
        date = zoo::as.Date(time(main_series_ts)),
        as.data.frame(main_series_ts)
    )
    head(main_series_df)

    # afficher la dernière obs realisée
    last_obs <- tail(main_series_df$date, n = 1)
    print(paste("Dernier point (non prevu) du ws :", last_obs))


    ## Série du preprocessing --------------------------------------------------

    print(paste("Séries du preprocessing du workspace", summary_ws[i, "name"]))
    pre_processing_ts <- model_sa$regarima$model$effects
    pre_processing_df <- cbind(
        date = zoo::as.Date(time(pre_processing_ts)),
        as.data.frame(pre_processing_ts)
    )
    head(pre_processing_df)

    # On enleve effet de Paques et fêtes mobiles
    pre_processing_df <- subset(pre_processing_df, select = -c(ee, omhe))
    head(pre_processing_df)


    ## Output user defined -----------------------------------------------------

    print(paste("Séries user-defined du workspace", summary_ws[i, "name"]))
    # Changement des noms de colonne
    names(model_sa$user_defined) <- names(model_sa$user_defined) |>
        gsub(pattern = "preprocessing.model.", replacement = "") |>
        gsub(pattern = "decomposition.", replacement = "") |>
        gsub(pattern = "fullresiduals", replacement = "full_res")

    # Sélection des séries de prévision : series "_f" dans la liste + d10a
    index_prev <- unique(c(
        grep("_f", names(model_sa$user_defined)),
        grep("d10a", names(model_sa$user_defined))
    ))

    print("Index des séries de prévision :")
    print(index_prev)

    # Récupération et aggrégation des séries
    prev_series_ts <- do.call(cbind, model_sa$user_defined[index_prev])

    # Changement en data.frame
    prev_series_df <- cbind(
        date = zoo::as.Date(time(prev_series_ts)),
        as.data.frame(prev_series_ts)
    )
    print("Séries de prévision :")
    print(prev_series_df)

    # Afficher la derniere obs realisée
    last_obs_prev <- tail(prev_series_df$date, n = 1)
    print(paste("Dernier point prevu du ws :", last_obs_prev))


    ## Ajustement schéma modèle ------------------------------------------------

    # AJUSTEMENT y_lin : prendre l exponenetielle en cas de modele multiplicatif
    if (model_sa$regarima$model$spec_rslt$`Log transformation`) {
        # Schéma multiplicatif
        pre_processing_df$y_lin <- exp(pre_processing_df$y_lin)
        prev_series_df$y_lin_f <- exp(prev_series_df$y_lin_f)
    }
    # Schéma additif --> ne rien faire

    head(pre_processing_df)
    print(prev_series_df)


    ## Séries complémentaires --------------------------------------------------

    print(paste("Séries complémentaires du workspace", summary_ws[i, "name"]))
    # Ce sont les séries NON STOCKEES précédemment dans la liste :
    # cal, fullresiduals = fr, b1, c20, d1, d10
    index_comp <- setdiff(seq_len(length(model_sa$user_defined)), index_prev)
    print("Index des séries de complémentaire :")
    print(index_comp)

    comp_series_ts <- do.call(cbind, model_sa$user_defined[index_comp])
    comp_series_df <- cbind(
        date = zoo::as.Date(time(comp_series_ts)),
        as.data.frame(comp_series_ts)
    )
    print("Début de série :")
    head(comp_series_df)
    print("Fin de série :")
    tail(comp_series_df)


    ## Réunion des tables ------------------------------------------------------

    # suffixe = id du ws avec _
    suffixe <- paste0("_", summary_ws[i, "id"])
    print(suffixe)

    # merge REALISEES df = main + pre proc + comp
    print(paste("Séries réalisées du workspace", summary_ws[i, "name"]))
    realises_df <- main_series_df |>
        merge(comp_series_df, by = "date", all = TRUE) |>
        merge(pre_processing_df, by = "date", all = TRUE)
    colnames(realises_df) <- c(
        "date", paste0(colnames(realises_df)[-1], suffixe)
    )
    print("Merge séries main + comp + prepro :")
    head(realises_df)

    # Réunion des séries réalisées et des prévisions associées

    # Séries réalisées
    temp_main_series <- main_series_df[, c(
        "date", "y", "sa",
        "t", "s", "i"
    )] |>
        merge(comp_series_df[, c("date", "cal")],
            by = "date",
            all.x = TRUE, all.y = FALSE
        ) |>
        merge(pre_processing_df[, c("date", "y_lin")],
            by = "date",
            all.x = TRUE, all.y = FALSE
        )
    # Prévisions des séries
    temp_prev_series <- prev_series_df[, c(
        "date",
        "y_f", "sa_f", "t_f", "s_f",
        "i_f", "cal_f", "y_lin_f"
    )]
    colnames(temp_prev_series) <- colnames(temp_main_series)
    extended_series_df <- rbind(
        temp_main_series,
        temp_prev_series
    )

    # Calcul des taux de croissance / évolution des séries y et sa
    ev_extended_series <- apply(
        X = subset(extended_series_df, select = c(y, sa)),
        MARGIN = 2, FUN = tx_cr
    )
    ev_extended_series <- rbind(NA, ev_extended_series)
    colnames(ev_extended_series) <- paste0("ev_", colnames(ev_extended_series))
    # Regroupement des séries
    extended_series_df <- cbind(extended_series_df, ev_extended_series)

    colnames(extended_series_df) <- c(
        "date", paste0(colnames(extended_series_df)[-1], suffixe)
    )
    print("Réunion des séries réalisées et des prévisions associées :")
    head(extended_series_df)
    tail(extended_series_df)

    # Séries prévues

    colnames(prev_series_df) <- c(
        "date", paste0(colnames(prev_series_df)[-1], suffixe)
    )

    ## Outliers ----------------------------------------------------------------

    # Outliers par workspace
    outliers_df <- as.data.frame(model_sa$regarima$regression.coefficients)
    # Regresseurs cjo
    regs_cjo_ts <- model_sa$regarima$specification$regression$userdef$variables$serie
    regs_cjo_df <- cbind(
        date = zoo::as.Date(time(regs_cjo_ts)),
        as.data.frame(regs_cjo_ts)
    )


    ## Réunions des différents WS ----------------------------------------------

    # pour chaque WS de la boucle, on récupère les tables :
    #   - realises_df
    #   - prev_series_df
    #   - extended_series_df
    #   - outliers_df
    #   - regs_cjo_df

    objets_ws <- list(
        rea = realises_df,
        prev = prev_series_df,
        extended = extended_series_df,
        outliers = outliers_df,
        regs_cjo = regs_cjo_df,
        modele = model_sa
    )

    list_ws[[summary_ws[i, "id"]]] <- objets_ws
}


# Regroupement des tables ------------------------------------------------------

# Les séries réalisées (donc qui s'arrêtent en décembre 2021)
realises_ws_df <- purrr::reduce(
    .x = lapply(X = summary_ws$id, FUN = \(x) list_ws[[x]]$rea),
    .f = \(x, y) merge(x, y, by = "date", all = TRUE)
)

# Les séries de forecast (prévision)
prevision_ws_df <- purrr::reduce(
    .x = lapply(X = summary_ws$id, FUN = \(x) list_ws[[x]]$prev),
    .f = \(x, y) merge(x, y, by = "date", all = TRUE)
)

# Les séries étendues réalisées puis forecast
extended_ws_df <- purrr::reduce(
    .x = lapply(X = summary_ws$id, FUN = \(x) list_ws[[x]]$extended),
    .f = \(x, y) merge(x, y, by = "date", all = TRUE)
)

# L'ensemble des séries réalisées et forecast (mais sans être étendue)
total_ws_df <- merge(
    x = realises_ws_df, y = prevision_ws_df,
    by = "date", all = TRUE
)
head(total_ws_df)
tail(total_ws_df)


# Export des données sous Excel ------------------------------------------------

## Premier export : un fichier par WS ------------------------------------------

for (i in summary_ws$num_ws) {
    file_path <- paste0(
        "./output/", summary_ws$name[i], "_",
        serie_a_exp, ".xlsx"
    )
    xlsx::write.xlsx(
        x = list_ws[[i]]$rea, file = file_path,
        sheetName = "Séries réalisées", row.names = FALSE
    )
    xlsx::write.xlsx(
        x = list_ws[[i]]$prev, file = file_path,
        sheetName = "Séries prévues", row.names = FALSE
    )
    xlsx::write.xlsx(
        x = list_ws[[i]]$extended, file = file_path,
        sheetName = "Séries réalisées étendues", row.names = FALSE
    )
    if (nrow(list_ws[[i]]$outliers) > 0) {
        xlsx::write.xlsx(
            x = list_ws[[i]]$outliers, file = file_path,
            sheetName = "Outliers", row.names = FALSE
        )
    }
    xlsx::write.xlsx(
        x = list_ws[[i]]$regs_cjo, file = file_path,
        sheetName = "Régresseurs cjo", row.names = FALSE
    )
}

## Deuxième fichier: un onglet par type de données sans la référence -----------

### Séries réalisées -----------------------------------------------------------

xlsx::write.xlsx(
    x = realises_ws_df,
    file = paste0("./output/Comparaison_ws_", serie_a_exp, ".xlsx"),
    sheetName = "Séries réalisées", row.names = FALSE
)

### Séries prévues -------------------------------------------------------------

xlsx::write.xlsx(
    x = prevision_ws_df,
    file = paste0("./output/Comparaison_ws_", serie_a_exp, ".xlsx"),
    sheetName = "Séries prévues", row.names = FALSE
)

### Séries réalisées et prolongées ---------------------------------------------

xlsx::write.xlsx(
    x = extended_ws_df,
    file = paste0("./output/Comparaison_ws_", serie_a_exp, ".xlsx"),
    sheetName = "Séries réalisées étendues", row.names = FALSE
)


# Visionner des séries ---------------------------------------------------------

## Graphique des séries ajustées -----------------------------------------------

data_to_plot <- extended_ws_df[, c("date", paste0("sa_", summary_ws$id))] |>
    subset(date >= "2017-01-01")
tail(data_to_plot)

data_to_plot <- data_to_plot |>
    tidyr::pivot_longer(
        col = !date, names_to = "WS",
        values_to = "sa", names_prefix = "sa_"
    ) |>
    dplyr::group_by(WS) |>
    subset(!is.na(sa))

ggplot2::ggplot(data_to_plot, ggplot2::aes(x = date, y = sa, colour = WS)) +
    ggplot2::ggtitle(paste("Série CVS :", serie_a_exp)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_x_date(
        name = "Année",
        date_breaks = "1 year",
        date_labels = "%Y"
    ) +
    ggplot2::scale_y_continuous(name = serie_a_exp)

## Graphique des taux de croissance --------------------------------------------

data_to_plot <- extended_ws_df[, c("date", paste0("ev_sa_", summary_ws$id))] |>
    subset(date >= "2017-01-01")
tail(data_to_plot)

data_to_plot <- data_to_plot |>
    tidyr::pivot_longer(
        col = !date, names_to = "WS",
        values_to = "ev", names_prefix = "ev_"
    ) |>
    dplyr::group_by(WS) |>
    subset(!is.na(ev))

ggplot2::ggplot(data_to_plot, ggplot2::aes(x = date, y = ev, colour = WS)) +
    ggplot2::ggtitle(paste("Taux de croissance : série", serie_a_exp)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_x_date(
        name = "Année",
        date_breaks = "1 year",
        date_labels = "%Y"
    ) +
    ggplot2::scale_y_continuous(name = serie_a_exp)


## Graphique des prévisions ----------------------------------------------------
# en pointillés : le ws de reference contient des données jusqu'en sept. 2021
# séries sa et sa_f

data_to_plot <- total_ws_df[, c(
    "date", paste0("sa_", summary_ws$id),
    paste0("sa_f_", summary_ws$id)
)] |>
    subset(date >= "2017-01-01")

head(data_to_plot)
tail(data_to_plot)

data_to_plot <- data_to_plot |>
    tidyr::pivot_longer(
        col = !date, names_to = "WS",
        values_to = "sa", names_prefix = "sa_"
    ) |>
    tidyr::separate(
        col = WS, sep = "f_",
        into = c("serie", "WS"), fill = "left"
    ) |>
    dplyr::mutate(
        serie = dplyr::case_when(
            serie == "" ~ "Prévue",
            is.na(serie) ~ "Réalisé",
            TRUE ~ "Il y a un pb dans le programme !"
        )
    ) |>
    subset(!is.na(sa))

ggplot2::ggplot(data_to_plot, ggplot2::aes(x = date, y = sa, colour = WS, linetype = serie)) +
    ggplot2::ggtitle(paste("Série", serie_a_exp)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_linetype_manual(values = c("dashed", "solid")) +
    ggplot2::scale_x_date(
        name = "Année",
        date_breaks = "1 year",
        date_labels = "%Y"
    ) +
    ggplot2::scale_y_continuous(name = serie_a_exp)


## Graphique du pré-ajustement -------------------------------------------------

### Graphique des y linéarisées ------------------------------------------------

data_to_plot <- extended_ws_df[, c("date", paste0("y_lin_", summary_ws$id))] |>
    subset(date >= "2017-01-01")
tail(data_to_plot)

data_to_plot <- data_to_plot |>
    tidyr::pivot_longer(
        col = !date, names_to = "WS",
        values_to = "lin", names_prefix = "y_lin_"
    ) |>
    dplyr::group_by(WS) |>
    subset(!is.na(lin))

ggplot2::ggplot(data_to_plot, ggplot2::aes(x = date, y = lin, colour = WS)) +
    ggplot2::ggtitle(paste("Série", serie_a_exp)) +
    ggplot2::geom_line(linewidth = 1) +
    # ggplot2::geom_vline(xintercept = c(as.Date("2021-09-01"), as.Date("2022-01-01")),
    #            linetype = "dashed", colour = c("blue", "red")) +
    ggplot2::scale_x_date(
        name = "Année",
        date_breaks = "1 year",
        date_labels = "%Y"
    ) +
    ggplot2::scale_y_continuous(name = serie_a_exp)
