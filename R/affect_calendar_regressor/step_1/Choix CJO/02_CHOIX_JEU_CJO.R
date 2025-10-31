################################################################################
########              Sélection du jeu de régresseur CJO                ########
################################################################################

# Chargement packages -----------------------------------------------------

library("JDCruncheR")


# Chemin ------------------------------------------------------------------

# repertoire de travail = celui qui contient les donnees, a ajuster ...
# setwd("V:/DG75-L120/Desaisonnalisation/SSMSI_2022/")

# liste WS = liste des fichiers xml du repertoire
# On doit obtenir les .xml WS_export_choix_cjo.xml et WS_import_choix_cjo.xml
WS_xml <- list.files(
    path = "./Choix CJO/WS",
    pattern = "*.xml",
    full.names = TRUE
)

# liste des dossier ws : idem sans le .xml
# On doit obtenir les dossiers WS_export_choix_cjo et WS_import_choix_cjo
WS_dossier <- list.dirs(
    path = "./Choix CJO/WS",
    full.names = TRUE,
    recursive = FALSE
)

# liste des noms de WS
# On doit obtenir les dossiers WS_export_choix_cjo et WS_import_choix_cjo
WS_name <- list.dirs(
    path = "./Choix CJO/WS",
    full.names = FALSE,
    recursive = FALSE
)


# Mise en forme -----------------------------------------------------------

summary_tot_PN <- tot_PN |>
    # On récupère les variables note et aicc pour comparaison
    dplyr::select(series, dplyr::starts_with(c("note", "aicc"))) |>
    tidyr::pivot_longer(
        -series,
        names_to = c(".value", "choix_reg_cjo"),
        names_pattern = "(aicc|note)_(\\w+)"
    ) |>
    dplyr::mutate(
        note = dplyr::case_when(
            choix_reg_cjo == "Pas_CJO" & is.na(note) ~ Inf,
            TRUE ~ note
        ),
        aicc = dplyr::case_when(
            choix_reg_cjo == "Pas_CJO" & is.na(aicc) ~ Inf,
            TRUE ~ aicc
        )
    ) |>
    dplyr::group_by(series) |>
    # On sélectionne les jeux de régresseur avec la note la plus basse
    dplyr::filter(note == min(note, na.rm = TRUE)) |>
    # Puis avec l'aicc le plus élevé
    dplyr::filter(aicc == min(aicc, na.rm = TRUE)) |>
    # Enfin, en cas d'égalité
    dplyr::filter(dplyr::row_number() == 1)

summary_tot_GN <- tot_GN |>
    # On récupère les variables note et aicc pour comparaison
    dplyr::select(series, dplyr::starts_with(c("note", "aicc"))) |>
    tidyr::pivot_longer(
        -series,
        names_to = c(".value", "choix_reg_cjo"),
        names_pattern = "(aicc|note)_(\\w+)"
    ) |>
    dplyr::mutate(
        note = dplyr::case_when(
            choix_reg_cjo == "Pas_CJO" & is.na(note) ~ Inf,
            TRUE ~ note
        ),
        aicc = dplyr::case_when(
            choix_reg_cjo == "Pas_CJO" & is.na(aicc) ~ Inf,
            TRUE ~ aicc
        )
    ) |>
    dplyr::group_by(series) |>
    # On sélectionne les jeux de régresseur avec la note la plus basse
    dplyr::filter(note == min(note, na.rm = TRUE)) |>
    # Puis avec l'aicc le plus élevé
    dplyr::filter(aicc == min(aicc, na.rm = TRUE)) |>
    # Enfin, en cas d'égalité
    dplyr::filter(dplyr::row_number() == 1)

if (nrow(summary_tot_PN) != 135) {
    stop("Il y a un souci dans le nombre de série.")
}
if (nrow(summary_tot_GN) != 135) {
    stop("Il y a un souci dans le nombre de série.")
}


# Export et écriture des tables -------------------------------------------

write.table(
    x = summary_tot_GN,
    file = "./Choix CJO/choix_jo_GN.csv",
    quote = FALSE,
    sep = ";",
    row.names = FALSE
)
write.table(
    x = summary_tot_PN,
    file = "./Choix CJO/choix_jo_PN.csv",
    quote = FALSE,
    sep = ";",
    row.names = FALSE
)

save(
    liste_BQ_WS,
    liste_coeff_WS,
    tot_GN,
    tot_PN,
    summary_tot_GN,
    summary_tot_PN,
    file = "./Choix CJO/output_data_jeu_cjo.RData"
)
