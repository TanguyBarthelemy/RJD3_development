################################################################################
#                                                                              #
#                Affecter les régresseurs via le WS_template_cjo               #
#                                                                              #
################################################################################


# Chargement packages ----------------------------------------------------------

library("rjdworkspace")
library("RJDemetra")


# Création WS_auto -------------------------------------------------------------

ws_auto <- new_workspace()
sap1 <- new_multiprocessing(ws_auto, "SAProcessing-1")


# Chargement ws_template_cjo ---------------------------------------------------

# il faut au préalable, créer le ws_template_cjo sous la GUI

ws_template_cjo <- load_workspace(file = "./workspace_template_cjo/industrie.xml")
compute(ws_template_cjo)

series_name <- ws_template_cjo |>
    get_object() |>
    get_all_names()


# Chargement fichier de choix de regresseurs -----------------------------------

choix_regs_cjo <- read.csv("./choix_cjo.csv", sep = ";")
choix_regs_cjo$selected_N_outliers <- choix_regs_cjo$selected_N_outliers |> gsub(pattern = "Pas_CJO_LY", replacement = "LY")


# Transfert des séries du ws_template_cjo au WS_auto ---------------------------

for (k in seq_along(series_name)) {
    name_serie <- series_name[k]
    cat(paste0("Série ", name_serie, " en cours... ", k, "/", length(series_name)), "\n")

    regs_cjo <- choix_regs_cjo$selected_N_outliers[choix_regs_cjo$series == name_serie]
    transfer_series(
        ws_from = ws_template_cjo, ws_to = ws_auto,
        selected_series = name_serie,
        pos_mp_to = 1, name_mp_from = regs_cjo
    )
}


# Sauvegarde du WS_auto --------------------------------------------------------

save_workspace(ws_auto, file = "./workspace_automatique/industrie.xml")

# Il faut maintenant ajouter à la main les variables externes au WS auto sous la GUI
