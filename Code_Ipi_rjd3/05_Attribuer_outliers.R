################################################################################
#                                                                              #
#                Affecter les régresseurs via le WS_template_cjo               #
#                                                                              #
################################################################################


# Chargement packages ----------------------------------------------------------

library("rjdworkspace")
library("RJDemetra")


# Déclaration fonctions --------------------------------------------------------

source("./R/utilities.R")


# Lectures des WS --------------------------------------------------------------

ws_auto <- load_workspace(file = "./workspace_automatique/industrie.xml")
ws_ref <- load_workspace(file = "./workspace_ref/industrie.xml")

compute(ws_auto)
compute(ws_ref)

sap_auto <- ws_auto |> get_object()
sap_ref <- ws_ref |> get_object()

sap_auto2 <- new_multiprocessing(ws_auto, name = "SAProcessing-2")

# sap_auto3 <- new_multiprocessing(ws_auto, name = "SAProcessing-3")

series_name_ref <- sap_ref |> get_all_names()
series_name_auto <- sap_auto |> get_all_names()


# Ajouter les outliers du WS ref sur le WS auto --------------------------------

for (k in seq_len(sap_auto |> count())) {
    series_name <- series_name_auto[k]
    cat(paste0("Série ", series_name, " en cours... ", k, "/", sap_auto |> count()), "\n")

    sai_auto <- sap_auto |> get_object(which(series_name_auto == series_name))
    sai_ref <- sap_ref |> get_object(which(series_name_ref == series_name))

    outliers <- get_outliers(sai_ref |> get_model(workspace = ws_ref))

    if (length(outliers$date) > 0) {
        spec_init <- sai_auto |> get_model(ws_auto)

        # lors de la création de la new_spec, RJDemetra récupère les variables de régression sans leur nom de groupe
        new_spec <- x13_spec(
            spec = spec_init,
            estimate.from = "2012-01-01",
            usrdef.outliersEnabled = TRUE,
            usrdef.outliersType = outliers$type,
            usrdef.outliersDate = outliers$date |> as.character()
        )

        # Lors de la création du sa-item, rjdworkspace ecrit les variables de régression dans un seul et même groupe appelé r
        new_sai <- x13(series = spec_init$final$series[, "y"], spec = new_spec)
        # new_sai2 <- rjdworkspace::set_spec(sai_auto, new_spec)

        # SAP 1
        add_sa_item(workspace = ws_auto, multiprocessing = "SAProcessing-2", sa_obj = new_sai, name = series_name)

        # SAP 2
        # replace_sa_item(mp = sap_auto2, pos = k, sa_item = new_sai2)
        # SAP 3
        # add_new_sa_item(mp = sap_auto3, sa_item = new_sai2)
    } else {
        add_new_sa_item(mp = sap_auto2, sa_item = sai_auto)
    }
}


# Sauvegarde du WS_auto --------------------------------------------------------

save_workspace(ws_auto, file = "./workspace_automatique/industrie.xml")
