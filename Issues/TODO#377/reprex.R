path_folder <- "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#377"

# WS tramo V3 ------------------------------------------------------------------

path_ws <- file.path(path_folder, "ws_tramo_v3")

options(
    cruncher_bin_directory = "C:/Users/UTZK0M/Software/jdemetra-related/jwsacruncher-3.2.4/bin/",
    v3 = TRUE
)

rjwsacruncher::cruncher_and_param(
    workspace = paste0(path_ws, ".xml"),
    rename_multi_documents = TRUE, # Pour renommer les dossiers en sortie
    delete_existing_file = TRUE, # Pour remplacer les sorties existantes
    policy = "complete", # Politique de rafraichissement
    csv_layout = "vtable", # Format de sortie des tables
    log_file = "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#67/log.txt"
)

QR_auto <- JDCruncheR::extract_QR(file.path(path_ws, "Output/SAProcessing-1/demetra_m.csv"))
QR_auto |>
    compute_score(na.rm = TRUE) |>
    extract_score()

# WS tramo X-13 V3 ------------------------------------------------------------------

path_ws <- file.path(path_folder, "ws_tramo_x13_v3")

options(
    cruncher_bin_directory = "C:/Users/UTZK0M/Software/jdemetra-related/jwsacruncher-3.2.4/bin/",
    v3 = TRUE
)

rjwsacruncher::cruncher_and_param(
    workspace = paste0(path_ws, ".xml"),
    rename_multi_documents = TRUE, # Pour renommer les dossiers en sortie
    delete_existing_file = TRUE, # Pour remplacer les sorties existantes
    policy = "complete", # Politique de rafraichissement
    csv_layout = "vtable", # Format de sortie des tables
    log_file = "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#67/log.txt"
)

QR_auto <- JDCruncheR::extract_QR(file.path(path_ws, "Output/SAProcessing-1/demetra_m.csv"))
QR_auto |>
    compute_score(na.rm = TRUE) |>
    extract_score()


# WS tramo V2 ------------------------------------------------------------------

path_ws <- file.path(path_folder, "ws_tramo_v2")

options(
    cruncher_bin_directory = "C:/Users/UTZK0M/Software/jdemetra-related/jwsacruncher-2.2.4/bin/",
    v3 = FALSE
)

rjwsacruncher::cruncher_and_param(
    workspace = paste0(path_ws, ".xml"),
    rename_multi_documents = TRUE, # Pour renommer les dossiers en sortie
    delete_existing_file = TRUE, # Pour remplacer les sorties existantes
    policy = "complete", # Politique de rafraichissement
    csv_layout = "vtable", # Format de sortie des tables
    log_file = "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#67/log.txt"
)

QR_auto <- JDCruncheR::extract_QR(file.path(path_ws, "Output/SAProcessing-1/demetra_m.csv"))
QR_auto |>
    compute_score(na.rm = TRUE) |>
    extract_score()


# WS tramo X-13 V2 ------------------------------------------------------------------

path_ws <- file.path(path_folder, "ws_tramo_x13_v2")

options(
    cruncher_bin_directory = "C:/Users/UTZK0M/Software/jdemetra-related/jwsacruncher-2.2.4/bin/",
    v3 = FALSE
)

rjwsacruncher::cruncher_and_param(
    workspace = paste0(path_ws, ".xml"),
    rename_multi_documents = TRUE, # Pour renommer les dossiers en sortie
    delete_existing_file = TRUE, # Pour remplacer les sorties existantes
    policy = "complete", # Politique de rafraichissement
    csv_layout = "vtable", # Format de sortie des tables
    log_file = "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#67/log.txt"
)

QR_auto <- JDCruncheR::extract_QR(file.path(path_ws, "Output/SAProcessing-1/demetra_m.csv"))
QR_auto |>
    compute_score(na.rm = TRUE) |>
    extract_score()
