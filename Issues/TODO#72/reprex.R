################################################################################
#######               Démonstration du package JDCruncheR                #######
################################################################################

# Chargement des packages ------------------------------------------------------

library("JDCruncheR")
library("rjwsacruncher")

# Fonction de cruncher' --------------------------------

options(cruncher_bin_directory = "C:/Users/UTZK0M/Software/jdemetra-related/jwsacruncher-2.2.4/bin/")

path_ws <- "./Issues/TODO#72/ws_accent"

rjwsacruncher::cruncher_and_param(
    workspace = paste0(path_ws, ".xml"),
    rename_multi_documents = TRUE, # Pour renommer les dossiers en sortie
    delete_existing_file = TRUE, # Pour remplacer les sorties existantes
    policy = "complete", # Politique de rafraichissement
    csv_layout = "vtable", # Format de sortie des tables
    log_file = "./Issues/TODO#72/log.txt"
)

QR_auto <- JDCruncheR::extract_QR(matrix_output_file = paste0(path_ws, "/Output/SAProcessing-1/demetra_m.csv"))
QR_auto_score <- JDCruncheR::compute_score(QR_auto)
scores <- JDCruncheR::extract_score(QR_auto_score)
