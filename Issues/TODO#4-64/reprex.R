################################################################################
#######               Démonstration du package JDCruncheR                #######
################################################################################

# Chargement des packages ------------------------------------------------------

library("JDCruncheR")
library("rjwsacruncher")


# Chargement fonctions utiles --------------------------------------------------

path_fct <- "./R/demo/utility/"
function2import <- list.files(path_fct, full.names = TRUE, recursive = TRUE)
sapply(X = function2import, FUN = source, encoding = "UTF-8") |> invisible()


# Fonction de cruncher' --------------------------------

options(
    cruncher_bin_directory = "C:/Users/UTZK0M/Software/jdemetra-related/jwsacruncher-2.2.4/bin/"
)

# Première chose : on met les WS à l'abri
id <- pull_out_fire("ws_output")

rjwsacruncher::cruncher_and_param(
    workspace = "./WS/ws_output.xml",
    rename_multi_documents = TRUE, # Pour renommer les dossiers en sortie
    delete_existing_file = TRUE, # Pour remplacer les sorties existantes
    policy = "complete", # Politique de rafraichissement
    csv_layout = "vtable", # Format de sortie des tables
    log_file = "./WS/log.txt"
)

QR_auto <- JDCruncheR::extract_QR(
    "./WS/ws_output/Output/SAProcessing-1/demetra_m.csv"
)
QR_auto_score <- JDCruncheR::compute_score(QR_auto, n_contrib_score = 5)

JDCruncheR::export_xlsx(
    x = QR_auto_score,
    file_name = "./Issues/TODO#4-64/export_all.xlsx",
    layout = "all"
)
JDCruncheR::export_xlsx(
    x = QR_auto_score,
    file_name = "./Issues/TODO#4-64/export_modalities.xlsx",
    layout = "modalities"
)
JDCruncheR::export_xlsx(
    x = QR_auto_score,
    file_name = "./Issues/TODO#4-64/export_values.xlsx",
    layout = "values"
)
JDCruncheR::export_xlsx(
    x = QR_auto_score,
    file_name = "./Issues/TODO#4-64/export_combined.xlsx",
    layout = "combined"
)

bring_back(id)
