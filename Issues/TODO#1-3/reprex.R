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


# Fonction d'accès aux noms des multiprocessing --------------------------------

# Ici on teste la fonction :
#   - multiprocessing_names()

# Première chose : on met les WS à l'abri
id1 <- pull_out_fire("ws_V2_names")
id2 <- pull_out_fire("ws_V3_names")
id3 <- pull_out_fire("ws_rjdemetra_names")
id3 <- pull_out_fire("ws_cjo")

print(rjwsacruncher::multiprocessing_names("WS/ws_V2_names"))
print(rjwsacruncher::multiprocessing_names("WS/ws_V3_names"))
print(rjwsacruncher::multiprocessing_names("WS/ws_rjdemetra_names"))
print(rjwsacruncher::multiprocessing_names("WS/ws_rjdemetra_names"))
print(rjwsacruncher::multiprocessing_names("WS/ws_cjo"))

bring_all_back()

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
scores <- JDCruncheR::extract_score(QR_auto_score)


bring_back(id)
