
################################################################################
#######               Démonstration du package JDCruncheR                #######
################################################################################

# Chargement des packages ------------------------------------------------------

library("JDCruncheR")


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

print(JDCruncheR::multiprocessing_names("WS/ws_V2_names"))
print(JDCruncheR::multiprocessing_names("WS/ws_V3_names"))
print(JDCruncheR::multiprocessing_names("WS/ws_rjdemetra_names"))
print(JDCruncheR::multiprocessing_names("WS/ws_cjo"))

bring_all_back()
