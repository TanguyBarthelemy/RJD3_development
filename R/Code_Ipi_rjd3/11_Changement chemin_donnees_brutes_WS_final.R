################################################################################
#                                                                              #
#                 Mise a jour du chemin vers les données brutes                #
#                                                                              #
################################################################################

# Chargement des packages R ----------------------------------------------------

library("rjdworkspace")

# Chemin vers le workspace (xml maître)
workspace <- "V:/Methodo-ICA/Campagne_annuelle_2024/IPI/workspace_final/industrie.xml"

# Chemin vers le fichier des donnees brutes a indiquer au cruncher
donnees <- "V:/DG75-E320/Section IPI/IPI-rebasement/cvs/data_pour_jdemetra/IPI_nace4.csv"

update_path(ws_xml_path = workspace, raw_data_path = donnees)
