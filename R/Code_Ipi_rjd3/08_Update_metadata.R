################################################################################
#                                                                              #
#                           Mis à jour des metadatas                           #
#                                                                              #
################################################################################

# Chargement des packages ------------------------------------------------------

library("rjdworkspace")


# Modification des metadatas ----------------------------------------------

ws_ref <- load_workspace("./workspace_ref/industrie.xml")
ws_auto <- load_workspace("./workspace_automatique/industrie.xml")

updated_ws_auto <- update_metadata(ws_from = ws_ref, ws_to = ws_auto)

save_workspace(
    workspace = updated_ws_auto,
    file = "./workspace_automatique/industrie.xml"
)
