
################################################################################
#######             Test des fonctions v2 de gestion des WS              ####### 
################################################################################

# Chargement des packages ------------------------------------------------------

library("RJDemetra")
library("rjdworkspace")


# Chargement fonctions de print ------------------------------------------------

path <- "./Code/rjd3workspace/utility/"
function2import <- list.files(path, full.names = TRUE, recursive = TRUE)
sapply(X = function2import, FUN = source, encoding = "UTF-8") |> invisible()


# WS manipulation --------------------------------------------------------------

## Transfert series ------------------------------------------------------------

# Ici on teste la fonction rjdworkspace::transfer_series()
# on considère 2 WS :
#   - WS input
#   - WS output

# Première chose : on met les WS à l'abri
id1 <- pull_out_fire("ws_input")
id2 <- pull_out_fire("ws_output")

ws_from <- RJDemetra::load_workspace("WS/ws_input.xml")
ws_to <- RJDemetra::load_workspace("WS/ws_output.xml")

transfer_series(ws2 = ws_from, ws1 = ws_to, mp_from = "SAProcessing-1", mp_to = "SAProcessing-1", print_indications = FALSE)

RJDemetra::save_workspace(ws_to, "./WS/ws_output.xml")

bring_back(id1)
bring_back(id2)


## Replace series --------------------------------------------------------------

# Ici on teste la fonction rjdworkspace::replace_series()
# on considère 2 WS :
#   - WS input
#   - WS output

# Première chose : on met les WS à l'abri
id1 <- pull_out_fire("ws_input")
id2 <- pull_out_fire("ws_output")

ws_from <- RJDemetra::load_workspace("WS/ws_input.xml")
ws_to <- RJDemetra::load_workspace("WS/ws_output.xml")

replace_series(ws2 = ws_from, ws1 = ws_to, mp_name = "SAProcessing-3", selected_series = c("RF1011", "RF1012"))

RJDemetra::save_workspace(ws_to, "./WS/ws_output.xml")

bring_back(id1)
bring_back(id2)


## Update metadata -------------------------------------------------------------

# Ici on teste la fonction rjdworkspace::update_metadata()
# on considère 2 WS :
#   - WS input
#   - WS output

# Première chose : on met les WS à l'abri
id1 <- pull_out_fire("ws_input")
id2 <- pull_out_fire("ws_output")

ws_from <- RJDemetra::load_workspace("WS/ws_input.xml")
ws_to <- RJDemetra::load_workspace("WS/ws_output.xml")

update_metadata(workspace1 = ws_to, workspace2 = ws_from)

RJDemetra::save_workspace(ws_to, "./WS/ws_output.xml")

bring_back(id1)
bring_back(id2)



