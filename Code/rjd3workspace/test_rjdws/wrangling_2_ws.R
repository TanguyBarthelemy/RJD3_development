
################################################################################
#######             Test des fonctions v2 de gestion des WS              ####### 
################################################################################

# Installation chez AQLT -------------------------------------------------------

# remotes::install_github("https://github.com/AQLT/rjdemetra3")


# Chargement des packages ------------------------------------------------------

library("RJDemetra")
# library("rjdemetra3")
library("rjdworkspace")


# Chargement fonctions de print ------------------------------------------------

path_fct <- "./Code/rjd3workspace/utility/"
function2import <- list.files(path_fct, full.names = TRUE, recursive = TRUE)
sapply(X = function2import, FUN = source, encoding = "UTF-8") |> invisible()


# WS manipulation --------------------------------------------------------------

## Transfert series ------------------------------------------------------------

# Ici on teste la fonction rjdworkspace::transfer_series()
# on considère 2 WS :
#   - WS input
#   - WS output

id1 <- pull_out_fire("ws_input")
id2 <- pull_out_fire("ws_output")

ws_from <- RJDemetra::load_workspace("WS/ws_input.xml")
ws_to <- RJDemetra::load_workspace("WS/ws_output.xml")

transfer_series(ws2 = ws_from, ws1 = ws_to, 
                mp_from = "SAProcessing-1", 
                mp_to = "SAProcessing-1", print_indications = TRUE)

RJDemetra::save_workspace(ws_to, "./WS/ws_output.xml")

bring_back(id1)
bring_back(id2)


## Replace series --------------------------------------------------------------

# Ici on teste la fonction rjdworkspace::replace_series()
# on considère 2 WS :
#   - WS input
#   - WS output

id1 <- pull_out_fire("ws_input")
id2 <- pull_out_fire("ws_output")

ws_from <- RJDemetra::load_workspace("WS/ws_input.xml")
ws_to <- RJDemetra::load_workspace("WS/ws_output.xml")

stop("Si une série est présente dans différents MP, laquelle est utilisée ?")
stop("je crois que c'est le premier --> ajouter un argument mp_from/mp_to pour spécifier")
replace_series(ws2 = ws_from, ws1 = ws_to, 
               mp_name = "SAProcessing-3", 
               selected_series = c("RF1011", "RF1012"))

RJDemetra::save_workspace(ws_to, "./WS/ws_output.xml")

bring_back(id1)
bring_back(id2)


## Update metadata -------------------------------------------------------------

# Ici on teste la fonction rjdworkspace::update_metadata()
# on considère 2 WS :
#   - WS input
#   - WS output

id1 <- pull_out_fire("ws_input")
id2 <- pull_out_fire("ws_output")

ws_from <- RJDemetra::load_workspace("WS/ws_input.xml")
ws_to <- RJDemetra::load_workspace("WS/ws_output.xml")

update_metadata(workspace1 = ws_from, workspace2 = ws_to)

RJDemetra::save_workspace(ws_to, "./WS/ws_output.xml")

bring_back(id1)
bring_back(id2)


# Update_metatada_roughly
id1 <- pull_out_fire("ws_input")
id2 <- pull_out_fire("ws_output")

ws_from <- RJDemetra::load_workspace("WS/ws_input.xml")
ws_to <- RJDemetra::load_workspace("WS/ws_output.xml")

update_metadata_roughly(workspace1 = ws_from, workspace2 = ws_to)

RJDemetra::save_workspace(ws_to, "./WS/ws_output.xml")

bring_back(id1)
bring_back(id2)


## Replace series -----------------------------------------------------

# Ici on teste la fonction :
#   - rjdworkspace::replace_series()
# on considère 2 WS :
#   - WS output
#   - WS input

id1 <- pull_out_fire("ws_output")
id2 <- pull_out_fire("ws_input")

ws_output <- RJDemetra::load_workspace("WS/ws_output.xml")
ws_input <- RJDemetra::load_workspace("WS/ws_input.xml")

replace_series(ws2 = ws_input, ws1 = ws_output, mp_name = "SAProcessing-2", 
               selected_series = c("RF0812", "RF1020", "RF1039"))

replace_series(ws2 = ws_input, ws1 = ws_output, mp_name = "SAProcessing-2", 
               selected_series = c("RF1041", "RF1042"))

replace_series(ws2 = ws_input, ws1 = ws_output, mp_name = "SAProcessing-3", 
               selected_series = c("RF1039", "RF1042"))

RJDemetra::save_workspace(ws_output, "./WS/ws_output.xml")

bring_all_back()

