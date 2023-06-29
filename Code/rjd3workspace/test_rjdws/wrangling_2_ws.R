
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


# MP manipulation --------------------------------------------------------------

## replace / remove / add sa-item ----------------------------------------------

# Ici on teste les fonctions :
#   - rjdworkspace::replace_sa_item()
#   - rjdworkspace::add_sa_item() 
#   - rjdworkspace::remove_sa_item()
#   - rjdworkspace::remove_all_sa_item()
# on considère 2 WS :
#   - WS input
#   - WS output

# Première chose : on met les WS à l'abri
id1 <- pull_out_fire("ws_output")
id2 <- pull_out_fire("ws_input")

ws <- RJDemetra::load_workspace("WS/ws_output.xml")
ws_input <- RJDemetra::load_workspace("WS/ws_input.xml")
# compute(ws)
# compute(ws_input)

mp1 <- ws |> get_object(pos = 1)
mp2 <- ws |> get_object(pos = 2)
mp3 <- ws |> get_object(pos = 3)

sa_jx13 <- RJDemetra::jx13(ipi_c_eu[, "FR"])
sa_x13 <- RJDemetra::x13(ipi_c_eu[, "FR"])
sa_ts <- RJDemetra::jtramoseats(ipi_c_eu[, "FR"])

# Suppression de tous les SA-item du 2ème MP
remove_all_sa_item(mp = mp2) # Erreur

# Suppression du 1er SA-item du 1er MP (RF1011)
remove_sa_item(mp = mp1, pos = 1)
# mp1 |> get_all_objects() |> length()

# Replacement du 2ème SA-item du 3ème MP (RF1012)
sa_item <- ws_input |> get_object(pos = 3L) |> get_object(pos = 2L)
replace_sa_item(mp = mp3, pos = 2, sa_item = sa_item)

# Ajout d'un nouveau SA-item dans le 3ème MP
add_sa_item(workspace = ws, multiprocessing = "SAProcessing-3", 
            sa_obj = sa_ts, name = "IPI_EU_FR")
# mp3 |> get_all_objects() |> length()

RJDemetra::save_workspace(ws, "./WS/ws_output.xml")

bring_back(id1)
bring_back(id2)


