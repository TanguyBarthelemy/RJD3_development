
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

## Set metadata ----------------------------------------------------------------

# Ici on teste la fonction :
#   - rjdworkspace::set_metadata()
# on considère 2 WS :
#   - WS input
#   - WS output

# Première chose : on met les WS à l'abri
id1 <- pull_out_fire("ws_output")
id2 <- pull_out_fire("ws_input")

ws <- RJDemetra::load_workspace("WS/ws_output.xml")
ws_input <- RJDemetra::load_workspace("WS/ws_input.xml")

sa_item_from <- ws_input |> 
    get_object(3) |> 
    get_object(3)
sa_item_to <- ws |> 
    get_object(3) |> 
    get_object(1)

new_sa_item <- set_metadata(sa_from = sa_item_from, sa_to = sa_item_to)
replace_sa_item(mp = ws |> get_object(3), pos = 1, sa_item = new_sa_item)

RJDemetra::save_workspace(ws, "./WS/ws_output.xml")

bring_all_back()


## Commentaires ----------------------------------------------------------------

# Ici on teste les fonctions :
#   - rjdworkspace::get_comment() 
#   - rjdworkspace::set_comment()
# on considère 1 WS :
#   - WS output

# Première chose : on met les WS à l'abri
id1 <- pull_out_fire("ws_output")

ws <- RJDemetra::load_workspace("WS/ws_output.xml")

sa_item_1 <- ws |> get_object(3) |> get_object(3)
sa_item_2 <- ws |> get_object(3) |> get_object(5)
sa_item_3 <- ws |> get_object(3) |> get_object(4)

get_comment(x = sa_item_1) # erreur
new_sa_item_2 <- set_comment(x = sa_item_2, comment = "Commentaire depuis R")
new_sa_item_3 <- set_comment(x = sa_item_3, comment = "Modification du commentaire depuis R")

replace_sa_item(mp = ws |> get_object(3), pos = 5, sa_item = new_sa_item_2)
replace_sa_item(mp = ws |> get_object(3), pos = 4, sa_item = new_sa_item_3)

RJDemetra::save_workspace(ws, "./WS/ws_output.xml")

bring_all_back()


## Set metadata ----------------------------------------------------------------

# Ici on teste la fonction :
#   - rjdworkspace::set_spec()
# on considère 2 WS :
#   - WS input
#   - WS output

# Première chose : on met les WS à l'abri
id1 <- pull_out_fire("ws_output")
id2 <- pull_out_fire("ws_input")

ws <- RJDemetra::load_workspace("WS/ws_output.xml")
ws_input <- RJDemetra::load_workspace("WS/ws_input.xml")

RJDemetra::compute(ws_input)

spec <- ws_input |> 
    get_object(3) |> 
    get_object(5) |> 
    get_jmodel(workspace = ws_input)

sa_item <- ws |> 
    get_object(3) |> 
    get_object(5)

new_sa_item <- set_spec(sa_item = sa_item, spec = sa_item_input)
replace_sa_item(mp = ws |> get_object(3), pos = 5, sa_item = new_sa_item)

RJDemetra::save_workspace(ws, "./WS/ws_output.xml")

bring_all_back()


## Set path ----------------------------------------------------------------

# Ici on teste la fonction :
#   - rjdworkspace::update_path()
# on considère 2 WS :
#   - WS path

# Première chose : on met les WS à l'abri
id <- pull_out_fire("ws_path")
move_data()

source("./Code/rjd3workspace/new_developpements/new_change_path.R", encoding = "UTF-8")
update_path(ws_xml_path = "./WS/ws_path.xml", raw_data_path = "./data_temp/path_2/data_ipi.csv", pos_mp = 1)
update_path(ws_xml_path = "./WS/ws_path.xml", raw_data_path = "./data_temp/path_2/data_ipi.xls", pos_mp = 2)
update_path(ws_xml_path = "./WS/ws_path.xml", raw_data_path = "./data_temp/path_2/data_ipi.xlsx", 
            pos_mp = 3, pos_sa_item = 4)

move_data()
bring_all_back()


# Remaining functions ----------------------------------------------------------

# - replace_series
# - verif_ws_duplicates
# - set_ts
# - set_name

