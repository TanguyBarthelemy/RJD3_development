
################################################################################
#######              Démonstration du package rjdworkspace               ####### 
################################################################################

# Chargement des packages ------------------------------------------------------

# library("RJDemetra")
# library("rjdemetra3")
library("rjdworkspace")


# Chargement fonctions utiles --------------------------------------------------

path_fct <- "./R/rjd3workspace/utility/"
function2import <- list.files(path_fct, full.names = TRUE, recursive = TRUE)
sapply(X = function2import, FUN = source, encoding = "UTF-8") |> invisible()


# Wrangling 2 WS ---------------------------------------------------------------

## Transfert series ------------------------------------------------------------

# Ici on teste la fonction rjdworkspace::transfer_series()
# on considère 2 WS :
#   - WS input
#   - WS output

id1 <- pull_out_fire("ws_input")
id2 <- pull_out_fire("ws_output")

ws_input <- RJDemetra::load_workspace("WS/ws_input.xml")
ws_output <- RJDemetra::load_workspace("WS/ws_output.xml")

# Existing MP
transfer_series(ws_from = ws_input, ws_to = ws_output, 
                mp_from_name = "SAProcessing-1", 
                mp_to_name = "SAProcessing-1", 
                print_indications = TRUE)

# Missing MP
transfer_series(ws_from = ws_input, ws_to = ws_output, 
                mp_from_name = "SAProcessing-1", 
                mp_to_name = "New-SAProcessing-from-R", 
                print_indications = TRUE, create = FALSE)

transfer_series(ws_from = ws_input, ws_to = ws_output, 
                mp_from_name = "SAProcessing-1", 
                mp_to_name = "New-SAProcessing-from-R", 
                print_indications = TRUE, create = TRUE)

RJDemetra::save_workspace(ws_output, "./WS/ws_output.xml")

bring_all_back()


## Replace series --------------------------------------------------------------

# Ici on teste la fonction rjdworkspace::replace_series()
# on considère 2 WS :
#   - WS input
#   - WS output

id1 <- pull_out_fire("ws_input")
id2 <- pull_out_fire("ws_output")

ws_output <- RJDemetra::load_workspace("WS/ws_output.xml")
ws_input <- RJDemetra::load_workspace("WS/ws_input.xml")

# Pas de remplacement
replace_series(ws_from = ws_input, ws_to = ws_output, 
               mp_from_name = "SAProcessing-2", mp_to_name = "SAProcessing-2", 
               selected_series = c("RF0812", "RF1020", "RF1039"))

replace_series(ws_from = ws_input, ws_to = ws_output, 
               mp_from_name = "SAProcessing-2", mp_to_name = "SAProcessing-2", 
               selected_series = c("RF1041", "RF1042"))

replace_series(ws_from = ws_input, ws_to = ws_output, 
               mp_from_name = "SAProcessing-2", mp_to_name = "SAProcessing-3", 
               selected_series = c("RF1039", "RF1042"))

RJDemetra::save_workspace(ws_output, "./WS/ws_output.xml")

bring_all_back()


## Update metadata -------------------------------------------------------------

# Ici on teste la fonction rjdworkspace::update_metadata()
# on considère 2 WS :
#   - WS input
#   - WS output

id1 <- pull_out_fire("ws_input")
id2 <- pull_out_fire("ws_output")

ws_input <- RJDemetra::load_workspace("WS/ws_input.xml")
ws_output <- RJDemetra::load_workspace("WS/ws_output.xml")

update_metadata(ws_from = ws_input, ws_to = ws_output)

RJDemetra::save_workspace(ws_output, "./WS/ws_output.xml")

bring_back(id1)
bring_back(id2)


# Update_metatada_roughly
id1 <- pull_out_fire("ws_input")
id2 <- pull_out_fire("ws_output")

ws_input <- RJDemetra::load_workspace("WS/ws_input.xml")
ws_output <- RJDemetra::load_workspace("WS/ws_output.xml")

update_metadata_roughly(ws_from = ws_input, ws_to = ws_output)

RJDemetra::save_workspace(ws_output, "./WS/ws_output.xml")

bring_back(id1)
bring_back(id2)


# Wrangling 1 WS ---------------------------------------------------------------

## replace / remove / add sa-item ----------------------------------------------

# Ici on teste les fonctions :
#   - rjdworkspace::replace_sa_item()
#   - rjdworkspace::add_new_sa_item() 
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

mp1 <- ws |> RJDemetra::get_object(pos = 1)
mp2 <- ws |> RJDemetra::get_object(pos = 2)
mp3 <- ws |> RJDemetra::get_object(pos = 3)

sa_jx13 <- RJDemetra::jx13(RJDemetra::ipi_c_eu[, "FR"])
sa_x13 <- RJDemetra::x13(RJDemetra::ipi_c_eu[, "FR"])
sa_ts <- RJDemetra::jtramoseats(RJDemetra::ipi_c_eu[, "FR"])

# Suppression de tous les SA-item du 2ème MP
remove_all_sa_item(mp = mp2) # Erreur

# Suppression du 1er SA-item du 1er MP (RF1011)
remove_sa_item(mp = mp1, pos = 1)
# mp1 |> get_all_objects() |> length()

# Replacement du 2ème SA-item du 3ème MP (RF1012)
sa_item_1 <- ws_input |> 
    RJDemetra::get_object(pos = 3L) |> 
    RJDemetra::get_object(pos = 2L)

replace_sa_item(mp = mp3, pos = 2, sa_item = sa_item_1)

# Ajout d'un nouveau SA-item dans le 3ème MP
sa_item_2 <- ws_input |> 
    RJDemetra::get_object(pos = 1L) |> 
    RJDemetra::get_object(pos = 1L)
add_new_sa_item(mp = mp1, sa_item = sa_item_2)
# mp3 |> get_all_objects() |> length()

RJDemetra::save_workspace(ws, "./WS/ws_output.xml")

bring_all_back()


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

mp3_to <- ws |> 
    RJDemetra::get_object(3)

sa_item_from <- ws_input |> 
    RJDemetra::get_object(3) |> 
    RJDemetra::get_object(3)
sa_item_to <- mp3_to |> 
    RJDemetra::get_object(1)

new_sa_item <- set_metadata(sa_from = sa_item_from, sa_to = sa_item_to)
replace_sa_item(mp = mp3_to, pos = 1, sa_item = new_sa_item)

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

mp3 <- ws |> RJDemetra::get_object(3)

sa_item_1 <- mp3 |> RJDemetra::get_object(3)
sa_item_2 <- mp3 |> RJDemetra::get_object(5)
sa_item_3 <- mp3 |> RJDemetra::get_object(4)

print(get_comment(x = sa_item_1))
print(get_comment(x = mp3))

new_sa_item_2 <- set_comment(x = sa_item_2, comment = "Commentaire depuis R")
new_sa_item_3 <- set_comment(x = sa_item_3, comment = "Modification du commentaire depuis R")

replace_sa_item(mp = mp3, pos = 5, sa_item = new_sa_item_2)
replace_sa_item(mp = mp3, pos = 4, sa_item = new_sa_item_3)

RJDemetra::save_workspace(ws, "./WS/ws_output.xml")

bring_all_back()


## Set metadata ----------------------------------------------------------------

# Ici on teste la fonction :
#   - rjdworkspace::set_spec()
# on considère 2 WS :
#   - WS input
#   - WS output

id1 <- pull_out_fire("ws_output")
id2 <- pull_out_fire("ws_input")

ws <- RJDemetra::load_workspace("WS/ws_output.xml")
ws_input <- RJDemetra::load_workspace("WS/ws_input.xml")

RJDemetra::compute(ws_input)

spec <- ws_input |> 
    RJDemetra::get_object(3) |> 
    RJDemetra::get_object(5) |> 
    RJDemetra::get_jmodel(workspace = ws_input)

sa_item <- ws |> 
    RJDemetra::get_object(3) |> 
    RJDemetra::get_object(5)

new_sa_item <- set_spec(sa_item = sa_item, spec = sa_item_input)
replace_sa_item(mp = ws |> RJDemetra::get_object(3), 
                pos = 5, sa_item = new_sa_item)

RJDemetra::save_workspace(ws, "./WS/ws_output.xml")

bring_all_back()


## Set path --------------------------------------------------------------------

# Ici on teste la fonction :
#   - rjdworkspace::update_path()
# on considère 1 WS :
#   - WS path

id <- pull_out_fire("ws_path")
move_data()

# source("./R/rjd3workspace/new_developpements/new_change_path.R", encoding = "UTF-8")
update_path2(ws_xml_path = "./WS/ws_path.xml", 
             raw_data_path = "./data_temp/path_2/data_ipi.csv", pos_mp = 1)
update_path2(ws_xml_path = "./WS/ws_path.xml", 
             raw_data_path = "./data_temp/path_2/data_ipi.xls", pos_mp = 2)
update_path2(ws_xml_path = "./WS/ws_path.xml", 
             raw_data_path = "./data_temp/path_2/data_ipi.xlsx", 
             pos_mp = 3, pos_sa_item = 4)

move_data()
bring_all_back()


## Set ts ----------------------------------------------------------------------

# Ici on teste la fonction :
#   - rjdworkspace::set_ts()
# on considère 1 WS :
#   - WS output

id <- pull_out_fire("ws_output")

ws <- RJDemetra::load_workspace("WS/ws_output.xml")

mp1 <- ws |> RJDemetra::get_object(1)

ts1 <- nottem
ts2 <- JohnsonJohnson
ts3 <- ts(1:200, start = 2000, frequency = 12)

sa_item_1 <- mp1 |> RJDemetra::get_object(1)
sa_item_2 <- mp1 |> RJDemetra::get_object(2)
sa_item_3 <- mp1 |> vRJDemetra::get_object(3)

new_sa_item_1 <- set_ts(sa_item = sa_item_1, ts = ts1)
new_sa_item_2 <- set_ts(sa_item = sa_item_2, ts = ts2)
new_sa_item_3 <- set_ts(sa_item = sa_item_3, ts = ts3)

replace_sa_item(mp = mp1, pos = 1, sa_item = new_sa_item_1)
replace_sa_item(mp = mp1, pos = 2, sa_item = new_sa_item_2)
replace_sa_item(mp = mp1, pos = 3, sa_item = new_sa_item_3)

RJDemetra::save_workspace(ws, "./WS/ws_output.xml")

bring_all_back()


## Verif duplicated series -----------------------------------------------------

# Ici on teste la fonction :
#   - rjdworkspace::verif_ws_duplicates()
# on considère 2 WS :
#   - WS output
#   - WS duplicated

id1 <- pull_out_fire("ws_output")
id2 <- pull_out_fire("ws_duplicated")

ws_out <- RJDemetra::load_workspace("WS/ws_output.xml")
ws_dup <- RJDemetra::load_workspace("WS/ws_duplicated.xml")

verif_ws_duplicates(ws_out)
verif_ws_duplicates(ws_dup)

bring_all_back()


## Set name --------------------------------------------------------------------

# Ici on teste la fonction :
#   - rjdworkspace::set_ts()
# on considère 1 WS :
#   - WS output

id <- pull_out_fire("ws_output")

ws <- RJDemetra::load_workspace("WS/ws_output.xml")

mp1 <- ws |> RJDemetra::get_object(1)
sa_item_1 <- mp1 |> RJDemetra::get_object(1)

new_sa_item_1 <- set_name(sa_item = sa_item_1, name = "name_from_r")

replace_sa_item(mp = mp1, pos = 1, sa_item = new_sa_item_1)

RJDemetra::save_workspace(ws, "./WS/ws_output.xml")

bring_all_back()

