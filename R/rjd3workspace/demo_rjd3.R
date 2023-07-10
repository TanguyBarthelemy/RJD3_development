
################################################################################
#######           Démonstration des packages en version 3                ####### 
################################################################################


# Chargement des packages ------------------------------------------------------

library("rjd3toolkit")
library("rjdemetra3")


# Chargement fonctions utiles --------------------------------------------------

path_fct <- "./R/rjd3workspace/utility/"
function2import <- list.files(path_fct, full.names = TRUE, recursive = TRUE)
sapply(X = function2import, FUN = source, encoding = "UTF-8") |> invisible()


# Fonction de création et de sauvegarde ----------------------------------------

## Create / save WS and MP -----------------------------------------------------

# Ici on teste les fonctions :
#   - rjdemetra3::.jws_new()
#   - rjdemetra3::.jws_multiprocessing_new()
#   - rjdemetra3::save_workspace()

if (!dir.exists("temp")) {
    dir.create("temp")
}

new_ws <- .jws_new()
mp1 <- .jws_multiprocessing_new(jws = new_ws, name = "SAP-1")

save_workspace(new_ws, "./temp/new_ws.xml")

empty_temp()


# Fonction de chargement -------------------------------------------------------

# Ici on teste les fonctions :
#   - rjdemetra3::.jws_open()
#   - rjdemetra3::.jws_compute()

if (!dir.exists("temp")) {
    dir.create("temp")
}

ws_in <- .jws_open(file = "./WS/ws_input.xml")
.jws_compute(ws_in)


# Fonction d'accès -------------------------------------------------------------

# Ici on teste les fonctions :
#   - .jws_multiprocessing()
#   - .jmp_load()
#   - .jmp_sa()
#   - .jmp_name()
#   - .jsa_name()
#   - .jsa_results()
#   - .jsa_read()
#   - .jws_multiprocessing_count()
#   - .jmp_sa_count()

ws_in <- .jws_open(file = "./WS/ws_input.xml")
.jws_multiprocessing_count(ws_in)

jmp1 <- .jws_multiprocessing(ws_in, idx = 1)
name_mp1 <- .jmp_name(jmp1)
.jmp_sa_count(jmp1)

jsa1_mp1 <- .jmp_sa(jmp1, idx = 1)
name_sa1_mp1 <- .jsa_name(jsa1_mp1)

.jws_compute(ws_in)

all_sa_mp1 <- .jmp_load(mp1)
sa1_mp1 <- .jsa_read(jsa1_mp1)
res_sa1_mp1 <- .jsa_results(jsa1_mp1)

jestimation <- .jcall(jsa1_mp1, "Ljdplus/sa/base/api/SaEstimation;", "getEstimation")
jrslt <- .jcall(jestimation, "Ljdplus/toolkit/base/api/information/Explorable;", "getResults")
.proc_dictionary2(jrslt)


# Fonction de modification d'un SA-ITEM ----------------------------------------

# Ici on teste la fonction :
#   - add_sa_item()
#   - remove_sa_item()
#   - replace_sa_item()

# Première chose : on met les WS à l'abri
id1 <- pull_out_fire("ws_output")
id2 <- pull_out_fire("ws_input")

ws <- .jws_open(file = "./WS/ws_output.xml")
ws_in <- .jws_open(file = "./WS/ws_input.xml")

jmp1 <- .jws_multiprocessing(ws, idx = 1)
jmp2 <- .jws_multiprocessing(ws, idx = 2)
jmp3 <- .jws_multiprocessing(ws, idx = 3)

jmp3_in <- .jws_multiprocessing(ws_in, idx = 3)

sa_x13 <- rjd3x13::x13(rjd3toolkit::ABS[, 1])
sa_ts <- rjd3tramoseats::tramoseats(rjd3toolkit::ABS[, 2])

# Ajout d'un nouveau SA-item dans le 1er MP
add_sa_item(jmp = jmp1, name = "ABS_1", x = sa_x13)
add_sa_item(jmp = jmp1, name = "ABS_2", x = sa_ts)

# Suppression d'un SA-item du 2ème MP
remove_sa_item(jmp2, idx = 1)
remove_sa_item(jmp2, idx = 2)
remove_sa_item(jmp2, idx = 3)
remove_sa_item(jmp2, idx = 4)
remove_sa_item(jmp2, idx = 5)

# Replacement du 1er SA-item du 3ème MP (RF1012)
jsa1_mp3 <- .jmp_sa(jmp3_in, idx = 1)
replace_sa_item(jmp = jmp1, jsa = jsa1_mp3, idx = 1)

save_workspace(jws = ws, file = "./temp/new_ws.xml", replace = TRUE)

bring_all_back()
empty_temp()
