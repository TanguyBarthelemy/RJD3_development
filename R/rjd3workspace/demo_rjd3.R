
################################################################################
#######           Démonstration des packages en version 3                ####### 
################################################################################


# Chargement des packages ------------------------------------------------------

# library("rjd3toolkit")
library("rjdemetra3")
library("rJava")


# Chargement fonctions utiles --------------------------------------------------

path_fct <- "./R/rjd3workspace/utility/"
function2import <- list.files(path_fct, full.names = TRUE, recursive = TRUE)
sapply(X = function2import, FUN = source, encoding = "UTF-8") |> invisible()

if (!dir.exists("temp")) {
    dir.create("temp")
}

# Fonction de création et de sauvegarde ----------------------------------------

## Create / save WS and MP -----------------------------------------------------

# Ici on teste les fonctions :
#   - .jws_new()
#   - .jws_multiprocessing_new()
#   - save_workspace()

new_ws <- .jws_new()
mp1 <- .jws_multiprocessing_new(jws = new_ws, name = "SAP-1")

save_workspace(new_ws, "./temp/new_ws.xml")

empty_temp()


# Fonction de chargement -------------------------------------------------------

# Ici on teste les fonctions :
#   - .jws_open()
#   - .jws_compute()

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

all_sa_mp1 <- .jmp_load(jmp1)
sa1_mp1 <- .jsa_read(jsa1_mp1)
res_sa1_mp1 <- .jsa_results(jsa1_mp1)

jestimation <- .jcall(jsa1_mp1, "Ljdplus/sa/base/api/SaEstimation;", "getEstimation")
jrslt <- .jcall(jestimation, "Ljdplus/toolkit/base/api/information/Explorable;", "getResults")
.proc_dictionary2(jrslt)


# Fonction de modification d'un SA-ITEM ----------------------------------------

# Ici on teste la fonction :
#   - add_sa_item()
#   - remove_sa_item()
#   - remove_all_sa_item()
#   - replace_sa_item()

# Première chose : on met les WS à l'abri
id1 <- pull_out_fire("ws_output")
id2 <- pull_out_fire("ws_input")

ws <- .jws_open(file = "./WS/ws_output.xml")
ws_in <- .jws_open(file = "./WS/ws_input.xml")

jmp1 <- .jws_multiprocessing(ws, idx = 1)
jmp2 <- .jws_multiprocessing(ws, idx = 2)
jmp3 <- .jws_multiprocessing(ws, idx = 3)

jmp2_in <- .jws_multiprocessing(ws_in, idx = 2)
jmp3_in <- .jws_multiprocessing(ws_in, idx = 3)

sa_x13 <- rjd3x13::x13(rjd3toolkit::ABS[, 1])
sa_ts <- rjd3tramoseats::tramoseats(rjd3toolkit::ABS[, 2])

spec1 <- rjd3x13::spec_x13(name = "RSA5c")

jsa1_mp1 <- .jmp_sa(jmp1, idx = 1)

# Ajout d'un nouveau SA-item dans le 1er MP
add_sa_item(jmp = jmp1, name = "ABS_1", x = sa_x13)
add_sa_item(jmp = jmp1, name = "ABS_2", x = sa_ts)
add_sa_item(jmp = jmp1, name = "ABS_3", x = jsa1_mp1)
add_sa_item(jmp = jmp1, name = "ABS_4", x = rjd3toolkit::ABS[, 1], spec = spec1)

# Suppression d'un SA-item du 2ème MP
remove_sa_item(jmp3, idx = 5)
remove_sa_item(jmp3, idx = 6)

# Supression du 2ème SAP
remove_all_sa_item(jmp = jmp2)

# Replacement du 1er SA-item du 3ème MP (RF1012)
jsa1_mp3 <- .jmp_sa(jmp3_in, idx = 1)
replace_sa_item(jmp = jmp1, jsa = jsa1_mp3, idx = 1)

.jws_compute(ws_in)
# Transfert de séries
transfer_series(jmp_from = jmp2_in, 
                jmp_to = jmp2, 
                selected_series = c("RF0899", "RF1039", "RF1041"))

save_workspace(jws = ws, file = "./temp/new_ws.xml", replace = TRUE)

bring_all_back()
empty_temp()


## Set metadata ----------------------------------------------------------------

# Ici on teste la fonction :
#   - set_domain_specification()
#   - set_specification()
# on considère 2 WS :
#   - WS input
#   - WS output

id <- pull_out_fire("ws_output")

ws <- .jws_open(file = "./WS/ws_output.xml")
jmp1 <- .jws_multiprocessing(ws, idx = 1)

spec1 <- rjd3x13::spec_x13(name = "RSA3")
spec2 <- rjd3tramoseats::spec_tramoseats(name = "trfull")

set_specification(jmp = jmp1, spec = spec1, idx = 1)
set_specification(jmp = jmp1, spec = spec2, idx = 2)

set_domain_specification(jmp = jmp1, spec = spec2, idx = 1)
.jws_compute(ws)

save_workspace(jws = ws, file = "./temp/new_ws.xml", replace = TRUE)

bring_all_back()
empty_temp()


## Set/get ts ------------------------------------------------------------------

# Ici on teste la fonction :
#   - set_raw_data()
#   - set_ts_metadata()
#   - get_raw_data()
# on considère 1 WS :
#   - WS output

id <- pull_out_fire("ws_output")

ws <- .jws_open("WS/ws_output.xml")
jmp1 <- .jws_multiprocessing(ws, idx = 1)
jmp2 <- .jws_multiprocessing(ws, idx = 1)
jmp3 <- .jws_multiprocessing(ws, idx = 1)

ts2 <- JohnsonJohnson
ts3 <- ts(1:200, start = 2000, frequency = 12)
ts4 <- nottem

jsa1_mp1 <- .jmp_sa(jmp1, idx = 1)

ts1 <- get_raw_data(jsa = jsa1_mp1)

set_raw_data(jmp = jmp2, y = ts1, idx = 1)
set_raw_data(jmp = jmp2, y = ts2, idx = 2)
set_raw_data(jmp = jmp2, y = ts3, idx = 3)
set_raw_data(jmp = jmp2, y = ts4, idx = 4)

set_ts_metadata(jmp = jmp3, ref_jsa = jsa1_mp1, idx = 1)

save_workspace(jws = ws, file = "./temp/new_ws.xml", replace = TRUE)

bring_all_back()
empty_temp()


## Set/get comment ----------------------------------------------------------------

# Ici on teste la fonction :
#   - set_comment()
#   - get_comment()
# on considère 1 WS :
#   - WS output

id <- pull_out_fire("ws_output")

ws <- .jws_open("WS/ws_output.xml")
jmp3 <- .jws_multiprocessing(ws, idx = 3)
jsa3_mp3 <- .jmp_sa(jmp3, idx = 3)

get_comment(jsa3_mp3)
set_comment(jmp3, idx = 1, comment = "Comment written in R")

save_workspace(jws = ws, file = "./temp/new_ws.xml", replace = TRUE)

bring_all_back()
empty_temp()


## Set name --------------------------------------------------------------------

# Ici on teste la fonction :
#   - set_name
# on considère 1 WS :
#   - WS output

id <- pull_out_fire("ws_output")

ws <- .jws_open("WS/ws_output.xml")
jmp1 <- .jws_multiprocessing(ws, idx = 1)

set_name(jmp = jmp1, idx = 2, name = "Robert")

save_workspace(jws = ws, file = "./temp/new_ws.xml", replace = TRUE)

bring_all_back()
empty_temp()

