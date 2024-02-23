
################################################################################
#######           Démonstration des packages en version 3                ####### 
################################################################################


# Chargement des packages ------------------------------------------------------

# library("rjd3toolkit")
library("rjdemetra3")
library("rJava")


# Chargement fonctions utiles --------------------------------------------------

path_fct <- "./R/demo/utility/"
function2import <- list.files(path_fct, full.names = TRUE, recursive = TRUE)
sapply(X = function2import, FUN = source, encoding = "UTF-8") |> invisible()

if (!dir.exists("temp")) {
    dir.create("temp")
}

# Fonction de création et de sauvegarde ----------------------------------------

## Create / save WS and SAP ----------------------------------------------------

# Ici on teste les fonctions :
#   - .jws_new()
#   - .jws_sap_new()
#   - save_workspace()

new_jws <- .jws_new()
jsap1 <- .jws_sap_new(jws = new_jws, name = "SAP-1")

save_workspace(new_jws, "./temp/new_ws.xml")

empty_temp()


# Fonction de chargement -------------------------------------------------------

# Ici on teste les fonctions :
#   - .jws_open()
#   - .jws_compute()

jws_from <- .jws_open(file = "./WS/ws_input.xml")
.jws_compute(jws_from)


# Fonction d'accès -------------------------------------------------------------

# Ici on teste les fonctions :
#   - .jws_sap()
#   - read_sap()
#   - .jsap_sa()
#   - .jsap_name()
#   - .jsa_name()
#   - .jsa_results()
#   - .jsa_read()
#   - .jws_sap_count()
#   - .jsap_sa_count()

jws_from <- .jws_open(file = "./WS/ws_input.xml")
.jws_sap_count(jws_from)

jsap1 <- .jws_sap(jws_from, idx = 1)
name_sap1 <- .jsap_name(jsap1)
.jsap_sa_count(jsap1)

jsa1_sap1 <- .jsap_sa(jsap1, idx = 1)
name_sa1_sap1 <- .jsa_name(jsa1_sap1)

.jws_compute(jws_from)

all_sa_sap1 <- read_sap(jsap1)
sa1_sap1 <- .jsa_read(jsa1_sap1)
res_sa1_sap1 <- .jsa_results(jsa1_sap1)

jestimation <- .jcall(jsa1_sap1, "Ljdplus/sa/base/api/SaEstimation;", "getEstimation")
jrslt <- .jcall(jestimation, "Ljdplus/toolkit/base/api/information/Explorable;", "getResults")
rjd3toolkit::.proc_dictionary2(jrslt)


# Fonction de modification d'un SA-ITEM ----------------------------------------

# Ici on teste la fonction :
#   - add_sa_item()
#   - remove_sa_item()
#   - remove_all_sa_item()
#   - replace_sa_item()

# Première chose : on met les WS à l'abri
id1 <- pull_out_fire("ws_output")
id2 <- pull_out_fire("ws_input")

jws_to <- .jws_open(file = "./WS/ws_output.xml")
jws_from <- .jws_open(file = "./WS/ws_input.xml")

jsap1 <- .jws_sap(jws = jws_to, idx = 1)
jsap2 <- .jws_sap(jws = jws_to, idx = 2)
jsap3 <- .jws_sap(jws = jws_to, idx = 3)

jsap2_in <- .jws_sap(jws_from, idx = 2)
jsap3_in <- .jws_sap(jws_from, idx = 3)

sa_x13 <- rjd3x13::x13(rjd3toolkit::ABS[, 1])
sa_ts <- rjd3tramoseats::tramoseats(rjd3toolkit::ABS[, 2])

spec1 <- rjd3x13::x13_spec(name = "RSA5c")

jsa1_sap1 <- .jsap_sa(jsap1, idx = 1)

# Ajout d'un nouveau SA-item dans le 1er SAP
add_sa_item(jsap = jsap1, name = "ABS_1", x = sa_x13)
add_sa_item(jsap = jsap1, name = "ABS_2", x = sa_ts)
add_sa_item(jsap = jsap1, name = "ABS_3", x = jsa1_sap1)
add_sa_item(jsap = jsap1, name = "ABS_4", x = rjd3toolkit::ABS[, 1], spec = spec1)

# Suppression d'un SA-item du 2ème SAP
remove_sa_item(jsap = jsap3, idx = 5)
remove_sa_item(jsap = jsap3, idx = 6)

# Supression du 2ème SAP
remove_all_sa_item(jsap = jsap2)

# Replacement du 1er SA-item du 3ème SAP (RF1012)
jsa1_sap3 <- .jsap_sa(jsap3_in, idx = 1)
replace_sa_item(jsap = jsap1, jsa = jsa1_sap3, idx = 1)

.jws_compute(jws_from)
# Transfert de séries
transfer_series(jsap_from = jsap2_in, 
                jsap_to = jsap2, 
                selected_series = c("RF0899", "RF1039", "RF1041"))

save_workspace(jws = jws_to, file = "./temp/new_ws.xml", replace = TRUE)

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

jws_to <- .jws_open(file = "./WS/ws_output.xml")
jsap1 <- .jws_sap(ws, idx = 1)

spec1 <- rjd3x13::x13_spec(name = "RSA3")
spec2 <- rjd3tramoseats::spec_tramoseats(name = "trfull")

set_specification(jsap = jsap1, spec = spec1, idx = 1)
set_specification(jsap = jsap1, spec = spec2, idx = 2)

set_domain_specification(jsap = jsap1, spec = spec2, idx = 1)
.jws_compute(jws_to)

save_workspace(jws = jws_to, file = "./temp/new_ws.xml", replace = TRUE)

bring_all_back()
empty_temp()


## Set/get ts ------------------------------------------------------------------

# Ici on teste la fonction :
#   - set_raw_data()
#   - set_ts_metadata()
#   - get_raw_data()
# on considère 2 WS :
#   - WS output
#   - WS input

id1 <- pull_out_fire("ws_output")
id2 <- pull_out_fire("ws_input")

jws_to <- .jws_open("WS/ws_output.xml")
jws_from <- .jws_open("WS/ws_input.xml")

jsap1 <- .jws_sap(jws_to, idx = 1)
jsap2 <- .jws_sap(jws_to, idx = 2)
jsap3 <- .jws_sap(jws_to, idx = 3)

jsa1_sap1 <- .jsap_sa(jsap1, idx = 1)
jsa1_sap3 <- .jsap_sa(jsap3, idx = 1)

jsap3_in <- .jws_sap(jws_from, idx = 3)
jsa1_sap3_in <- .jsap_sa(jsap3_in, idx = 1)

ts1 <- get_raw_data(jsa = jsa1_sap1)
ts2 <- JohnsonJohnson
ts3 <- ts(1:200, start = 2000, frequency = 12)
ts4 <- nottem

# Changer le contenu des raw data
set_raw_data(jsap = jsap2, y = ts1, idx = 1)
set_raw_data(jsap = jsap2, y = ts2, idx = 2)
set_raw_data(jsap = jsap2, y = ts3, idx = 3)
set_raw_data(jsap = jsap2, y = ts4, idx = 4)

# Changer les metadata des raw data
set_ts_metadata(jsap = jsap3, ref_jsa = jsa1_sap3_in, idx = 1)

save_workspace(jws = jws_to, file = "./temp/new_ws.xml", replace = TRUE)

bring_all_back()
empty_temp()


## Set/get comment -------------------------------------------------------------

# Ici on teste la fonction :
#   - set_comment()
#   - get_comment()
# on considère 1 WS :
#   - WS output

id <- pull_out_fire("ws_output")

jws_to <- .jws_open("WS/ws_output.xml")
jsap3 <- .jws_sap(ws, idx = 3)
jsa3_sap3 <- .jsap_sa(jsap3, idx = 3)

get_comment(jsa3_sap3)
set_comment(jsap3, idx = 1, comment = "Comment written in R")

save_workspace(jws = jws_to, file = "./temp/new_ws.xml", replace = TRUE)

bring_all_back()
empty_temp()


## Set name --------------------------------------------------------------------

# Ici on teste la fonction :
#   - set_name
# on considère 1 WS :
#   - WS output

id <- pull_out_fire("ws_output")

jws_to <- .jws_open("WS/ws_output.xml")
jsap1 <- .jws_sap(ws, idx = 1)

set_name(jsap = jsap1, idx = 2, name = "Robert")

save_workspace(jws = jws_to, file = "./temp/new_ws.xml", replace = TRUE)

bring_all_back()
empty_temp()

