
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




