
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
#   - get_all_objects()
#   - get_object()  
#   - get_name()    
#   - get_ts()      
#   - get_model()   
#   - get_jmodel()  
#   - get_dictionary() 
#   - get_indicators() 
#   - .jws_multiprocessing_count()      

ws_in <- .jws_open(file = "./WS/ws_input.xml")
.jws_multiprocessing_count(ws_in)

mp1 <- .jws_multiprocessing(ws_in, idx = 1)
name_mp1 <- .jmp_name(mp1)
.jmp_sa_count(mp1)

jsa1_mp1 <- .jmp_sa(mp1, idx = 1)
name_sa1_mp1 <- .jsa_name(jsa1_mp1)

.jws_compute(ws_in)

all_sa_mp1 <- .jmp_load(mp1)
sa1_mp1 <- .jsa_read(jsa1_mp1)



mod_ws <- get_model(ws_in)
mod_mp1 <- get_model(mp1, ws_in)
mod_sa1_mp1 <- get_model(sa1_mp1, ws_in)

jmod_ws <- get_jmodel(ws_in)
jmod_mp1 <- get_jmodel(mp1, ws_in)
jmod_sa1_mp1 <- get_jmodel(sa1_mp1, ws_in)

get_dictionary(jmod_sa1_mp1)
get_indicators(jmod_sa1_mp1, c("diagnostics.seas-sa-kw", "residuals.tdpeaks"))





