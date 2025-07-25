################################################################################
#######                Démonstration du package RJDemetra                #######
################################################################################

# Chargement des packages ------------------------------------------------------

library("RJDemetra")


# Chargement fonctions utiles --------------------------------------------------

path_fct <- "./R/demo/utility/"
function2import <- list.files(path_fct, full.names = TRUE, recursive = TRUE)
sapply(X = function2import, FUN = source, encoding = "UTF-8") |> invisible()


# Fonction de création et de sauvegarde ----------------------------------------

## Create / save WS and MP -----------------------------------------------------

# Ici on teste les fonctions :
#   - new_workspace()
#   - new_multiprocessing()
#   - save_spec()
#   - save_workspace()

if (!dir.exists("temp")) {
    dir.create("temp")
}

new_ws <- new_workspace()
new_multiprocessing(workspace = new_ws, name = "SAP-1")

spec_out <- x13_spec(
    spec = "RSA5c",
    usrdef.outliersEnabled = TRUE,
    usrdef.outliersType = c("LS", "AO"),
    usrdef.outliersDate = c("2008-10-01", "2002-01-01"),
    usrdef.outliersCoef = c(36, 14),
    transform.function = "None"
)

save_spec(spec_out, "./temp/spec_temp.RData")
save_workspace(new_ws, "./temp/new_ws.xml")

empty_temp()


# Fonction de chargement -------------------------------------------------------

# Ici on teste les fonctions :
#   - load_spec()
#   - load_workspace()
#   - compute()

spec_in <- load_spec(file = "./data_temp/spec1.RData")
ws_in <- load_workspace(file = "./WS/ws_input.xml")
compute(ws_in)


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
#   - count()

ws_in <- RJDemetra::load_workspace(file = "./WS/ws_input.xml")

count(ws_in)
print(get_all_names(ws_in))

all_mp <- get_all_objects(ws_in)

mp1 <- get_object(ws_in, pos = 1)
print(get_name(mp1))
print(get_all_names(mp1))
count(mp1)

all_sa_mp1 <- get_all_objects(mp1)
get_position(mp1, name = "RF0620")
sa1_mp1 <- get_object(mp1, pos = 1)

name_sa1_mp1 <- get_name(sa1_mp1)
ts_sa1_mp1 <- get_ts(sa1_mp1)

compute(ws_in)

mod_ws <- get_model(ws_in)
mod_mp1 <- get_model(mp1, ws_in)
mod_sa1_mp1 <- get_model(sa1_mp1, ws_in)

jmod_ws <- get_jmodel(ws_in)
jmod_mp1 <- get_jmodel(mp1, ws_in)
jmod_sa1_mp1 <- get_jmodel(sa1_mp1, ws_in)

get_dictionary(jmod_sa1_mp1)
get_indicators(jmod_sa1_mp1, c("diagnostics.seas-sa-kw", "residuals.tdpeaks"))


# Fonction de modification -----------------------------------------------------

# Ici on teste la fonction :
#   - add_sa_item()

# Première chose : on met les WS à l'abri
id <- pull_out_fire("ws_output")

ws <- RJDemetra::load_workspace("WS/ws_output.xml")

sa_jx13 <- RJDemetra::jx13(ipi_c_eu[, "FR"])
sa_x13 <- RJDemetra::x13(ipi_c_eu[, "FR"])
sa_ts <- RJDemetra::jtramoseats(ipi_c_eu[, "FR"])

# Ajout d'un nouveau SA-item dans le 3ème MP
add_sa_item(
    workspace = ws,
    multiprocessing = "SAProcessing-3",
    sa_obj = sa_ts,
    name = "IPI_EU_FR"
)
# mp3 |> get_all_objects() |> length()

RJDemetra::save_workspace(ws, "./WS/ws_output.xml")

bring_back(id)
