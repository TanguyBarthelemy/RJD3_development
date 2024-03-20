
################################################################################
#######              Démonstration du package rjdworkspace               #######
################################################################################

# Chargement des packages ------------------------------------------------------

library("rjd3providers")


# Chargement fonctions utiles --------------------------------------------------

path_fct <- "./R/demo/utility/"
function2import <- list.files(path_fct, full.names = TRUE, recursive = TRUE)
sapply(X = function2import, FUN = source, encoding = "UTF-8") |> invisible()


# Set paths --------------------------------------------------------------------

# old and new paths
path1_csv <- normalizePath("./data_temp/path_1/data_ipi.csv")
path1_xlsx <- normalizePath("./data_temp/path_1/data_ipi.xlsx")

path2_csv <- normalizePath("./data_temp/path_2/data_ipi.csv")
path2_xlsx <- normalizePath("./data_temp/path_2/data_ipi.xlsx")


# Manipulate files -------------------------------------------------------------

txt_content(file = path1_csv, delimiter = "SEMICOLON", fmt.date = "dd/MM/yyyy")
txt_data(file = path1_csv, delimiter = "SEMICOLON", fmt.date = "dd/MM/yyyy")
txt_series(file = path1_csv, series = 2L, delimiter = "SEMICOLON", fmt.date = "dd/MM/yyyy")

spreadsheet_content(file = path1_xlsx)
spreadsheet_data(file = path1_xlsx, sheet = 1L, cleanMissings = TRUE)
spreadsheet_series(file = path1_xlsx, sheet = 1L, series = 3L)


# Change path ------------------------------------------------------------------

id <- pull_out_fire("ws_path_v3")
move_data()


# Ouverture du WS
jws_path <- rjdemetra3::.jws_open(file = "./WS/ws_path_v3.xml")
rjdemetra3::.jws_sap_count(jws_path)

jsap_xlsx <- rjdemetra3::.jws_sap(jws_path, idx = 2L)
rjdemetra3::.jsap_name(jsap_xlsx)
rjdemetra3::.jsap_sa_count(jsap_xlsx)

jsai <- rjdemetra3::.jsap_sa(jsap_xlsx, idx = 1L)


# Affecter un path à un SA-Item existant ---------------------------------------

new_jd3_ts <- spreadsheet_series(file = path2_xlsx, sheet = 1L, series = 1L)
rjdemetra3::set_ts(jsap = jsap_xlsx, idx = 1L, y = new_jd3_ts)


# Modifier le path d'un SA-Item existant ---------------------------------------

old_jd3_ts <- rjdemetra3::get_ts(jsai)

## Utilisation de spreadsheet_change_file vraiment utile ??
## On a besoin du chemin formatté (par java) et on ne l'a pas

# new_id <- spreadsheet_change_file(
#     id = old_jd3_ts$metadata$`@id`,
#     nfile = path2_xlsx,
#     ofile = path1_xlsx
# )


## Utilisation et manipulation des properties
properties <- rjd3providers::spreadsheet_id_properties(old_jd3_ts$metadata$`@id`)
properties$file <- path2_xlsx
new_id <- spreadsheet_to_id(properties)

new_jd3_ts <- old_jd3_ts
new_jd3_ts$metadata$`@id` <- new_id
rjdemetra3::set_ts(jsap = jsap_xlsx, idx = 1L, y = new_jd3_ts)


## Update_massively the path
txt_update_path(
    jws = jws_object,
    new_path = path_csv,
    idx_sap = 1L
)
spreadsheet_update_path(
    jws = jws_object,
    new_path = path_xlsx,
    idx_sap = 2L
)


# Enregistrement du WS

rjdemetra3::save_workspace(jws = jws_path, file = "./temp/ws_path_v3.xml", replace = TRUE)

move_data()
empty_temp()
bring_all_back()





# En plus ----------------------------------------------------------------------

set_spreadsheet_paths(paths = path2_xlsx)
