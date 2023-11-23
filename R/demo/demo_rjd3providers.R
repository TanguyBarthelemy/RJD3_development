
################################################################################
#######              Démonstration du package rjdworkspace               ####### 
################################################################################

# Chargement des packages ------------------------------------------------------

library("rjd3providers")


# Chargement fonctions utiles --------------------------------------------------

path_fct <- "./R/demo/utility/"
function2import <- list.files(path_fct, full.names = TRUE, recursive = TRUE)
sapply(X = function2import, FUN = source, encoding = "UTF-8") |> invisible()

# Change path ------------------------------------------------------------------

id <- pull_out_fire("ws_path_v3")
move_data()

path1_csv <- normalizePath("./data_temp/path_1/data_ipi.csv")
path1_xslx <- normalizePath("./data_temp/path_1/data_ipi.xlsx")








csv_file <- paste0(system.file("examples", package="rjd3providers"), "/ABS.csv")

txt_content(csv_file)
txt_data(csv_file)

xlsx_file <- paste0(system.file("examples", package="rjd3providers"), "/Insee.xlsx")
spreadsheet_content(file = xlsx_file)


ts1 <- spreadsheet_series(file = xlsx_file, sheet = 1L, series = 3L)
str(ts1)

all_ts <- spreadsheet_data(file = xlsx_file, sheet = 2L)

ts1$data









# utre fichier
xlsx_file <- paste0(system.file("examples", package="rjd3providers"), "/Insee.xlsx")
spreadsheet_content(xlsx_file)

ts1 <- spreadsheet_series(file = xlsx_file, sheet = 1L, series = 3)
str(ts1)



# Ouverture du WS
ws_path <- rjdemetra3::.jws_open(file = "./WS/ws_path_v3.xml")
rjdemetra3::.jws_sap_count(ws_path)

jsap1 <- rjdemetra3::.jws_sap(ws_path, idx = 1)
name_mp1 <- rjdemetra3::.jsap_name(jsap1)
rjdemetra3::.jsap_sa_count(jsap1)

jsa1 <- rjdemetra3::.jsap_sa(jsap1, idx = 1)

# Création du JD3_TS
rjdemetra3::get_ts(jsa1)


new_jd3_ts <- spreadsheet_series(file = xlsx_file, sheet = 1L, series = 3)
rjdemetra3::set_ts(jsap1, 2, new_jd3_ts)


rjdemetra3::save_workspace(jws = ws_path, file = "./ws/ws_path_v3.xml", replace = TRUE)



txt_change_file(id = a$moniker$id, 
                nfile = path1, 
                ofile = normalizePath("./data_temp/path_2/data_ipi.csv", mustWork = FALSE))



update_path2(ws_xml_path = "./WS/ws_path.xml", 
             raw_data_path = "./data_temp/path_2/data_ipi.csv", pos_mp = 1)
update_path2(ws_xml_path = "./WS/ws_path.xml", 
             raw_data_path = "./data_temp/path_2/data_ipi.xls", pos_mp = 2)
update_path2(ws_xml_path = "./WS/ws_path.xml", 
             raw_data_path = "./data_temp/path_2/data_ipi.xlsx", 
             pos_mp = 3, pos_sa_item = 4)

move_data()
bring_all_back()
