
# Chargement package -----------------------------------------------------------

library("rjd3x13")
library("rjd3workspace")
library("rjd3providers")


# Paramètre --------------------------------------------------------------------

data_dir <- "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_forked_suite/rjd3workspace/inst/data/"


# Initialisation d'un workspace ------------------------------------------------

jws <- jws_new()
jsap <- jws_sap_new(jws, "test")

add_sa_item(jsap, name = "sai1", x = AirPassengers, spec = x13_spec())
add_sa_item(jsap, name = "sai2", x = mdeaths, spec = x13_spec())
add_sa_item(jsap, name = "sai3", x = fdeaths, spec = x13_spec())

# write ws, visible in GUI, pas de meta data: pas de refresh...

### Create TS meta data
# functions in rjd3providers
ts_object1 <- txt_series(
    file = normalizePath(file.path(data_dir, "IPI_nace4.csv")),
    series = 1L,
    delimiter = "SEMICOLON",
    fmt.date = "dd/MM/yyyy", # pkoi pas de clean missing
)

ts_object2 <- spreadsheet_series(
    file = normalizePath(file.path(data_dir, "Ipi_nace4.xlsx")),
    sheet = 1L,
    series = 2L,
    period = 12L,
    fullName = TRUE,
    cleanMissings = TRUE
)


ts_object3 <- txt_series(
    file = normalizePath(file.path(data_dir, "IPI_nace4.csv")),
    series = 3L,
    delimiter = "SEMICOLON",
    fmt.date = "dd/MM/yyyy"
)
############# SET TS metatadata
# pour set TS les meta data du ts doivent etre complets
set_ts(jsap = jsap, idx = 1L, ts_object1)
set_ts(jsap = jsap, idx = 2L, ts_object2) # attention: meta data supplementaires excel ...pas grave ?
set_ts(jsap = jsap, idx = 3L, ts_object3)

# get_ts: tout à la fois

# put_ts: on peut changer 1 seul champ à la fois

### priority: pas un meta data : un simple champ du SAI
set_priority(jsap, idx = 1, priority = 1L)
set_priority(jsap, idx = 2, priority = 20L)
set_priority(jsap, idx = 3, priority = 7L)

# add to rjd3workspace: changement ou ajout d'une meta data pour le sai item (hors TS)
put_metadata <- function (jsap, idx, key, value) {
    jsai <- jsap_sai(jsap, idx = idx)

    meta <- .jcall(jsai, "Ljava/util/Map;", "getMeta")
    new_meta <- .jnew("java/util/HashMap", meta)

    jkey <- .jnew("java/lang/String", key)
    jvalue <- .jnew("java/lang/String", value)

    .jcall(
        obj = new_meta,
        returnSig = "Ljava/lang/Object;",
        method = "put",
        .jcast(jkey, "java/lang/Object"),
        .jcast(jvalue, "java/lang/Object")
    )

    jsai <- .jcall(
        obj = jsai,
        returnSig = "Ljdplus/sa/base/api/SaItem;",
        method = "withInformations",
        .jcast(new_meta, "java/util/Map")
    )

    replace_sa_item(jsap, jsai = jsai, idx = idx)
}
library(rJava)

put_metadata(jsap = jsap, idx = 1L, key = "color", value = "blue")

set_comment(jsap, 1L, comment = "Good series")
set_comment(jsap, 2L, comment = "Bad series")
set_comment(jsap, 3L, comment = "Weird series")

save_workspace(jws, "ws_meta_data.xml", replace = TRUE)


# Manipulation Workspace -------------------------------------------------------

jws_r <- jws_open(normalizePath("ws_meta_data.xml"))
jsap_r <- jws_sap(jws_r, 1L)
jsai_r <- jsap_sai(jsap_r, 1L)

get_priority(jsai_r)

get_comment(jsai_r)
.jsai_metadata(jsai_r, "comment")
.jsai_metadata(jsai_r, "color") # ajouté avec put_metadata, pas de fonction retrieve
# renommer en sai et pas jsai (laisser en . tant que que comment en meta data)

get_ts(jsai_r)

# pour le ts
.jsai_ts_metadata(jsai_r, "@id") # modifiable avec providers
.jsai_ts_metadata(jsai_r, "@source")
.jsai_ts_metadata(jsai_r, "@timestamp")
# renommer en get ts meta data
get_raw_data(jsai_r)


# Mise à jour du chemin --------------------------------------------------------

# from csv to csv (cf sai1): meme structure (sinon changer formats, séparateurs): faire un exemple
getwd()
txt_update_path(
    jws_r, idx_sap = 1L, idx_sai = 1L,
    new_path = normalizePath(file.path("Data/IPI_nace4.csv"))
)
# xls même noms
spreadsheet_update_path(
    jws_r, idx_sap = 1L, idx_sai = 2L,
    new_path = normalizePath(file.path("Data/ipi_nace4_ind.xlsx"))
)


# Changement de providers : de csv à xls (et reverse) -------------------------------------------

new_ts_object3 <- spreadsheet_series(
    file = normalizePath(file.path(data_dir, "world.xlsx")),
    sheet = 1L,
    series = 3L,
    period = 12L,
    fullName = TRUE,
    cleanMissings = TRUE
)

set_ts(jsap_r, 3L, new_ts_object3) # on remplace le ts object dans le sai item 3
# remplace aussi les raw data : garder les mêmes ....

save_workspace(jws_r, "ws2.xml", replace = TRUE)

