
# Chargement package -----------------------------------------------------------

library("rjd3x13")
library("rjd3workspace")
library("rjd3providers")


# Paramètre --------------------------------------------------------------------

data_dir <- "C:\\Users\\UTZK0M\\Desktop\\testing_output\\data"


# Initialisation d'un workspace ------------------------------------------------

jws <- jws_new()
jsap <- jws_sap_new(jws, "test")

add_sa_item(jsap, name = "sai1", x = AirPassengers, spec = x13_spec())
add_sa_item(jsap, name = "sai2", x = mdeaths, spec = x13_spec())
add_sa_item(jsap, name = "sai3", x = fdeaths, spec = x13_spec())

ts_object1 <- txt_series(
    file = normalizePath(file.path(data_dir, "world.csv")),
    series = 1L,
    delimiter = "SEMICOLON",
    fmt.date = "dd/MM/yyyy"
)

ts_object2 <- spreadsheet_series(
    file = normalizePath(file.path(data_dir, "world.xlsx")),
    sheet = 1L,
    series = 2L,
    period = 12L,
    fullName = TRUE,
    cleanMissings = TRUE
)

ts_object3 <- txt_series(
    file = normalizePath(file.path(data_dir, "world.csv")),
    series = 3L,
    delimiter = "SEMICOLON",
    fmt.date = "dd/MM/yyyy"
)

set_ts(jsap = jsap, idx = 1L, ts_object1)
set_ts(jsap = jsap, idx = 2L, ts_object2)
set_ts(jsap = jsap, idx = 3L, ts_object3)

set_priority(jsap, idx = 1, priority = 1L)
set_priority(jsap, idx = 2, priority = 20L)
set_priority(jsap, idx = 3, priority = 7L)

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
put_metadata(jsap = jsap, idx = 1L, key = "color", value = "blue")

set_comment(jsap, 1L, comment = "Good series")
set_comment(jsap, 2L, comment = "Bad series")
set_comment(jsap, 3L, comment = "Weird series")

save_workspace(jws, "ws.xml", replace = TRUE)


# Manipulation Workspace -------------------------------------------------------

jws_r <- jws_open(normalizePath("ws.xml"))
jsap_r <- jws_sap(jws_r, 1L)
jsai_r <- jsap_sai(jsap_r, 1L)

get_priority(jsai_r)

get_comment(jsai_r)
.jsai_metadata(jsai_r, "comment")
.jsai_metadata(jsai_r, "color")
.jsai_metadata(jsai_r, "ocean")

get_ts(jsai_r)

.jsai_ts_metadata(jsai_r, "@id")
.jsai_ts_metadata(jsai_r, "@source")
.jsai_ts_metadata(jsai_r, "@timestamp")

get_raw_data(jsai_r)


# Mise à jour du chemin --------------------------------------------------------

txt_update_path(
    jws_r, idx_sap = 1L, idx_sai = 1L,
    new_path = normalizePath(file.path(data_dir, "world2.csv"))
)
spreadsheet_update_path(
    jws_r, idx_sap = 1L, idx_sai = 2L,
    new_path = normalizePath(file.path(data_dir, "world2.xlsx"))
)


# Changement de providers ------------------------------------------------------

new_ts_object3 <- spreadsheet_series(
    file = normalizePath(file.path(data_dir, "world.xlsx")),
    sheet = 1L,
    series = 3L,
    period = 12L,
    fullName = TRUE,
    cleanMissings = TRUE
)

set_ts(jsap_r, 3L, new_ts_object3)

save_workspace(jws_r, "ws2.xml", replace = TRUE)

