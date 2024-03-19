################################################################################
#######                        Wrangling WS in V3                        #######
################################################################################

# Objectifs :
#   - instant reading (load_workspace --> ça c'est fait)
#   - creation + saving
#   - modification = vrai wrangling (add sa item, replace_sa_item...)
#   - update_path
#   - handling metadata (comments)
#   - handling several workspaces (transfer series, ...)

# Loading packages -------------------------------------------------------------

library("rjdemetra3")


# Loading useful functions  ----------------------------------------------------

path_fct <- "./R/demo/utility/"
function2import <- list.files(path_fct, full.names = TRUE, recursive = TRUE)
sapply(X = function2import, FUN = source, encoding = "UTF-8") |> invisible()

if (!dir.exists("temp")) {
    dir.create("temp")
}


# Workspace creation -----------------------------------------------------------

new_jws <- .jws_new()
new_jsap <- .jws_sap_new(jws = new_jws, name = "SAP-1")


# Workspace saving -------------------------------------------------------------

save_workspace(jws = new_jws, file = "./temp/new_ws.xml")


# Loading ----------------------------------------------------------------------

## Workspace loading -----------------------------------------------------------

# Get the Java object
jws_from <- .jws_open(file = "WS/ws_input.xml")

# Get the readable object
read_workspace(jws = jws_from, compute = TRUE)


## SA-Processing loading -------------------------------------------------------

# Get the Java object
jsap_from <- .jws_sap(jws = jws_from, idx = 1L)

# Get the readable object
read_sap(jsap = jsap_from)


## SA-Item loading -------------------------------------------------------------

# Get the Java object
jsai_from <- .jsap_sa(jsap_from, idx = 1)

# Get the readable object
.jsa_read(jsai_from)


# Workspace wrangling ----------------------------------------------------------

## add_sa_item() ---------------------------------------------------------------

# add a SA-Item created with R
sa_x13 <- rjd3x13::x13(rjd3toolkit::ABS[, 1])
sa_ts <- rjd3tramoseats::tramoseats(rjd3toolkit::ABS[, 2])

add_sa_item(jsap = new_jsap, name = "ABS_1", x = sa_x13)
add_sa_item(jsap = new_jsap, name = "ABS_2", x = sa_ts)


# add a raw series with a spec created in R
add_sa_item(
    jsap = new_jsap,
    name = "ABS_3",
    x = rjd3toolkit::ABS[, 3],
    spec = rjd3x13::x13_spec(name = "RSA5c")
)
add_sa_item(
    jsap = new_jsap,
    name = "ABS_4",
    x = rjd3toolkit::ABS[, 4],
    spec = rjd3tramoseats::tramoseats_spec(name = "rsafull")
)


# add a SA_Item from another workspace
add_sa_item(jsap = new_jsap, name = "ABS_4", x = jsai_from)


## remove_sa_item() ------------------------------------------------------------

remove_sa_item(jsap = jsap_from, idx = 5L)
remove_sa_item(jsap = jsap_from, idx = 6L)


## remove_all_sa_item() --------------------------------------------------------

remove_all_sa_item(jsap = jsap_from)


# Wrangling 2 workspaces -------------------------------------------------------

## replace_sa_item() -----------------------------------------------------------

replace_sa_item(
    jsap = new_jsap,
    jsa = jsai_from,
    idx = 1L
)


## transfer_series() -----------------------------------------------------------

transfer_series(
    jsap_from = jsap_from,
    jsap_to = new_jsap,
    selected_series = c("RF0899", "RF1039", "RF1041")
)


# Initialising a project: update_path ------------------------------------------

new_jd3_ts <- spreadsheet_series(file = xlsx_file, sheet = 1L,
                                 series = 3)

# open an existing WS
ws <- .jws_open(file = some_ws_path)
# open an existing SAP
jsap <- .jws_sap(ws, idx = 1L)

# Change the TS object with the new moniker
set_ts(jsap = jsap, idx = 1, y = new_jd3_ts)
save_workspace(jws = ws_path, file = "./WS/WS_path_V3.xml",
               replace = TRUE)
