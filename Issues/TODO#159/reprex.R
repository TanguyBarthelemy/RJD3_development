# Préparation des données --------------------------------------------

x <- TractorTsbox:::ts2df(AirPassengers)
x$date <- seq(from = as.Date("1949-01-01"), length.out = 144, by = "month")
write.csv(x, file = "./Issues/TODO#159/airpassengers.csv", row.names = FALSE)


# Lecture des outputs ------------------------------

comparaison_GUI <- readxl::read_excel("~/../Desktop/testing_output/Difference_output_GUI_v2_v3.xlsx")
names_in_file <- comparaison_GUI$RJDemetra
names_in_file <- names_in_file[!is.na(names_in_file)]


# Coup de cruncher ------------------------------

options(
    cruncher_bin_directory = "C:/Users/UTZK0M/Software/jdemetra-related/jwsacruncher-2.2.4/bin/",
    v3 = FALSE,
    default_matrix_item = (comparaison_GUI |> subset(select = `cruncher V2`))[, 1, drop = TRUE]
)

path_ws <- "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#159/ws_airpassenger"

demetra_m_GUI <- read.csv(
    file = file.path(path_ws, "SAProcessing-1/demetra_m.csv"),
    sep = ";",
    dec = ",",
    stringsAsFactors = FALSE,
    na.strings = c("NA", "?"),
    fileEncoding = "latin1",
    quote = ""
)

rjwsacruncher::cruncher_and_param(
    workspace = paste0(path_ws, ".xml"),
    rename_multi_documents = TRUE, # Pour renommer les dossiers en sortie
    delete_existing_file = TRUE, # Pour remplacer les sorties existantes
    policy = "complete", # Politique de rafraichissement
    csv_layout = "vtable", # Format de sortie des tables
    log_file = "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#159/log.txt"
)
demetra_m_cruncher <- read.csv(
    file = file.path(path_ws, "Output/SAProcessing-1/demetra_m.csv"),
    sep = ";",
    dec = ",",
    stringsAsFactors = FALSE,
    na.strings = c("NA", "?"),
    fileEncoding = "latin1",
    quote = ""
)


# Comparaison R v2 --------------------------------------------

ud_var <- comparaison_GUI |>
    subset(is.na(RJDemetra)) |>
    do.call(what = c) |>
    unique() |>
    sort()
mod <- RJDemetra::tramoseats(AirPassengers,
    spec = "RSAfull",
    userdefined = user_defined_variables("TRAMO-SEATS")
)

# Extraction des noms non-ts = diagnostiques
name_to_place <- setdiff(user_defined_variables("TRAMO-SEATS"), names_in_file)
# setdiff(names(mod$user_defined), names_in_file)


# name_to_place <- name_to_place[substr(name_to_place, 1, 13) != "decomposition"]
for (nam in name_to_place) {
    obj <- mod$user_defined[[nam]]
    if (is.ts(obj)) {
        name_to_place <- setdiff(name_to_place, nam)
    }
}


# Appariement -------------------------------------------------------

for (nam in name_to_place) {
    cat("\nName from RJDemetra:", nam, "\n")
    obj <- mod$user_defined[[nam]]
    print(obj)
    concurrent <- NULL
    for (nam2 in colnames(demetra_m_cruncher)) {
        val <- demetra_m_cruncher[1, nam2]
        if (isTRUE(any(sapply(obj, all.equal, val, tolerance = 0.0001)))) {
            cat("Equivalence avec", nam2, "?\n")
            print(val)
            concurrent <- c(concurrent, nam2)
        }
    }
    if (length(concurrent) > 0) {
        readline("Ok?")
    }
}

decomposition.model.ar
decomposition.tmodel.ar
X.39
X.40
