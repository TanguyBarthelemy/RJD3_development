comparaison_GUI <- readxl::read_excel(
    "~/../Desktop/testing_output/Difference_output_GUI_v2_v3.xlsx"
)

## En V2 (d'abord) -------------------------------------------------------------

input_cruncher_v2 <- comparaison_GUI$`cruncher V2`
input_cruncher_v2 <- input_cruncher_v2[!is.na(input_cruncher_v2)]

list_all_output_v2 <- list()

path_ws <- "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#67/ws_V2"
path_ws <- "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#159/ws_airpassenger"


cat("\nIn V2...\n")
for (input in input_cruncher_v2) {
    print(input)

    options(
        cruncher_bin_directory = "C:/Users/UTZK0M/Software/jdemetra-related/jwsacruncher-2.2.4/bin/",
        v3 = FALSE,
        default_matrix_item = input
    )

    rjwsacruncher::cruncher_and_param(
        workspace = paste0(path_ws, ".xml"),
        rename_multi_documents = TRUE, # Pour renommer les dossiers en sortie
        delete_existing_file = TRUE, # Pour remplacer les sorties existantes
        policy = "complete", # Politique de rafraichissement
        csv_layout = "vtable", # Format de sortie des tables
        log_file = "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#67/log.txt"
    )

    demetra_m_cruncher <- read.csv(
        file = file.path(path_ws, "Output/SAProcessing-1/demetra_m.csv"),
        sep = ";",
        dec = ",",
        stringsAsFactors = FALSE,
        na.strings = c("NA", "?"),
        fileEncoding = "latin1",
        quote = "",
        check.names = FALSE
    )

    print(colnames(demetra_m_cruncher))
    new_item <- list(colnames(demetra_m_cruncher))
    names(new_item) <- input

    list_all_output_v2 <- c(list_all_output_v2, new_item)
}


df <- data.frame()
for (k in seq_along(list_all_output_v2)) {
    mini_df <- cbind(
        names(list_all_output_v2)[k],
        setdiff(list_all_output_v2[[k]], "X")
    )
    df <- rbind(df, mini_df)
}


## En V3 (Ensuite) -------------------------------------------------------------

input_cruncher_v3 <- comparaison_GUI$`cruncher V3`
input_cruncher_v3 <- input_cruncher_v3[!is.na(input_cruncher_v3)]

list_all_output_v3 <- list()

path_ws <- "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#159/ws_tram_x13_v3"

cat("\nIn V3...\n")
for (input in input_cruncher_v3) {
    print(input)

    options(
        cruncher_bin_directory = "C:/Users/UTZK0M/Software/jdemetra-related/jwsacruncher-3.2.4/bin/",
        v3 = TRUE,
        default_matrix_item = input
    )

    rjwsacruncher::cruncher_and_param(
        workspace = paste0(path_ws, ".xml"),
        rename_multi_documents = TRUE, # Pour renommer les dossiers en sortie
        delete_existing_file = TRUE, # Pour remplacer les sorties existantes
        policy = "complete", # Politique de rafraichissement
        csv_layout = "vtable", # Format de sortie des tables
        log_file = "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#67/log.txt"
    )

    demetra_m_cruncher <- read.csv(
        file = file.path(path_ws, "Output/SAProcessing-1/demetra_m.csv"),
        sep = ";",
        dec = ",",
        stringsAsFactors = FALSE,
        na.strings = c("NA", "?"),
        fileEncoding = "latin1",
        check.names = FALSE,
        quote = ""
    )

    print(colnames(demetra_m_cruncher))
    new_item <- list(colnames(demetra_m_cruncher))
    names(new_item) <- input

    list_all_output_v3 <- c(list_all_output_v3, new_item)
}

df3 <- data.frame()
for (k in seq_along(list_all_output_v3)) {
    mini_df <- cbind(
        names(list_all_output_v3)[k],
        setdiff(list_all_output_v3[[k]], "X")
    )
    df3 <- rbind(df3, mini_df)
}


# merge --------------------------------

colnames(df2) <- c("input V2", "output V2")
colnames(df3) <- c("input V3", "output V3")
new_comp <- merge(
    comparaison_GUI,
    df2,
    by.x = "cruncher V2",
    by.y = "input V2",
    all = TRUE
)
new_comp <- merge(
    new_comp,
    df3,
    by.x = "cruncher V3",
    by.y = "input V3",
    all = TRUE
)
new_comp <- new_comp |>
    rename(
        "cruncher V3 input" = "cruncher V3",
        "cruncher V2 input" = "cruncher V2",
        "cruncher V3 output" = "output V3",
        "cruncher V2 output" = "output V2",
    ) |>
    select(
        "Type",
        "cruncher V2 input",
        "cruncher V2 output",
        "cruncher V3 input",
        "cruncher V3 output",
        "GUI output V2",
        "GUI output V3",
        "RJDemetra"
    )

write.table(
    new_comp,
    file = "~/../Desktop/test.csv",
    row.names = FALSE,
    quote = FALSE,
    sep = ";",
    na = ""
)
