### A quels output a t-on accès en v2 et en v3 ?

source(
    "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#67/R/parameters.R"
)
comparaison_GUI <- readxl::read_excel(
    "~/../Desktop/testing_output/Difference_output_GUI_v2_v3.xlsx"
)

# Les questions qu'on se pose:
#   - Quelle est la liste exacte des output que l'on peut obtenir en v2 / v3 / avec GUI / avec cruncher ?
#   - Est ce que ces listes sont identiques ?

# But définir le périmètre actuel pour :
#   - Créer des issues si il manque des éléments d'une liste à l'autre
#   - Alimenter les paramètres par défault de rjwsacruncher et en discuter avec Alain (comment avoir différents défault selon v2 et v3 ?
#       - piste : initialiser à chaque cruncher le default matrix item (mais risque de supprimer une option de l'utilisateur)
#       - piste : ne pas toucher à ce que met l'utilisateur (à part defaut à l'ouverture du package) mettre un warning / message si un des outputs n'est pas dans la liste.
#   - mettre à jour JDC avec les outputs disponibles

## Usefull functions -----------------------------------------------------------

get_empty_col <- function(x) {
    x |>
        apply(MARGIN = 2, FUN = \(.x) all(is.na(.x))) |>
        which() |>
        names()
}
get_not_empty_col <- function(x) {
    x |>
        apply(MARGIN = 2, FUN = \(.x) any(!is.na(.x))) |>
        which() |>
        names()
}

### V3 -------------------------------------------------------------------------

### on fait des tests avec différentes compositions d'output

# on travaille avec le ws_V3_bis

options(
    cruncher_bin_directory = "C:/Users/UTZK0M/Software/jdemetra-related/jwsacruncher-3.2.4/bin/",
    v3 = TRUE,
    default_matrix_item = subset(
        comparaison_GUI,
        !is.na(`cruncher V3`)
    )$`cruncher V3`
)

path_ws <- "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#67/ws_V3"

rjwsacruncher::cruncher_and_param(
    workspace = paste0(path_ws, ".xml"),
    rename_multi_documents = TRUE, # Pour renommer les dossiers en sortie
    delete_existing_file = TRUE, # Pour remplacer les sorties existantes
    policy = "complete", # Politique de rafraichissement
    csv_layout = "vtable", # Format de sortie des tables
    log_file = "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#67/log.txt"
)

### V2 -------------------------------------------------------------------------

options(
    cruncher_bin_directory = "C:/Users/UTZK0M/Software/jdemetra-related/jwsacruncher-2.2.4/bin/",
    v3 = FALSE,
    default_matrix_item = subset(
        comparaison_GUI,
        !is.na(`cruncher V2`)
    )$`cruncher V2`
)

path_ws <- "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#67/ws_V2"

rjwsacruncher::cruncher_and_param(
    workspace = paste0(path_ws, ".xml"),
    rename_multi_documents = TRUE, # Pour renommer les dossiers en sortie
    delete_existing_file = TRUE, # Pour remplacer les sorties existantes
    policy = "complete", # Politique de rafraichissement
    csv_layout = "vtable", # Format de sortie des tables
    log_file = "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#67/log.txt"
)

### Matrice demetra_m ----------------------------------------------------------

demetra_m <- read.csv(
    file = file.path(path_ws, "Output/SAProcessing-1/demetra_m.csv"),
    sep = ";",
    dec = ",",
    stringsAsFactors = FALSE,
    na.strings = c("NA", "?"),
    fileEncoding = "latin1",
    quote = ""
)

get_empty_col(demetra_m)
get_not_empty_col(demetra_m)

## Résultats -------------------------------------------------------------------

### en V3

# Le tableau est rempli !
