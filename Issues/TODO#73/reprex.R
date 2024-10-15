
################################################################################
#######               Démonstration du package JDCruncheR                #######
################################################################################

# Chargement des packages ------------------------------------------------------

library("JDCruncheR")
library("rjwsacruncher")

# Fonction de cruncher' --------------------------------

options(cruncher_bin_directory = "C:/Users/UTZK0M/Software/jdemetra-related/jwsacruncher-2.2.4/bin/")

path_ws <- "C:\\Users\\UTZK0M\\Documents\\Projets R\\Projets MTS\\Packages\\rjduniverse\\test\\RJD3_development\\Issues\\TODO#73\\ws_tramo"

rjwsacruncher::cruncher_and_param(
    workspace = paste0(path_ws, ".xml"),
    rename_multi_documents = TRUE, # Pour renommer les dossiers en sortie
    delete_existing_file = TRUE, # Pour remplacer les sorties existantes
    policy = "complete", # Politique de rafraichissement
    csv_layout = "vtable", # Format de sortie des tables
    log_file = "./Issues/TODO#73/log.txt"
)

# Error !
QR_auto <- JDCruncheR::extract_QR(matrix_output_file = paste0(path_ws, "/Output/SAProcessing-1/demetra_m.csv"))

### Etude de JDCruncheR::extract_QR --------------------------------------------

matrix_output_file = paste0(path_ws, "/Output/SAProcessing-1/demetra_m.csv")
demetra_m <- read.csv(
    file = matrix_output_file,
    sep = ";",
    dec = ",",
    stringsAsFactors = FALSE,
    na.strings = c("NA", "?"),
    fileEncoding = "latin1",
    quote = ""
)

demetra_m$series <- gsub(
    "(^ *)|(* $)", "",
    gsub("(^.* \\* )|(\\[frozen\\])", "", demetra_m[, 1])
)
demetra_m$frequency <- JDCruncheR:::extractFrequency(demetra_m)

demetra_m <- cbind(
    demetra_m,
    JDCruncheR:::extractARIMA(demetra_m),
    JDCruncheR:::extractStatQ(demetra_m), ## Erreur
    JDCruncheR:::extractOOS_test(demetra_m),
    JDCruncheR:::extractNormalityTests(demetra_m),
    JDCruncheR:::extractOutliers(demetra_m) ## Erreur
)


### Etude de JDCruncheR::extractStatQ ------------------------------------------

col_q <- q_possibles <- grep("(^q$)|(^q\\.\\d$)", colnames(demetra_m))
col_q_m2 <- q_m2_possibles <- grep("(^q\\.m2$)|(^q\\.m2\\.\\d$)", colnames(demetra_m))

if (length(q_possibles) > 1) {
    col_q_possibles <- demetra_m[, q_possibles]
    NA_cols <- which(unlist(lapply(
        X = col_q_possibles,
        FUN = \(x) all(is.na(x))
    )))
    non_character_non_integer_non_na_cols <- which(unlist(lapply(
        X = col_q_possibles,
        FUN = \(x) !all(is.integer(x) | is.character(x) | is.na(x))
    )))

    if (length(non_character_non_integer_non_na_cols) > 1) {
        stop("Error in the extraction of the Q stats: multiple colum found")
    } else if (length(non_character_non_integer_non_na_cols) == 1) {
        col_q <- q_possibles[non_character_non_integer_non_na_cols]
    } else if (length(NA_cols) > 0) {
        col_q <- q_possibles[col_all_NA[1]]
    } else {
        stop("Error in the extraction of the Q stats")
    }
}

if (length(q_m2_possibles) > 1) {
    col_q_m2_possibles <- demetra_m[, q_m2_possibles]
    NA_cols <- which(unlist(lapply(
        X = col_q_m2_possibles,
        FUN = \(x) all(is.na(x))
    )))
    non_character_non_integer_non_na_cols <- which(unlist(lapply(
        X = col_q_m2_possibles,
        FUN = \(x) !all(is.integer(x) | is.character(x) | is.na(x))
    )))

    if (length(non_character_non_integer_non_na_cols) > 1) {
        stop("Error in the extraction of the Q stats: multiple colum found")
    } else if (length(non_character_non_integer_non_na_cols) == 1) {
        col_q_m2 <- q_m2_possibles[non_character_non_integer_non_na_cols]
    } else if (length(NA_cols) > 0) {
        col_q_m2 <- q_m2_possibles[col_all_NA[1]]
    } else {
        stop("Error in the extraction of the Q-M2 stats")
    }
}

stat_Q <- data.frame(
    q_modality = cut(
        x = as.numeric(demetra_m[, col_q]),
        breaks = c(-Inf, thresholds[["q"]]),
        labels = names(thresholds[["q"]]),
        right = FALSE,
        include.lowest = TRUE,
        ordered_result = TRUE
    ),
    q_value = demetra_m[, col_q],
    q_m2_modality = cut(
        x = as.numeric(demetra_m[, col_q_m2]),
        breaks = c(-Inf, thresholds[["q_m2"]]),
        labels = names(thresholds[["q_m2"]]),
        right = FALSE,
        include.lowest = TRUE,
        ordered_result = TRUE
    ),
    q_m2_value = demetra_m[, col_q_m2],
    stringsAsFactors = FALSE
)


## Autres trucs (quand c'est x13 et tramo) -------------------------------------

QR_auto_score <- JDCruncheR::compute_score(QR_auto)
scores <- JDCruncheR::extract_score(QR_auto_score)


# Solution --------------------

# Changement de formule pour tramo
QR_auto |>
    JDCruncheR::compute_score(
        score_pond = c(qs_residual_sa_on_sa = 30,
                       f_residual_sa_on_sa = 30,
                       qs_residual_sa_on_i = 20,
                       f_residual_sa_on_i = 20,
                       f_residual_td_on_sa = 30,
                       f_residual_td_on_i = 20,
                       oos_mean = 15,
                       oos_mse = 10,
                       residuals_independency = 15,
                       residuals_homoskedasticity = 5,
                       residuals_skewness = 5)
    ) |>
    JDCruncheR::extract_score()
