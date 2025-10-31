library("JDCruncheR")
library("rjwsacruncher")

options(
    cruncher_bin_directory = "C:/Users/UTZK0M/Software/jdemetra-related/jwsacruncher-2.2.4/bin/"
)

# workspace_automatique: ATT TEMP for test
cruncher_and_param(
    workspace = "Issues/TODO#69/ws_ipi",
    rename_multi_documents = FALSE, # Pour renommer les dossiers en sortie : pas utile ici
    delete_existing_file = TRUE, # Pour remplacer les sorties existantes
    policy = "complete", # Politique de rafraîchissement
    csv_layout = "vtable", # Format de sortie des tables
    log_file = "Issues/TODO#69/log.txt"
)

BQ_auto <- extract_QR(
    "Issues/TODO#69/ws_ipi/Output/SAProcessing-1/demetra_m.csv"
)

str(BQ_auto)

BQ_auto_score1 <- compute_score(
    BQ_auto,
    n_contrib_score = 3,
    conditional_indicator = list(list(
        indicator = "oos_mse",
        conditions = c(
            "residuals_independency",
            "residuals_homoskedasticity",
            "residuals_normality"
        ),
        conditions_modalities = c("Bad", "Severe")
    )),
    na.rm = TRUE
)
BQ_auto_score2 <- compute_score(BQ_auto, n_contrib_score = 2)

# Extraction du score (pour visualisation)
head(extract_score(BQ_auto_score1))
head(extract_score(BQ_auto_score2))

# Je n'arrive pas à reproduire le bug...
# Si ça se représente, il est possible que ce soit le cruncher qui ai eu un pb.
