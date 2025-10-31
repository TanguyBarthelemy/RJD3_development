library("rjwsacruncher")
library("dplyr")


# V2 ----------------------------------------------------------------------

cruncher_bin_directory_v2 <- "C:/Users/UTZK0M/Software/jdemetra-related/jwsacruncher-2.2.6/bin/"
options(
    cruncher_bin_directory = cruncher_bin_directory_v2,
    is_cruncher_v3 = FALSE,
    default_matrix_item = c(
        "span.start",
        "span.end",
        "span.n",
        "arima",
        "arima.mean",
        "arima.p",
        "arima.d",
        "arima.q",
        "arima.bp",
        "arima.bd",
        "arima.bq",
        "m-statistics.m7",
        "m-statistics.q",
        "m-statistics.q-m2",
        "diagnostics.out-of-sample.mean:2",
        "diagnostics.out-of-sample.mse:2",
        "regression.nout",
        "residuals.kurtosis:3",
        "residuals.skewness:3",
        "residuals.lb2:3",
        "diagnostics.seas-sa-qs",
        "diagnostics.seas-sa-f",
        "diagnostics.seas-i-qs",
        "diagnostics.seas-i-f",
        "diagnostics.td-sa-last",
        "diagnostics.td-i-last",
        "diagnostics.regarima residuals.independence:2",
        "diagnostics.regarima residuals.normality:2",
        "residuals.dh:3",
        "residuals.lb:3",
        "residuals.doornikhansen:3"
    )
)

rjwsacruncher::cruncher_and_param(
    workspace = "Issues/difference_v2_v3/v2.xml",
    rename_multi_documents = TRUE, # Pour renommer les dossiers en sortie
    delete_existing_file = TRUE, # Pour remplacer les sorties existantes
    policy = "complete", # Politique de rafraichissement
    csv_layout = "vtable", # Format de sortie des tables
    log_file = "iss541.log"
)

demetra_m_v2 <- read.csv(
    file = "C:\\Users\\UTZK0M\\Documents\\Projets R\\Projets MTS\\Packages\\rjduniverse\\test\\RJD3_development\\Issues\\difference_v2_v3\\v2\\Output\\SAProcessing-1\\demetra_m.csv",
    sep = ";",
    dec = ",",
    stringsAsFactors = FALSE,
    na.strings = c("NA", "?"),
    fileEncoding = "latin1",
    quote = "",
    check.names = TRUE
) |>
    arrange(X)


# V3 ----------------------------------------------------------------------

cruncher_bin_directory_v3 <- "C:/Users/UTZK0M/Software/jdemetra-related/jwsacruncher-3.5.1/bin/"
options(
    cruncher_bin_directory = cruncher_bin_directory_v3,
    is_cruncher_v3 = TRUE,
    default_matrix_item = c(
        "span.start",
        "span.end",
        "span.n",
        "arima.p",
        "arima.d",
        "arima.q",
        "arima.bp",
        "arima.bd",
        "arima.bq",
        "m-statistics.m7",
        "m-statistics.q",
        "m-statistics.q-m2",
        "regression.nout",
        "diagnostics.seas-sa-qs:2",
        "diagnostics.seas-sa-f:2",
        "diagnostics.seas-i-qs:2",
        "diagnostics.seas-i-f:2",
        "diagnostics.td-sa-last:2",
        "diagnostics.td-i-last:2",
        "residuals.doornikhansen:3",
        "residuals.kurtosis:3",
        "residuals.skewness:3",
        "residuals.lb:3",
        "residuals.lb2:3",
        "diagnostics.fcast-outsample-mean:2",
        "diagnostics.fcast-outsample-variance:2"
    )
)

rjwsacruncher::cruncher_and_param(
    workspace = "C:\\Users\\UTZK0M\\Documents\\Projets R\\Projets MTS\\Packages\\rjduniverse\\test\\RJD3_development\\Issues\\difference_v2_v3\\v3.xml",
    rename_multi_documents = TRUE, # Pour renommer les dossiers en sortie
    delete_existing_file = TRUE, # Pour remplacer les sorties existantes
    policy = "complete", # Politique de rafraichissement
    csv_layout = "vtable", # Format de sortie des tables
    short_column_headers = FALSE,
    log_file = "iss541.log"
)

file_demetra <- "C:\\Users\\UTZK0M\\Documents\\Projets R\\Projets MTS\\Packages\\rjduniverse\\test\\RJD3_development\\Issues\\difference_v2_v3\\v3\\Output\\SAProcessing-1\\demetra_m.csv"

demetra_m_v3 <- read.csv(
    file = file_demetra,
    sep = ";",
    dec = ",",
    stringsAsFactors = FALSE,
    na.strings = c("NA", "?"),
    fileEncoding = "latin1",
    quote = "",
    check.names = TRUE
)


file.copy(
    file_demetra,
    "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/v3/JDCruncheR/tests/testthat/data/demetra_m3.csv",
    overwrite = TRUE
)
