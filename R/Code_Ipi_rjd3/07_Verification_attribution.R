################################################################################
#                                                                              #
#                  Programme de vérification des attributions                  #
#                                                                              #
################################################################################

# Chargement des packages R ----------------------------------------------------

library("RJDemetra")


# Déclaration fonctions --------------------------------------------------------

source("./R/utilities.R")


# Vérification régresseurs CJO -------------------------------------------------

ws_auto <- load_workspace("./workspace_automatique/industrie.xml")
compute(ws_auto)

sap_auto <- ws_auto |> get_object()
all_series_name <- get_all_names(sap_auto)

choix_cjo <- read.csv("./choix_cjo.csv", sep = ";")

for (k in seq_along(all_series_name)) {
    series_name <- all_series_name[k]
    cat(
        paste0(
            "Série ",
            series_name,
            " en cours... ",
            k,
            "/",
            length(all_series_name)
        ),
        "\n"
    )

    regs_auto <- sap_auto |>
        get_object(which(get_all_names(sap_auto) == series_name)) |>
        get_model(workspace = ws_auto) |>
        get_cjo_regressor()
    selected_reg <- choix_cjo[
        choix_cjo$series == series_name,
        "selected_N_outliers"
    ]

    if (regs_auto != selected_reg) {
        stop("Les regresseurs ne sont pas bons !")
    }
}


# Vérification outliers --------------------------------------------------------

## Chargement WS ---------------------------------------------------------------

ws_ref <- load_workspace("./workspace_ref/industrie.xml")
compute(ws_ref)
sap_ref <- ws_ref |> get_object()

for (k in seq_along(all_series_name)) {
    series_name <- all_series_name[k]
    cat(
        paste0(
            "Série ",
            series_name,
            " en cours... ",
            k,
            "/",
            length(all_series_name)
        ),
        "\n"
    )

    outliers_auto <- sap_auto |>
        get_object(which(get_all_names(sap_auto) == series_name)) |>
        get_model(workspace = ws_auto) |>
        get_outliers()
    outliers_ref <- sap_ref |>
        get_object(which(get_all_names(sap_ref) == series_name)) |>
        get_model(workspace = ws_ref) |>
        get_outliers()

    if (
        any(!outliers_ref$type %in% outliers_auto$type) ||
            any(!outliers_ref$date %in% outliers_auto$date)
    ) {
        stop("Les outliers ne sont pas les mêmes !")
    }
}
