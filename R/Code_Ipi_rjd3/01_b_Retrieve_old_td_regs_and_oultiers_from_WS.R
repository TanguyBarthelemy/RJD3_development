################################################################################
#                                                                              #
#  Retrieve old calendar regressors and outliers from previous Workspac        #
#                                                                              #
################################################################################

## obj here: to v3

# Chargement des packages R ----------------------------------------------------

library("RJDemetra")


# Load WS       ----------------------------------------------------------------

ws_ref <- load_workspace("Data/Ipi_rjd3/industrie.xml")
compute(ws_ref)


# Ecriture tableau cjo ---------------------------------------------------------

sap <- ws_ref |> get_object()
regs_cjo_N_1 <- data.frame(
    series = get_all_names(sap),
    selected_N_1 = character(count(sap))
) # cree vecteur vide

# selected_N_1 = character(count(sap) ?

for (k in seq_len(count(sap))) {
    #
    k <- 1
    cat(paste0("Série ", k, "/", count(sap), "\n")) ##

    sai_mod <- sap |>
        get_object(k) |>
        get_model(workspace = ws_ref)
    regressors <- sai_mod$regarima$regression.coefficients |> rownames()
    regressors <- regressors[
        !substr(regressors, 1, 2) %in% c("AO", "TC", "LS", "SO", "Me")
    ]

    regs_cjo <- "Pas_CJO"
    if (any(grepl("REG1", regressors))) {
        regs_cjo <- "REG1"
    }
    if (any(grepl("REG5", regressors))) {
        regs_cjo <- "REG5"
    }
    if (any(grepl("REG6", regressors))) {
        regs_cjo <- "REG6"
    }
    if (any(grepl("LeapYear", regressors))) {
        regs_cjo <- paste0(regs_cjo, "_LY")
    }

    regs_cjo_N_1[k, "selected_N_1"] <- regs_cjo
}

# Merge tableau cjo ------------------------------------------------------------

selected_N_outliers <- read.csv(
    file = "./selected_cjo_N_outliers.csv",
    sep = ";"
)
selected_N <- read.csv(file = "./selected_cjo_N.csv", sep = ";")

choix_regs_cjo <- selected_N |>
    merge(selected_N_outliers, by = "series") |>
    merge(regs_cjo_N_1, by = "series")

# Export du tableau ------------------------------------------------------------

write.table(
    choix_regs_cjo,
    file = "./choix_cjo.csv",
    sep = ";",
    row.names = FALSE
)
