
# Dans ce programme on sélectionne les régresseurs à partir de modèles créés exclusivement sur RJDemetra (sans aucun WS)

# Import data -------------------------------------------------------------

raw_series_ipi <- read.csv("./data/IPI_nace4.csv",
                           sep = ";", dec = ".")

series_ipi_ts <- raw_series_ipi |>
    ts(start = 1994L, frequency = 12L)

regs_cjo_ts <- read.csv("./data/regs_cjo.csv", sep = ";", dec = ".") |>
    ts(start = 1990L, frequency = 12L)


# CJO regressor selection ------------------------------------------------------

spec_sets <- create_spec_sets()

z <- all_diagnostics(series_ipi_ts[, 16], spec_sets = spec_sets)
# cjo_selected <- select_regs(series_ipi_ts[, 1:4])
cjo_selected <- select_regs(series_ipi_ts[, -16], spec_sets = spec_sets)
