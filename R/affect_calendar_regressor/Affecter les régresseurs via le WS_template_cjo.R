
# Affecter les régresseurs via le WS_template_cjo

library("rjdworkspace")
library("RJDemetra")


ws_auto <- new_workspace()
sap1 <- new_multiprocessing(ws_auto, "SAProcessing-1")

raw_series_ipi <- read.csv("./data/IPI_nace4.csv", sep = ";", dec = ".")
series_ipi_ts <- raw_series_ipi |>
    ts(start = 1990L, frequency = 12L)

raw_data <- series_ipi_ts[, 2:5]

choix_cjo <- read.csv("./R/affect_calendar_regressor/regs_chosen.csv", sep = ";", dec = ".")

ws_cjo <- load_workspace(file = "./WS/ws_cjo.xml")
compute(ws_cjo)

for (i in seq_len(ncol(raw_data))) {

    name_serie <- colnames(raw_data)[i]
    regs_cjo <- choix_cjo$reg[choix_cjo$series == name_serie]
    transfer_series(ws_cjo, ws_auto,
                    selected_series = name_serie,
                    pos_sap_to = 1, name_sap_from = regs_cjo)
}

save_workspace(ws_auto, file = "./WS/ws_auto.xml")
