ws_cjo <- new_workspace()
raw_series_ipi <- read.csv("./data/IPI_nace4.csv", sep = ";", dec = ".")
series_ipi_ts <- raw_series_ipi |>
    ts(start = 1990L, frequency = 12L)

raw_data <- series_ipi_ts[, 2:5]
spec_sets <- create_spec_sets()

for (k in seq_along(spec_sets)) {
    name_spec <- names(spec_sets)[k]
    spec <- spec_sets[[k]]

    cat(paste0("SAPprocessing ", k, "...\n"))
    mp1 <- new_multiprocessing(workspace = ws_cjo, name = name_spec)

    for (i in seq_len(ncol(raw_data))) {
        name_serie <- colnames(raw_data)[i]

        cat(paste0(
            "SA-Item ",
            i,
            "/",
            ncol(raw_data),
            ", serie : ",
            name_serie,
            "..."
        ))
        sa_item <- x13(
            series = series_ipi_ts[, i],
            spec = spec
        )
        add_sa_item(
            workspace = ws_cjo,
            multiprocessing = name_spec,
            sa_obj = sa_item,
            name = name_serie
        )
        cat(" Done!\n")
    }
}

save_workspace(workspace = ws_cjo, file = "./WS/ws_cjo.xml")
