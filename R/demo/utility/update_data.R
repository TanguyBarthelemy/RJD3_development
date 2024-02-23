
update_data <- function(file_name,
                        origin) {

    file_path <- file.path("./data_temp/", file_name)
    origin_path <- file.path("./data/", origin)

    file.copy(from = origin_path,to = file_path, overwrite = TRUE)

    return()
}

update_refresh_data <- function() {
    update_data(file_name = "data_ipi_ws_refresh.csv", origin = "IPI_nace4_sep2021.csv")
}

outdate_refresh_data <- function() {
    update_data(file_name = "data_ipi_ws_refresh.csv", origin = "IPI_nace4_dec2020.csv")
}
