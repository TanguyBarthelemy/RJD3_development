
move_data <- function() {
    
    if (list.files("./data_temp/path_1") |> length() == 0) {
        path_from <- "./data_temp/path_2"
        path_to <- "./data_temp/path_1"
    } else {
        path_from <- "./data_temp/path_1"
        path_to <- "./data_temp/path_2"
    }
    
    paths_data <- list.files(path_from, full.names = TRUE)
    invisible(sapply(paths_data, FUN = file.copy, to = path_to))
    invisible(sapply(paths_data, FUN = file.remove))
}
