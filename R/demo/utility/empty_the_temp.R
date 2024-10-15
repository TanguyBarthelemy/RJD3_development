empty_temp <- function() {
    if (dir.exists("temp")) {
        list.dirs("./temp/", full.names = TRUE, recursive = FALSE) |>
            unlink(recursive = TRUE)
        list.files("./temp/", full.names = TRUE, recursive = TRUE) |>
            file.remove()
        print("Done!")
    }

    return(invisible(NULL))
}
