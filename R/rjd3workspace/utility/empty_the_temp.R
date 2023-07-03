
empty_temp <- function() {
    if (dir.exists("temp")) {
        list.files("./temp/", recursive = TRUE, full.names = TRUE) |> 
            file.remove()
        print("Done!")
    }
    
    return(invisible(NULL))
}
