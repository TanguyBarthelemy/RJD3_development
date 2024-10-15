pull_out_fire <- function(ws_name, id = NULL) {
    ws_path <- file.path("./WS", ws_name)
    xml_path <- file.path("./WS", paste0(ws_name, ".xml"))

    if (!(dir.exists(ws_path) && file.exists(xml_path))) {
        stop("Le WS n'existe pas.")
    }

    if (is.null(id)) {
        id <- new_name()
        cat("Your new id is : ", id, ".\n", sep = "")
    }
    secured_path <- file.path("./WS/SECURED", id)


    if (!dir.exists("./WS/SECURED/")) {
        dir.create("./WS/SECURED/")
    }

    dir.create(secured_path)
    file.copy(from = ws_path, to = secured_path, recursive = TRUE)
    file.copy(from = xml_path, to = secured_path, recursive = TRUE)

    return(id)
}

bring_back <- function(id) {
    secured_path <- file.path("./WS/SECURED", id)
    if (!dir.exists(secured_path)) {
        warning("Il n'y a pas de copie à cet id.")
        return(invisible(NULL))
    }

    objects_name <- list.files(secured_path, full.names = FALSE)
    objects_secured_path <- list.files(secured_path, full.names = TRUE)
    objects_current_path <- file.path("./WS", objects_name)

    for (index_object in seq_along(objects_name)) {
        unlink(objects_current_path[index_object], recursive = TRUE)
        file.copy(
            from = objects_secured_path[index_object],
            to = "./WS",
            overwrite = TRUE,
            recursive = TRUE
        ) |> invisible()
        unlink(objects_secured_path[index_object], recursive = TRUE)
    }

    unlink(secured_path, recursive = TRUE)

    return(id)
}

bring_all_back <- function() {
    ids <- list.dirs(
        path = "./WS/SECURED/",
        recursive = FALSE,
        full.names = FALSE
    )

    for (id in ids) {
        print(bring_back(id))
    }

    return(invisible(NULL))
}
