
ws_cjo <- new_workspace()
raw_data <- series_aerien_ts[, -16]

for (k in seq_along(spec_sets)) {
    
    name_spec <- names(spec_sets)[k]
    spec <- spec_sets[[k]]
    
    cat(paste0("SAPprocessing ", k, "...\n"))
    mp1 <- new_multiprocessing(workspace = ws_cjo, name = name_spec)
    
    for (i in seq_len(nrow(raw_data))) {
        
        name_serie <- colnames(raw_data)[i]
        
        cat(paste0("SA-Item ", i, "/22", ", serie : ", name_serie, "..."))
        sa_item <- x13(
            series = series_aerien_ts[, i], 
            spec = spec)
        add_sa_item(workspace = ws_cjo, 
                    multiprocessing = name_spec, 
                    sa_obj = sa_item, 
                    name = name_serie)
        cat(" Done!\n")
    }
}

save_workspace(workspace = ws_cjo, file = "./Choix CJO/WS/ws_cjo.xml")

