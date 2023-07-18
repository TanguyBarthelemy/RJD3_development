################################################################################
#########          Comparaison wrangling workspace v2 --> v3           #########
################################################################################


# Afficher toutes les méthodes existantes --------------------------------------

rjdv2 <- cbind(pkg = "RJDemetra", 
               fct = getNamespace("RJDemetra") |> getNamespaceExports()) |> 
    as.data.frame() |> 
    subset(substr(fct, 1, 1) != ".")
rownames(rjdv2) <- NULL

View(rjdv2)


# Wrangling workspaces ---------------------------------------------------------

# Type de fonction :
# 
#    - Fonction de création
#       new_workspace()     
#       new_multiprocessing()
#
#    - Fonction de sauvegarde
#       save_workspace()    
#       save_spec()         
# 
#    - Fonction de chargement
#       load_spec()         
#       load_workspace()    
#       compute()           
# 
#    - Fonction d'accès
#       get_all_objects()   
#       get_object()        
#                           
#       get_name()          
#                           
#       get_ts()            
#       get_model()         
#       get_jmodel()        
#                           
#       get_indicators()    
#       count()             
#                           
#    - Fonction de modification 
#       add_sa_item()
# 

ws_fct <- c("new_workspace", "new_multiprocessing", 
        "load_workspace", "save_workspace", 
        "compute",
        "load_spec", "save_spec", 
        "get_all_objects", "get_object", "get_name", "get_ts", 
        "get_model", "get_jmodel", "get_indicators", "count", 
        "add_sa_item")

rjdv2_without_ws <- rjdv2 |> subset(!fct %in% ws_fct)
rownames(rjdv2_without_ws) <- NULL

View(rjdv2_without_ws)


# SA with X13-Arima or Tramo-Seats ---------------------------------------------

# Pre-processing :
#   - regarima_x13()
#   - regarima_tramoseats()
#   - regarima()
# 
# Full SA :
#   - x13()
#   - tramoseats()
# 
# Spec manipulation :
#   - regarima_spec_x13()
#   - regarima_spec_tramoseats()
#   - regarima_spec()
#   - x13_spec()
#   - tramoseats_spec()


# Java mirror functions ---------------------------------------------------------

# All functions beginning with j


# Access functions ---------------------------------------------------------

# All functions beginning with s_

ws_fct2 <- c("regarima_x13", "regarima_tramoseats", "regarima", 
             "x13", "tramoseats", 
             "regarima_spec_x13", "regarima_spec_tramoseats", "regarima_spec", 
             "x13_spec", "tramoseats_spec")

remaining <- rjdv2_without_ws |> 
    subset(substr(fct, 1, 2) != "s_") |> 
    subset(substr(fct, 1, 1) != "j") |>  
    subset(!fct %in% ws_fct2)

View(remaining)


# Other functions ----------------------------------------------------------

#   - get_jspec()
#   - user_defined_variables()
#   - get_dictionary()

