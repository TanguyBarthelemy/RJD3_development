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

# fct_v2()                  
# 
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
