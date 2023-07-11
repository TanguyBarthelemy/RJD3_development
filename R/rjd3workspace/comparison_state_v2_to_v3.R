################################################################################
#########          Comparaison wrangling workspace v2 --> v3           #########
################################################################################


# Afficher toutes les méthodes existantes ---------------------------------

a <- rbind(
    cbind(pkg = "rjdemetra3", fct = getNamespace("rjdemetra3") |> getNamespaceExports()), 
    cbind(pkg = "rjd3toolkit", fct = getNamespace("rjd3toolkit") |> getNamespaceExports()), 
    cbind(pkg = "RJDemetra", fct = getNamespace("RJDemetra") |> getNamespaceExports()), 
    cbind(pkg = "rjdworkspace", fct = getNamespace("rjdworkspace") |> getNamespaceExports())
)
View(a)

# RJDemetra ---------------------------------------------------------------

# fct_v2()                    --> equivalent_v3()
# 
# Type de fonction :
# 
#    - Fonction de création
#       new_workspace()       --> rjdemetra3::.jws_new()
#       new_multiprocessing() --> rjdemetra3::.jws_multiprocessing_new()

#    - Fonction de sauvegarde
#       save_workspace()      --> rjdemetra3::save_workspace()
#       save_spec()           --> XXX
# 
#    - Fonction de chargement
#       load_spec()           --> XXX
#       load_workspace()      --> rjdemetra3::.jws_open()
#       compute()             --> rjdemetra3::.jws_compute()
# 
#    - Fonction d'accès
#       get_all_objects()     --> rjdemetra3::.jmp_load()
#       get_object()          --> rjdemetra3::.jws_multiprocessing()
#                                 rjdemetra3::.jmp_sa()
#       get_name()            --> rjdemetra3::.jmp_name()
#                                 rjdemetra3::.jsa_name()
#       get_ts()              --> XXX
#       get_model()           --> rjdemetra3::.jsa_results()
#       get_jmodel()          --> XXX
#                             --> rjd3toolkit::.proc_dictionary2() [Forme différente]
#       get_indicators()      --> XXX
#       count()               --> rjdemetra3::.jws_multiprocessing_count() 
#                                 rjdemetra3::.jmp_sa_count()
# 
#    - Fonction de modification 
#       add_sa_item()         --> rjdemetra3::add_sa_item()
# 
#    - Foction combiné 
#       XXX                   --> rjdemetra3::load_workspace()

# rjdworkspace ------------------------------------------------------------

# TYpe de fonction :
# 
#     - Fonction de modification d'un SA-ITEM :
#       add_new_sa_item()         --> rjdemetra3::add_sa_item()
#       remove_all_sa_item()      --> DEV
#       remove_sa_item()          --> rjdemetra3::remove_sa_item()
#       replace_sa_item()         --> rjdemetra3::replace_sa_item()
# 
#     - Fonction de modification des WS (globalement) :
#       replace_series()          --> XXX (matériel OK)
#       transfer_series()         --> XXX (matériel OK)
#       update_metadata()         --> XXX
#       update_metadata_roughly() --> XXX
#       update_path()             --> XXX (matériel OK)
# 
#     - Fonction de modification des metadatas :
#       set_comment()             --> XXX
#       set_metadata()            --> XXX
#       set_name()                --> XXX
#       set_spec()                --> rjdemetra3::set_domain_specification()
#                                     rjdemetra3::set_specification()
#       set_ts()                  --> rjdemetra3::set_data()
# 
#     - Fonction d'accès
#       get_comment()             --> XXX










