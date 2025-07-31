################################################################################
#########          Comparaison wrangling workspace v2 --> v3           #########
################################################################################

# Afficher toutes les méthodes existantes --------------------------------------

a <- rbind(
    cbind(
        pkg = "rjdemetra3",
        fct = getNamespace("rjdemetra3") |> getNamespaceExports()
    ),
    cbind(
        pkg = "rjd3toolkit",
        fct = getNamespace("rjd3toolkit") |> getNamespaceExports()
    ),
    cbind(
        pkg = "RJDemetra",
        fct = getNamespace("RJDemetra") |> getNamespaceExports()
    ),
    cbind(
        pkg = "rjdworkspace",
        fct = getNamespace("rjdworkspace") |> getNamespaceExports()
    )
) |>
    as.data.frame()
View(a)

# RJDemetra --------------------------------------------------------------------

# fct_v2()                    --> equivalent_v3()
#
# Type de fonction :
#
#    - Fonction de création
#       new_workspace()       --> .jws_new()
#       new_multiprocessing() --> .jws_multiprocessing_new()

#    - Fonction de sauvegarde
#       save_workspace()      --> save_workspace()
#       save_spec()           --> XXX
#
#    - Fonction de chargement
#       load_spec()           --> XXX
#       load_workspace()      --> .jws_open()
#       compute()             --> .jws_compute()
#
#    - Fonction d'accès
#       get_all_objects()     --> .jmp_load()
#       get_object()          --> .jws_multiprocessing()
#                                 .jmp_sa()
#       get_name()            --> .jmp_name()
#                                 .jsa_name()
#       get_ts()              --> get_raw_data()
#       get_model()           --> .jsa_results()
#       get_jmodel()          --> XXX
#                             --> rjd3toolkit::.proc_dictionary2() [Forme différente]
#       get_indicators()      --> XXX
#       count()               --> .jws_multiprocessing_count()
#                                 .jmp_sa_count()
#
#    - Fonction de modification
#       add_sa_item()         --> add_sa_item()
#
#    - Fonction combiné
#       XXX                   --> load_workspace()

# rjdworkspace -----------------------------------------------------------------

# TYpe de fonction :
#
#     - Fonction de modification d'un SA-ITEM :
#       add_new_sa_item()         --> add_sa_item()
#       remove_all_sa_item()      --> remove_all_sa_item()
#       remove_sa_item()          --> remove_sa_item()
#       replace_sa_item()         --> replace_sa_item()
#
#     - Fonction de modification des WS (globalement) :
#       transfer_series()         --> transfer_series()
#       update_metadata()         --> XXX
#       update_metadata_roughly() --> XXX
#       update_path()             --> XXX (matériel OK)
#
#     - Fonction de modification des metadatas :
#       set_comment()             --> set_comment()
#       set_metadata()            --> XXX
#       set_name()                --> set_name()
#       set_spec()                --> set_domain_specification()
#                                     set_specification()
#       set_ts()                  --> set_raw_data()
#                                 --> set_ts_metadata()
#
#     - Fonction d'accès
#       get_comment()             --> get_comment()
