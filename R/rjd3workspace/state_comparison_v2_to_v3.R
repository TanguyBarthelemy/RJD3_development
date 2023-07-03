################################################################################
#########          Comparaison wrangling workspace v2 --> v3           #########
################################################################################


# Chargement packages -----------------------------------------------------

library("RJDemetra")
library("rjdworkspace")

library("rjdemetra3")
library("rjd3toolkit")


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
#       get_model()           -->
#       get_jmodel()          -->
#       get_dictionary()      -->
#       get_indicators()      -->
#       count()               --> rjdemetra3::.jws_multiprocessing_count() 
#                                 rjdemetra3::.jmp_sa_count()
# 
#    - Fonction de modification 
#       add_sa_item()         -->
# 
#    - Foction combiné 
#       XXX                   --> rjdemetra3::load_workspace()

# rjdworkspace ------------------------------------------------------------

# TYpe de fonction :
# 
#     - Fonction de modification
#       add_new_sa_item()         -->
#       remove_all_sa_item()      -->
#       remove_sa_item()          -->
#       replace_sa_item()         -->
#       replace_series()          -->
#       transfer_series()         -->
#       set_comment()             -->
#       set_metadata()            -->
#       set_name()                -->
#       set_spec()                -->
#       set_ts()                  -->
#       update_metadata()         -->
#       update_metadata_roughly() -->
#       update_path()             -->
# 
#     - Fonction d'accès
#       get_comment()             -->










