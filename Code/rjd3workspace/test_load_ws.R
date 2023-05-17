
################################################################################
#######             Test des fonctions v2 de gestion des WS              ####### 
################################################################################

# Chargement des packages ------------------------------------------------------

library("RJDemetra")
library("rjdworkspace")


# Chargement fonctions de print ------------------------------------------------

path <- "./Code/Study_new_prints/print_functions/"
function2import <- list.files(path, full.names = TRUE, recursive = TRUE)
sapply(X = function2import, FUN = source, encoding = "UTF-8") |> invisible()


# Test -------------------------------------------------------------------------

load_workspace("")

