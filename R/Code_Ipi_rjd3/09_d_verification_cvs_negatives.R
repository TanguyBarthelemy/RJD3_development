################################################################################
######                  Vérification des séries CVS-CJO                   ######
################################################################################

# Lecture séries ----------------------------------------_____------------------

IPI_cvs <- read.csv(
    "./workspace_automatique/industrie/Output/industrie/series_sa.csv",
    sep = ";",
    dec = ","
)

col_incorrect <- vapply(
    X = IPI_cvs,
    FUN = \(x) sum(x <= 0, na.rm = TRUE) > 0L,
    FUN.VALUE = logical(1L)
)
col_names <- names(col_incorrect)[col_incorrect]
