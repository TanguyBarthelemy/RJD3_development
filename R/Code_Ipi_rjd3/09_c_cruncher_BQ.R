library("JDCruncheR")
library("readxl")
library("xlsx")

options(
    cruncher_bin_directory = "Y:/Logiciels/JDemetraplus/jwsacruncher-2.2.4/bin"
)

# workspace_automatique: ATT TEMP for test
cruncher_and_param(
    workspace = "workspace_de_travail/industrie.xml",
    rename_multi_documents = FALSE, # Pour renommer les dossiers en sortie : pas utile ici
    delete_existing_file = TRUE, # Pour remplacer les sorties existantes
    policy = "complete", # Politique de rafraîchissement
    csv_layout = "vtable", # Format de sortie des tables
    log_file = "workspace_automatique/log.txt"
)


# ## Lecture des informations auxilaires (a jouter au BQ)
# 1 Lecture du fichier de ponderations et priorites
POND_NAF4 <- read.csv("Donnees/Ponderations_2024.csv", encoding = "UTF-8")
str(POND_NAF4)
colnames(POND_NAF4) <- c("series", "pond", "rebasee", "commentaire", "prio")
head(POND_NAF4)
# attention si "," pour les decimales et pas ".", genere des NA
POND_NAF4$pond <- gsub(",", ".", POND_NAF4$pond)
POND_NAF4$pond <- round(as.numeric(POND_NAF4$pond), digits = 0)
str(POND_NAF4)
head(POND_NAF4)

table(POND_NAF4$prio)

# 2 Lecture des decisions de la campagne precedente
decisions_passees <- read_excel("Donnees/Decisions_2023.xlsx")
head(decisions_passees)
colnames(decisions_passees)[1] <- "series"
# On restreint le fichier aux noms des series et aux decisions associees
decisions_passees <- decisions_passees[, c("series", "decision")]
head(decisions_passees)
# On renomme la colonne des anciennes decisions
colnames(decisions_passees) <- c("series", "Ancienne Decision")

## Generation du bilan qualite pour le workspace automatique

# (extract_QR = fonction native du cruncher : lecture fichier csv = matrice)
# BQ_auto <- extract_QR("workspace_de_travail/industrie/output/SAProcessing-1/demetra_m.csv")
BQ_auto <- extract_QR(
    "workspace_de_travail/industrie/output/industrie/demetra_m.csv"
)
# le resultat est une liste de 3 data frames
str(BQ_auto)
# a ce stade, on n'a pas encore de score, ni de formule du score
# -> compute_score : fonction native du cruncher
# Calcul du score (personnalise) et ajout au bilan qualite:
BQ_auto <- compute_score(
    BQ_auto,
    n_contrib_score = 3,
    conditional_indicator = list(list(
        indicator = "oos_mse",
        conditions = c(
            "residuals_independency",
            "residuals_homoskedasticity",
            "residuals_normality"
        ),
        conditions_modalities = c("Bad", "Severe")
    )),
    na.rm = TRUE
)

# Extraction du score (pour visualisation)
head(extract_score(BQ_auto))

# Ajout de la variable de ponderation dans le bilan qualite
# add_indicator : fonction native du cruncher
# la variable le data frame "POND_NAF4" est automatiquement fusionne avec le data frame BQ_auto$values
BQ_auto <- add_indicator(BQ_auto, POND_NAF4)

# Calcul du score pondere par le poids des secteurs dans l'activite economique
# weighted score : fonction native du cruncher qui calcule et ajoutele score pondere dans le data frame "values"
BQ_auto <- weighted_score(BQ_auto, "pond") # on indique la variable qui sert à poderer le score
str(BQ_auto)
BQ_auto$score_formula

# Ajout dans le data frame "values" des decisions de la campagne precedente
# (on ne garde que les valeurs pour les series presentes dans values : all.x = TRUE, all.y = FALSE
# -> on agit sur le data frame BQ_auto$values, un des 3 elements de la liste BQ_auto)
BQ_auto$values <- merge(
    BQ_auto$values,
    decisions_passees,
    by = "series",
    all.x = TRUE,
    all.y = FALSE
)
head(BQ_auto$values)

scores <- extract_score(BQ_auto, weighted_score = TRUE)
scores[1:20, ]
colnames(BQ_auto$values)

## Export d'un bilan qualite raccourci

exp_auto <- BQ_auto$values[, c(
    "series",
    "prio",
    "m7",
    "score",
    "pond",
    "score_pond",
    "1_highest_contrib_score",
    "2_highest_contrib_score",
    "3_highest_contrib_score"
)]

write.xlsx(exp_auto, "Bilans qualite/BQ_work_2012.xlsx")


##########################################################################################
# Ajout de la nouvelle colonne "Decision" a remplir pour l'annee en cours

BQ_auto$values$decision <- ""
colnames(BQ_auto$values)
# # rearrangement des variables
fichier_decisions <- BQ_auto$values[, c(
    "series",
    "prio",
    "rebasee",
    "score",
    "score_pond",
    "1_highest_contrib_score",
    "m7",
    "pond",
    "decision",
    "commentaire",
    "Ancienne Decision"
)]

head(fichier_decisions)


write.xlsx(
    fichier_decisions,
    "Bilans qualite/Decisions_2024_2012.xlsx",
    showNA = FALSE
)
