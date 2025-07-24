################################################################################
######      Vérification de la bonne attribution des régresseurs CJO      ######
################################################################################

# Ce programme permet de:
# 1) extraire les noms de series et les modeles associes dans le fichier xml
# + de verifier qu'il n'y a pas de doublon -> on n'en veut pas dans le fichier xml final
# 2) d'importer le fichier Choix_CJO + verif doublons (ca ne devrait pas etre le cas par construction)
# 3) verifier que les deux fichiers concordent et isole les series pour lesquelles ca n'est pas le cas
# 4) exporter des tables d'analyse si necessaire

# Chargement des packages -------------------------------------------------

# install.packages("RJDemetra", type="source", INSTALL_opts = "--no-multiarch")
library("RJDemetra")
library("xlsx")


# Paramétrage des chemins -------------------------------------------------

setwd("V:/DG75-L120/Desaisonnalisation/Douanes_2022")


# Lecture des données -----------------------------------------------------

# Lecture des series du ws, recuperation des couples (nom de serie, modele associe dans le ws) et verif d'absence de doublons (de serie)

ws <- RJDemetra::load_workspace("workspace_automatique/Export.xml")

# Recompilation le ws
RJDemetra::compute(ws)


## Extraction des series, resultats et diagnostiques du multi-processing 1 -----

# Extraction du multiprocessing 1
msa <- RJDemetra::get_object(ws, 1)
# get_name(msa)

# Nombre de series du multiprocessing
n <- RJDemetra::count(msa)
n
if (n != 230) stop("Il n'y a pas 230 séries dans le WS.")

# Extraction des series du multiprocessing
sa <- get_all_objects(msa)

# Et de leur nom
nom <- names(sa)


# On initialise les vecteurs qui contiendront les variables CJO+LY de chaque serie ainsi que le nom du modele associe
variables_cjo <- 1:n
modele <- 1:n


# Pour chaque serie
for (i in seq_len(n)) {
    print(i)

    # On recupere les specifs
    res <- RJDemetra::get_model(sa[[i]], ws)

    # essai pour recuperer directement la pesence de LY : m1$regarima$specification$regression$trading.days$leapyear
    # -> ne marche pas parce que la var est userdefined -> la chercher dans le bloc correspondant
    jeux_cjo <- res$regarima$specification$regression$userdef$variables$description
    ly <- "LY" %in% rownames(jeux_cjo)

    # Si rien n'est renseigne, il n'y a pas d'effet CJO ni LY
    if (is.null(dim(jeux_cjo)) || all(is.na(jeux_cjo$coeff))) {
        variables_cjo[i] <- "Pas_CJO"
        modele[i] <- "Pas_CJO"
    } else {
        # sinon on enregistre la liste des regresseurs dans le vecteur "variable"...
        m <- nrow(jeux_cjo)
        if (m == 0) {
            print(paste("Variables de CJO bizarres pour la serie ", i))

            # cas du "REG0.LY" à enregistrer en "LY" tout court
        } else if (m == 1 && ly) {
            variables_cjo[i] <- paste(row.names(jeux_cjo), collapse = ",")
            modele[i] <- "LY"

            # codage des autres cas "standards"
        } else if (ly) {
            variables_cjo[i] <- paste(row.names(jeux_cjo), collapse = ",")
            modele[i] <- paste("REG", m - 1, "_LY", sep = "")
        } else {
            variables_cjo[i] <- paste(row.names(jeux_cjo), collapse = ",")
            modele[i] <- paste("REG", m, sep = "")
        }
    }
}

# ignorer les 50+ warnings() -> dûs au test is.na.data.frame(jeux_cjo), notamment quand celui-ci est vide
# le code fonctionne quand-meme

# !!! Pb serie 126, RF2529 : variable_CJO = "PSO__072009__082009_,Semaine"
# -> c'est un SO ajoute à la main en entreant une user-defined variable qui vaut 1 en juillet et -1 en août, jusqu'en 2008...
# Le calendrier Insee n'etant pas celui de JD+ par defaut, il est userdefined au meme titre que le SO -> presence de la variable ok

## Flemme d'automatiser le recodage des SO/autres variables userdefined hors calendrier Insee :
# Toujours parcourir la table modele pour controler la presence de telle variables, auquel cas adapter le code ci-dessous
# -> correction:  modele[126] <- "REG1"   (code alternatif plus sûr mais pas robuste à la presence de doublon: result[result$serie =="RF2529",]$modele <- "REG1")
# -> puis verif:  result[126,]

result <- data.frame("serie" = nom, "modele" = modele)
head(result)
# write.csv2(result,"V:/Methodo-ICA/Campagne_annuelle_2022/IPI/Donnees/Choix_CJO_2020.csv")

# result[result$serie =="RF2529",]$modele <- "REG1"

# On verifie qu'il n'y a pas de doublon = serie specifiee plus d'une fois dans le xml
result[duplicated(result), ]
# result[result$serie=="RF3212",]

#### 2) Lecture des specifs retenues dans le fichier excel Choix_CJO et verif d'absence de doublons

choix_cjo <- read.xlsx(
    file = "Choix_cjo_temp/Resultats_CJO_2022_export.xls",
    header = TRUE,
    sheetIndex = 1
)
# choix_cjo <- read.csv2(file = "V:/Methodo-ICA/Campagne_annuelle_2022/IPI/Donnees/Choix_CJO_2020.csv")
head(choix_cjo)
choix_cjo <- choix_cjo[, -1]
str(choix_cjo)
colnames(choix_cjo)[1] <- "serie" # pour homogeneiser

# Attention, s'assurer que les series sans effet JO ont bien pour modalite "Pas_CJO" (et non "null", par exemple)
# Si une colonne de NA est importee en fin du fichier, faire tourner la commande suivante: choix_cjo <- choix_cjo[,c(1,2)]

# verif des doublons
choix_cjo[duplicated(choix_cjo), ]
# Si besoin, pour regarder les diff specifs associees à une meme serie: choix_cjo[choix_cjo$serie=="",]

#### 3) Comparaison des series presentes et modeles associes à chaque serie

# on trie les deux data frames
result[order(result$serie), ]
choix_cjo[order(choix_cjo$serie), ]

# On verifie que les modeles possibles sont les memes dans les deux sets de donnees
head(choix_cjo)
# levels(result[,2]) == levels(choix_cjo[,3])

# fusion et comparaison (via la variable divergence)
comparaison <- merge(result, choix_cjo, by = "serie")
str(comparaison)
comparaison$divergence <- comparaison$modele != comparaison$choix_jo

# On isole les series pour lesquelles les modeles specifies dans le xml et le fichier Choix_CJO.csv sont differents
pbs <- comparaison[comparaison$divergence, ]
pbs
# -> pbs doit etre vide

### !!! attention, il faut absolument que chaque serie n'apparaisse qu'une seule fois dans chaque fichier/ws!!
# Sinon ca cree des decalages.

#### 4) Si besoin d'exporter

write.csv2(
    pbs,
    "N:/L120/SECTION IIA/CVS/Analyse IPI/problemes_modeles.csv",
    row.names = FALSE
)
pbs
