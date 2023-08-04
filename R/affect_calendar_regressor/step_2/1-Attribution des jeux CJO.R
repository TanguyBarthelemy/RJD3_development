
################################################################################
#                                                                              #
#           Affectation des jeux de régression CJO retenus aux séries          #
#                                                                              #
################################################################################


# Mettre les données de l'année passée dans le répertoire Donnees
# À partir des workspace types (workspace_type_ini_mensuel.xml et workspace_type_ini_trimestriel.xml),
# contenus dans le dossier Workspaces_vides_avec_specs_cjo : 
# 1 les ouvrir et les enregistrer dans le dossier Workspaces_automatiques
#   Faire cela pour chaque ws à traiter (donc 1 mens et 2 trim) en les renommant
# 2 puis, pour chaque ws:
# 2.1 créer un nouveau SAP et lui donner le nom du workspace
# 2.2 sélectionner une spécification parmi les REGx et glisser les séries voulues depuis le panneau Providers
# 2.3 sauvegarder le tout dans le répertoire Workspaces_automatiques
# 2.4 Fermer JDemetra+, lancer le programme suivant et rouvrir l'application


# Chargement des packages R ----------------------------------------------------

library("readxl")


# Paramètre de chemin ----------------------------------------------------------

setwd("V:/DG75-L120/Desaisonnalisation/Douanes_2022")
# Chemin vers le workspace
chemin_vers_workspace <- "workspace_automatique"

# Workspace à traiter
ws <- "Export"

nom_sa_proc <- "SAProcessing-1.xml"
chemin_sa_proc <- sprintf("%s/%s/SAProcessing/%s",
                          chemin_vers_workspace,
                          ws,
                          nom_sa_proc)
chemin_sa_proc  


# Lecture et traitement des données --------------------------------------------


# Lecture du fichier xml contenant tous les paramètres
sa_proc <- readLines(chemin_sa_proc)
head(sa_proc)

# Spécifications déjà créées dans le ws et qu'on va attribuer aux séries grâce au fichier de décision CJO
noms_specs <- c("Pas_CJO","REG1","REG2","REG3","REG5","REG6","LY","REG1_LY","REG2_LY","REG3_LY","REG5_LY","REG6_LY")
data_names_spec <- data.frame(name_reg_cjo = noms_specs,
                              domainspec = sprintf("spec%s", seq_along(noms_specs)))
data_names_spec


# reg_cjo_series <- read.csv("Choix CJO/choix_jo_export.csv", sep = ";")
library("readxl")
reg_cjo_series <- read_excel("Choix_cjo_temp/Resultats_CJO_2022_export.xls")
reg_cjo_series <- reg_cjo_series[, -1]
colnames(reg_cjo_series)[c(1, 2)] <- c("series", "choix_reg_cjo")
head(reg_cjo_series)
# on ajoute le numéro des specs
reg_cjo_series <- merge(reg_cjo_series[, c("series", "choix_reg_cjo")], data_names_spec,
    by.x = "choix_reg_cjo", by.y = "name_reg_cjo", all.x = TRUE) 


# all.x = TRUE : si certaines specs n'ont été retenues pour aucune série, on ne crée pas de ligne associée
# suppl dans data_serie, sans nom de série "en face"
head(reg_cjo_series)

# patch pour les 5 séries "impossible": on ne leur attribue pas de régresseur CJO
reg_cjo_series$domainspec[is.na(reg_cjo_series$domainspec)]  <- "Pas_CJO"
head(reg_cjo_series)
nrow(reg_cjo_series)

if (nrow(reg_cjo_series) != 230) stop("Il y a un souci dans le nombre de série.")


# Etape 1 ----------------------------------------------------------------------

# Modification des domainspecs (specifications de base) dans le xml (à la fin)

# On identifie les lignes de début et de fin des spécifications
debut_domains_spec <- grep("<item name=\"domainspecs\">", sa_proc) + 1
fin_domains_spec <- length(sa_proc) - 2
debut_domains_spec
fin_domains_spec

# Déterminer le nombre d'espaces avant chaque spécification
space_spec <- gsub("<.*", "", sa_proc[debut_domains_spec + 1])

nouvelles_spec <- unlist(lapply(seq_len(nrow(data_names_spec)), function(num_spec) {
    # num_spec=1
    fichier_spec <- sprintf("%s/%s/X13Spec/%s.xml",
                            chemin_vers_workspace,
                            ws,
                            data_names_spec[num_spec, 1]
    )
    spec <- readLines(fichier_spec)
    spec <- spec[-1] # On enlève la première ligne
    
    # La première ligne sera <item name="spec1"> si num_spec = 1 :
    prem_ligne <- sprintf("<item name=\"%s\">", data_names_spec[num_spec, 2]) 
    # La dernière ligne sera </item> :
    der_ligne <- "</item>"
    
    spec[1] <- "<subset>"
    spec[length(spec)] <- "</subset>"
    
    c(prem_ligne,
      sprintf("    %s", spec),
      der_ligne)
}))

# On ajoute les espaces :
nouvelles_spec <- sprintf("%s%s", space_spec, nouvelles_spec)

# On modifie le sa_processing :
sa_proc <- c(sa_proc[1:debut_domains_spec],
             nouvelles_spec,
             sa_proc[fin_domains_spec:length(sa_proc)])


# Etape 2 -----------------------------------------------------------------

# Modification des spécifications par série

head(reg_cjo_series)

# Récupération des noms des séries :
nom_series <- grep("               <ts name=\"", sa_proc, value = TRUE)
nom_series <- gsub("(^.*=\")|(\">.*$)", "", nom_series)
nom_series

# Récupération des lignes du début des domainspec
i_domainspec <- grep("            <item name=\"domainspec\">", sa_proc) + 1
names(i_domainspec) <- nom_series
i_domainspec

series_oubliees <- c()

for (n_serie in nom_series) {
    #n_serie="AEFRANCETRCOM"    
    
    if (n_serie %in% reg_cjo_series$series) {
        
        domainspec_serie <- subset(x = reg_cjo_series, series == n_serie, select = domainspec)
        
        # on règle le pb des séries AEREG06 qui sont vides et qu'on veut garder intactes dans le ws
        if (is.na(domainspec_serie)) {
            warning(sprintf("Attention, la série %s n'a pas de domainspec. Elle ne sera donc pas traitée.", n_serie))
        } else{
            
            # Pour chaque série on remplace la spécification actuelle
            n_blanc_domainspec <- gsub("<.*", "",
                                       sa_proc[i_domainspec[n_serie]])
            new_spec <- sprintf("%s<string>%s</string>",
                                n_blanc_domainspec,
                                domainspec_serie)
            sa_proc[i_domainspec[n_serie]] <- new_spec
        }
    } else {
        warning(sprintf("Il manque la série %s dans le fichier des décisions de JO", n_serie))
        #log_ae <- c(log_ae,sprintf("Il manque la série %s dans le fichier des décisions de JO", n_serie))
        series_oubliees <- c(series_oubliees, n_serie)
        #warning(sprintf("Il manque la série %s dans le fichier des JO qui aura le régresseur %s", n_serie, reg_cjo_series[n_serie,1]))
    }
}
series_oubliees

if (!is.null(series_oubliees)) stop("Il y a une série oubliée.")

# Export
writeLines(sa_proc, chemin_sa_proc)



# Faire tourner le code de vérif RJD_recup_et_compare_regresseurs_CJO.R 

