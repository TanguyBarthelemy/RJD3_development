
################################################################################
########               Création du premier bilan qualité                ########
################################################################################

# Précaution

# On s'apprète à faire tourner le cruncher donc il faut que nos WS soient bien 
# connectés à nos données.

# Chargement packages -----------------------------------------------------

library("JDCruncheR")
library("purrr")

ch <- getwd()
setwd("../../../../Production/SSMSI/")

# Chemin ------------------------------------------------------------------

# repertoire de travail = celui qui contient les donnees, a ajuster ...
# setwd("V:/DG75-L120/Desaisonnalisation/SSMSI_2022/")

# liste WS = liste des fichiers xml du repertoire
# On doit obtenir les .xml WS_export_choix_cjo.xml et WS_import_choix_cjo.xml
WS_xml <- list.files(path = "./Choix CJO/WS", pattern = "*.xml", full.names = TRUE)

# liste des dossier ws : idem sans le .xml
# On doit obtenir les dossiers WS_export_choix_cjo et WS_import_choix_cjo
WS_dossier <- list.dirs(path = "./Choix CJO/WS", full.names = TRUE, recursive = FALSE)

# liste des noms de WS
# On doit obtenir les dossiers WS_export_choix_cjo et WS_import_choix_cjo
WS_name <- list.dirs(path = "./Choix CJO/WS", full.names = FALSE, recursive = FALSE)


# Paramètres et options du cruncher --------------------------------------------

# Si les fonctions suivantes ne renvoient rien ou que cela ne correspond pas 
#   à un chemin valide ou au bon chemin, il faut compléter les options

# Chemin du cruncher (dossier contenant le fichier .bat)
getOption("cruncher_bin_directory")
# Exemple d'option à poser :
# options(cruncher_bin_directory = "Y:/Logiciels/jwsacruncher-2.2.0/jdemetra-cli-2.2.0/bin")
# options(cruncher_bin_directory = "Y:/Logiciels/JDemetraplus/jwsacruncher-2.2.3/bin/")
options(cruncher_bin_directory = "C:/Users/UTZK0M/Software/jdemetra-related/jwsacruncher-2.2.4/bin/")

# Séries issues de la décomposition
getOption("default_tsmatrix_series")
# Exemple d'option à poser :
# options(default_tsmatrix_series = c("y","sa","sa_f","s","s_f","ycal","cal","cal_f","d10","d10a"))

# Matrice de qualité
getOption("default_matrix_item")
# Exemple d'option à poser :
# options(default_matrix_item = c("period", "span.start", "span.end", "span.n", "span.missing",
#                                 "espan.start", "espan.end", "espan.n", "log", "adjust", "regression.lp",
#                                 "regression.ntd", "regression.nmh", "regression.td-derived",
#                                 "regression.td-ftest", "regression.easter", "regression.nout",
#                                 "regression.noutao", "regression.noutls", "regression.nouttc",
#                                 "regression.noutso", "regression.td(*):4", "regression.out(*)",
#                                 "regression.user(*)", "likelihood.neffectiveobs", "likelihood.np",
#                                 "likelihood.logvalue", "likelihood.adjustedlogvalue", "likelihood.ssqerr",
#                                 "likelihood.aic", "likelihood.aicc", "likelihood.bic", "likelihood.bicc",
#                                 "residuals.ser", "residuals.ser-ml", "residuals.mean", "residuals.skewness:3",
#                                 "residuals.kurtosis:3", "residuals.dh", "residuals.lb", "residuals.lb2:3",
#                                 "residuals.seaslb", "residuals.bp", "residuals.bp2", "residuals.seasbp",
#                                 "residuals.nudruns", "residuals.ludruns", "residuals.nruns",
#                                 "residuals.lruns", "arima", "arima.mean", "arima.p", "arima.d",
#                                 "arima.q", "arima.bp", "arima.bd", "arima.bq", "arima.phi(*)",
#                                 "arima.bphi(*)", "arima.th(*)", "arima.bth(*)", "decomposition.seasonality",
#                                 "decomposition.parameters_cutoff", "decomposition.model_changed",
#                                 "decomposition.tvar-estimator", "decomposition.tvar-estimate",
#                                 "decomposition.tvar-pvalue", "decomposition.savar-estimator",
#                                 "decomposition.savar-estimate", "decomposition.savar-pvalue",
#                                 "decomposition.svar-estimator", "decomposition.svar-estimate",
#                                 "decomposition.svar-pvalue", "decomposition.ivar-estimator",
#                                 "decomposition.ivar-estimate", "decomposition.ivar-pvalue", "decomposition.tscorr-estimator",
#                                 "decomposition.tscorr-estimate", "decomposition.tscorr-pvalue",
#                                 "decomposition.ticorr-estimator", "decomposition.ticorr-estimate",
#                                 "decomposition.ticorr-pvalue", "decomposition.sicorr-estimator",
#                                 "decomposition.sicorr-estimate", "decomposition.sicorr-pvalue",
#                                 "decomposition.ar_root(*)", "decomposition.ma_root(*)", "method",
#                                 "variancedecomposition.cycle", "variancedecomposition.seasonality",
#                                 "variancedecomposition.irregular", "variancedecomposition.tdh",
#                                 "variancedecomposition.others", "variancedecomposition.total",
#                                 "diagnostics.logstat", "diagnostics.levelstat", "diagnostics.fcast-insample-mean",
#                                 "diagnostics.fcast-outsample-mean", "diagnostics.fcast-outsample-variance",
#                                 "diagnostics.seas-lin-f", "diagnostics.seas-lin-qs", "diagnostics.seas-lin-kw",
#                                 "diagnostics.seas-lin-friedman", "diagnostics.seas-lin-periodogram",
#                                 "diagnostics.seas-lin-spectralpeaks", "diagnostics.seas-si-combined",
#                                 "diagnostics.seas-si-evolutive", "diagnostics.seas-si-stable",
#                                 "diagnostics.seas-res-f", "diagnostics.seas-res-qs", "diagnostics.seas-res-kw",
#                                 "diagnostics.seas-res-friedman", "diagnostics.seas-res-periodogram",
#                                 "diagnostics.seas-res-spectralpeaks", "diagnostics.seas-res-combined",
#                                 "diagnostics.seas-res-combined3", "diagnostics.seas-res-evolutive",
#                                 "diagnostics.seas-res-stable", "diagnostics.seas-i-f", "diagnostics.seas-i-qs",
#                                 "diagnostics.seas-i-kw", "diagnostics.seas-i-periodogram", "diagnostics.seas-i-spectralpeaks",
#                                 "diagnostics.seas-i-combined", "diagnostics.seas-i-combined3",
#                                 "diagnostics.seas-i-evolutive", "diagnostics.seas-i-stable",
#                                 "diagnostics.seas-sa-f", "diagnostics.seas-sa-qs", "diagnostics.seas-sa-kw",
#                                 "diagnostics.seas-sa-friedman", "diagnostics.seas-sa-periodogram",
#                                 "diagnostics.seas-sa-spectralpeaks", "diagnostics.seas-sa-combined",
#                                 "diagnostics.seas-sa-combined3", "diagnostics.seas-sa-evolutive",
#                                 "diagnostics.seas-sa-stable", "diagnostics.seas-sa-ac1", "diagnostics.td-sa-all",
#                                 "diagnostics.td-sa-last", "diagnostics.td-i-all", "diagnostics.td-i-last",
#                                 "diagnostics.td-res-all", "diagnostics.td-res-last", "diagnostics.ic-ratio-henderson",
#                                 "diagnostics.ic-ratio", "diagnostics.msr-global", "diagnostics.msr(*)",
#                                 "decomposition.trendfilter", "decomposition.seasfilter", "m-statistics.m1",
#                                 "m-statistics.m2", "m-statistics.m3", "m-statistics.m4", "m-statistics.m5",
#                                 "m-statistics.m6", "m-statistics.m7", "m-statistics.m8", "m-statistics.m9",
#                                 "m-statistics.m10", "m-statistics.m11", "m-statistics.q", "m-statistics.q-m2",
#                                 "diagnostics.basic checks.definition:2", "diagnostics.basic checks.annual totals:2",
#                                 "diagnostics.visual spectral analysis.spectral seas peaks", "diagnostics.visual spectral analysis.spectral td peaks",
#                                 "diagnostics.regarima residuals.normality:2", "diagnostics.regarima residuals.independence:2",
#                                 "diagnostics.regarima residuals.spectral td peaks:2", "diagnostics.regarima residuals.spectral seas peaks:2",
#                                 "diagnostics.outliers.number of outliers:2", "diagnostics.out-of-sample.mean:2",
#                                 "diagnostics.out-of-sample.mse:2", "diagnostics.m-statistics.q:2",
#                                 "diagnostics.m-statistics.q-m2:2", "diagnostics.seats.seas variance:2",
#                                 "diagnostics.seats.irregular variance:2", "diagnostics.seats.seas/irr cross-correlation:2",
#                                 "diagnostics.residual seasonality tests.qs test on sa:2", "diagnostics.residual seasonality tests.qs test on i:2",
#                                 "diagnostics.residual seasonality tests.f-test on sa (seasonal dummies):2",
#                                 "diagnostics.residual seasonality tests.f-test on i (seasonal dummies):2",
#                                 "diagnostics.combined seasonality test.combined seasonality test on sa:2",
#                                 "diagnostics.combined seasonality test.combined seasonality test on sa (last 3 years):2",
#                                 "diagnostics.combined seasonality test.combined seasonality test on irregular:2",
#                                 "diagnostics.residual trading days tests.f-test on sa (td):2",
#                                 "diagnostics.residual trading days tests.f-test on i (td):2",
#                                 "diagnostics.quality"
# ))
options(default_matrix_item = c(getOption("default_matrix_item"), "regression.td(*):4"))


# Calcul des bilans qualité  ---------------------------------------------------

# On initialise une liste qui contiendra les informations pour chaque SAP
liste_BQ_WS <- list()

# On passe en revue chaque WS
for (i_ws in seq_along(WS_xml)) {
    
    print(paste0("WS n°", i_ws, " : ", WS_name[i_ws]))
    
    # Création de la matrice demetra_m et des séries dans un dossier Output
    cruncher_and_param(workspace = WS_xml[i_ws],
                       rename_multi_documents = TRUE, # Pour renommer les dossiers en sortie
                       delete_existing_file = TRUE, # Pour remplacer les sorties existantes
                       policy = "complete", # Politique de rafraichissement
                       csv_layout = "list", # Format de sortie des tables
                       log_file = "./Choix CJO/log.txt"
    )
    
    print("✔️️ Crunché !")
    
    # Liste des SA processing = sous-répertoire du dossier Output
    SAP_dossier <- list.dirs(paste0(WS_dossier[i_ws], "/Output"), recursive = FALSE, full.names = TRUE)
    SAP_nom <- list.dirs(paste0(WS_dossier[i_ws], "/Output"), recursive = FALSE, full.names = FALSE)
    
    # On initialise une liste qui contiendra les informations pour chaque SAP
    liste_BQ_sap <- list()
    
    # Pour chaque SA processing
    for (i_sap in seq_along(SAP_dossier)) {
        
        print(paste0("SAP n°", i_sap, " : ", SAP_nom[i_sap]))
        
        # on recupere les diagnostics
        BQ <- extract_QR(paste0(SAP_dossier[i_sap], "/demetra_m.csv"))
        # on calcule le score
        BQ <- compute_score(BQ, n_contrib_score = 5,
                            conditional_indicator = list(list(
                                indicator = "oos_mse",
                                conditions = c("residuals_independency",
                                               "residuals_homoskedasticity",
                                               "residuals_normality"),
                                conditions_modalities = c("Bad","Severe"))),
                            na.rm = TRUE)
        
        liste_BQ_sap <- c(liste_BQ_sap, list(BQ))
        
        print("✔️️ Bilan qualité exporté !")
    }
    names(liste_BQ_sap) <- SAP_nom
    liste_BQ_WS <- c(liste_BQ_WS, list(liste_BQ_sap))
}

names(liste_BQ_WS) <- WS_name


# Coefficients et p_value ------------------------------------------------------

# On initialise une liste qui contiendra les informations pour chaque SAP
liste_coeff_WS <- list()

for (i_ws in seq_along(WS_xml)) {
    
    print(paste0("WS n°", i_ws, " : ", WS_name[i_ws]))
    
    # récupération des noms de chaque SA processing ainsi que de leur chemin
    SAP_nom <- list.dirs(paste0(WS_dossier[i_ws], "/Output"), recursive = FALSE, full.names = FALSE)
    SAP_dossier <- list.dirs(paste0(WS_dossier[i_ws], "/Output"), recursive = FALSE, full.names = TRUE)
    
    # On initialise une liste qui contiendra les informations pour chaque SAP
    liste_sap <- list()
    
    # Pour chaque SA processing
    for (i_sap in seq_along(SAP_nom)) {
        
        print(paste0("SAP n°", i_sap, " : ", SAP_nom[i_sap]))
        
        # Lecture de la matrice demetra_m
        demetra_m <- read.csv(file = paste0(SAP_dossier[i_sap], "/demetra_m.csv"), sep = ";",
                              dec = ",", stringsAsFactors = FALSE, na.strings = c("NA","?"))
        head(demetra_m)
        colnames(demetra_m)
        # Noms de série
        demetra_m$series <- gsub("(^ *)|(* $)", "", gsub("(^.* \\* )|(\\[frozen\\])","", demetra_m$X))
        
        # On recupere les noms des series et l'aic corrige = aicc
        df_res_jeu_cjo <- demetra_m[, c("series", "aicc")]
        
        # recuperation coefficients et p values des regs cjo
        # regs cjo : on repere l'emplacement des noms de variables qui commencent par td et finissent par un chiffre 
        td_var_index <- grep(pattern = "^td\\.\\d\\.$", x = colnames(demetra_m))
        length(td_var_index) #nombre de variables
        td_var_index
        colnames(demetra_m)[td_var_index]
        
        # s'il y en a : 
        if (length(td_var_index) > 0) {
            
            # cf orga table : nom var / coeff reg / t stat / p value
            td_var_names <- colnames(demetra_m[1, td_var_index])
            # td_var_names <- gsub(pattern = "^.* ", replacement = "", x = demetra_m[1, td_var_index])
            
            # td var i va donner pour chaque var cjo stockée dan td_var, les numeros des colonnes à recuperer 
            # des colonnes à recuperer : il y en a 2 : coeff reg et p value
            index_p_value <- td_var_index + 1
            index_coeff <- td_var_index + 2
            
            # Création d'une table avec coeff + p_value
            td_var_table <- demetra_m[, c(rbind(index_coeff, index_p_value))]
            
            colnames(td_var_table)[seq_len(length(td_var_index) * 2)] <- paste0(rep(td_var_names, each = 2), 
                                                                           rep(c("_(pvalue)","_(coeff)"), length(td_var_index)))
            
            # si on a un seul regresseur JO
            if (length(td_var_names) > 1) {      
                # Ajout du ftest (= F joint)
                td_var_table <- cbind(td_var_table, "F-test" = demetra_m$td.ftest)
            }
            
            # Nouveaux indicateurs ajoutés au BQ = aicc + vars cjo 
            df_res_jeu_cjo <- cbind(df_res_jeu_cjo, td_var_table)
            head(df_res_jeu_cjo)
        }
        
        # Ajout test EJOR (effets jours ouvrables)
        # on repere leur position dans le nom des variables et on prend la valeur (pas la modalité : good..)
        res_td_var_index <- c(grep("f.test.on.sa..td.", colnames(demetra_m)),
                              grep("f.test.on.i..td.", colnames(demetra_m)))
        res_td_var_names <- colnames(demetra_m)[res_td_var_index + 1]
        res_td_var_names
        head(demetra_m[, res_td_var_names])
        
        # sélection
        ejor <- demetra_m[, res_td_var_names]
        colnames(ejor) <- c("Res_TD_sa", "Res_TD_i")
        head(ejor)
        
        # Regroupement et ajout des EJOR
        df_res_jeu_cjo <- cbind(df_res_jeu_cjo,ejor)
        head(df_res_jeu_cjo)
        
        # ajout du schema
        df_res_jeu_cjo$schema <- ifelse(demetra_m$log == 0, "Additif", "Multiplicatif")
        head(df_res_jeu_cjo)
        
        # ajout du modele arima 
        df_arima <- JDCruncheR:::extractARIMA(demetra_m)
        head(df_arima) 
        
        # table finale resultats
        df_res_jeu_cjo$`Modele ARIMA` <- df_arima[, 7]
        head(df_res_jeu_cjo)
        
        # creation des notes
        note_td_res_on_sa <- (df_res_jeu_cjo$Res_TD_sa < .05) * 2
        note_td_res_on_i <- (df_res_jeu_cjo$Res_TD_i < .05) * 1
        note_td_res_on_sa[is.na(note_td_res_on_sa)] <- 0
        note_td_res_on_i[is.na(note_td_res_on_i)] <- 0
        
        df_res_jeu_cjo$note <- note_td_res_on_sa + note_td_res_on_i
        df_res_jeu_cjo$aicc <- trunc(df_res_jeu_cjo$aicc)                             
        head(df_res_jeu_cjo)
        
        # Changement du nom avec ajout du jeu de régresseurs
        colnames(df_res_jeu_cjo)[-1] <- paste0(colnames(df_res_jeu_cjo)[-1], "_", SAP_nom[i_sap])
        head(df_res_jeu_cjo)
        
        # Ajout à une liste de toutes les tables des SA processing du WS
        liste_sap <- c(liste_sap, list(df_res_jeu_cjo))
    }
    
    names(liste_sap) <- SAP_nom
    liste_coeff_WS <- c(liste_coeff_WS, list(liste_sap))
    
}

names(liste_coeff_WS) <- WS_name

tot_PN <- purrr::reduce(.x = liste_coeff_WS$WS_PN_choix_cjo, .f = merge, by = "series", all = TRUE)
tot_GN <- purrr::reduce(.x = liste_coeff_WS$WS_GN_choix_cjo, .f = merge, by = "series", all = TRUE)
