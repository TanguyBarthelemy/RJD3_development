
################################################################################
###                                CVS avec R                                ###
################################################################################

# But : effectuer une CVS directement avec R sans passer par l'interface
#   JDemetra+

# Plan du programme :
#   1) Initialisation de paramètres généraux et imports des packages
#   2) Imports et traitement des données
#   3) Création des modèles d'ajustement saisonniers basics
#   4) Création des modèles d'ajustement saisonniers personnalisés
#   5) Utilisation du package ggdemetra
#   6) Création de rapport automatisés avec rjdmarkdown


# Paramètres et initialisation -------------------------------------------------

# Si RJDemetra n'est pas installé sur votre poste,
# faire tourner la ligne suivante :

# install.packages("RJDemetra", type = "source",
#                  INSTALL_opts = "--no-multiarch" )

library("RJDemetra")
library("zoo")

library("rjdmarkdown")
library("ggdemetra")


# Import, traitement et affichage des données ----------------------------------

## Import ----------------------------------------------------------------------

ipi_df <- read.csv2("./data/IPI_nace4_full.csv", sep = ";", dec = ".")

## Traitement et formattage de certaines colonnes  -----------------------------

ipi_df$date <- base::as.Date(ipi_df$date, format = "%d/%m/%Y")
ipi_df[1:5, 1:10]

# Création d'objets ts
ipi_ts <- ts(subset(ipi_df, select = -date), start = 1990, frequency = 12)

## Affichage -------------------------------------------------------------------

# Afficher la première et derniere obs realisée
first_obs <- head(ipi_df$date, n = 1)
last_obs <- tail(ipi_df$date, n = 1)
print(paste("Premier point (non prevu) du jeu de données :", first_obs))
print(paste("Dernier point (non prevu) du jeu de données :", last_obs))

# Affichage de graphiques
ggplot(ipi_df, aes(date, RF3030)) + geom_line()

# Série à expertiser
serie_a_exp <- "RF3030"

# Création des séries brutes
ipi_3030_df <- subset(ipi_df, select = c(date, RF3030))
ipi_3030_ts <- ipi_ts[, "RF3030"]

serie_brute <- ipi_3030_ts

# Regresseurs cjo
regs_cjo <- read.csv("./data/regs_cjo.csv", sep = ";", dec = ".")
reg1 <- ts(subset(regs_cjo, select = REG1_AC1), start = 1990, frequency = 12)
reg2 <- ts(subset(regs_cjo, select = c(REG2_AC1, REG2_AC2)),
           start = 1990, frequency = 12)
reg3 <- ts(subset(regs_cjo, select = c(REG3_AC1, REG3_AC2, REG3_AC3)),
           start = 1990, frequency = 12)
reg5 <- ts(subset(regs_cjo, select = c(REG5_AC1, REG5_AC2, REG5_AC3,
                                       REG5_AC4, REG5_AC5)),
           start = 1990, frequency = 12)
reg6 <- ts(subset(regs_cjo, select = c(REG6_AC1, REG6_AC2, REG6_AC3,
                                       REG6_AC4, REG6_AC5, REG6_AC6)),
           start = 1990, frequency = 12)

# Variable d'intervention = régresseur externe
reg_externe_df <- read.csv("./data/regresseur_externe.csv",
                           sep = ";", dec = ".")
reg_externe_ts <- ts(reg_externe_df[, -1], start = c(1990, 1), frequency = 12)


# Construction des modèles SA --------------------------------------------------

## Construction de modèles classiques ------------------------------------------

# But : construire la CVS d'une série à expertiser avec le package RJDemetra
#   selon plusieurs méthodes "classiques" :
#       - X13-regarima
#       - TRAMO-SEATS (TST)

print(paste0("La série à expertiser est la série ", serie_a_exp, "."))
window(ipi_3030_ts, start = 2020)

# Désaisonnalisation avec X13
model_sa_x13 <- x13(serie_brute, spec = "RSA5c")

# Désaisonnalisation avec Tramo-Seats
model_sa_tst <- tramoseats(serie_brute, spec = "RSAfull")


### Affichage des résultats ----------------------------------------------------

print(model_sa_x13)
# final components
window(x = model_sa_x13$final$series, start = 2021)
# their forecasts y_f sa_f s_f t_f i_f
print(model_sa_x13$final$forecasts)


### Pre-processing -------------------------------------------------------------

# Pré-ajustement avec X13 REGARIMA
pre_adjust_x13 <- regarima_x13(serie_brute, spec = "RG5c")
print(identical(x = model_sa_x13$regarima, y = pre_adjust_x13))

# Série du pre-ajustement
pre_adjust_ts <- model_sa_x13$regarima$model$effects
window(pre_adjust_ts, start = 2021)

#  AJUSTEMENTS y_lin
# Correction de la série linéarisée (schéma additif ou multiplicatif)
if (isTRUE(model_sa_x13$regarima$model$spec_rslt$`Log transformation`)) {
    pre_adjust_ts[, "y_lin"] <- exp(pre_adjust_ts[, "y_lin"])
}
window(pre_adjust_ts[, "y_lin"], start = 2020)


### DATA VISUALIZATION ---------------------------------------------------------

# monthplot de la série brute
monthplot(serie_brute, xlab = "mois", main = "Série brute")

# Les graphiques au format GUI
plot(model_sa_x13, type_chart = "sa-trend", first_date = c(2017, 1))
plot(model_sa_x13, type = "cal-seas-irr", first_date = c(2017, 1))

layout(matrix(1:6, 3, 2))
plot(model_sa_x13$regarima, ask = FALSE)

# Affichage des SI ratios
plot(model_sa_x13$decomposition, first_date = c(2015, 1), caption = "S-I ratio")

### DIAGNOSTICS ----------------------------------------------------------------

#### Diagnostique du pré-ajustement --------------------------------------------

# regression variables et coefficients
# (effets déterministes : automatiques ou variable user-defined)
# trading days, easter, outliers...
model_sa_x13$regarima$regression.coefficients

# arima coeffs (modele explicite avec noms des coeffs, quick exp)
model_sa_x13$regarima$arima.coefficients

# stats et tests sur les résidus
model_sa_x13$regarima$residuals.stat$tests


#### Diagnostic SA -------------------------------------------------------------

# Decomposition : stats M (ref to Lothian et Mory)
model_sa_x13$decomposition$mstats

# Diagnostiques finaux
# Tests de présence de saisonnalité (X-11)
model_sa_x13$diagnostics$combined_test

# Tests de saisonnalité résiduelle et des effets de calendriers résiduels sur
# les séries CVS et sur l'irrégulier
model_sa_x13$diagnostics$residuals_test

# X-11 Variance decomposition
model_sa_x13$diagnostics$variance_decomposition


## Construction de modèles personnalisés ---------------------------------------

## Paramètres personnalisables
# - Nouvelle spécification
# - Changer période d'estimation (seulement sur le pré-ajustement)
# - Ajout d'outliers
# - Nouveau modèle ARIMA
# - Ajout de regresseurs de calendrier et externes
# - Choix des filtres de décomposition pour X13

# Cadre = pre-ajustement + decomposion


### Modèle 2 -------------------------------------------------------------------

#### Specs ---------------------------------------------------------------------

# Récupérer la précédente spec
# Ici Spec_1 a été définit à partir du model_sa_x13 --> RSA5
spec_1 <- x13_spec(spec = model_sa_x13)

# Changement :
#   - des périodes temporelles d'estimation
#   - ajout d'outliers personnalisés
#   - Imposer un modèle additif
spec_2 <- x13_spec(spec = spec_1,
                   estimate.from = "2004-01-01",
                   usrdef.outliersEnabled = TRUE,
                   usrdef.outliersType = c("LS", "AO"),
                   usrdef.outliersDate = c("2008-10-01", "2018-01-01"),
                   transform.function = "None") # additive model
# le modèle REG-ARIMA sera ré-estimé à partir du  "2004-01-01"
# La décomposition sera faite sur toute la période néanmoins


#### SA processing -------------------------------------------------------------

model_sa_x13_2 <- x13(serie_brute, spec = spec_2)


### Modèle 3 -------------------------------------------------------------------
# Changer le modèle ARIMA

#### Specs ---------------------------------------------------------------------

# Changer le modèle ARIMA à partir de la spec_1
# On impose les ordres (1, d, 1)(0, D, 1),
# Certains coefficients sont fixés :
#   - ar(1) est "arima.p = -0.8"
#   - ma(1) est "arima.q = -0.6"
# L'ordre saisonnier = 0
# Le coefficient de la moyenne mobile saisonnière ma(1) ("arima.bq = 1) n'est
# pas fixé par l'utilisateur ("Undefined")
spec_3 <- x13_spec(spec = spec_1, automdl.enabled = FALSE,
                   arima.p = 1, arima.q = 1,
                   arima.bp = 0, arima.bq = 1,
                   arima.coefEnabled = TRUE,
                   arima.coef = c(-.8, -.6, 0), # 0 stands for not fixed
                   arima.coefType = c(rep("Fixed", 2), "Undefined"))


#### SA processing -------------------------------------------------------------

model_sa_x13_3 <- x13(serie_brute, spec = spec_3)

# Pour voir les paramètres de la spécification
s_arimaCoef(object = spec_3)
# Argument = model ou spec
s_arimaCoef(object = model_sa_x13_3)
# Pour plus de paramètres du modèles ARIMA
s_arima(object = model_sa_x13_3)
# Argument = model ou spec
s_arima(object = model_sa_x13_3)
# Pour accéder aux estimations du modèle ARIMA
model_sa_x13_3$regarima$arima.coefficient


### Modèle 4 -------------------------------------------------------------------
# Variables user-defined --> jeux de calendrier

#### Specs ---------------------------------------------------------------------

# On définit les régresseurs de calendrier
# les régresseurs externes doivent être des ts
spec_4 <- x13_spec(spec = spec_1,
                   tradingdays.option = "UserDefined",
                   tradingdays.test = "None",
                   usrdef.varEnabled = TRUE,
                   usrdef.varType = "Calendar",
                   usrdef.var = reg3)

#### SA processing -------------------------------------------------------------

# Nouveau SA processing
model_sa_x13_4 <- x13(serie_brute, spec = spec_4)
# Les fonctions s_.. permettent d'accéder à d'autres paramètres
# Pour les résultats
model_sa_x13_4$regarima$regression.coefficients


### Modèle 5 -------------------------------------------------------------------
# On définit les régresseurs externes (variables d'intervention)

#### Specs ---------------------------------------------------------------------

# on ajoute le régresseur externe relatif au covid
spec_5 <- x13_spec(spec = spec_1,
                   usrdef.varEnabled = TRUE,
                   # On choisit d'attribuer l'outlier à la composante tendance
                   usrdef.varType = "Trend",
                   # Les régresseurs doivent être au format ts
                   usrdef.var = reg_externe_ts)

#### SA processing -------------------------------------------------------------

# Nouveau SA processing
model_sa_x13_5 <- x13(serie_brute, spec = spec_5)
# Les fonctions s_.. permettent d'accéder à d'autres paramètres
# Pour les résultats
model_sa_x13_5$regarima$regression.coefficients


### Modèle 6 -------------------------------------------------------------------
# Paramétrer les filtres de décomposition X11

#### Specs ---------------------------------------------------------------------

# Moyenne mobile d'Henderson (x11.trendma) --> longueur pour la tendance
# Moyenne mobile saisonnière (x11.seasonalma) --> longueur pour l'extraction
# de la composante saisonnière

# Nouvelle spec
spec_6 <- x13_spec(spec = spec_1, x11.trendma = 23, x11.seasonalma = "S3X9")

#### SA processing -------------------------------------------------------------

# Nouveau sa processing
model_sa_x13_6 <- x13(serie_brute, spec = spec_6)
# Pour accéder aux paramètres
s_x11(model_sa_x13_6)


# Enregistrement spec ----------------------------------------------------------

# On peut enregistrer puis recharger un spec en .Rdata

# Enregistrement
save_spec(object = spec_3,
          file = file.path("./output/", "spec_3_user_def_arima.RData"))

# Chargement
new_spec <- load_spec(file = "./output/spec_3_user_def_arima.RData")

# Etude des différences
print(identical(new_spec, spec_3))
waldo::compare(spec_3, new_spec, max_diffs = Inf)

model_sa_x13_7 <- x13(serie_brute, spec = spec_3)
model_sa_x13_8 <- x13(serie_brute, spec = new_spec)
waldo::compare(model_sa_x13_7, model_sa_x13_8)

# USER DEFINED OUTPUT ----------------------------------------------------------

# Listes des différenc=tes sorties possibles
user_defined_variables("X13-ARIMA")
# ou
user_defined_variables("TRAMO-SEATS")

# Ajout de variable user-defined
# indice des variables
added_user_variables <- user_defined_variables("X13-ARIMA")[
    c(28, 174, 258, 259, 260)]
print(added_user_variables)

# Model avec des user-defined
model_sa_x13_UD <- x13(serie_brute, spec = "RSA5c",
                        userdefined = added_user_variables)

# Comparaison user-defined
names(model_sa_x13$user_defined)
names(model_sa_x13_UD$user_defined)

# get series (in list of TS)
user_defined_output_example <- model_sa_x13_UD$user_defined[1:2]
window(do.call(cbind, user_defined_output_example), start = 2021)


# Using ggdemetra --------------------------------------------------------------

# Graphics for communication

# On reprend les données de l'IPI
ipi_2010_ts <- window(ipi_ts, start = c(2010, 1))
ipi_2010_df <- cbind(time(ipi_2010_ts) |> as.numeric(),
                     ipi_2010_ts |> as.data.frame())
colnames(ipi_2010_df) <- colnames(ipi_df)
head(ipi_2010_df[, 1:6])

# Dans le ggplot general la date peut etre "normale" ou au format numerique
#   issu du "TS"

# Graphique classique avec ggplot2
ipi_RF2740_plot <- ggplot(data = ipi_2010_df,
                          mapping = aes(x = date, y = RF2740)) +
    geom_line() +
    labs(title = "Industrial Production Index (IPI)",
         x = "date", y = "RF2740")
ipi_RF2740_plot

# Création de la spec
spec_RSA3_WD <- RJDemetra::x13_spec("RSA3", tradingdays.option = "WorkingDays")

# Création du graphique
enhanced_plot <- ipi_RF2740_plot +
    # fonction "geom_sa" : ajouts de SA composants
    geom_sa(component = "y_f", linetype = 2,
            spec = spec_RSA3_WD) +
    geom_sa(component = "sa", color = "red") +
    geom_sa(component = "sa_f", color = "red", linetype = 2) +
    # Ajout d'outliers
    geom_outlier(geom = "label_repel",
                 vjust = 4,
                 ylim = c(NA, 65), force = 10,
                 arrow = arrow(length = unit(.03, "npc"),
                               type = "closed", ends = "last")) +
    # Ajout du modèle Arima
    geom_arima(geom = "label",
               x_arima = -Inf, y_arima = -Inf,
               vjust = -1, hjust = -.1,
               message = FALSE)

enhanced_plot

# Ajout des diagnostiques
diagnostics <- c(`Seasonality (combined)` = "diagnostics.combined.all.summary",
                 `Residual qs-test (p-value)` = "diagnostics.qs",
                 `Residual f-test (p-value)` = "diagnostics.ftest")

enhanced_plot +
    geom_diagnostics(diagnostics = diagnostics,
                     ymin = 130, ymax = 200, xmin = 2019,
                     table_theme = gridExtra::ttheme_default(base_size = 6))


# rjdmarkdown ------------------------------------------------------------------

# Préparation de rapports automatiques avec rjdmarkdown


# print_preprocessing() pour le modèle du pré-ajustement
# print_decomposition() pour la décomposition
# print_diagnostics() pour les diagnostiques de qualité d'ajustement saisonnier

# ipi <- RJDemetra::ipi_c_eu[, "FR"]
# jsa_x13 <- RJDemetra::jx13(ipi)

print_preprocessing(model_sa_x13, format = "latex")
print_decomposition(model_sa_x13)
print_diagnostics(model_sa_x13)

# Creation fichier markdown directement
jsa_x13 <- RJDemetra::jx13(serie_brute)

create_rmd(jsa_x13, output_file = "./output/rapport_ipi_3030.Rmd",
           output_format = "pdf_document")
create_rmd(jsa_x13, output_file = "./output/rapport_ipi_3030.Rmd",
           output_format = "html_document")

# Pour ouvrir directement le fichier depuis R (on peut aussi le faire à la main)
browseURL("./output/rapport_ipi_3030.pdf")
