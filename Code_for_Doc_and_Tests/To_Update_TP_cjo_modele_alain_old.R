#######################################
### TP alain et tentative d'extension TRM
#######################################

setwd("Z:/Anna_SMYK/0_R_Team/WG_Input")


# liste fonctions outils
#installer packages remote
# telecharger fichiers internet

# Questions to configure
#


# # # Configurer si besoin le proxy
# # proxy <- "http://MonNNI:MonMotDePasse@proxy-surf.rte-france.com:3128"
# # Sys.setenv(HTTPS_PROXY = proxy)
# remotes::install_github("palatej/rjd3toolkit",
#                         INSTALL_opts = "--no-multiarch")
# remotes::install_github("palatej/rjd3modelling",
#                         INSTALL_opts = "--no-multiarch")
# remotes::install_github("palatej/rjd3sa",
#                         INSTALL_opts = "--no-multiarch")

# les bonnes versions pour le TP

# library(remotes)
# remotes::install_github("palatej/rjd3toolkit@v0.0.7")
# remotes::install_github("palatej/rjd3modelling@v0.0.7")
# remotes::install_github("palatej/rjd3sa@v0.0.7")
# les bonnes versions pour le TP

library(remotes)
remotes::install_github("aqlt/rjd3toolkit@v0.0.7")
remotes::install_github("aqlt/rjd3modelling@v0.0.7")
remotes::install_github("aqlt/rjd3sa@v0.0.7")



#  donnees
fichier <-read.csv2("Z:/Anna_SMYK/0_Etude_SL/Donnees/IPI_nace4.csv")
fichier[1:5,1:10]
# # Ou en téléchargeant le fichier depuis internet :
# fichier <- tempfile(fileext = "xlsx")
# url <- "https://aqlt-formation-rte.netlify.app/data/data_rte.xlsx"
# download.file(url, fichier)
date_deb <- 1990
ipi <- ts(fichier[,-1], start = date_deb,
               frequency = 12)

library(rjd3modelling)
frenchCalendar <- calendar.new()
#  fonctions vs GUI
# fixed day in gui idem (avantage = code)
calendar.fixedday(frenchCalendar, month =  5, day = 8)
# ester related : idem GUI, ici 60 jours apres = pentecote
calendar.easter(frenchCalendar,
                offset = 60)
# holiday = special day in GUI
calendar.holiday(frenchCalendar, "NEWYEAR")

# french calendar complet
frenchCalendar <- calendar.new()
calendar.holiday(frenchCalendar, "NEWYEAR")
calendar.holiday(frenchCalendar, "EASTERMONDAY") # Lundi de Pâques
calendar.holiday(frenchCalendar, "MAYDAY") # 1er mai
calendar.fixedday(frenchCalendar, 5, 8)
calendar.holiday(frenchCalendar, "WHITMONDAY") # Lundi de Pentecôte
calendar.fixedday(frenchCalendar, 7, 14)
calendar.holiday(frenchCalendar, "ASSUMPTION") # Assomption
calendar.holiday(frenchCalendar, "ALLSAINTDAY") # Toussaint
calendar.holiday(frenchCalendar, "ARMISTICE")
str(frenchCalendar)
# Q comment voir contenu de french calendar
# Les régresseurs J0 peuvent être créés à partir de 2 fonctions
# htd() qui permet de les créer à partir d’un calendrier spécifique
# td()
# Q difference
groups <- c(1, 2, 3, 4, 5, 6, 0)
frequency <- 12
start <- c(2000,1)
# fonction htd : a partir d'un calendrier specifique
wkd <- htd(frenchCalendar, frequency = frequency, start = start, length = 12*35,
           groups = groups)
# transfo en time series
wkd <- ts(wkd, start = start, frequency = frequency)
# des -1 bizarres
# ######### comparaison wd
groups <- c(1, 1, 1, 1, 1, 0, 0)
frequency <- 12
start <- c(2000,1)
wkd <- htd(frenchCalendar, frequency = frequency, start = start, length = 12*35,
           groups = groups)
wkd <- ts(wkd, start = start, frequency = frequency)
# avec la fonction built in "td" fondee sur calendrier par defaut _def
# td et htd renvoient des vecteurs numeriques

wkd_def <- td(frequency = frequency, start = start, length = 12*35,
              groups = groups)
wkd_def <- ts(wkd_def, start = start, frequency = frequency)
data <- ts.union(wkd, wkd_def)
str(data)
plot(data, col = c("orange","black"),
     plot.type = "single")
# TP à comparer avec regresseurs insee officiels

############ regresseur LY
leap_year <- function(start = 1990, end = 2030, frequency = 12){
  ly <- ts(0, start = start, end = end, frequency = 12)
  mois_feb <- cycle(ly) == 2
  annees <- trunc(round(time(ly), 3)) # arrondi car parfois des pbs avec fonction time
  # On utilise la définition exacte
  is_ly <- (annees %% 400 == 0) |
    ((annees %% 4 == 0) & (annees %% 100 != 0))
  ly[mois_feb] <- 28 - 28.2425
  ly[mois_feb & is_ly] <- 29 - 28.2425
  # on change si besoin la fréquence
  stats::aggregate(ly, nfrequency = frequency)
}
leap_year(frequency = 12)

# construction des differents jeux
frequency <- 12
start <- c(1990,1)
end = c(2030, 1)
length = (end[1] - start[1]) * 12 + end[2] - start[2]

ly <- leap_year(frequency = frequency, start = start,
                end = end)
reg6 <- htd(frenchCalendar, frequency = frequency, start = start, length = length,
            groups = c(1, 2, 3, 4, 5, 6, 0))
reg5 <- htd(frenchCalendar, frequency = frequency, start = start, length = length,
            groups = c(1, 2, 3, 4, 5, 0, 0))
reg3 <- htd(frenchCalendar, frequency = frequency, start = start, length = length,
            groups = c(1, 2, 2, 2, 2, 0, 0))
reg2 <- htd(frenchCalendar, frequency = frequency, start = start, length = length,
            groups = c(1, 1, 1, 1, 1, 2, 0))
reg1 <- htd(frenchCalendar, frequency = frequency, start = start, length = length,
            groups = c(1, 1, 1, 1, 1, 0, 0))

str(reg1)
regresseurs_JO <- ts(cbind(reg1, reg2, reg3, reg5, reg6),
                     start = start, frequency = frequency)
regresseurs_JO <- ts.union(regresseurs_JO,
                           ly)
colnames(regresseurs_JO) <- c("REG1_semaine",
                              sprintf("REG2_%s", c("lundi_a_vendredi", "samedi")),
                              sprintf("REG3_%s", c("lundi", "mardi_a_vendredi")),
                              sprintf("REG5_%s", c("lundi", "mardi", "mercredi", "jeudi", "vendredi")),
                              sprintf("REG6_%s", c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi")),
                              "leap_year")
# effet paques
# dans gui : duration :1:20 en bas ..massage erreur : 1:25...1:25 a l'air possible

# Q pas d'exemple d'effet de paques dans TP

# utilisation des regrsseurs via Rjdemetra
# Dans RJDemetra, pour utiliser nos régresseurs jours ouvrables personnalisés, il faut :
# - creer sa propre spécification (fonctions x13_spec() ou  regarima_spec_x13())
# - en utilisant l’option usrdef.varEnabled = TRUE, en spécifiant les regresseurs dans usrdef.var
# - en indiquant que les regresseurs sont des regresseurs calendaires avec l’option usrdef.varType = "Calendar"

library(RJDemetra)
ipi_fr <- ipi_c_eu[, "FR"]
# On arrête la série en décembre 2019 pour éviter les changements de résultats
# liés aux futures actualisation des données de RJDemetra
ipi_fr <- window(ipi_fr, end = c(2019, 12))
# on garde le jeu reg6
wkd <- regresseurs_JO[,c(grep("REG6", colnames(regresseurs_JO), value = TRUE),
                         "leap_year")]
# Pour simplifier l'output, on enlève le "REG6_"
# mais ce n'est pas obligatoire
colnames(wkd) <- gsub("REG6_", "", colnames(wkd))
myspec1 <- regarima_spec_x13(spec = "RG5c",
                             usrdef.varEnabled = TRUE,
                             usrdef.var = wkd,
                             usrdef.varType = "Calendar",
                             easter.enabled = FALSE)
myreg1 <- regarima(ipi_fr, myspec1)
summary(myreg1)

# ### TEST contraintes lineaires avec librairie car
# package inconnu
library(car)
linearHypothesis(myreg1,
                 c("lundi","mardi","mercredi","jeudi","vendredi","samedi"),
                 c(0, 0, 0, 0, 0, 0), test = "F")
# 2eme test
linearHypothesis(myreg1,
                 c("lundi = mardi","mardi = mercredi","mercredi = jeudi","jeudi = vendredi"),
                 test = "F")

# EJOR : effet de jours ouvrables residuels
# test effectué après la décomposition : sur la composante sa ou i
# tests sont disponibles dans le sous-objet .$diagnostics
# (“f-test on sa (td)” et “f-test on i (td)”) :
myspec1_sa <- x13_spec(spec = "RSA5c",
                       usrdef.varEnabled = TRUE,
                       usrdef.var = wkd,
                       usrdef.varType = "Calendar",
                       easter.enabled = FALSE)
mysa <- x13(ipi_fr, myspec1_sa)
# On retrouve d'ailleurs la partie regarima
# summary(mysa$regarima)
mysa$diagnostics

# JDEMETRA +vs rjdemetra 3
# Sous JDemetra+, les tests affichés portent sur les
# 8 dernières années et dans RJDemetra sur la série entière


# Pour reproduire les résultats de JDemetra+, utiliser la fonction rjd3sa::td.f().
# Pour le test, trois spécifications différentes sont possibles :
# modele AR
# modele D1
# modele WM

# avec yt pris en logarithme si le schéma est multiplicatif.
# Dans tous les cas (H0):β1=⋯=β6=0 et les regresseurs utilisés
# ne prennent pas en compte le calendrier personnalise que l’on a cree


# Pour l'installer :
# remotes::install_github("palatej/rjd3sa")\
library(rjd3sa)
# Schéma multiplicatif :
mysa$regarima$model$spec_rslt["Log transformation"]
sa <- mysa$final$series[,"sa"]
i <- mysa$final$series[,"i"]
rjd3sa::td.f(log(sa), nyears = 8)
rjd3sa::td.f(log(i), nyears = 8)


# Résultats différents sur l'ensmeble de la série
rjd3sa::td.f(log(sa), nyears = 0)
rjd3sa::td.f(log(i), nyears = 0)

extract_sa_cmp <- function(x, comp = "sa", forecast = FALSE){
  # valeurs possibles pour comp : y, t, sa, s, i
  jmodel <- suppressWarnings(jx13(get_ts(x), x13_spec(x)))
  jres <- jmodel$result@internal$getResults()
  jres <- new(Class = "X13_java", internal = jres)
  if (forecast) {
    s_forecast = "_f"
  } else {
    s_forecast = ""
  }
  RJDemetra:::result(jres,
                     sprintf("decomposition.%s_cmp%s",
                             comp,
                             s_forecast))
}
sa <- extract_sa_cmp(mysa, "sa")
i <- extract_sa_cmp(mysa, "i")

# Pour mettre tous les résultats sous forme de matrice :
t(simplify2array(
  list(rjd3sa::td.f(log(sa), nyears = 8),
       rjd3sa::td.f(log(i), nyears = 8),
       rjd3sa::td.f(log(sa), nyears = 0),
       rjd3sa::td.f(log(i), nyears = 0))
))

# Une option serait de couper les régresseurs J0 en deux
td_reg_post_2003 <- td_reg_pre_2003  <-
  regresseurs_JO[,grep("REG6", colnames(regresseurs_JO))]
window(td_reg_pre_2003, end = c(2002, 12)) <- 0
window(td_reg_post_2003, start = c(2003, 1)) <- 0
wkd2 <- ts.union(td_reg_pre_2003, td_reg_post_2003,
                 leap_year(frequency = 12))
colnames(wkd2) <- c(paste0(c("lundi","mardi","mercredi","jeudi","vendredi",
                             "samedi"),"_av2003"),
                    paste0(c("lundi","mardi","mercredi","jeudi","vendredi",
                             "samedi"),"_ap2003"),"leap_year")

myspec2_sa <- x13_spec(spec = "RSA5c",
                       usrdef.varEnabled = TRUE,
                       usrdef.var = wkd2,
                       usrdef.varType = "Calendar",
                       easter.enabled = FALSE)
mysa2 <- x13(ipi_fr, myspec2_sa,
             userdefined = c("diagnostics.td-sa-last", "diagnostics.td-i-last"))
summary(mysa2$regarima)


