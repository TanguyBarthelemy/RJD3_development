################################################################################
#                                                                              #
#                         Expertiser 1 série dans N WS                         #
#                                                                              #
################################################################################

# Expertise "couches" en mode campagne annuelles
# Structure programme
# - recup series
# - graphiques

# library("RJDemetra")
# dplyr sinon LAG ne marche pas
# attention : masque compute
# library("dplyr")
# library("xlsx")
# library("zoo")

# Chargement packages -----------------------------------------------------

library("RJDemetra")
library("ggplot2")


# Fonctions auxiliaires --------------------------------------------------------

source("./R/utilities.R")


# Paramètres -------------------------------------------------------------------

# CHOIX SERIE
serie_a_exp <- "RF1072"

# liste noms WS manipulables dans le meme ordre (vocabulaire parlant) = COUCHES
liste_ws <- c("auto", "work")

# Liste de WS utilises : 2 couches possibles
ch_ws_auto <- "./workspace_automatique/industrie.xml"
ch_ws_work <- "./workspace_de_travail/industrie.xml"

# liste chemins ws (souplesse = les chemins n'ont pas forcement le meme pattern)
liste_ch_ws <- c(ch_ws_auto, ch_ws_work)
names(liste_ch_ws) <- liste_ws

liste_ch_ws


####################################################################################
####### Recuperation des composantes d'UNE SERIE dans chacun des WS de la liste (predefinie)
### on obtient une version de chaque composante par ws, suffixee avec nom du ws
for (k in seq_along(liste_ch_ws)) {
    ws_name <- liste_ws[k]
    cat(
        paste0("\nWS ", ws_name, " en cours... ", k, "/", length(liste_ch_ws)),
        "\n"
    )

    suffixe <- paste0("_", liste_ws[k])
    printt(suffixe)

    # chemins WS
    ch <- liste_ch_ws[k]
    printt(ch)

    # LOAD des WS UN par UN et cumul
    ws <- load_workspace(ch)
    compute(ws)
    trm <- get_object(ws, 1)
    series <- get_all_objects(trm)
    sa <- get_object(trm, which(names(series) == serie_a_exp)) #### CHOIX serie dans WS####
    model_sa <- get_model(sa, ws)

    # SERIES Main
    tsa <- model_sa$final$series
    head(tsa)
    tail(tsa)
    # de l'objet TS vers un data frame
    d <- data.frame(date = zoo::as.Date(time(tsa)))
    head(d)
    tail(d)
    df <- as.data.frame(tsa)
    df <- cbind(d, df)
    tail(df)

    # on stocke la derniere obs realisee
    last_obs <- df[nrow(df), 1]
    assign(paste0("last_obs", suffixe), last_obs)
    # afficher la derniere obs realisee
    print(paste0("dernier point (non prevu) du ws : ", df[nrow(df), 1]))

    ## TX CR SERIES MAIN Y et SA
    df_tx <- rbind(NA, sapply(df[, c(2, 3)], tx_cr))
    colnames(df_tx) <- paste0("tx_", colnames(df_tx))
    df <- data.frame(df, df_tx)
    head(df)
    tail(df)

    ########
    # series du PRE PROCESSING
    pre <- model_sa$regarima$model$effects
    # data frame
    d <- data.frame(date = zoo::as.Date(time(pre)))
    head(d)
    tail(d)
    df_pre <- as.data.frame(pre)
    df_pre <- cbind(d, df_pre)
    head(df_pre)
    tail(df_pre)
    df_pre <- df_pre[, -c(4, 5)] # on enleve effet de paques et fetes mobiles

    #  AJUSTEMENT y_lin : prendre l exponenetielle en cas de modele multiplicatif
    m <- model_sa$regarima$model$spec_rslt
    schema <- data.frame(Log = m[, 3])
    ifelse(schema, df_pre[, 2] <- exp(df_pre[, 2]), df_pre[, 2] <- df_pre[, 2])
    # la variable "tde" trading day effect est renommee "cal"
    colnames(df_pre)[3] <- "cal"
    # ajout d'un coeff saiso pur calcule (le D10 peut etre faux )
    # ifelse(schema, df_pre$sp <- df_pre$s/df_pre$cal, df_pre$sp <- df_pre$s-df_pre$cal)
    head(df_pre)
    tail(df_pre)

    # merge composantes finales et predj
    df_full <- merge(df, df_pre, by = "date", ALL = TRUE)
    tail(df_full)
    # pour chaque WS de la boucle
    # collage des suffixes correspondant au WS : renommage des variables (sauf la date [-1])
    suffixe
    names(df_full)[-1] <- paste0(names(df_full)[-1], suffixe)
    head(df_full)

    # cumul a chaque boucle = ajout d un ws
    # intialisation
    if (k == 1) {
        df_rea <- d
    }
    df_rea <- merge(df_rea, df_full, by = "date", all = TRUE)
    # outliers par workspace
    df_out <- as.data.frame(model_sa$regarima$regression.coefficients)
    assign(paste0("df_out", suffixe), df_out)
    # regresseurs cjo par workspace en data frame (il faut les couper..sinon 2030)
    regs_cjo <- model_sa$regarima$specification$regression$userdef$variables$serie
    d1 <- data.frame(date = zoo::as.Date(time(regs_cjo)))
    df_regs_cjo <- as.data.frame(regs_cjo)
    df_regs_cjo <- cbind(d1, df_regs_cjo)
    last_obs
    df_regs_cjo <- df_regs_cjo[df_regs_cjo$date <= last_obs, ]
    tail(df_regs_cjo)
    assign(paste0("regs_cjo", suffixe), regs_cjo)
}


# FIN BOUCLE RECUP)
# series realisees (= non prevues)
# tail(df_rea)

# Affichages graphiques ---------------------------------------------------

#  Brutes a regarder pour les rebasees
df_G <- df_rea[df_rea$date >= "2019-01-01", ]
head(df_G)
ggplot(df_G) +
    ggtitle(paste0("Brutes_", serie_a_exp)) +
    geom_line(aes(x = date, y = y_auto), color = "black") +
    geom_line(aes(x = date, y = y_work), color = "red") +
    scale_x_date(name = "date", date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(name = serie_a_exp)

# LIN
df_G <- df_rea[df_rea$date >= "2021-01-01", ]
head(df_G)
ggplot(df_G) +
    ggtitle(paste0("LIN_", serie_a_exp)) +
    # geom_line(aes(x = date, y = y_lin_old), color = "black")+
    geom_line(aes(x = date, y = y_lin_auto), color = "black") +
    geom_line(aes(x = date, y = y_lin_work), color = "red") +
    scale_x_date(name = "date", date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(name = serie_a_exp)

## SA
# add : graphiques disjoints dans 2 fenetres
# ....df_total ou df_rea selon si on prend les prevs ou pas
df_G <- df_rea[df_rea$date >= "2021-01-01", ]
head(df_G)
ggplot(df_G) +
    ggtitle(paste0("SA_", serie_a_exp)) +
    # geom_line(aes(x = date, y = sa_old), color = "black")+
    geom_line(aes(x = date, y = sa_auto), color = "black") +
    geom_line(aes(x = date, y = sa_work), color = "red") +
    scale_x_date(name = "date", date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(name = serie_a_exp) # tw CR
df_G <- df_rea[df_rea$date >= "2020-01-01", ]
head(df_G)
ggplot(df_G) +
    ggtitle(paste0("Tx-Cr_", serie_a_exp)) +
    geom_line(aes(x = date, y = tx_sa_auto), color = "black") +
    geom_line(aes(x = date, y = tx_sa_work), color = "red") +
    scale_x_date(name = "date", date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(name = serie_a_exp)
