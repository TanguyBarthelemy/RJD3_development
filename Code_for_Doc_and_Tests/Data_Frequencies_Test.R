

# Code du webinar ---------------------------------------------------------

# P2 : Seasonal adjustment in R with JD+ ----------------------------------

ipi <- read.csv2("C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Data/IPI_nace4.csv")
ipi$date <- as.Date(ipi$date, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)
# creating a TS object from a data frame 
y_raw <- ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(2019, 6))
y_new <- ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(2019, 9))

# 3 year length ?

### att pbs de declaration (cf GUI possibilites avec periodes)
y12<-ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(1996, 12))
y12

## comprendre le remplissage 
y6<-ts(ipi[, "RF3030"], frequency = 6, start = c(1990, 1), end = c(1996, 12))
y6

y4<-ts(ipi[, "RF3030"], frequency = 4, start = c(1990, 1), end = c(1996, 12))
y4

y3<-ts(ipi[, "RF3030"], frequency = 3, start = c(1990, 1), end = c(1996, 12))
y3

y2<-ts(ipi[, "RF3030"], frequency = 2, start = c(1990, 1), end = c(1996, 12))
y2

## mise en tableau ou qqch 


# X13 v3
sa_x13_v3<- rjd3x13::x13(y6, spec = "rsa5c")
sa_x13_v3$result$final$d11final

# TS v3
sa_ts_v3<- rjd3tramoseats::tramoseats(y2, spec = "rsafull")
sa_ts_v3$result$final$sa

# X13 v2
sa_x13_v2 <- RJDemetra::x13(y2, spec = "RSA5c")

# TS v2
sa_TS_v2 <- RJDemetra::tramoseats(y3, spec = "RSAfull")
sa_TS_v2$final$series

# Tout est implémenté (jusqu'à un certain niveau)

# 1 voir contraintes de longueur min de donnees

# 2 voir qd un module auto n'est pas actif 
## ex pas d'estimation de TD
## que du airline

