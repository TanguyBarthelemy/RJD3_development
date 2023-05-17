##################### French births

# artefact  : code with different params
### TPs with dplyr, tidyr, stats descp

################ questions and steps
## pb transfo en log ou pas : test aic
### stationnarité
## graphs 
### arima residuals : ljung box
## stationnarite
## quality checks
### seasonality tests
##### canova H, others 
### spectral analysis 

############ Table of contents 
## monthly
#### report from GUI tests (end of code)

## daily
##### report from gui tests (should help on calendar) ?
## list Q to JP exploring interface v3
## what to expect from reconciliation ?
# compare : SA on monthly data and compare to daily sa reaggragated 


# Chargement packages -----------------------------------------------------

library("tidyverse")
library("rjd3toolkit")
library("kableExtra")


# French daily births 1968 2016 

# compare with UK births $
# compare with monthly data

### cleaned time series with indicators
df_daily <- read.csv2("./data/TS_daily_births_franceM_1968_2016.csv")
str(df_daily)
# qd lecture csv perte du format date : evitable ?

# Modification de la table

df_daily <- df_daily |> 
    mutate(
        births = nb_naiss, 
        date = as.Date(date, format = "%Y-%m-%d"), 
        id.dom = format(date, "%d") |> as.numeric(), 
        id.dow = format(date, "%a"), 
        id.moy = format(date, "%b") |> gsub(replacement = "", pattern = ".", fixed = TRUE)
    )

str(df_daily)


#### tout en log
df_daily <- df_daily |> 
    mutate(log_births = log(births))

# # en prenant toute la periode saiso residuelle
# # filtrage 
# df_daily <- df_daily[df_daily$date >= "2000-01-01", ]


############# Philosophy
# when a series contains multiple periodicities, 
# we remove them iteratively (using in one step the 
# seasonally adjusted series of the previous step), 
# starting with the highest frequency
# rationale for this : 
# 1 final seasonal component is the sum/product of the different seasonal 
# components s = s1 + s2 + ..
# final trend/irregular/sa are the trend/irregular/sa of the final step.

#######################
# TS plot en ggplot
ggplot(df_daily) + 
    geom_line(aes(date, births), linewidth = .1) + 
    labs(x = "", y = "")

## zoom 2 ans 
df_daily2 <- df_daily[df_daily$date >= "2015-01-01", ]
cut <- which(df_daily2$date == "2016-01-01")
# TS plot en ggplot
ggplot(df_daily2) + 
    geom_line(aes(date, births), linewidth = .1) + 
    geom_vline(xintercept = as.numeric(df_daily2$date[cut]), col = "red") + 
    labs(x = "", y = "")
#geom_vline(xintercept = as.numeric(mydata$datefield[120]), linetype = 4)

## zoom 2 mois
df_daily3 <- df_daily[df_daily$date >= "2016-11-01", ]


# TS plot en ggplot
ggplot(df_daily3) + 
    geom_line(aes(date, births), linewidth = .1) + 
    labs(x = "", y = "")



# Box plot (grouped by day of week)
### attention ordre jours
df_daily <- df_daily |> 
    mutate(sem = recode(
        id.dow,
        "dim." = "D7",
        "lun." = "D1",
        "mar." = "D2",
        "mer." = "D3",
        "jeu." = "D4",
        "ven." = "D5",
        "sam." = "D6"))


table(df_daily$sem)

table(df_daily$day)

ggplot(df_daily) + 
    geom_boxplot(aes(sem, births), outlier.shape = NA) + 
    labs(x = "", y = "") + 
    theme_classic() + 
    theme(panel.grid.major.y = element_line(color = "black", linewidth = .5))

# ggplot(df) + 
#   geom_boxplot(aes(as.factor(id.hour.of.day), obs)) + 
#   facet_wrap(~ as.factor(id.day.of.week)) # muliples frames
# where
# id.hour.of.day = lubridate::hour(df$date)
# and
# id.day.of.week = lubridate::wday(df$date, week_start = 1, label = TRUE, abbr = TRUE, locale = "US")




# TP distrib selon 3 periodes : 1968-1981 1981-19... 


# MOY month of year 

df_daily <- df_daily |> mutate(
    mois = recode(id.moy, 
                  "janv" = "M01", 
                  "févr" = "M02", 
                  "mars" = "M03", 
                  "avr" = "M04", 
                  "mai" = "M05", 
                  "juin" = "M06", 
                  "juil" = "M07", 
                  "août" = "M08", 
                  "sept" = "M09", 
                  "oct" = "M10", 
                  "nov" = "M11", 
                  "déc" = "M12"))
table(df_daily$id.moy)
table(df_daily$mois)

# Box plot (grouped by day of month) ordonner facteurs
ggplot(df_daily) + 
    geom_boxplot(aes(as.factor(id.dom), births), outlier.shape = NA) + 
    labs(x = "", y = "") + 
    theme_classic() + 
    theme(panel.grid.major.y = element_line(color = "black", linewidth = .5))

# Box plot (grouped by month of year)
ggplot(df_daily) + 
    geom_boxplot(aes(as.factor(mois), births), outlier.shape = NA) + 
    labs(x = "", y = "") + 
    theme_classic() + 
    theme(panel.grid.major.y = element_line(color = "black", linewidth = .5))

## analyses supplementaires
#### last day of month or end of month effect ?

### jours feriés vs autres jours : comparer en box plots
##TP summary group by

### extraction s patterns par dates
h <- substr(as.character(df_daily$date), 6, 10)

df_daily <- df_daily |> 
    mutate(md = substr(as.character(date), 6, 10))

c3 <- df_daily
# raw s and sa values by holiday
c1j <- c3[c3$md == "01-01", ] 
c31dec <- c3[c3$md == "12-31", ] 
c25dec <- c3[c3$md == "12-25", ]
c15 <- c3[c3$md == "08-15", ]

## Canova H from 
ch.sp <- 2:367 # Seasonal periodicities

df_ch <- data.frame(
    sp = ch.sp, 
    ch.raw = rjd3toolkit::seasonality_canovahansen(df_daily$births, 
                                              p0 = min(ch.sp), p1 = max(ch.sp), 
                                              np = max(ch.sp) - min(ch.sp) + 1, original = TRUE), 
    ch.log = rjd3toolkit::seasonality_canovahansen(df_daily$log_births, 
                                              p0 = min(ch.sp), p1 = max(ch.sp), 
                                              np = max(ch.sp) - min(ch.sp) + 1, original = TRUE), 
    ch.dlg = rjd3toolkit::seasonality_canovahansen(diff(df_daily$log_births, lag = 1, differences = 1), 
                                              p0 = min(ch.sp), p1 = max(ch.sp), 
                                              np = max(ch.sp) - min(ch.sp) + 1, original = TRUE))

# Significant periodicities (Harvey, 2001, Table I(b))

which(df_ch$ch.raw > .211) + 1 # 10% level of significance 
which(df_ch$ch.raw > .247) + 1 #  5% level of significance
which(df_ch$ch.raw > .329) + 1 #  1% level of significance

# Barplot

ggplot(df_ch) + 
    geom_col(aes(sp, ch.raw), linewidth = .25) + 
    labs(x = "Periodicity (in days)", y = "") + 
    ggthemes::theme_hc()


# Periodogram 
### voir si tools dans jd + 
# ts_raw <- ts(df_raw[, 2], start = c(2004, 1), frequency = 12) : pb as freq = 365.25
## avec start end ?
#ts_raw <- ts(df_raw[, 2], start = c(2004, 1), frequency = 12)

# 178 obs, 1/178 = .05
# 178/12 = 14.8 ans
# p mensuelle ? en fait annuelle = month in a year (moy) f = 14.8/178 = .0831

# pas obligatoire d"avoir un objet TS
y <- df_daily$log_births
p <- TSA::periodogram(y)

p$bandwidth
p$spec # valeur ordonnée
p$freq[which.max(as.vector(p$spec))]
#0.08333333 pas .0831 : mensuel (ipi)
# ici .1428498 = 1/7

#les 5 plus grosses frequences 
## positions (données par order)
ab <- order(-p$spec)[1:5]
ab
#tableau des frequences et valeurs 
fr <- rbind(p$freq[ab], p$spec[ab], p$freq[ab]*length(y))
# comprendre ce tableau 
rownames(fr) <- c("Frequence", "Spectre", "cycle")
p$freq[ab]
print(fr)


# Spectral density
n.freq <- 10000 # roughly n obs/2 ?
df_daily.spec <- data.frame(freq = .5 * seq(1 / n.freq, 1, length.out = n.freq), 
                            ar300 = spectrum(df_daily$births, 
                                             plot = FALSE, n.freq = n.freq, 
                                             method = "ar", order = 300)$spec, 
                            ar300d = spectrum(diff(df_daily$births, lag = 1, differences = 1), 
                                              plot = FALSE, n.freq = n.freq, 
                                              method = "ar", order = 300)$spec)
# clarify this one 
# AR spectrum plot
### questions on harmonics (smth to show to JP)
ggplot(df_daily.spec) + 
    geom_vline(xintercept = 1:3 / 365.25, col = "red") + # DOY 
    geom_vline(xintercept = 1:3 / 7, col = "blue") + # weekly : harmonics
    geom_vline(xintercept = 1:3 / (365.25/12), col = "yellow") + # day of month 30.44
    geom_line(aes(freq, ar300), linewidth = .1, linetype = 2) + 
    geom_line(aes(freq, ar300d), linewidth = .1) + 
    labs(x = "", y = "") + 
    scale_y_log10(breaks = c(.001, .01, .1, 1, 10, 100, 1000), 
                  labels = c("0.001", "0.01", "0.1", "1", "10", "100", "1000"))

# clarify this one 
# AR spectrum plot
### questions on harmonics (smth to show to JP)
ggplot(df_daily.spec) + 
    geom_vline(xintercept = 1 / 365.25, col = "red") + # DOY 
    geom_vline(xintercept = 1:3 / 7, col = "blue") + # weekly : harmonics
    geom_vline(xintercept = 1:3 / (365.25 / 12), col = "yellow") + # day of month 30.44
    geom_line(aes(freq, ar300), linewidth = .1, linetype = 2) + 
    geom_line(aes(freq, ar300d), linewidth = .1) + 
    labs(x = "", y = "") + 
    scale_y_log10(breaks = c(.001, .01, .1, 1, 10, 100, 1000), 
                  labels = c("0.001", "0.01", "0.1", "1", "10", "100", "1000"))


### general periodicity questions
#### how to choose vs box-plots, spectrum and or tests 
# example exact weekly periodicities in testx11.R : 365/7 and not 7
# just not the same : 52 weeks : week n°k, year n is alike to week n°k year p
# depends on data : 365/7 no sense for daily, but for weekly data
# cf tableau beamer DL
# hint : how often does a given observation come back

## x11 parameters questions 
# cf exemple JP with edf 
# p = 7 h = 9
# p = 30.43 (Dom) h = 65 ! pas tres compréhensible vu les deux autres choix 
# p = 365, 25 h = 367

## bias correction when log (cf prog testbiais)


################ setting calendar variables 

## ordre (anyway) : fixedday 2 easter 3 holiday
frenchCalendar <- national_calendar(days = list(
    fixed_day(7, 14), # Fete nationale
    fixed_day(5, 8, validity = list(start = "1982-05-08")), # Victoire 2nd guerre mondiale
    special_day('NEWYEAR'), # Nouvelle année
    special_day('CHRISTMAS'), # Noël
    special_day('MAYDAY'), # 1er mai
    special_day('EASTERMONDAY'), # Lundi de Pâques
    special_day('ASCENSION'), # attention +39 et pas 40 jeudi ascension
    special_day('WHITMONDAY'), # Lundi de Pentecôte (1/2 en 2005 a verif)
    special_day('ASSUMPTION'), # Assomption
    special_day('ALLSAINTSDAY'), # Toussaint
    special_day('ARMISTICE'))
)

frCal_2005 <- weighted_calendar(list(frenchCalendar), 0.5)
final_cal <- chained_calendar(frenchCalendar, frCal_2005, break_date = "2005-05-01")

#### pb "type" and non working days in daily data ?
q <- holidays(frenchCalendar, "1968-01-01", length = length(df_daily$births), type = "All", 
                             nonworking = 7L)
## j"hesite entre "Skip" et "All"
## ici revoir R doc pour ordre 

# plot(apply(q, 1, max))
str(q) # structure = MATRIX, on doit on rentrer ça dans fonction : yes + dimensions right
regs_cjo <- as.data.frame(q)
str(regs_cjo)
# verif pour coller noms 
r <- regs_cjo |> dplyr::filter(V2 == 1)
r
### names in english
colnames(regs_cjo) <- c("14th_july", "8th_may", "1st_jan", "Xmas", "1st_may", 
                        "asc", "east_mon", "pen_mon", 
                        "15th_aug", "1st_nov", 
                        "11th_nov")
length(colnames(regs_cjo))


##################### PRE TREATMENT :fractional Airline model
# just p = 7 (p = 365 mvt lent)

# attention param ndiff a renseigner : une reguliere + une saisonniere = 2

pre.mult <-  rjd3highfreq::fractionalAirlineEstimation(
    y = df_daily$log_births, 
    x = q, # q = regs de calendrier
    periods = 7, 
    ndiff = 2, ar = FALSE, mean = FALSE, 
    outliers = c("ao", "wo"), criticalValue = 0, 
    precision = 1e-9, approximateHessian = TRUE)

pre.mult_cal <-  rjd3highfreq::fractionalAirlineEstimation(
    y = df_daily$log_births, 
    x = q, # q = regs de calendrier
    periods = 7, 
    ndiff = 2, ar = FALSE, mean = FALSE, 
    # outliers = c("ao", "wo"), criticalValue = 0, 
    precision = 1e-9, approximateHessian = TRUE)

# pas de LS, assez improbable, evols lentes de toute façon

# quality opre-adj : LB on residuals ....

# calcul de Ycal approx


## seasonality tests

# op 

#### canova h : rjd3sa : do it here 


####################### Explo OUTPUT pre-adjustment
##### will feed the doc 
#model summary de l"input, yc coeffs/se des vars en input + linearized = final result !
pre.mult$model$y #raw series
pre.mult$model$variables #regression vars yx outliers = vars utilisees 
pre.mult$model$X #regresssors yc outliers
pre.mult$model$b #
pre.mult$model$bcov # matrice des se
pre.mult$model$linearized #linearized series 

length(pre.mult$model$linearized)
length(y)
#likelihood
pre.mult$likelihood$ll
pre.mult$likelihood$ssq
pre.mult$likelihood$neffective #donne -1 : bizarre : anecdote
pre.mult$likelihood$nparams
pre.mult$likelihood$df #?
pre.mult$likelihood$aic
pre.mult$likelihood$aicc #rappel teneur correction : look up 

# estimation
pre.mult$estimation$parameters #coeffs du modele arima 
pre.mult$estimation$score # ???
pre.mult$estimation$covariance #" mat var covar des coeffs du modele ARIMA !! att
##############################################################
############## Decomposition ################################# 
##############################################################
# calendar vs seasonal
##si pas de jours feries : p = 7 traite entierement l"effet type de jours ? oui
# on enleve tout pour ne pas pertubrber p = 7
# pour p = 365 moins evident pour j fixer mais JP recommande de ne pas reinjecter 
# pour JP Xmas = calendar, even in 365

### pb on veut peut etre traiter effet jour feries fixes via decomp p = 365
##### sol1 ne pas le traiter dans reg cal 
##### le reinjecter pares trt p = 7 ?

### sauvegarde 
# save(pre.mult, file = "pre.mult.RData")

# q alain : 
# recup <- load(file = "pre.mult.RData")

# Estimated outlier & calendar effects (coefs, se, student)
### variables are in "model" part as pre-set by the user : pre.mdl$model$variables
## que cal 
regs_mult_cal <-  data.frame(
    "Variable" = pre.mult_cal$model$variables, 
    "Coef" = pre.mult_cal$model$b, 
    "Coef_SE" = sqrt(diag(pre.mult_cal$model$bcov))) |> 
    mutate(Tstat = round(Coef / Coef_SE, 2))
regs_mult_cal
regs_mult_cal$Variable[1:11] <- colnames(regs_cjo)
regs_mult_cal

###
regs_mult <-  data.frame(
    "Variable" = pre.mult$model$variables, 
    "Coef" = pre.mult$model$b, 
    "Coef_SE" = sqrt(diag(pre.mult$model$bcov))) |> 
    mutate(Tstat = round(Coef / Coef_SE, 2))
regs_mult
regs_mult$Variable[1:11] <- colnames(regs_cjo)
regs_mult

# creation dates des outliers détectés 
## TP do it well and creat a QT

# number of outliers
nb_out <- nrow(regs_mult) - length(colnames(regs_cjo))
nb_out
v <- regs_mult$Variable[(length(colnames(regs_cjo)) + 1):nrow(regs_mult)]
out_nom <- substr(v, 1, 2)
out_pos <- substr(v, 4, 9)
out_dates <- as.character(df_daily$date[as.numeric(out_pos)])
outliers <- paste0(out_nom, ".", out_dates)
outliers
regs_mult$Variable[(length(colnames(regs_cjo)) + 1):nrow(regs_mult)] <- outliers
regs_mult

# Estimated MA parameters (coefs, se, student)
### MA parameters are in "estimation" part 
pre.mult$estimation$parameters

data.frame(
    MA_parameter = c("MA1", "DOW"), 
    Coef = pre.mult$estimation$parameters, 
    Coef_SE = sqrt(diag(pre.mult$estimation$covariance)), 
    check.names = FALSE) |> 
    mutate(Tstat = Coef / Coef_SE)


regs_mult <- regs_mult |> mutate(across(where(is.numeric), ~ round(., 2)))

regs_mult
resultats_cal <- regs_mult[1:length(colnames(regs_cjo)), ]
resultats_cal
resultats_out <- regs_mult[(length(colnames(regs_cjo)) + 1):nrow(regs_mult), ]
resultats_out
resultats_out1 <- resultats_out[1:25, ]
resultats_out1
resultats_out2 <- resultats_out[26:nrow(resultats_out), ]
resultats_out2

kbl(resultats_cal) |> 
    kable_styling(bootstrap_options = "striped", 
                  full_width = F)

kbl(resultats_out1) |> 
    kable_styling(bootstrap_options = "striped", 
                  full_width = F)

kbl(resultats_out2) |> 
    kable_styling(bootstrap_options = "striped", 
                  full_width = F)



## Q coeff of seasonal MA close to 1

############### LINEARIZED SERIES
# Storage : linearized series with all options 

df_daily_r <- df_daily |> 
    mutate(
        lin_log = pre.mult$model$linearized, 
        lin = exp(pre.mult$model$linearized), 
        y_cal = exp(pre.mult_cal$model$linearized)) #approx
head(df_daily_r)
# avant decomp
# TP : gerer l"approximation du Y cal 

tail(df_daily_r)

#### 2 premieres annees 
plot(df_daily_r$births[1:366], type = "l")
lines(df_daily_r$lin[1:366], col = "blue")

##

## seasonality tests
#### canova h : rjd3sa

##############################################################
############## Decomposition ################################# 
##############################################################

#### Run X-11 on linearised data
### pb of param choice

##### si le trt des p etait inverse, debile ? par exemple
## l"effet jours feries fixe serait bien traité ?..

############## on raw data : get a sense of seasonal pattern

# Extract DOW pattern highest frequency first

x11.dow <- rjd3highfreq::x11(
    exp(pre.mult$model$linearized), 
    period = 7, # DOW pattern
    mul = TRUE, 
    trend.horizon = 9, # 1/2 Filter length : not too long vs p
    trend.degree = 3, # Polynomial degree
    trend.kernel = "Henderson", # Kernel function
    trend.asymmetric = "CutAndNormalize", # Truncation method
    seas.s0 = "S3X9", seas.s1 = "S3X9", # Seasonal filters
    extreme.lsig = 1.5, extreme.usig = 2.5)   # Sigma-limits

########### recup sa p = 7 et s : notamment for box plots 
df_daily_r <- df_daily_r |> 
    mutate(
        sa_p7 = x11.dow$decomposition$sa, 
        s_p7 = x11.dow$decomposition$s)
head(df_daily_r)
# Extract DOY pattern from DOW-adjusted data : run on SA (pb here outliers and cal not reinjected)
x11.doy <- rjd3highfreq::x11(
    x11.dow$decomposition$sa, 
    period = 365.2425, # DOY pattern
    mul = TRUE, 
    trend.horizon = 371, 
    trend.degree = 3, 
    trend.kernel = "Henderson", 
    trend.asymmetric = "CutAndNormalize", 
    seas.s0 = "S3X3", seas.s1 = "S3X3", 
    extreme.lsig = 1.5, extreme.usig = 2.5)


df_daily_r <- df_daily_r |> 
    mutate(
        sa_p365 = x11.doy$decomposition$sa, 
           s_p365 = x11.doy$decomposition$s, 
           sa_x11 = x11.doy$decomposition$sa, 
           sa_f = y_cal / (s_p365 * s_p7)) # sans outliers ?

df_daily_r <- df_daily_r |> 
    mutate(t_p365 = x11.doy$decomposition$t)

tail(df_daily_r)

######################################### Analysis ###########################
# Box plot (grouped by day of week)
### attention ordre jours
ggplot(df_daily_r) + 
    geom_boxplot(aes(as.factor(id.dow), sa_p7), outlier.shape = NA) + 
    labs(x = "", y = "")



#result = "model" list og lists as usual x
################## explo OUTPUT from X-11
## series from decomposition 
x11.dow$decomposition$y
x11.dow$decomposition$t
x11.dow$decomposition$s
x11.dow$decomposition$i
x11.dow$decomposition$sa
# parameters
x11.dow$parameters$multiplicative
x11.dow$parameters$trend.horizon
x11.dow$parameters$trend.degree
x11.dow$parameters$trend.kernel
x11.dow$parameters$trend.asymmetric
x11.dow$parameters$extreme.lsig
x11.dow$parameters$extreme.usig


#### Check estimated seasonal patterns
# here diagnostics are "just" graphical : IMPROVE
# graphs pb
plot(df_daily$date, x11.dow$decomposition$s, type = "l", xlab = "", ylab = "", axes = FALSE)
par(new = TRUE) 
plot(df_daily$date, x11.doy$decomposition$s, type = "l", col = "red", xlab = "", ylab = "", 
     main = "Estimated DOW (black) and DOY (red) patterns")




# pb estimation debut de periode et filtres asymetriques 
### le debut de periode a l"air tres different pour p = 7 et p = 365

# Zoom in on 2016

zoom <- which(df_daily_r$date >= "2016-01-01")

plot(df_daily_r$date[zoom], x11.dow$decomposition$s[zoom], type = "l", xlab = "", ylab = "", axes = FALSE)
par(new = TRUE)
plot(df_daily_r$date[zoom], x11.doy$decomposition$s[zoom], type = "l", col = "red", xlab = "", ylab = "", 
     main = "Estimated DOW (black) and DOY (red) patterns")

# Zoom in on 1976

zoom <- which(df_daily_r$date >= "1976-01-01" & df_daily_r$date <= "1976-12-31")

plot(df_daily_r$date[zoom], x11.dow$decomposition$s[zoom], type = "l", xlab = "", ylab = "", axes = FALSE)
par(new = TRUE)
plot(df_daily_r$date[zoom], x11.doy$decomposition$s[zoom], type = "l", col = "red", xlab = "", ylab = "", 
     main = "Estimated DOW (black) and DOY (red) patterns")


########### grosses differences de rythmes par periode 
### x11 a fenetre mobile....

###################### HERE
################################### SA with Seats like algo :STEP by STEP


#### Run AMB on RAW data first (as lin cal pbs to solve)
# Extract DOW pattern from linearized series
# step 1

amb.dow <- rjd3highfreq::fractionalAirlineDecomposition(
    pre.mult$model$linearized, # input time series
    period = 7, # DOW pattern
    sn = FALSE, # Signal (SA)-noise decomposition 
    stde = FALSE, # Calculate standard deviations
    nbcasts = 0, nfcasts = 0)  # Numbers of back- and forecasts


##################### Explo OUTput for seats 
#explo model list of lists 
# amb.dow : one more step $ucarima$model (not model for params)
amb.dow$estimation$score # max likelihood solution
# ucarima$model = ?
amb.dow$ucarima$model$name # ? imposer un modele ??
amb.dow$ucarima$model$ar
amb.dow$ucarima$model$delta
amb.dow$ucarima$model$ma
amb.dow$ucarima$model$var
# ucarima$components
amb.dow$ucarima$components
amb.dow$ucarima$complements ##

# decompostion series (pas de pb lin vs final ?)
#amb.dow$decomposition
amb.dow$decomposition$y
amb.dow$decomposition$t
amb.dow$decomposition$s
amb.dow$decomposition$i
amb.dow$decomposition$sa

# amb.dow$likelihood$...les memes que dans linearization
amb.dow$likelihood$neffective # -1 ? que"est 

#amb.dow$estimation 
amb.dow$estimation$parameters # coeffs arima (cf airline estimation = > les memes ??)
amb.dow$estimation$score # ?
amb.dow$estimation$covariance #matrice var covar parametres  


# Extract DOY pattern from DOW-adjusted linearised data
# step 2 en log
amb.doy <- rjd3highfreq::fractionalAirlineDecomposition(
    amb.dow$decomposition$sa, # DOW-adjusted linearised data
    period = 365.2425, # DOY pattern
    sn = FALSE, 
    stde = FALSE, 
    nbcasts = 0, nfcasts = 0)

# Store seasonal components in dataframe
# pay attention to logs
u <- exp(amb.doy$decomposition$sa)
u
df_daily_r <- df_daily_r |> 
    mutate(
        amb.dow = exp(amb.dow$decomposition$s), 
        amb.doy = exp(amb.doy$decomposition$s), 
        amb.sa_f = y_cal / (amb.dow * amb.doy))

tail(df_daily_r)
df_daily_r <- df_daily_r |> 
    mutate(amb_sa = u)


#### Check estimated seasonal patterns : AMB p 7 vs p 365 2-step 
plot(df_daily$date, exp(amb.dow$decomposition$s), type = "l", xlab = "", ylab = "", axes = FALSE)
par(new = TRUE) 
plot(df_daily$date, exp(amb.doy$decomposition$s), type = "l", col = "red", xlab = "", ylab = "") 

#main = "AMB estimated DOW (black) and DOY (red) patterns")


#### y, sa, t 
plot(df_daily$date[zoom], df_daily$births[zoom], type = "l", xlab = "", ylab = "", axes = FALSE)
par(new = TRUE) 
plot(df_daily$date[zoom], df_daily_r$amb.sa_f[zoom], type = "l", col = "red", xlab = "", ylab = "")
lines(df_daily$date[zoom], exp(amb.doy$decomposition$t)[zoom], type = "l", col = "blue", xlab = "", ylab = "")

ggplot(df_daily_r) + 
    geom_line(aes(date, births), linewidth = .1) + 
    geom_line(aes(date, amb.sa_f), linewidth = .1, col = "red") + 
    labs(x = "", y = "") + 
    theme_classic() + 
    theme(panel.grid.major.y = element_line(color = "black", linewidth = .5))




# geom_line(aes(date, t_p365), linewidth = .1, col = "blue") + 
#   labs(x = "", y = "")

## comp x11 Vs 2 step AMB amb
zoom <- which(df_daily_r$date >= "2012-01-01" & df_daily_r$date <= "2016-12-31")
# p = 7
plot(df_daily$date[zoom], exp(amb.dow$decomposition$s)[zoom], type = "l", xlab = "", ylab = "", axes = FALSE)
par(new = TRUE) 
plot(df_daily$date[zoom], x11.dow$decomposition$s[zoom], type = "l", col = "red", xlab = "", ylab = "", 
     main = "Estimated DOW patterns: AMB (black) and X-11 (red)")
# p = 365
plot(df_daily$date, exp(amb.doy$decomposition$s), type = "l", xlab = "", ylab = "", axes = FALSE)
par(new = TRUE) 
plot(df_daily$date, x11.doy$decomposition$s, type = "l", col = "red", xlab = "", ylab = "", 
     main = "Estimated DOW patterns: AMB (black) and X-11 (red)")

### etude distrib coeffs ?

# comp sa
# sa directe sur serie linearisee (pas mal car effet decomp pure)
plot(df_daily$date[zoom], exp(amb.doy$decomposition$sa[zoom]), type = "l", xlab = "", ylab = "", axes = FALSE)
par(new = TRUE) 
plot(df_daily$date[zoom], x11.doy$decomposition$sa[zoom], type = "l", col = "red", xlab = "", ylab = "", 
     main = "SA 2-step AMB (black) and X-11 (red)")

# stat sur sa ?
# demander à K...

# sa avec outliers reinjectes ? useless ?


## zoom 2 ans 
# df_daily_r2 <- df_daily_r[df_daily$date >= "2012-01-01", ]
#   
# # TS plot en ggplot
#   ggplot(df_daily_r2) + 
#   geom_line(aes(date, births), linewidth = .1) + 
#   geom_line(aes(date, sa_f), linewidth = .1, col = "red") + 
#   geom_line(aes(date, t_p365), linewidth = .1, col = "blue") + 
#   labs(x = "", y = "")





## MULTI AMB
amb.multi <- rjd3highfreq::multiAirlineDecomposition(
    pre.mult$model$linearized, # input time series
    periods = c(7, 365.2425), # DOW pattern
    ar = F, 
    stde = FALSE, # Calculate standard deviations
    nbcasts = 0, nfcasts = 0)  # Numbers of back- and forecasts

# composantes
amb.multi$likelihood
amb.multi$decomposition[[1]]
# comp 1
plot(df_daily$date, exp(amb.multi$decomposition[[4]]), type = "l", xlab = "", ylab = "")
plot(df_daily$date, exp(amb.multi$decomposition[[1]]), type = "l", xlab = "", ylab = "")
#

### Check estimated seasonal patterns : AMB p 7 vs p 365 MULTI

plot(df_daily$date, exp(amb.multi$decomposition$s), type = "l", xlab = "", ylab = "", axes = FALSE)
par(new = TRUE) 
plot(df_daily$date, exp(amb.doy$decomposition$s), type = "l", col = "red", xlab = "", ylab = "", 
     main = "Estimated DOW (black) and DOY (red) patterns")

### comp multi vs not multi



## Canova H for final sa 
#sa_f = finale x11
ch.sp = 2:367 # Seasonal periodicities

df_ch = data.frame(
    "sp" = ch.sp, 
    "ch.sa_x11" = rjd3sa::seasonality.canovahansen(df_daily_r$sa_f, 
                                                   p0 = min(ch.sp), p1 = max(ch.sp), 
                                                   np = max(ch.sp) - min(ch.sp) + 1, original = TRUE))


# Significant periodicities (Harvey, 2001, Table I(b))

which(df_ch$ch.sa_x11 > .211) + 1 # 10% level of significance 
which(df_ch$ch.sa_x11 > .247) + 1 #  5% level of significance
which(df_ch$ch.sa_x11 > .329) + 1 #  1% level of significance

# Barplot

ggplot(df_ch) + 
    geom_col(aes(sp, ch.sa_x11), linewidth = .25) + 
    labs(x = "Periodicity (in days)", y = "") + 
    ylim(0, 80) + 
    ggthemes::theme_hc()


# 
# 
# ####################################################################
# 
# ### indicateurs inter methodes à la JMS
# 
# ##############################
# 
# 
# 
# 
# ### residual seaso check 
# #available tests in classical case vs HF
# ### list of classic test = > hf ?
# 
# 
# 
# 
# # With the canonical decomposition, optimal filters are automatically obtained. The trend and the seasonal
# # components are as smooth as possible (all the noise is in the irregular)
# 
# # If we want compute still smoother trend, we should apply - for instance - a long Henderson filter.
# 
# 
# 
# 
# 
# 
# #################################################################################
# ############# Monthly analysis 
# ## via GUI : spectral
# ### pas de pics cjo !! normal ? + tukey : pics décalés legerement ??
# ### reg6_LY? reg1_ ly ? pb pas de bons tests de residus sur toute la periode 
# ### linearizer par moreceaux ?
# 
# ####### compare monthly sa to daily sa aggregation ?
# ## is this the final best evaluation
# ## agregate daily data
# 
# 
# 
# 

# 
# # The main problem with STL and X11 is that we don"t know which filters we should apply 
# # -> future research
# # Huge differences are possible.
# 
# # With the canonical decomposition, optimal filters are automatically obtained. The trend and the seasonal
# # components are as smooth as possible (all the noise is in the irregular)
# 
# # If we want compute still smoother trend, we should apply - for instance - a long Henderson filter.
# 
# 
# 
# 
# 
# 
# #################################################################################
# ############# Monthly analysis 
# ## via GUI : spectral
# ### pas de pics cjo !! normal ? + tukey : pics décalés legerement ??
# ### reg6_LY? reg1_ ly ? pb pas de bons tests de residus sur toute la periode 
# ### linearizer par moreceaux ?
# 
# ####### compare monthly sa to daily sa aggregation ?
# ## is this the final best evaluation
# ## agregate daily data
# 
#  
# 

