##################### French births
### cleaned time series with indicators
df_daily<-read.csv2("Data/TS_daily_births_franceM_1968_2020.csv")
df_daily$date<-as.Date(df_daily$date,format ="%Y-%m-%d")
str(df_daily)
#### tout en log
df_daily= df_daily %>% 
  mutate(log_births = log(births)) %>% 
  mutate(id.dow.US = lubridate::wday(date, week_start = 1, label = TRUE, 
                                  abbr = TRUE, locale = "US")) %>% 
  mutate(id.moy.US = lubridate::month(date, label = TRUE, abbr = TRUE, locale = "US"))

#######################
# TS plot en ggplot
ggplot(df_daily) +
  geom_line(aes(date,births), size = .000001) +
  labs(x = "", y = "")

# Box plot (grouped by day of week)
df_daily<-df_daily%>%mutate(sem=recode(id.dow,
                                       'dim\\.'="Sun",
                                       'lun\\.'="Mon",
                                       'mar\\.'="Tue",
                                       'mer\\.'="Wed",
                                       'jeu\\.'="Thu",
                                       'ven\\.'="Fri",
                                       'sam\\.'="Sat"))
                             
                             
table(df_daily$sem)

table(df_daily$id.dow)
### Q : template                             
pdf(paste(fig.dir, "FB2_boxplots_dow.", plot.type, sep = ""), width = 9, height = 4)

print(ggplot(df_daily) +
        geom_boxplot(aes(as.factor(id.dow.US), births), outlier.shape = NA,
                     fill = "grey") + 
        labs(x = "", y = "")+
        ggthemes::theme_hc()
)
dev.off()

#theme(panel.grid.major.y = element_line(color = "black", size = 0.5))

# MOY month of year 

df_daily<-df_daily%>%mutate(mois=recode(id.moy,
                                       'janv'="Jan",
                                       'févr'="Feb",
                                       'mars'="Mar",
                                       'avr'="Apr",
                                       'mai'="May",
                                       'juin'="June",
                                       'juil'="July",
                                       'août'="Aug",
                                       'sept'="Sep",
                                       'oct'="Oct",
                                       'nov'="Nov",
                                       'déc'="Dec"))
table(df_daily$id.moy)
table(df_daily$mois)

# Box plot (grouped by day of month)
ggplot(df_daily) +
  geom_boxplot(aes(as.factor(id.moy), births), outlier.shape = NA) + 
  labs(x = "", y = "")+
  theme_classic()+
  theme(panel.grid.major.y = element_line(color = "black", size = 0.5))


# Box plot (grouped by month of year)

pdf(paste(fig.dir, "FB2_boxplots_moy.", plot.type, sep = ""), width = 9, height = 4)
print(ggplot(df_daily) +
        geom_boxplot(aes(as.factor(id.moy.US), births), outlier.shape = NA,
                     fill = "grey") + 
        labs(x = "", y = "")+
        ggthemes::theme_hc()
)
dev.off()

## Canova H from 
ch.sp = 2:367 # Seasonal periodicities

df_ch = data.frame(
  "sp" = ch.sp,
  "ch.raw" = rjd3sa::seasonality.canovahansen(df_daily$births,
                                              p0 = min(ch.sp), p1 = max(ch.sp), 
                                              np = max(ch.sp) - min(ch.sp) + 1, original = TRUE),
  "ch.log" = rjd3sa::seasonality.canovahansen(df_daily$log_births,
                                              p0 = min(ch.sp), p1 = max(ch.sp), 
                                              np = max(ch.sp) - min(ch.sp) + 1, original = TRUE),
  "ch.dlg" = rjd3sa::seasonality.canovahansen(diff(df_daily$log_births, lag = 1, differences = 1),
                                              p0 = min(ch.sp), p1 = max(ch.sp), 
                                              np = max(ch.sp) - min(ch.sp) + 1, original = TRUE))

# Significant periodicities (Harvey, 2001, Table I(b))

which(df_ch$ch.raw > .211) + 1 # 10% level of significance 
which(df_ch$ch.raw > .247) + 1 #  5% level of significance
which(df_ch$ch.raw > .329) + 1 #  1% level of significance

# Barplot

ggplot(df_ch) +
  geom_col(aes(sp, ch.raw), size = .25) +
  labs(x = "Periodicity (in days)", y = "") +
  ggthemes::theme_hc()



# Spectral density
n.freq = 10000 # roughly n obs/2 ?
df_daily.spec <- data.frame("freq" = .5 * seq(1 / n.freq, 1, length.out = n.freq), 
                      "ar300" = spectrum(df_daily$births, 
                                         plot = FALSE, n.freq = n.freq, 
                                         method = "ar", order = 300)$spec,
                      "ar300d" = spectrum(diff(df_daily$births, lag = 1, differences = 1), 
                                          plot = FALSE, n.freq = n.freq, 
                                          method = "ar", order = 300)$spec)

# AR spectrum plot
ggplot(df_daily.spec) +
  geom_vline(xintercept = 1:3 / 365.25, col = "red") +    # DOY 
  geom_vline(xintercept = 1:3 / 7, col = "blue") +  # weekly : harmonics
  geom_vline(xintercept = 1:3 /(365.25/12), col = "yellow") +  # day of month 30.44
  geom_line(aes(freq, ar300), size = .1, linetype = 2) + 
  geom_line(aes(freq, ar300d), size = .1) + 
  labs(x = "", y = "") +
  scale_y_log10(breaks = c(.001, .01, .1, 1, 10, 100, 1000), 
                labels = c("0.001", "0.01", "0.1", "1", "10", "100", "1000"))
################ setting calendar variables 

frenchCalendar <- rjd3modelling::calendar.new()
rjd3modelling::calendar.holiday(frenchCalendar, "NEWYEAR")
#rjd3modelling::calendar.holiday(frenchCalendar, "GOODFRIDAY")
rjd3modelling::calendar.holiday(frenchCalendar, "EASTERMONDAY") # Lundi de Pâques
rjd3modelling::calendar.holiday(frenchCalendar, "MAYDAY") # 1er mai
rjd3modelling::calendar.fixedday(frenchCalendar, 5, 8,start="1982-05-08") # since 1982, 2nd world war 
rjd3modelling::calendar.easter(frenchCalendar,offset = 39) ## ascension
rjd3modelling::calendar.holiday(frenchCalendar, "WHITMONDAY") # Lundi de Pentecôte
rjd3modelling::calendar.fixedday(frenchCalendar, 7, 14)  # national holiday
rjd3modelling::calendar.holiday(frenchCalendar, "ASSUMPTION") # Assomption
rjd3modelling::calendar.holiday(frenchCalendar, "ALLSAINTSDAY") # Toussaint
rjd3modelling::calendar.holiday(frenchCalendar, "ARMISTICE") # first world war
rjd3modelling::calendar.holiday(frenchCalendar, "CHRISTMAS") #  25th December


q<-rjd3modelling::holidays(frenchCalendar, "1968-01-01", 
                           length = length(df_daily$births), type="All",
                           nonworking = as.integer(7))

regs_cjo<-as.data.frame(q)
str(regs_cjo)
### names in english
#colnames(regs_cjo)<-c("14th_july","8th_may","asc","1st_jan","east_mon","1st_may","pen_mon",
#                      "15th_aug","1st_nov",
#                      "11th_nov","Xmas")
colnames(regs_cjo)<-c("1st_jan", 
                      #"GoodFriday",
                      "east_mon", "1st_may", "8th_may",
                      "asc", "pen_mon", "14th_july", "15th_aug", "1st_nov", "11th_nov",
                      "Xmas")
n.reg <- length(colnames(regs_cjo))

################## PRE TREATMENT: fractional Airline model


pre.mult<- rjd3highfreq::fractionalAirlineEstimation(df_daily$log_births, 
                                                    x = q, # q= regs de calendrier
                                                    periods = c(7, 365.2425),
                                                    ndiff = 2, ar = FALSE, mean = FALSE,
                                                    outliers = c("ao", "ls", "wo"), criticalValue = 0,
                                                    precision = 1e-9, approximateHessian = TRUE)

# Estimated outlier & calendar effects (coefs, se, student)
### variables are in "model" part as pre-set by the user : pre.mdl$model$variables
regs_mult<- data.frame(
  "Variable" = pre.mult$model$variables,
  "Coef"     = pre.mult$model$b,
  "Coef_SE"  = sqrt(diag(pre.mult$model$bcov))) %>% 
  mutate(Tstat= Coef / Coef_SE)
regs_mult$Variable[1:n.reg] <- colnames(regs_cjo)
regs_mult

#
# number of outliers
nb_out<-nrow(regs_mult)-length(colnames(regs_cjo))
nb_out
v<-regs_mult$Variable[(length(colnames(regs_cjo))+1):nrow(regs_mult)]
out_nom<-substr(v,1,2)
out_pos<-substr(v,4,9)
out_dates<-as.character(df_daily$date[as.numeric(out_pos)])
outliers<-paste0(out_nom,".",out_dates)
outliers
regs_mult$Variable[(length(colnames(regs_cjo))+1):nrow(regs_mult)]<-outliers
regs_mult

# Estimated MA parameters (coefs, se, student)
### MA parameters are in "estimation" part 
pre.mult$estimation$parameters

fam_out <- data.frame(
  "MA parameter" = c("MA1", "DOW", "DOY"),
  "Coef"         = pre.mult$estimation$parameters,
  "Coef_SE"      = sqrt(diag(pre.mult$estimation$covariance)),
  check.names = FALSE) %>% 
  mutate(Tstat = Coef / Coef_SE)


regs_mult<-regs_mult%>%mutate(across(where(is.numeric), ~ round(., 2)))

regs_mult
resultats_cal<-regs_mult[1:length(colnames(regs_cjo)),]
resultats_cal
resultats_out<-regs_mult[(length(colnames(regs_cjo))+1):nrow(regs_mult),]
resultats_out
resultats_out1<-resultats_out[1:25,]
resultats_out1
resultats_out2<-resultats_out[26:nrow(resultats_out),]
resultats_out2

kbl(resultats_cal) %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F)

kbl(resultats_out1) %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F)
kbl(resultats_out2) %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F)

##############################################################
############## Decomposition ################################# 
##############################################################

# Extract DOW pattern from linearized series
# step 1
amb.dow <- rjd3highfreq::fractionalAirlineDecomposition(
  pre.mult$model$linearized, # input time series
  period = 7,                # DOW pattern
  sn = FALSE,                # Signal (SA)-noise decomposition 
  stde = FALSE,              # Calculate standard deviations
  nbcasts = 0, nfcasts = 0)  # Numbers of back- and forecasts



# Extract DOY pattern from DOW-adjusted linearised data
# step 2 en log
amb.doy <- rjd3highfreq::fractionalAirlineDecomposition(
  amb.dow$decomposition$sa,  # DOW-adjusted linearised data
  period = 365.2425,         # DOY pattern
  sn = FALSE, 
  stde = FALSE, 
  nbcasts = 0, nfcasts = 0)




#### PLOT Estimated seasonal patterns 2-step AMB p 7 p 365 
plot(df_daily$date,exp(amb.dow$decomposition$s), type = "l", xlab = "", ylab = "", axes = FALSE)
par(new = TRUE) 
plot(df_daily$date,exp(amb.doy$decomposition$s), type = "l", col = "red", xlab = "", ylab = "") 

#main = "AMB estimated DOW (black) and DOY (red) patterns")

# Store final sa in data frame
#calendar component 
df_daily<- df_daily%>%
mutate(cal.cmp = exp(pre.mult$model$X[, 1:length(colnames(regs_cjo))] %*% 
                       pre.mult$model$b[1:length(colnames(regs_cjo))]))

#final sa  
df_daily <- df_daily%>% 
mutate(amb.dow = exp(amb.dow$decomposition$s))%>% 
mutate(amb.doy = exp(amb.doy$decomposition$s))%>% 
mutate(amb.sa_f = births/(cal.cmp*amb.dow * amb.doy))

### PLOT: DOW and DOY patterns

pdf(paste(fig.dir, "FB_AMB_seasonal_patterns.", plot.type, sep = ""), width = 9, height = 4)
print(ggplot(df_daily) +
        geom_line(aes(date, amb.dow), size = .000001, col = "gray", linetype = 2) +
        geom_line(aes(date, amb.doy), size = .000001) +
        scale_x_date(breaks = as.Date(paste(seq(1970, 2020, by = 10), "-01-01", sep = "")),
                     labels = as.character(seq(1970, 2020, by = 10))) +
        labs(x = "", y = "")+
        theme_hc()
)
dev.off()

#### PLOT :RAW and SA  

pdf(paste(fig.dir, "FB_AMB_y_sa.", plot.type, sep = ""), width = 9, height = 4)
print(ggplot(df_daily) +
        geom_line(aes(date, births), size = .000001, col = "gray") +
        geom_line(aes(date, amb.sa_f), size = .000001) +
        scale_x_date(breaks = as.Date(paste(seq(1970, 2020, by = 10), "-01-01", sep = "")),
                     labels = as.character(seq(1970, 2020, by = 10))) +
        labs(x = "", y = "")+
        theme_hc()
)
dev.off()