##################### French births
### cleaned time series with indicators
df_daily<-read.csv2("Data/TS_daily_births_franceM_1968_2020.csv")
colnames(df_daily)

##############################################################
############## Extended X11 Decomposition #################### 
##############################################################


x11.dow <- rjd3highfreq::x11(df_daily$births, 
                             period = 7,                               # DOW pattern
                             mul = TRUE,                              
                             trend.horizon = 9,  # 1/2 Filter length : not too long vs p
                             trend.degree = 3,                         # Polynomial degree
                             trend.kernel = "Henderson",               # Kernel function
                             trend.asymmetric = "CutAndNormalize",     # Truncation method
                             seas.s0 = "S3X9", seas.s1 = "S3X9",       # Seasonal filters
                             extreme.lsig = 1.5, extreme.usig = 2.5)   # Sigma-limits

print()

x11.dow$decomposition |> class()
x11.dow$parameters |> dput()

summary(x11.dow)
################## PRE TREATMENT: fractional Airline model
## objective : clean output regression results + arima model coeffs

pre.mult<- rjd3highfreq::fractionalAirlineEstimation(df_daily$births, 
                                                    #x = q, # q= regs de calendrier
                                                    #periods = c(7, 365.2425),
                                                    periods = 7,
                                                    ndiff = 2, ar = FALSE, mean = FALSE,
                                                    outliers = c("ao", "ls", "wo"), criticalValue = 0,
                                                    precision = 1e-9, approximateHessian = TRUE)


pre.mult

# Example of output: Estimated outlier & calendar effects (coefs, se, student)



regs_mult<- data.frame(
  "Variable" = pre.mult$model$variables,
  "Coef"     = pre.mult$model$b,
  "Coef_SE"  = sqrt(diag(pre.mult$model$bcov))) %>% 
  mutate(Tstat= Coef / Coef_SE)
regs_mult$Variable[1:n.reg] <- colnames(regs_cjo)
regs_mult


# number of outliers : TO IMPROVE
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



##############################################################
############## AMB Decomposition ################################# 
##############################################################

# Extract DOW pattern from linearized series
# step 1
amb.dow <- rjd3highfreq::fractionalAirlineDecomposition(
  df_daily$births, # input time series
  period = 7,                # DOW pattern
  sn = FALSE,                # Signal (SA)-noise decomposition 
  stde = FALSE,              # Calculate standard deviations
  nbcasts = 0, nfcasts = 0)  # Numbers of back- and forecasts

amb.dow
 




