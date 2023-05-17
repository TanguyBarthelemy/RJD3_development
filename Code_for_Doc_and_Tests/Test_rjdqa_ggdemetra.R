#############
### Presentation 1 : seasonal adjustment in R from v2 to v3
# here NO link with JD+

# 1 basic SA (x13 et Ts) v2 and V3
# adjusting series (pre def spec) 
# output orga

# 2 Diagnostics (quality assessment, tests)

# test on residuals
# residual seasonality / seasonality tests 
# residual trading days

# 3 customizing parameters

# 4 refresh (production chain in R ?)

# 5 Plots and additional data visualisation 

# 6 modelling and sa custo tools 
#creates an Sarima model
## rjd3 modelling estimates arima model, time comp : 20 times faster that native R


options(stringasfactors = FALSE)



# Mes donnees
ipi<-read.csv2("C:/Users/YWYD5I/Documents/00_RJD3_Developpement/Donnees/IPI_nace4.csv")
head(ipi)
# import en CHR...?
str(ipi)
# Dates
# FORMAT %Y : 4 chiffres %y 2 chiffres
ipi$date<-as.Date(ipi$date,format="%d/%m/%Y")
# Formatage sans objet TS
ipi[,-1]<-sapply(ipi[,-1],as.numeric)
ipi[1:5,1:10]
nrow(ipi)
last_obs<-ipi[nrow(ipi),1]
last_obs
# ggplot2 
ggplot(ipi,aes(date,RF3030))+geom_line()
# creation des objets TS
# TS global 
mts_ipi<-ts(ipi[,-1],frequency=12,start=c(1990,1),end=c(2019,9))
class(mts_ipi)
# une serie IPI pour la suite : nom generique 

# creation d'un objet TS à partir d'un data frame 
serie_brute<-ts(ipi[,"RF3030"],frequency=12,start=c(1990,1),end=c(2019,9))
serie_brute
y_raw<-serie_brute



###########################################################
# Graphics for communication
# GGDemetra in itself 
library(ggplot2)
library(ggdemetra)


#  il faut une date au format ts 
# head(ipi)
# 
mts_ipi_total<-ts(ipi[,-1],frequency=12,start=c(1990,1),end=c(2019,9))
mts_ipi<-window(mts_ipi_total,start=c(2010,1))

head(mts_ipi)
date_num<-data.frame(date=as.numeric(time(mts_ipi)))
str(date_num)
ipi_df<-cbind(date_num,ipi[ipi$date>="2010-01-01",-1])
str(ipi_df)
head(ipi_df)

# dans le ggplot general la date peut etre "normale" 
# ou au format numerique issu du "TS"
# classical plot of the raw series using ggplot2 package
base_plot<- ggplot(data = ipi_df, mapping = aes(x = date, y = RF2740)) +
  geom_line() +
  labs(title = "Industrial Production Index (IPI)",
       x = "date", y = "RF2740")
base_plot

# here we re-create a specification 
spec <- RJDemetra::x13_spec("RSA3", tradingdays.option = "WorkingDays")

enhanced_plot<- base_plot+
  # function "geom_sa" adds specified components
  geom_sa(component = "y_f", linetype = 2,
          spec = spec) + 
  geom_sa(component = "sa", color = "red") +
  geom_sa(component = "sa_f", color = "red", linetype = 2)+
  #adding outliers 
  geom_outlier(geom = "label_repel",
               vjust = 4,
               ylim = c(NA, 65), force = 10,
               arrow = arrow(length = unit(0.03, "npc"),
                             type = "closed", ends = "last"))+
  # adding Arima Model 
  geom_arima(geom = "label",
             x_arima = - Inf, y_arima = -Inf, 
             vjust = -1, hjust = -0.1,
             message = FALSE)
print(enhanced_plot)
# adding diagnostics 
diagnostics <- c(`Seasonality (combined)` = "diagnostics.combined.all.summary",
                 `Residual qs-test (p-value)` = "diagnostics.qs",
                 `Residual f-test (p-value)` = "diagnostics.ftest")

enhanced_plot+ geom_diagnostics(diagnostics = diagnostics,
                   ymin = 150, ymax = 200, xmin = 2016,
                   table_theme = gridExtra::ttheme_default(base_size = 6))

print(enhanced_plot)

##################
# rjdmarkdown


# install.packages("RJDemetra", type="source", INSTALL_opts = "--no-multiarch" )
# library(RJDemetra)
install.packages("rjdmarkdown", INSTALL_opts = "--no-multiarch")
library(rjdmarkdown)

# print_preprocessing() for the pre-processing model;
# print_decomposition() for the decomposition;
# print_diagnostics() to print diagnostics tests on the quality of the seasonal adjustment.

# ipi <- RJDemetra::ipi_c_eu[, "FR"]
# jsa_x13 <- RJDemetra::jx13(ipi)

print_preprocessing(sa_x13_v2,format="latex")
print_decomposition(sa_x13_v2)
print_diagnostics(sa_x13_v2)


# reste vu avec Alain optionnel : creation fichier markdown directement 
ipi <- RJDemetra::ipi_c_eu[, "FR"]
jsa_x13 <- RJDemetra::jx13(ipi)

create_rmd(jsa_x13, "test1.Rmd", output_format = "pdf_document")
# Pour ouvrir directement le fichier depuis R (on peut aussi le faire à la main)
browseURL("test1.pdf")

# À partir d'un workspace :
sa_ts <- jtramoseats(ipi)
wk <- new_workspace()
mp <- new_multiprocessing(wk, "sa1")
add_sa_item(wk, "sa1", jsa_x13, "X13")
add_sa_item(wk, "sa1", sa_ts, "TramoSeats")
compute(wk)

create_rmd(wk, "test2.Rmd", 
           output_format = "pdf_document",
           output_options = list(toc = TRUE,
                                 number_sections = TRUE)) # Pour rajouter une table des matières
# Pour ouvrir directement le fichier depuis R (on peut aussi le faire à la main) :
browseURL("test2.pdf")
