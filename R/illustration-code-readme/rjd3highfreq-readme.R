df_daily <- read.csv2("https://raw.githubusercontent.com/TanguyBarthelemy/Tsace_RJD_Webinar_Dec22/b5fcf6b14ae47393554950547ef4788a0068a0f6/Data/TS_daily_births_franceM_1968_2020.csv") |> 
    dplyr::mutate(log_births = log(births))

frenchCalendar <- rjd3toolkit::national_calendar(days = list(
    rjd3toolkit::fixed_day(7, 14), # Bastille Day
    rjd3toolkit::fixed_day(5, 8, validity = list(start = "1982-05-08")), # End of 2nd WW
    rjd3toolkit::special_day('NEWYEAR'), 
    rjd3toolkit::special_day('CHRISTMAS'), 
    rjd3toolkit::special_day('MAYDAY'),
    rjd3toolkit::special_day('EASTERMONDAY'), 
    rjd3toolkit::special_day('ASCENSION'), #
    rjd3toolkit::special_day('WHITMONDAY'), 
    rjd3toolkit::special_day('ASSUMPTION'), 
    rjd3toolkit::special_day('ALLSAINTSDAY'),
    rjd3toolkit::special_day('ARMISTICE'))
)

q <- rjd3toolkit::holidays(
    calendar = frenchCalendar, 
    "1968-01-01", length = nrow(df_daily) + 12, type = "All", nonworking = 7L)

rjd3highfreq::fractionalAirlineEstimation(
    y = df_daily$log_births, # here a daily series in log
    x = q, # q = calendar
    periods = 7, # approx  c(7,365.25)
    ndiff = 2, ar = FALSE, mean = FALSE,
    outliers = c("ao","wo","LS"), 
    # WO compensation
    criticalValue = 0, # computed in the algorithm
    precision = 1e-9, approximateHessian = TRUE,
    nf = 12)