## Error ?

# Les regresseurs n'ont pas de colnames avec q

# spec stock td ---------------------

spec1 <- rjd3x13::spec_x13("rsa3") |> 
    set_tradingdays(stocktd = 28)

spec2 <- rjd3x13::spec_x13("rsa3") |> 
    set_tradingdays(stocktd = 28) |> 
    set_tradingdays(option = "None")

waldo::compare(spec1, spec2)


# Estimation stock td ---------------------

spec_x13_d <- rjd3x13::spec_x13("rsa3")
spec_x13_d <- set_tradingdays(spec_x13_d, stocktd = 28)
sa_x13_d <- rjd3x13::x13(y_raw, spec_x13_d)


# Easter ---------------------

stop("Specific TRAMO...")

spec_x13_d1 <- rjd3tramoseats::spec_tramo("trfull")
spec_x13_d1 <- set_easter(spec_x13_d1, 
                          type = "Standard")

spec_x13_d2 <- rjd3tramoseats::spec_tramo("trfull")
spec_x13_d2 <- set_easter(spec_x13_d2, 
                          type = "IncludeEaster")

spec_x13_d3 <- rjd3tramoseats::spec_tramo("trfull")
spec_x13_d3 <- set_easter(spec_x13_d3, 
                          type = "IncludeEasterMonday")

waldo::compare(spec_x13_d1, spec_x13_d2)
waldo::compare(spec_x13_d2, spec_x13_d3)


# Calendar regressor ------------------

frenchCalendar <- national_calendar(days = list(
    fixed_day(7, 14), # Fete nationale
    fixed_day(5, 8, validity = list(start = "1982-05-08")), # Victoire 2nd guerre mondiale
    special_day("NEWYEAR"), # Nouvelle année
    special_day("CHRISTMAS"), # Noël
    special_day("MAYDAY"), # 1er mai
    special_day("EASTERMONDAY"), # Lundi de Pâques
    special_day("ASCENSION"), # attention +39 et pas 40 jeudi ascension
    special_day("WHITMONDAY"), # Lundi de Pentecôte (1/2 en 2005 a verif)
    special_day("ASSUMPTION"), # Assomption
    special_day("ALLSAINTSDAY"), # Toussaint
    special_day("ARMISTICE"))
)

weighted_cal <- weighted_calendar(list(frenchCalendar, frenchCalendar), c(0.1, 0.5))

final_cal <- chained_calendar(frenchCalendar, weighted_cal, break_date = "2005-05-01")

calendar_td(weighted_cal, 
                      frequency = 12, start = c(1990, 12), length = 500)
calendar_td(chained_calendar, 
                      frequency = 12, start = c(1990, 12), length = 500)

