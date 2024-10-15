### Dictionnaries x13, ts, toolkit
library(rjd3toolkit)
library(rjd3x13)
library(rjd3tramoseats)
library(xlsx)

a <- rjd3x13::x13_full_dictionary() |> as.data.frame()
write.xlsx(a, "x13dico.xlsx")

b <- rjd3tramoseats::tramoseats_full_dictionary() |> as.data.frame()
write.xlsx(b, "TSdico.xlsx")

# toolkit
# rjd3toolkit::.r2jd_modellingcontext(my_context)$getCalendars()$getNames()
