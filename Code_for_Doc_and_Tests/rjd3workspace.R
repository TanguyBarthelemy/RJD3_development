library("rjd3toolkit")
library("rjd3workspace")
library("rjd3providers")

#-------- objectif: tests de rjd3workspace pour ameliorer doc package


#### ws test (dans repo ws_doc)

# - 1  Ipi_x13_10 (10 series x13, avec regs cjo et outliers)


## Path to ws  ------------------------------------------------------------------

ws <- file.path("ws_doc", "Ipi_x13_10.xml") |>
    normalizePath()

## Load ws  -------------------------------------------

# 2 fonctions identiques: laisser load, name: jws_load

jws_ref<- .jws_open(file = ws)
class(jws_ref)

jws_ref2<- .jws_load(file = ws)

## compute Q: diff avec read

jws_refc<-.jws_compute(jws_ref)
class(jws_refc)

.jws_load()
.jws_compute(ws)

