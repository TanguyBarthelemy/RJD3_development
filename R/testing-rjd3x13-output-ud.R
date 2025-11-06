
library("rjd3toolkit")
library("rjd3x13")

a <- "Diagnostics		regression.out(*)	out(1)	preprocessing.model.out(*)		regression.outlier(*)	outlier(1)	regression.out(*)	regression.outlier(*)
Diagnostics		regression.out(*)	out(2)	preprocessing.model.out(*)		regression.outlier(*)	outlier(2)	regression.out(*)	regression.outlier(*)
Diagnostics		regression.out(*)	out(3)	preprocessing.model.out(*)		regression.outlier(*)	outlier(3)	regression.out(*)	regression.outlier(*)
Diagnostics		regression.td(*)	td(1)	preprocessing.model.td(*)		regression.td(*)	td(1)	regression.td(*)	regression.td(*)
Diagnostics		regression.td(*)	td(2)	preprocessing.model.td(*)		regression.td(*)	td(2)	regression.td(*)	regression.td(*)
Diagnostics		regression.td(*)	td(3)	preprocessing.model.td(*)		regression.td(*)	td(3)	regression.td(*)	regression.td(*)
Diagnostics		regression.td-derived	td-derived			regression.td-derived	td-derived	regression.td-derived	regression.td-derived
Diagnostics		regression.td-ftest	td-ftest			regression.td-ftest	td-ftest	regression.td-ftest	regression.td-ftest
Diagnostics		regression.user(*)	user(1)			regression.user(*)		regression.user(*)	regression.user(*)
Diagnostics		regression.user(*)	user(2)			regression.user(*)		regression.user(*)	regression.user(*)
Diagnostics		regression.user(*)	user(3)			regression.user(*)		regression.user(*)	regression.user(*)																"

ud <- strsplit(a, "\n") |>
    unlist() |>
    strsplit("\t") |>
    unlist() |>
    gsub(pattern = "*", replacement = "1", fixed = TRUE)
ud <- ud[nchar(ud) > 0] |> unique()



iv1 <- intervention_variable(12, c(2010, 1), 60,
                             starts = "2001-01-01", ends = "2001-12-01"
)
iv2 <- intervention_variable(12, c(2010, 1), 60,
                             starts = "2001-01-01", ends = "2001-12-01", delta = 1
)
vars <- list(
    reg1 = list(x = iv1),
    reg2 = list(x = iv2)
)
my_context <- modelling_context(variables = vars)

spec <- rjd3x13::x13_spec("RSA5c") |>
    set_basic(
        type = "Between",
        d0 = "2009-01-01",
        d1 = "2018-01-01",
        preliminary.check = TRUE,
        preprocessing = TRUE
    ) |>
    set_estimate(
        type = "From", d0 = "2012-01-01", tol = 0.0000002,
        exact.ml = FALSE, unit.root.limit = 0.98
    ) |>
    set_outlier(
        span.type = "From", d0 = "2013-01-01",
        outliers.type = c("LS", "AO"),
        critical.value = 5,
        tc.rate = 0.85
    ) |>
    set_automodel(
        enabled = FALSE,
        acceptdefault = TRUE
    ) |>
    add_outlier(type = "AO", date = "2015-01-01") |>
    add_ramp(start = "2013-10-01", end = "2014-12-01") |>
    set_tradingdays(
        option = "TD4",
        test =  "None"
    ) |>
    set_easter(
        enabled = TRUE,
        julian = NA,
        duration = 12,
        test = "None",
        type = "IncludeEasterMonday"
    ) |>
    set_transform(
        fun = "Log",
        adjust = "LengthOfPeriod", # ISSUE : fait un lp classique malgré tout ?
        outliers = TRUE
    ) |>
    add_usrdefvar(group = "reg1", name = "x", regeffect = "Trend", coef = 4)

y <- ts(AirPassengers,start = 2008, frequency = 12)

mod <- rjd3x13::x13(y, userdefined = ud, spec = spec, context = my_context)
mod$user_defined[!mod$user_defined |> sapply(is.null)]
