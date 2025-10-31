################################################################################
########             But comparaison des outputs R V2 et V3             ########
################################################################################

## Parameters ------------------------------------------------------------------

comparaison_GUI <- readxl::read_excel(
    "~/../Desktop/testing_output/Difference_output_GUI_v2_v3.xlsx"
)

dej_input_v2 <- (comparaison_GUI |>
    subset(
        !is.na(`rjdverse V3 ({rjd3tramoseats} et {rjd3x13})`)
    ))$`rjdverse V2 {RJDemetra} input - output`
dej_input_v2 <- dej_input_v2[!is.na(dej_input_v2)]
dej_input_v3 <- comparaison_GUI$`rjdverse V3 ({rjd3tramoseats} et {rjd3x13})`
dej_input_v3 <- dej_input_v3[!is.na(dej_input_v3)]

## Data ------------------------------------------------------------------------

x <- AirPassengers +
    c(rep(0, 100), 200, rep(0, 43)) + # Ajout d'un AO
    c(rep(0, 80), rep(-100, 64)) # Ajout d'un LS
# x[25] <- NA # Ajout de valeurs manquantes
# x <- ts(c(x, NA, NA), start = start(x), frequency = frequency(x))

## Spec ------------------------------------------------------------------------

### V2 -------------------------------------------------------------------------

spec_v2 <- RJDemetra::x13_spec(
    spec = "RSA3",
    estimate.from = "1951-06-01",
    estimate.to = "1957-06-01",
    outlier.from = "1952-01-01",
    outlier.enabled = TRUE,
    outlier.ao = TRUE,
    outlier.ls = TRUE,
    outlier.tc = FALSE,
    outlier.so = FALSE,
    outlier.cv = 5.34,
    outlier.tcrate = 0.85,
    outlier.method = "AddOne",
    usrdef.outliersEnabled = TRUE,
    usrdef.outliersType = c("LS", "AO"),
    usrdef.outliersDate = c("1954-10-01", "1955-01-01"),
    usrdef.outliersCoef = c(36.9, 14.1),
    transform.function = "Log",
    transform.adjust = "LeapYear",
    transform.aicdiff = -5.21,
    preliminary.check = TRUE,
    estimate.tol = 0.45,

    # arima.coefEnabled = TRUE,
    # arima.p = 2,
    # arima.d = 2,
    # arima.q = 0,
    # arima.bp = 1,
    # arima.bd = 0,
    # arima.bq = 2,
    # arima.coef = (1:5) / 10,
    # arima.coefType = c(rep("Fixed", 2), rep("Initial", 2), "Undefined"),

    automdl.enabled = TRUE,
    # automdl.ub1 = 1.0212,
    # automdl.ub2 = 1.2101,
    # automdl.ubfinal = 1.042,
    # automdl.mixed = FALSE,
    # automdl.acceptdefault = TRUE,
    # automdl.cancel = 0.098,
    # automdl.ljungboxlimit = 2.54,
    # automdl.armalimit = 0.9985,
    # automdl.balanced = TRUE,
    # automdl.reducecv = 0.15,

    x11.mode = "Multiplicative",
    x11.lsigma = 1.6,
    x11.usigma = 2.4,
    x11.bcasts = 15,
    x11.fcasts = 17,
    x11.seasonalComp = TRUE,
    x11.trendma = 23,
    x11.seasonalma = "S3X9",
    x11.calendarSigma = "Select",
    x11.excludeFcasts = FALSE,
    x11.sigmaVector = c(
        "Group1",
        "Group1",
        "Group1",
        "Group1",
        "Group2",
        "Group2",
        "Group1",
        "Group1",
        "Group1",
        "Group1",
        "Group1",
        "Group1"
    ),
)

all_input_v2 <- union(
    RJDemetra::user_defined_variables(),
    comparaison_GUI$`rjdverse V2 {RJDemetra} input - output`
)
all_input_v2 <- all_input_v2[!is.na(all_input_v2)]

var_v2 <- setdiff(RJDemetra::user_defined_variables(), dej_input_v2)

mod_v2 <- RJDemetra::x13(x, spec = spec_v2, userdefined = var_v2)
mod_v2 <- RJDemetra::x13(x, spec = "RSA3", userdefined = all_input_v2)

### V3 -------------------------------------------------------------------------

spec_v3 <- rjd3x13::x13_spec("rsa3") |>
    rjd3toolkit::set_outlier(
        span.type = "From",
        d0 = "1952-01-01",
        outliers.type = c("LS", "AO"),
        critical.value = 5.34,
        tc.rate = 0.85,
        method = "AddOne"
    ) |>
    rjd3toolkit::add_outlier(
        type = c("LS", "AO"),
        date = c("1954-10-01", "1955-01-01"),
        coef = c(36.9, 14.1)
    ) |>
    rjd3toolkit::set_transform(
        fun = "Log",
        adjust = "LeapYear",
        outliers = TRUE,
        aicdiff = -5.21,
        fct = -0.45
    ) |>
    rjd3toolkit::set_basic(
        type = "Between",
        d0 = "1949-01-01",
        d1 = "1960-12-01",
        preliminary.check = TRUE,
        preprocessing = TRUE
    ) |>
    rjd3toolkit::set_estimate(
        type = "Between",
        d0 = "1951-06-01",
        d1 = "1957-06-01",
        tol = 0.45
    ) |>
    rjd3toolkit::set_automodel(
        enabled = TRUE,
        ub1 = 1.0212,
        ub2 = 1.2101,
        ubfinal = 1.042,
        mixed = FALSE,
        acceptdefault = TRUE,
        cancel = 0.098,
        ljungboxlimit = 2.54,
        tsig = 0.9985,
        balanced = TRUE,
        reducecv = 0.15
    ) |>
    # rjd3toolkit::set_arima(p = 2,
    #                        d = 2,
    #                        q = 0,
    #                        bp = 1,
    #                        bd = 0,
    #                        bq = 2,
    #                        coef = (1:5) / 10,
    #                        coef.type = c(rep("Fixed", 2), rep("Initial", 2), "Undefined")) |>
    rjd3x13::set_x11(
        mode = "Multiplicative",
        lsigma = 1.6,
        usigma = 2.4,
        fcasts = 17,
        bcasts = 15,
        seasonal.comp = TRUE,
        henderson.filter = 23,
        seasonal.filter = "S3X9",
        calendar.sigma = "Select",
        exclude.forecast = FALSE,
        sigma.vector = c(
            1,
            1,
            1,
            1,
            2,
            2,
            1,
            1,
            1,
            1,
            1,
            1
        )
    )

var_v3 <- setdiff(
    union(rjd3x13::x13_dictionary(), rjd3x13::userdefined_variables_x13()),
    dej_input_v3
)

mod_v3 <- rjd3x13::x13(x, spec = spec_v3, userdefined = var_v3)
mod_v3 <- rjd3x13::x13(x, spec = "RSA3", userdefined = var_v3)
mod_v3 <- rjd3x13::x13(
    x,
    spec = spec_v3,
    userdefined = c("likelihood.bic", "likelihood.bic2")
)

## Paramètres ------------------------------------------------------------------

list_input_v2 <- names(mod_v2$user_defined)
list_input_v2_nu <- names(mod_v2$user_defined) |>
    gsub(pattern = "residuals.", replacement = "", fixed = TRUE) |>
    gsub(pattern = "regression.", replacement = "", fixed = TRUE) |>
    gsub(pattern = "diagnostics.", replacement = "", fixed = TRUE) |>
    gsub(pattern = "decomposition.", replacement = "", fixed = TRUE) |>
    gsub(pattern = "benchmarking.", replacement = "", fixed = TRUE) |>
    gsub(pattern = "preadjustment.", replacement = "", fixed = TRUE) |>
    gsub(pattern = "finals.", replacement = "", fixed = TRUE)

list_input_v3 <- names(mod_v3$user_defined)
list_input_v3_nu <- names(mod_v3$user_defined) |>
    gsub(pattern = "residuals.", replacement = "", fixed = TRUE) |>
    gsub(pattern = "regression.", replacement = "", fixed = TRUE) |>
    gsub(pattern = "diagnostics.", replacement = "", fixed = TRUE) |>
    gsub(pattern = "decomposition.", replacement = "", fixed = TRUE) |>
    gsub(pattern = "benchmarking.", replacement = "", fixed = TRUE) |>
    gsub(pattern = "preadjustment.", replacement = "", fixed = TRUE) |>
    gsub(pattern = "finals.", replacement = "", fixed = TRUE)


## Assignation par tests -------------------------------------------------------

for (input_v3 in list_input_v3) {
    cat("\nName from rjd3x13:", input_v3, "\n")
    obj_v3 <- mod_v3$user_defined[[input_v3]]
    print(obj_v3)
    concurrent <- NULL
    for (input_v2 in list_input_v2) {
        obj_v2 <- mod_v2$user_defined[[input_v2]]
        # comparaison <- waldo::compare(obj_v2, obj_v3, tolerance = 0.001)
        # if (length(comparaison) == 0L) {
        if (
            try(isTRUE(any(sapply(
                obj_v2,
                all.equal,
                obj_v3,
                tolerance = 0.0001
            ))))
        ) {
            cat("Equivalence avec", input_v2, "?\n")
            print(obj_v2)
            concurrent <- c(concurrent, input_v2)
        }
    }
    if (length(concurrent) > 0) {
        readline("Ok?")
    }
}


## Assignation par comparaison GUI / R -----------------------------------------

for (input_v2 in names(mod_v2$user_defined)) {
    cat("\nName from RJDemetra:", input_v2, "\n")
    obj_v2 <- mod_v2$user_defined[[input_v2]]
    print(obj_v2)

    input_v3_GUI <- comparaison_GUI |>
        subset(
            `rjdverse V2 {RJDemetra} input - output` == input_v2,
            select = `cruncher V3 input`
        )
    input_v3_GUI <- input_v3_GUI[1L, 1L, drop = TRUE]
    obj_v3 <- mod_v3$user_defined[[input_v3_GUI]]

    if (!is.na(input_v3_GUI) && !is.null(obj_v2) && !is.null(obj_v3)) {
        cat("Le concurrent probable est", input_v3_GUI, ".\n Sa valeur est :\n")
        print(obj_v3)
        readline("Ok?")
    }
}

## Assignation par comparaison R V2 v3 -----------------------------------------

for (index_v2 in seq_along(list_input_v2)) {
    input_v2 <- list_input_v2[index_v2]
    input_v2_nu <- list_input_v2_nu[index_v2]
    cat("\nName from RJDemetra:", input_v2, "\n")
    obj_v2 <- mod_v2$user_defined[[input_v2]]
    print(obj_v2)

    if (input_v2_nu %in% list_input_v3_nu) {
        indexes_v3 <- which(list_input_v3_nu == input_v2_nu)

        for (index_v3 in indexes_v3) {
            input_v3 <- list_input_v3[index_v3]
            obj_v3 <- mod_v3$user_defined[[input_v3]]
            cat("Le concurrent probable est", input_v3, ".\n Sa valeur est :\n")
            print(obj_v3)
            try(ts.plot(cbind(obj_v2, obj_v3), col = c("blue", "red")))
            cat("\nRappel du nom:\n\t-", input_v2, "\n\t-", input_v3, "\n")
            readline("Ok?")
        }
    }
}

# Test -------------------------------------------------------------------------

## v3
a <- rjd3x13::x13_fast(x, spec = "rsa3", userdefined = "t_f(1153)")
v <- a$user_defined$`t_f(1153)`
cbind(AirPassengers, v) |> ts.plot()

## v2
a <- RJDemetra::x13(
    x,
    spec = "RSA3",
    userdefined = c(
        "mstats.M(*)",
        "mstats.M(4)",
        "mstats.M(2)",
        "mstats.M(7)",
        "mstats.M(11)",
        "mstats.M(12)"
    )
)
v <- a$user_defined$`mstats.M(3)`
print(v)
cbind(AirPassengers, v) |> ts.plot()
