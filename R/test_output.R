library("googlesheets4")
library("dplyr")
library("rjwsacruncher")
library("RJDemetra")
library("rjd3x13")

x <- googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1FJtnHCkL7KCeqYikfjJvTNA-krdnC_PARF37NQrL2IY/edit?pli=1&gid=903960345#gid=903960345",
    sheet = "X13"
)


## v3 --------------------------------------------------------------------------

# But : récupérer tous les outputs possibles de séries X13

colnames(x)
current_v2 <- x |>
    filter(Type == "Series",
           !is.na(`{RJDemetra} user-defined output`)) |>
    pull(`{RJDemetra} user-defined output`)

current_v3 <- x |>
    filter(Type == "Series",
           !is.na(`{rjd3x13} user-defined output`)) |>
    pull(`{rjd3x13} user-defined output`)

other_v3_items <- c(
    rjd3x13::userdefined_variables_x13("RegArima"),
    rjd3x13::userdefined_variables_x13("X13"),
    rjd3x13::x13_dictionary()
) |>
    unique() %>%
    .[!startsWith(., prefix = "regression.")] %>%
    .[!startsWith(., prefix = "residuals.")] %>%
    .[!startsWith(., prefix = "span.")] %>%
    .[!startsWith(., prefix = "likelihood.")] %>%
    .[!startsWith(., prefix = "arima.")] %>%
    .[!startsWith(., prefix = "diagnostics.")] %>%
    .[!startsWith(., prefix = "variancedecomposition.")] %>%
    .[!startsWith(., prefix = "m-statistics.")] %>%
    .[!. %in% c("period", "adjust", "log")]

mod_v3 <- rjd3x13::x13(AirPassengers, userdefined = setdiff(other_v3_items, x$`{rjd3x13} user-defined output`))

# classes <- mod$user_defined |> lapply(class)
# classes[!classes %in% c("ts", "NULL")]
# classes[classes == "ts"]
# classes[!classes %in% c("ts", "NULL")] |> names()
# mod$user_defined[!classes %in% c("ts", "NULL")]
#
# mod <- rjd3x13::x13(AirPassengers, userdefined = "decomposition.x11-all")
# head(mod$user_defined$`decomposition.x11-all`)

mod_v2 <- RJDemetra::x13(AirPassengers, userdefined = current_v2)

for (k in seq_along(mod_v3$user_defined)) {
    nomk <- names(mod_v3$user_defined)[k]
    objk <- mod_v3$user_defined[[k]]

    if ("ts" %in% class(objk) && !all(objk == 1)) {
        cat("\nV3 :", nomk, "\n")

        if (nomk %in% names(mod_v2$user_defined)) {
            plot(objk, main = nomk, lwd = 2, type = "b", col = "yellow")
            lines(mod_v2$user_defined[[nomk]], col = "blue", type = "b")
        }

        for (i in seq_along(mod_v2$user_defined)) {
            nomi <- names(mod_v2$user_defined)[i]
            obji <- mod_v2$user_defined[[i]]

            if ("ts" %in% class(obji) && length(objk) == length(obji)) {
                comp <- max(abs(objk - obji), na.rm = TRUE)
                if (!is.infinite(comp) && comp < 1) {
                    cat("candidat v2 :", nomi, "\n")
                }
            }
        }
    }
}
