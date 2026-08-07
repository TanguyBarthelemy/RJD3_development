
# Installation -----------------------------------------------------------------

remotes::install_gitlab("team-data-science/camplet-r",
                        host = "gitrepo.service.rug.nl")


# Test -------------------------------------------------------------------------

library("camplet")

res <- camplet(AirPassengers)

output_r <- res$data$sa
output_gui <- read.csv("~/Documents/tmp_camplet.txt", sep = "\t", dec = ",")$Seasonally.adjusted

waldo::compare(round(output_r, 3L), output_gui)


# Comparaison ------------------------------------------------------------------

library("rjd3x13")

add_AO <- function(y, date, n) {
    if (missing(n)) {
        n <- sample(length(y) %/% 10, size = 1)
    }
    if (missing(date)) {
        date <- sample(length(y), size = n, replace = FALSE)
    }
    y[date] <- y[date] + rnorm(n, mean = 0, sd = sd(y))
    return(y)
}
add_LS <- function(y, date, n) {
    if (missing(n)) {
        n <- sample(length(y) %/% 10, size = 1)
    }
    if (missing(date)) {
        date <- sample(length(y), size = n, replace = FALSE)
    }
    for (k in date) {
        y[1:k] <- y[1:k] + rnorm(1, mean = 0, sd = sd(y))
    }
    return(y)
}


## Séries avec des outliers ------------------------------------------------

y <- AirPassengers |>
    add_AO() |>
    add_LS(n = 3L)


### Code JD+ --------------------------------------------------------------------

mod <- x13(y)
sa_jd <- mod$result$final$d11final


### Code CAMPLET ----------------------------------------------------------------

res <- camplet(y)
sa_camplet <- ts(res$data$sa, start = 1949L, frequency = 12L)


### Comparaison -----------------------------------------------------------------

plot(y, type = "l")
lines(sa_camplet, type = "l", col = "red")
lines(sa_jd, col = "blue")


## Séries avec des effets de calendrier ------------------------------------------------

tde <- 50 * calendar_td(groups = c(1, 1, 1, 1, 1, 0, 0), s = AirPassengers, contrasts = FALSE)[, 1]
y <- AirPassengers + tde


### Code JD+ --------------------------------------------------------------------

mod <- x13(y)
sa_jd <- mod$result$final$d11final


### Code CAMPLET ----------------------------------------------------------------

res <- camplet(y, )
sa_camplet <- ts(res$data$sa, start = 1949L, frequency = 12L)


### Comparaison -----------------------------------------------------------------

plot(y, type = "l")
lines(sa_camplet, type = "l", col = "red")
lines(sa_jd, col = "blue")
