library("rjd3toolkit")
library("rjd3filters")
library("rjd3x11plus")


# Data (conventionnal)

ipi <- read.csv2(
    "C:/Users/YWYD5I/Documents/00_RJD3_Developpement/RJD3_development/Data/IPI_nace4.csv"
)
ipi$date <- as.Date(ipi$date, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)
# creating a TS object from a data frame
y_raw <- ts(
    ipi[, "RF0812"],
    frequency = 12,
    start = c(1990, 1),
    end = c(2022, 9)
)
y_new <- ts(
    ipi[, "RF0812"],
    frequency = 12,
    start = c(1990, 1),
    end = c(2022, 9)
)
y_raw

## More esoteric data
