getwd()
library(rjd3x13)


# table with metadata and aliases for BSI seasonally adjusted series
sa_table <- read.csv("DaTA/BCE/sa_table.csv", sep = ";")
View(sa_table)
sa_table$sa_ser_key <- gsub(
    "\\.N\\.",
    "\\.Y\\.",
    gsub("\\.I\\.", "\\.1\\.", sa_table$ser_key)
)
## reminder
sa_table$nsa_stocks <- gsub("\\.I\\.", "\\.1\\.", sa_table$ser_key)
## reminder

# index of national stocks BSI data, not seasonally adjusted
# these series are used to run our seasonal adjustment procedures
nsa_df <- readRDS("DaTA/BCE/BSI_index_of_notional_stocks.RData")
View(nsa_df)
# BSI stocks series, non-seasonally adjusted
nsa_stocks_df <- readRDS("DaTA/BCE/BSI_stocks_non_seasonally_adjusted.RData")
View(nsa_stocks_df)
# BSI stocks series, officially seasonally adjusted
sa_stocks_df <- readRDS("DaTA/BCE/BSI_stocks_seasonally_adjusted.RData")
View(sa_stocks_df)

# converting df to list of ts
convert_date_string <- function(date_string) {
    parts <- unlist(strsplit(date_string, "-"))
    year <- as.numeric(parts[1])
    month <- as.numeric(parts[2])
    return(c(year, month))
}

time_series_list <- list()

for (col_name in names(nsa_df)[-1]) {
    i <- which(sa_table$alias == col_name)
    if (sa_table$type[i] == "D") {
        start_date <- sa_table$start[i]
        start_index <- which(nsa_df$TIME_PERIOD == start_date)
        end_date <- tail(rownames(nsa_df), 1)
        end_index <- nrow(nsa_df)
        time_series_list[[col_name]] <- ts(
            nsa_df[[col_name]][start_index:end_index],
            start = convert_date_string(start_date),
            end = convert_date_string(end_date),
            frequency = 12
        )
    }
}

# example rjdemetra SA with 1 series (consumer credit)
loanshhcc <- time_series_list[["LOANSHHCC"]]

ts.plot(loanshhcc)

# remove rows with missing values
nsa_loanshhcc <- ts(
    nsa_stocks_df[["LOANSHHCC"]][277:514],
    frequency = 12,
    start = c(2003, 1),
    end = c(2022, 10)
)
sa_loanshhcc <- ts(
    sa_stocks_df[["LOANSHHCC"]][277:514],
    frequency = 12,
    start = c(2003, 1),
    end = c(2022, 10)
)

# creating a spec from default
x13_spec_d <- rjd3x13::x13_spec("rsa3")

# set basic : series span for the estimation
x13_spec_d <- rjd3toolkit::set_basic(
    x13_spec_d,
    type = "All",
    preliminary.check = TRUE,
    preprocessing = TRUE
)
# set  transform : log or not
x13_spec_d <- rjd3toolkit::set_transform(
    x13_spec_d,
    fun = "Log",
    outliers = TRUE
)

x13_spec_d

# saveRDS(x13_spec_d, "loanshhcc_spec.RData")
# x13_spec_d <- readRDS("loanshhcc_spec.RData")

# check results
m <- rjd3x13::x13(nsa_loanshhcc, x13_spec_d) # originally loanshhcc
m
# saveRDS(m, "loanshhcc_result.RData")
# m <- readRDS("loanshhcc_result.RData")

# read factors
loanshhcc_factors <- m$result$final$d16
loanshhcc_direct_sa <- m$result$final$d11final
loanshhcc_sa_verif <- loanshhcc / loanshhcc_factors
all.equal(loanshhcc_sa_verif, loanshhcc_direct_sa, tolerance = 10**-6)

diff <- loanshhcc_sa_verif - loanshhcc_direct_sa # ok

# final stocks series for consumer credit
rjd_odhh <- round(nsa_loanshhcc / loanshhcc_factors)

### check
all.equal(nsa_loanshhcc, loanshhcc, tolerance = 10**-4)

diff_raw <- nsa_loanshhcc - loanshhcc

# plotting all 3 series
ts.plot(
    window(sa_loanshhcc, start = 2003, end = 2014),
    window(nsa_loanshhcc, start = 2003, end = 2014),
    window(rjd_odhh, start = 2003, end = 2014),
    gpars = list(col = c("blue", "red", "black"))
)
