# install.packages(c("remotes", "tidyverse", "flextable", "forecast", "lubridate", "prophet", "tsbox", "tictoc"))
# remotes::install_github("rjdverse/rjd3x11plus")
# remotes::install_github("rjdverse/rjd3sts")
# remotes::install_github("rjdverse/rjd3highfreq")
# remotes::install_github("rjdverse/rjd3stl")
# remotes::install_local("R/HF/HF_simulations/tssim_0.2.15.tar.gz")

library("tictoc")
library("flextable")

nb_series <- 50L
nb_years <- 12L

start_date <- as.Date("2020-01-01")
end_date <- start_date + lubridate::years(nb_years) - lubridate::days(1L)

# getwd()
# setwd(dir = "C:/Users/INSEE_User/Documents/RJD3_development/R/HF")

source("R/HF/HF_simulations/UC_on_Sim_data_additive_det_S1.R")
gamma_1 <- gamma

source("R/HF/HF_simulations/UC_on_Sim_data_additive_det_S2.R")
gamma_2 <- gamma

source("R/HF/HF_simulations/UC_on_Sim_data_additive_det_S3.R")
gamma_3 <- gamma

source("R/HF/HF_simulations/UC_on_Sim_data_additive_det_S4.R")
gamma_4 <- gamma
sigma_4 <- sigma2

source("R/HF/HF_simulations/UC_on_Sim_data_additive_det_S5.R")
gamma_5 <- gamma
sigma_5 <- sigma2

source("R/HF/HF_simulations/UC_on_Sim_data_additive_det_S6.R")
gamma_6 <- gamma
sigma_6 <- sigma2

nb_method <- 7L
df <- data.frame(
    type = rep(
        c("Deterministic DGP", "Stochastic DGP"),
        each = nb_method * 3L
    ),
    gamma = rep(
        c(gamma_1, gamma_2, gamma_3, gamma_4, gamma_5, gamma_6),
        each = nb_method
    ),
    sigma = c(
        rep(0, nb_method * 3L),
        rep(c(sigma_4, sigma_5, sigma_6), each = nb_method)
    ),
    method = c(
        "X11",
        "rjd-STL",
        "MSTL",
        "AMB",
        "Multi-AMB",
        "TBATS",
        "PROPHET"
    ),
    trend_RMSE = c(t_s1, t_s2, t_s3, t_s4, t_s5, t_s6),
    weekly_RMSE = c(s7_s1, s7_s2, s7_s3, s7_s4, s7_s5, s7_s6),
    yearly_RMSE = c(s365_s1, s365_s2, s365_s3, s365_s4, s365_s5, s365_s6),
    remainder_RMSE = c(i_s1, i_s2, i_s3, i_s4, i_s5, i_s6),
    sa_RMSE = c(sa_s1, sa_s2, sa_s3, sa_s4, sa_s5, sa_s6)
)


rmse_cols <- names(df)[grepl("RMSE$", names(df))]

set_flextable_defaults(digits = 4L)

ft <- df |>
    as_grouped_data(groups = c("type")) |>
    as_flextable(
        spread_first_col = TRUE,
        separate_with = "type",
        hide_grouplabel = TRUE
    ) |>
    bold(i = ~ !is.na(type), j = 1L, bold = TRUE) |>
    hline(
        i = c(
            nb_method + 1L,
            2L * nb_method + 1L,
            3L * nb_method + 1L,
            4L * nb_method + 2L,
            5L * nb_method + 2L
        )
    ) |>
    set_header_labels(
        gamma = "\u03B3",
        sigma = "\u03C3\u00B2",
        method = "Method",
        trend_RMSE = "Trend RMSE",
        weekly_RMSE = "Weekly RMSE",
        yearly_RMSE = "Yearly RMSE",
        remainder_RMSE = "Remainder RMSE",
        sa_RMSE = "SA RMSE"
    ) |>
    colformat_double(
        j = c(
            "trend_RMSE",
            "weekly_RMSE",
            "yearly_RMSE",
            "remainder_RMSE",
            "sa_RMSE"
        )
    ) |>
    set_table_properties(layout = "autofit", width = 0.8) |>
    bg(
        part = "all",
        bg = "white"
    ) |>
    bg(
        j = rmse_cols,
        bg = scales::col_numeric(
            palette = c("white", "steelblue"),
            domain = NULL
        )
    )

ft

# save_as_image(ft, path = "~/work/table.png")

## for beamer
