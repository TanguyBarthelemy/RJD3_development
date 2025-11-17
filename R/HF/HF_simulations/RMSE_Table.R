# RMSE Table --------------------------------------------------------------

library("flextable")
library("dplyr")

all_estimation <- TBox::get_data("R/HF/HF_simulations/all_estimation.csv") |>
    filter(nb_years == 12L) |>
    select(-time, -nb_years)

set_flextable_defaults(digits = 4L)

rmse_cols <- names(all_estimation)[grepl("RMSE", names(all_estimation))]
nb_method <- length(unique(all_estimation$method))

ft <- all_estimation |>
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
    set_table_properties(layout = "autofit", width = 0.8)

ft_color <- ft |>
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
ft_color


# Example Table Souche ---------------------------------------------------

library("flextable")

df <- data.frame(
    type = rep(c("Deterministic DGP", "Stochastic DGP"), each = 12L),
    gamma = c(
        rep(c(0.2, 0.4, 0.6), each = 4L),
        rep(c(0.2, 0.4, 0.6), each = 4L)
    ),
    sigma = c(rep(0, 12), rep(c(0.025, 0.05, 0.075), each = 4L)),
    method = c("STR", "TBATS", "PROPHET", "MSTL"),
    trend_RMSE = runif(n = 24L, 0L, 1L),
    weekly_RMSE = runif(n = 24L, 0L, 1L),
    yearly_RMSE = runif(n = 24L, 0L, 1L),
    remainder_RMSE = runif(n = 24L, 0L, 1L)
)

set_flextable_defaults(digits = 4L)

ft <- df |>
    as_grouped_data(groups = c("type")) |>
    as_flextable(
        spread_first_col = TRUE,
        separate_with = "type",
        hide_grouplabel = TRUE
    ) |>
    bold(i = ~ !is.na(type), j = 1L, bold = TRUE) |>
    hline(i = c(5L, 9L, 13L, 18L, 22L)) |>
    set_header_labels(
        gamma = "\u03B3",
        sigma = "\u03C3\u00B2",
        method = "Method",
        trend_RMSE = "Trend RMSE",
        weekly_RMSE = "Weekly RMSE",
        yearly_RMSE = "Yearly RMSE",
        remainder_RMSE = "Remainder RMSE"
    ) |>
    colformat_double(
        j = c("trend_RMSE", "weekly_RMSE", "yearly_RMSE", "remainder_RMSE")
    ) |>
    set_table_properties(layout = "autofit", width = 0.8)
ft
