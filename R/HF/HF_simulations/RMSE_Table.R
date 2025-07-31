


# RMSE Table --------------------------------------------------------------




library("flextable")

df <- data.frame(
  type = rep(c("Deterministic DGP", "Stochastic GDP"), each = 12L), 
  gamma = c(rep(c(0.2, 0.4, 0.6), each = 4L), rep(c(0.2, 0.4, 0.6), each = 4L)), 
  sigma = c(rep(0, 12), rep(c(0.025, 0.05, 0.075), each = 4L)), 
  method = c("X11", "rjd-STL","MSTL", "AMB","Multi-AMB", "TBATS","PROPHET"), 
  trend_RMSE = runif(n = 24L, 0L, 1L), 
  weekly_RMSE = runif(n = 24L, 0L, 1L), 
  yearly_RMSE = runif(n = 24L, 0L, 1L), 
  remainder_RMSE = runif(n = 24L, 0L, 1L)
)

set_flextable_defaults(digits = 4L)

ft <- df |> as_grouped_data(groups = c("type")) |> 
  as_flextable(spread_first_col = TRUE, separate_with = "type", hide_grouplabel = TRUE) |> 
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
  colformat_double(j = c("trend_RMSE", "weekly_RMSE", "yearly_RMSE", "remainder_RMSE")) |> 
  set_table_properties(layout = "autofit", width = 0.8)
ft


# Example Table Souche ---------------------------------------------------

library("flextable")

df <- data.frame(
  type = rep(c("Deterministic DGP", "Stochastic GDP"), each = 12L), 
  gamma = c(rep(c(0.2, 0.4, 0.6), each = 4L), rep(c(0.2, 0.4, 0.6), each = 4L)), 
  sigma = c(rep(0, 12), rep(c(0.025, 0.05, 0.075), each = 4L)), 
  method = c("STR", "TBATS", "PROPHET", "MSTL"), 
  trend_RMSE = runif(n = 24L, 0L, 1L), 
  weekly_RMSE = runif(n = 24L, 0L, 1L), 
  yearly_RMSE = runif(n = 24L, 0L, 1L), 
  remainder_RMSE = runif(n = 24L, 0L, 1L)
)

set_flextable_defaults(digits = 4L)

ft <- df |> as_grouped_data(groups = c("type")) |> 
  as_flextable(spread_first_col = TRUE, separate_with = "type", hide_grouplabel = TRUE) |> 
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
  colformat_double(j = c("trend_RMSE", "weekly_RMSE", "yearly_RMSE", "remainder_RMSE")) |> 
  set_table_properties(layout = "autofit", width = 0.8)
ft



# Box plots ?---------------------------------------------------------------



