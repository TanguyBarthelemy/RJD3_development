
library("dplyr")
library("ggplot2")
library("tidyr")

all_estimation <- TBox::get_data("R/HF/HF_simulations/all_estimation.csv")

# Opening the graphical device
pdf("R/HF/HF_simulations/graphs.pdf", width = 10L)

for (method_k in unique(all_estimation$method)) {
    time_table <- all_estimation |>
        filter(method == method_k) |>
        summarise(time = median(time, na.rm = TRUE), .by = nb_years)

    print(
        ggplot(time_table, aes(x = nb_years, y = time)) +
            geom_line(size = 1) +
            geom_point(size = 2) +
            labs(
                title = paste("Execution time as a function of series length - Method", method_k),
                x = "Series length (years)",
                y = "Execution time (seconds)"
            )
    )

    RMSE_table <- all_estimation |>
        filter(method == method_k) |>
        select(-time, -gamma, -sigma, -method, -type) |>
        pivot_longer(cols = -nb_years, values_to = "RMSE", names_to = "cmp") |>
        summarise(RMSE = mean(RMSE, na.rm = TRUE), .by = c(nb_years, cmp))

    plot(
        ggplot(RMSE_table, aes(x = nb_years, y = RMSE, color = cmp)) +
            geom_line(size = 1) +
            geom_point(size = 2) +
            labs(
                title = paste("RMSE as a function of series length - Method", method_k),
                x = "Series length (years)",
                y = "RMSE",
                color = "Algorithm"
            )
    )
}

# Closing the graphical device
dev.off()
