library("dplyr")
devtools::load_all()

path_ws_v2 <- "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#67/ws_V2"

demetra_m_v2 <- read.csv(
    file = "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#67/demetra_m_v2.csv",
    sep = ";",
    dec = ",",
    stringsAsFactors = FALSE,
    na.strings = c("NA", "?"),
    fileEncoding = "latin1",
    quote = ""
)

path_ws_v3 <- "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#67/ws_V3"

demetra_m_v3 <- read.csv(
    file = "C:/Users/UTZK0M/Documents/Projets R/Projets MTS/Packages/rjduniverse/test/RJD3_development/Issues/TODO#67/demetra_m_v3_GUI.csv",
    sep = ";",
    dec = ",",
    stringsAsFactors = FALSE,
    na.strings = c("NA", "?"),
    fileEncoding = "latin1",
    quote = ""
)

QR_v2 <- extract_QR(
    matrix_output_file = paste0(
        path_ws_v2,
        "/Output/SAProcessing-1/demetra_m.csv"
    )
)
extractStatQ(demetra_m = demetra_m_v3)


options(
    jdc_thresholds = list(
        qs_residual_sa_on_sa = c(
            Severe = 0.001,
            Bad = 0.01,
            Uncertain = 0.05,
            Good = Inf
        ),
        qs_residual_sa_on_i = c(
            Severe = 0.001,
            Bad = 0.01,
            Uncertain = 0.05,
            Good = Inf
        ),
        f_residual_sa_on_sa = c(
            Severe = 0.001,
            Bad = 0.01,
            Uncertain = 0.05,
            Good = Inf
        ),
        f_residual_sa_on_i = c(
            Severe = 0.001,
            Bad = 0.01,
            Uncertain = 0.05,
            Good = Inf
        ),
        f_residual_td_on_sa = c(
            Severe = 0.001,
            Bad = 0.01,
            Uncertain = 0.05,
            Good = Inf
        ),
        f_residual_td_on_i = c(
            Severe = 0.001,
            Bad = 0.01,
            Uncertain = 0.05,
            Good = Inf
        ),
        residuals_independency = c(Bad = 0.01, Uncertain = 0.1, Good = Inf),
        residuals_homoskedasticity = c(Bad = 0.01, Uncertain = 0.1, Good = Inf),
        residuals_skewness = c(Bad = 0.01, Uncertain = 0.1, Good = Inf),
        residuals_kurtosis = c(Bad = 0.01, Uncertain = 0.1, Good = Inf),
        residuals_normality = c(Bad = 0.01, Uncertain = 0.1, Good = Inf),
        oos_mean = c(Bad = 0.01, Uncertain = 0.1, Good = Inf),
        oos_mse = c(Bad = 0.01, Uncertain = 0.1, Good = Inf),
        m7 = c(Good = 1, Bad = 2, Severe = Inf),
        q = c(Good = 1, Bad = Inf),
        q_m2 = c(Good = 1, Bad = Inf),
        pct_outliers = c(Good = 3, Uncertain = 5, Bad = Inf)
    )
)

colnames(QR_v2$modalities)


variable <- "pct_outliers"

v <- data.frame(
    mod = QR_v2$modalities[, variable],
    val = QR_v2$values[, variable]
) |>
    mutate(
        mod2 = cut(
            x = val,
            breaks = c(-Inf, getOption("jdc_thresholds")[[variable]]),
            labels = names(getOption("jdc_thresholds")[[variable]]),
            right = FALSE,
            include.lowest = TRUE,
            ordered_result = TRUE
        ),
        diff = as.character(mod) == as.character(mod2)
    )

print(v |> filter(mod == "Severe") |> select(val) |> max())
print(v |> filter(mod == "Bad") |> select(val) |> min())

print(v |> filter(mod == "Bad") |> select(val) |> max())
print(v |> filter(mod == "Uncertain") |> select(val) |> min())

print(v |> filter(mod == "Uncertain") |> select(val) |> max())
print(v |> filter(mod == "Good") |> select(val) |> min())

v[!v$diff & !is.na(v$diff), ]
