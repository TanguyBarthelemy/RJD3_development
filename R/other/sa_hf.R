f <- function(
        y,
        periods,
        x = NULL,
        ndiff = 2,
        ar = FALSE,
        mean = FALSE,
        outliers = NULL,
        criticalValue = 6,
        precision = 1e-12,
        approximateHessian = FALSE,
        sn = FALSE,
        stde = FALSE,
        nbcasts = 0,
        nfcasts = 0,
        log = FALSE,
        y_time = NULL
) {

    pre_adj <- rjd3highfreq::fractionalAirlineEstimation(
        y = y,
        y_time = y_time,
        x = q, # q= calendar regressors matrix
        periods = periods,
        ndiff = 2,
        ar = FALSE,
        mean = FALSE,
        outliers = c("ao", "wo"), # type of outliers detected
        criticalValue = 0, # automatically set
        precision = 1e-9,
        approximateHessian = TRUE,
        log = log
    )

    sa <- pre_adj$model$linearized

    for (p in sort(periods)) {
        amb <- rjd3highfreq::fractionalAirlineDecomposition(
            y = sa, # input time series
            period = p, # DOW pattern
            sn = FALSE, # Signal (SA)-noise decomposition
            stde = FALSE, # Calculate standard deviations
            nbcasts = 0, nfcasts = 0,
            log = log, y_time = y_time
        ) # Numbers of back- and forecasts
        sa <- amb$decomposition$sa
    }

    # ici faire des calculs, des regroupements pour conserver des éléments du pre_adj et de TOUS les amb et pas seulement le dernier

    return(amb)
}
