
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjd3highfreq

<!-- badges: start -->
<!-- badges: end -->

High-frequency time series

## Installation

You can install the development version of **rjd3highfreq** from
[GitHub](https://github.com/) with:

``` r
# Install development version from GitHub
# install.packages("remotes")
remotes::install_github("rjdemetra/rjd3toolkit")
remotes::install_github("rjdemetra/rjd3sts")
remotes::install_github("rjdemetra/rjd3highfreq")
```

## Demonstration with the daily french births

``` r
library("rjd3highfreq")


plot_jd <- function(x, y, col, legend_txt = NULL, ...){
    
    col_bg <- "#f5f4e7"
    col_grid <-"#dadad3"
    y_range <- range(do.call(c, y))
    
    plot.new()
    rect(xleft = par("usr")[1], xright = par("usr")[2], 
         ytop = par("usr")[4], ybottom = par("usr")[3], col = col_bg)
    par(new = TRUE)
    plot(y = y[[1]], x = x,
         col = col[1], type = "l", xlab = "", ylab = "", ylim = y_range,
         main = "", xaxt = "n", yaxt = "n")
    x_breaks <- Axis(x, side = 1)
    par(xaxp = c(x_breaks[1], x_breaks[length(x_breaks)], length(x_breaks) - 1))
    grid(nx = NULL, ny = NULL, col = col_grid)
    par(new = TRUE)
    
    plot(y = y[[1]],
         x = x,
         col = col[1], type = "l", xlab = "Time", ylim = y_range, 
         xaxt = "n", ...)
    
    for (k in (seq_along(y[-1]) + 1)) {
        lines(y = y[[k]],
              x = x,
              col = col[k], ...)
    }
    
    box(col = col_grid)
    
    if (!is.null(legend_txt)) {
        legend("bottomleft", legend = legend_txt, 
               pch = 16, col = col, horiz = TRUE, xpd = TRUE, 
               inset = c(0, 1), bty = "n")
    }
    
    return(invisible(NULL))
}

#' @export
plot.JDFractionalAirlineEstimation <- function(x, from, to, ...) {
    
    col_y <- "#f1ba1d"
    col_t <- "#1e6c0b"
    col_sa <- "#00488c"
    
    col_s1 <- "#ffab78"
    col_s2 <- "#9169be"
    
    y <- x$model$y
    y_lin <- x$model$linearized
    if (x$model$log) {
        y_lin <- exp(y_lin)
    }
    
    vect_x <- x$model$y_time
    if (is.null(vect_x)) {
        vect_x <- seq_along(y)
    } else {
        if (!missing(from)) {
            vect_x <- vect_x[vect_x >= from]
        }
        if (!missing(to)) {
            vect_x <- vect_x[vect_x <= to]
        }
        y <- y[which(x$model$y_time %in% vect_x)]
        y_lin <- y_lin[which(x$model$y_time %in% vect_x)]
    }
    
    list_args <- list(...)
    list_args$main <- ifelse("main" %in% names(list_args), 
                             yes = paste0("Raw data and linearised series", " - ", list_args$main), 
                             no = "Raw data and linearised series")
    list_args$ylab <- ifelse("ylab" %in% names(list_args), 
                             yes = list_args$ylab, no = "")
    list_args$col <- c(col_y, col_t)
    
    do.call(plot_jd, 
            c(list(
                x = vect_x, y = list(y, y_lin), 
                legend_txt = c("Raw data", "Linearised series")), 
              list_args)
    )
    
    return(invisible(NULL))
}

#' @export
plot.JDFractionalAirlineDecomposition <- function(
        x, from, to, type_chart = c("y-sa-trend", "cal-seas-irr"), ...) {
    
    if ("y-sa-trend" %in% type_chart) {
        
        col_y <- "#f1ba1d"
        col_t <- "#1e6c0b"
        col_sa <- "#00488c"
        
        y <- x$decomposition$y
        sa <- x$decomposition$sa
        tc <- x$decomposition$t
        if (x$estimation$log) {
            y <- exp(y)
            sa <- exp(sa)
            tc <- exp(tc)
        }
        
        vect_x <- x$decomposition$y_time
        if (is.null(vect_x)) {
            vect_x <- seq_along(y)
        } else {
            if (!missing(from)) {
                vect_x <- vect_x[vect_x >= from]
            }
            if (!missing(to)) {
                vect_x <- vect_x[vect_x <= to]
            }
            time_lim <- which(x$decomposition$y_time %in% vect_x)
            y <- y[time_lim]
            sa <- sa[time_lim]
            tc <- tc[time_lim]
        }
        
        list_args <- list(...)
        list_args$main <- ifelse("main" %in% names(list_args), 
                                 paste0("Decomposition AMB", " - ", list_args$main), 
                                 "Decomposition AMB")
        list_args$ylab <- ifelse("ylab" %in% names(list_args), 
                                 yes = list_args$ylab, no = "")
        list_args$col <- c(col_y, col_sa, col_t)
        
        do.call(plot_jd, 
                c(list(
                    x = vect_x, y = list(y, sa, tc), 
                    legend_txt = c("Raw data", "Seasonnal adjusted", "Trend")), 
                  list_args)
        )
    }
    
    if ("cal-seas-irr" %in% type_chart) {
        
        col_s <- c("#ffab78", "#9169be", "#4d8076", "#c34a36", "#00c9a7")
        s_variables <- names(x$decomposition)
        s_variables <- s_variables[grepl("^s(?!a)", s_variables, perl = TRUE)]
        
        s <- x$decomposition[s_variables]
        ic <- x$decomposition$i
        if (x$estimation$log) {
            s <- lapply(X = s, FUN = exp)
            ic <- exp(ic)
        }
        
        vect_x <- x$decomposition$y_time
        if (is.null(vect_x)) {
            vect_x <- seq_along(ic)
        } else {
            if (!missing(from)) {
                vect_x <- vect_x[vect_x >= from]
            }
            if (!missing(to)) {
                vect_x <- vect_x[vect_x <= to]
            }
            time_lim <- which(x$decomposition$y_time %in% vect_x)
            
            s <- lapply(X = s, FUN = base::`[`, time_lim)
            ic <- ic[time_lim]
        }
        
        list_args <- list(...)
        list_args$main <- ifelse("main" %in% names(list_args), 
                                 paste0("Irregular and Seasonal components", " - ", list_args$main), 
                                 "Irregular and Seasonal components")
        list_args$ylab <- ifelse("ylab" %in% names(list_args), 
                                 yes = list_args$ylab, no = "")
        list_args$col <- col_s[seq_len(length(s) + 1)]
        
        do.call(plot_jd, 
                c(list(
                    x = vect_x, y = c(s, list(ic)), 
                    legend_txt = c(s_variables, "Irregular")), 
                  list_args)
        )
    }
    
    return(invisible(NULL))
}
```

``` r
## Import of data
# df_daily <- read.csv2("https://raw.githubusercontent.com/TanguyBarthelemy/Tsace_RJD_Webinar_Dec22/b5fcf6b14ae47393554950547ef4788a0068a0f6/Data/TS_daily_births_franceM_1968_2020.csv")
df_daily <- read.csv2("../../data/TS_daily_births_franceM_1968_2020.csv")

# Creation of log variables to multiplicative model
df_daily$log_births <- log(df_daily$births)
df_daily$date <- as.Date(df_daily$date)

print(head(df_daily))
#>         date births id.dom id.dow id.moy log_births
#> 1 1968-01-01   2080      1 lun\\.   janv   7.640123
#> 2 1968-01-02   2319      2 mar\\.   janv   7.748891
#> 3 1968-01-03   2335      3 mer\\.   janv   7.755767
#> 4 1968-01-04   2338      4 jeu\\.   janv   7.757051
#> 5 1968-01-05   2319      5 ven\\.   janv   7.748891
#> 6 1968-01-06   2212      6 sam\\.   janv   7.701652
```

Plot of the raw series:

<img src="man/figures/README-raw data plot-1.png" width="100%" />

Preparation of the calendar with the package **rjd3toolkit**:

``` r
# French calendar
frenchCalendar <- rjd3toolkit::national_calendar(days = list(
  rjd3toolkit::fixed_day(7, 14), # Bastille Day
  rjd3toolkit::fixed_day(5, 8, validity = list(start = "1982-05-08")), # End of 2nd WW
  rjd3toolkit::special_day('NEWYEAR'),
  rjd3toolkit::special_day('MAYDAY'), # 1st may
  rjd3toolkit::special_day('EASTERMONDAY'),
  rjd3toolkit::special_day('ASCENSION'),
  rjd3toolkit::special_day('WHITMONDAY'),
  rjd3toolkit::special_day('ASSUMPTION'),
  rjd3toolkit::special_day('ALLSAINTSDAY'), # Toussaint
  rjd3toolkit::special_day('ARMISTICE'), # End of 1st WW
  rjd3toolkit::special_day('CHRISTMAS'))
)
```

Creation of the calendar regressor in a matrix with the package
**rjd3toolkit**:

``` r
# Calendar regressor matrix
cal_reg <- rjd3toolkit::holidays(
    calendar = frenchCalendar, 
    start = "1968-01-01", length = nrow(df_daily), 
    type = "All", nonworking = 7L)

colnames(cal_reg) <- c("14th_july", "8th_may", "1st_jan", "1st_may",
                       "east_mon", "asc", "pen_mon",
                       "15th_aug", "1st_nov", "11th_nov", "Xmas")
```

Preprocessing with the function `fractionalAirlineEstimation`:

``` r
pre_pro <- fractionalAirlineEstimation(
    y = df_daily$births, 
    x = cal_reg, 
    periods = 7, # weekly frequency
    outliers = c("ao", "wo"), log = TRUE, y_time = df_daily$date)

print(pre_pro)
#> Number of observations: 19359
#> Start: 1968-01-01 
#> End: 2020-12-31 
#> 
#> Estimate MA parameters:
#>       MA_parameter      Coef     Coef_SE    Tstat
#>           Theta(1) 0.7620698 0.005571472 136.7807
#>  Theta(period = 7) 0.9731793 0.001413477 688.5002
#> 
#> Number of calendar regressors: 11 , Number of outliers : 7
#> 
#> TD regressors coefficients:
#>   Variable    Coef Coef_SE    Tstat
#>  14th_july -0.1226  0.0047 -26.0615
#>    8th_may -0.1419  0.0054 -26.3419
#>    1st_jan -0.2223  0.0047 -47.3511
#>    1st_may -0.1225  0.0047 -26.2643
#>   east_mon -0.1891  0.0046 -40.7635
#>        asc -0.1726  0.0046 -37.1949
#>    pen_mon -0.1900  0.0046 -40.9429
#>   15th_aug -0.1181  0.0047 -25.3461
#>    1st_nov -0.1503  0.0046 -32.5662
#>   11th_nov -0.1238  0.0046 -26.8142
#>       Xmas -0.2310  0.0046 -49.7435
#> 
#> Outliers coefficients:
#>       Variable    Coef Coef_SE   Tstat
#>  WO.1999-12-31 -0.1762  0.0226 -7.7916
#>  AO.1995-08-15 -0.2224  0.0340 -6.5503
#>  WO.1999-12-24 -0.1447  0.0226 -6.3981
#>  AO.2012-01-01  0.2098  0.0340  6.1786
#>  AO.1998-07-14 -0.2101  0.0340 -6.1880
#>  AO.1997-07-14 -0.2092  0.0340 -6.1602
#>  AO.1995-05-01 -0.2042  0.0340 -6.0146
#> 
#> Sum of square residuals: 25.17 on 19330 degrees of freedom
#> Log likelihood = 3.682e+04, 
#>  aic = -7.361e+04, 
#>  aicc = -7.361e+04, 
#>  bic(corrected for length) = -6.635
#> Hannan–Quinn information criterion = -7.355e+04
```

``` r
plot(pre_pro, main = "French births")
```

<img src="man/figures/README-preprocessing plots-1.png" width="100%" />

``` r
plot(x = pre_pro, 
     from = as.Date("2000-01-01"), to = as.Date("2000-12-31"), 
     main = "French births in 2000")
```

<img src="man/figures/README-preprocessing plots-2.png" width="100%" />

Decomposition with the AMB (Arima Model Based) algorithm:

``` r
# Decomposition with weekly pattern
amb.dow <- rjd3highfreq::fractionalAirlineDecomposition(
    y = pre_pro$model$linearized, # linearized series from preprocessing
    period = 7, 
    log = TRUE, y_time = df_daily$date)

# Extract day-of-year pattern from day-of-week-adjusted linearised data
amb.doy <- rjd3highfreq::fractionalAirlineDecomposition(
    y = amb.dow$decomposition$sa, # DOW-adjusted linearised data
    period = 365.2425, # day of year pattern
    log = TRUE, y_time = df_daily$date)
```

Plot:

``` r
plot(amb.dow, main = "Weekly pattern")
```

<img src="man/figures/README-amb plots-1.png" width="100%" /><img src="man/figures/README-amb plots-2.png" width="100%" />

``` r
plot(amb.dow, main = "Weekly pattern - January 2018", 
     from = as.Date("2018-01-01"), 
     to = as.Date("2018-01-31"))
```

<img src="man/figures/README-amb plots-3.png" width="100%" /><img src="man/figures/README-amb plots-4.png" width="100%" />

``` r

plot(amb.doy, main = "Yearly pattern")
```

<img src="man/figures/README-amb plots-5.png" width="100%" /><img src="man/figures/README-amb plots-6.png" width="100%" />

``` r
plot(amb.doy, main = "Weekly pattern - 2000 - 2002", 
     from = as.Date("2000-01-01"), 
     to = as.Date("2002-12-31"))
```

<img src="man/figures/README-amb plots-7.png" width="100%" /><img src="man/figures/README-amb plots-8.png" width="100%" />

Perform an Arima Model Based (AMB) decomposition on several periodcities
at once:

Plot the comparison between the two AMB methods for the annual
periodicity:

<img src="man/figures/README-plot amb.multi-1.png" width="100%" /><img src="man/figures/README-plot amb.multi-2.png" width="100%" />

With the package
[**rjd3x11plus**](https://github.com/rjdemetra/rjd3x11plus), you can
perform an X-11 like decomposition with any (non integer) periodicity.
