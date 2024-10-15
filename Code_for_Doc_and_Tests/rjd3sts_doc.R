# Create new model object
library(rjd3sts)
bsm <- model()

# Add components to the model

llt <- locallineartrend("llt",
    levelVariance = .01, fixedLevelVariance = FALSE,
    slopevariance = .01, fixedSlopeVariance = FALSE
)

seasDOW <- seasonal("seasDOW",
    period = 7, type = "Trigonometric",
    variance = .01, fixed = FALSE
)

seasDOY <- seasonal("seasDOY",
    period = 365.2424, type = "Trigonometric",
    variance = .01, fixed = FALSE
)

# 365.2425: le fonction l'arrondit ..

noise <- noise("wn", variance = .01, fixed = FALSE)

# add components to model
## avec add : un par un

add(bsm, llt)
add(bsm, seasDOW)
add(bsm, seasDOY)
add(bsm, noise)

bsm


## avec agregation tout en meme temps (cree pour cumulator)


# si tout par defaut pas besoin de defnir l'eq de
# Define equation de mesure (boite vide)
eq_m <- rjd3sts::equation("eq")
# Add components to the equation (mesure) (loadings)
add_equation(eq_m, "llt") # argement et pas nom objet R
add_equation(eq_m, "seasDOW")
# add_equation(eq_m,"seasDOY")
add_equation(eq_m, "wn")


# Add measurement equation to the model
add(bsm, eq_m) # noms des objets R

# Estimate the model
class(df_daily$births)
# data<-ts(df_daily$births[1:2000],freq=12)
# data<-as.matrix(df_daily$births[1:2000])
data <- as.numeric(df_daily$births[1:2000])
class(data)
rslt <- rjd3sts::estimate(bsm, data,
    marginal = FALSE,
    concentrated = TRUE, initialization = "SqrtDiffuse"
)
# View(data)
rslt$internal
## seules les fonctions ds "tools" (sts_outliers ..renvoient des obj java)
# selecting results
library(rjd3toolkit)
rjd3toolkit::dictionary(rslt)
# fonction result (marche pour tout), un par un
comps <- rjd3toolkit::result(rslt, "ssf.smoothing.states")
###
add(bsm, llt)
add(bsm, seasDOW)
# add(bsm,seasDOY)
add(bsm, noise)
# -------------------------------------------------------------------------------------------------
# (6) Selected results
# -------------------------------------------------------------------------------------------------

n.usr <- dim(births.reg)[2]

# Retrieve deterministic effects

pre.est <- births.bsm$model$b # Estimates
pre.esd <- sqrt(diag(births.bsm$model$bcov)) # Standard errors
pre.tst <- pre.est / pre.esd # t-statistics

## User-defined deterministic effects

out.usr <- matrix(round(
    cbind(
        pre.est[1:n.usr],
        pre.esd[1:n.usr],
        pre.tst[1:n.usr]
    ),
    digits = c(rep(3, n.usr), rep(4, 2 * n.usr))
), ncol = 3)
dimnames(out.usr) <- list(
    colnames(births.reg),
    c("Est.", "SE", "t-stat.")
)
out.usr

## Automatically detected outliers

n.det <- length(pre.est)
typ.otl <- stringr::str_sub(births.bsm$model$variables[(n.usr + 1):n.det], start = 1, end = 2)
idx.otl <- as.numeric(stringr::str_sub(births.bsm$model$variables[(n.usr + 1):n.det], start = 4))

out.otl <- data.frame(
    "Type" = typ.otl,
    "Date" = births$date[idx.otl],
    "Est." = round(pre.est[(n.usr + 1):n.det], 3),
    "SE" = round(pre.esd[(n.usr + 1):n.det], 4),
    "t-stat." = round(pre.tst[(n.usr + 1):n.det], 4)
) %>%
    arrange(Date) %>%
    mutate(Idx = stringr::str_c(Type, Date, sep = " ")) %>%
    tibble::column_to_rownames(var = "Idx") %>%
    select(3:5)
out.otl

# Retrieve smoothed UC estimates

n.hol <- dim(births %>% select(starts_with("reg")))[2]

births <- births %>%
    mutate("obs.wn" = births.bsm$model$components[, 1]) %>%
    mutate("obs.llt" = births.bsm$model$components[, 2]) %>%
    mutate("obs.slp" = births.bsm$model$components[, 3]) %>%
    mutate("obs.woy" = births.bsm$model$components[, 4]) %>%
    mutate("obs.hol" = as.numeric(births.bsm$model$X[, 1:n.hol] %*% pre.est[1:n.hol])) %>%
    mutate("obs.otl" = as.numeric(births.bsm$model$X[, (n.hol + 1):n.det] %*%
        pre.est[(n.hol + 1):n.det])) %>%
    mutate(obs.csa = obs - (obs.hol + obs.woy))

# Retrieve q-ratios

round(c(
    births.bsm$bsm$final$level, births.bsm$bsm$final$slope, births.bsm$bsm$final$seasonal,
    births.bsm$bsm$final$noise
), 7)
