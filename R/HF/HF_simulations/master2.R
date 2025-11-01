tictoc::tic()
## Appel des packages ----------------------------------------------------------

library("future")
library("future.apply")


## Appel des fonctions ---------------------------------------------------------

source("R/HF/HF_simulations/utils.R")
source("R/HF/HF_simulations/utils-run.R")


## Paramétrage -----------------------------------------------------------------

plan(multisession, workers = parallelly::availableCores() - 1)


## Temps de calcul -------------------------------------------------------------

set.seed(123)

S1 <- run_estimation(nb_years = 5L, gamma = 0.2, deterministic = TRUE)
S2 <- run_estimation(nb_years = 5L, gamma = 0.4, deterministic = TRUE)
S3 <- run_estimation(nb_years = 5L, gamma = 0.6, deterministic = TRUE)
S4 <- run_estimation(nb_years = 5L, gamma = 0.2, sigma2 = 0.025, deterministic = FALSE)
S5 <- run_estimation(nb_years = 5L, gamma = 0.4, sigma2 = 0.05, deterministic = FALSE)
S6 <- run_estimation(nb_years = 5L, gamma = 0.6, sigma2 = 0.075, deterministic = FALSE)

all_estimation <- rbind(
    compute_summary(S1),
    compute_summary(S2),
    compute_summary(S3),
    compute_summary(S4),
    compute_summary(S5),
    compute_summary(S6)
)

# parallel::stopCluster(cl)

tictoc::toc()
