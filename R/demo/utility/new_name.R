new_name <- function(complexity = 7,
                     with_numbers = TRUE,
                     with_capital_letter = FALSE,
                     with_lowercase_letter = TRUE) {
    if (complexity <= 0) stop("La complexité doit être positive.")
    if (with_numbers + with_capital_letter + with_lowercase_letter == 0) {
        stop("Le nom doit comporter au moins des lettres ou des chiffres.")
    }

    reservoir <- c()

    if (with_capital_letter) reservoir <- c(reservoir, LETTERS)
    if (with_lowercase_letter) reservoir <- c(reservoir, letters)
    if (with_numbers) reservoir <- c(reservoir, 0:9)

    return(paste0(sample(reservoir, size = complexity, replace = TRUE),
        collapse = ""
    ))
}
