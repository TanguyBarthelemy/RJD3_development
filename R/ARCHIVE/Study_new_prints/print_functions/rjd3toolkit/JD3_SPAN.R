print_JD3_SPAN <- function(x, ...) {
    span <- x
    type <- span$type
    d0 <- span$d0
    d1 <- span$d1
    n0 <- span$n0
    n1 <- span$n1

    if (type == "ALL") {
        x <- "All"
    } else if (type == "FROM") {
        x <- paste("From", d0, sep = " ")
    } else if (type == "TO") {
        x <- paste("Until", d1, sep = " ")
    } else if (type == "BETWEEN") {
        x <- paste(d0, d1, sep = " - ")
    } else if (type == "FIRST") {
        x <- paste("First", n0, "periods", sep = " ")
    } else if (type == "LAST") {
        x <- paste("Last", n1, "periods", sep = " ")
    } else if (type == "EXCLUDING") {
        x <- paste("All but first", n0, "periods and last", n1, "periods", sep = " ")
    } else {
        x <- "Undefined"
    }

    cat(x, "\n")

    return(invisible(x))
}
