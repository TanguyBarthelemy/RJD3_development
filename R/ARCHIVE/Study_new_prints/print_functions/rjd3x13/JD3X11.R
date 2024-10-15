print_JD3X11 <- function(x) {
    table <- do.call(cbind, x[grepl(pattern = "^d(\\d+)$", x = names(x))])

    cat("Last values\n")
    print(tail(.preformat.ts(table)))

    return(invisible(x))
}
