
print_JDX11 <- function(x, digits = max(3L, getOption("digits") - 3L)#, 
                        #starting = as.Date("1968-01-01")
                        ) {
    
    
    if (x$parameters$multiplicative) cat("Multiplicative model")
    if (!x$parameters$multiplicative) cat("Additive model")
    cat("\n\n")
    
    cat("Trend:", 
        "\n\tFilter:", x$parameters$trend.horizon, 
        "\n\tPolynomial order:", x$parameters$trend.degree, 
        "\n\tType:", x$parameters$trend.kernel)
    
    cat("\n\n")
    cat("Decomposition:")
    cat("\n")
    decompo_table <- do.call(cbind, x$decomposition)
    # intervalle <- seq(starting, starting + decompo_table |> nrow() - 1, by = "day")
    # print(zoo::zoo(decompo_table, intervalle) |> tail(n = 10))
    print(decompo_table |> tail(n = 10))
    cat("\n")
    
    cat("Boundaries used for outlier correction in irregular :",
        "\n\t lower_sigma: ", x$parameters$extreme.lsig,
        "\n\t upper_sigma: ", x$parameters$extreme.usig)
    
    cat("\n\n")
    return(invisible(x))
}
