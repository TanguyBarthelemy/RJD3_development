
print_JD3X11 <- function(x) {
    
    series <- names(x)[toupper(names(x)) %in% dictionnary$name]
    table_output <- do.call(cbind, x[series])
    
    colnames(table_output) <- dictionnary[dictionnary$name %in% toupper(colnames(table_output)), "definition"]
    
    cat("Last values\n")
    print(tail(.preformat.ts(table_output)))
    
    return(invisible(x))
}
