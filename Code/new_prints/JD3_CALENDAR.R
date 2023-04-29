print_JD3_CALENDAR <- function(x, ...) {
    cat("Holiday:\n")
    for (day in x$day) {
        cat("\t- ")
        print(day)
        cat('\n')
    }
    cat("\nMean correction: ", ifelse(x$mean_correction, "Yes", "No"), "\n", sep = "")
    
    return(invisible(x))
}
