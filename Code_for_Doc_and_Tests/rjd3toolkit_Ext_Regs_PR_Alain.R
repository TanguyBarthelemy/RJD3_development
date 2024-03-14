### KEY = clarify doc


### pull request pas comprise
library("rjd3toolkit")
y_raw - rjd3toolkit::ABS$X0.2.09.10.M
regs_td <- td(s = y_raw, groups = c(1, 2, 3, 4, 5, 6, 0),
              contrasts = TRUE)
colnames(regs_td) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
class(regs_td)
variables <- list(regs_td, Tuesday = regs_td[,2], r = list(Monday = regs_td[,1]), Thursday = regs_td[,4])
my_context <- modelling_context(variables=variables)
.r2jd_modellingcontext(my_context)$getTsVariableDictionary()
#> [1] "Java-Object{[var.Monday, var.Tuesday, var.Wednesday, var.Thursday, var.Friday,
#> var.Saturday, v3.Monday, r.Tuesday, r.Thursday]}"
#>

## erreur chez moi tant que pas acceptée


# cas 1 : regs_td : mts nommé : va dans groupe r
# cas 2 : un ts dans gpe 1
# cas 3 : un nouveau groupe mais par malheur nommé r
