

library("rjd3toolkit")
library("rjd3x13")

# Je créé une spec par défaut
a <- x13_spec("RSA5C")

# j'affiche le SARIMA et j'obtiens des ordres alors que je devrais obtenir (p, d, q)(P, D, Q)
a$regarima$arima
class(a$regarima$arima)

# Cela vient de la fonction rjd3toolkit:::.sarima_coef_table.default
rjd3toolkit:::.sarima_coef_table.default(a$regarima$arima)

# A ce niveau là on a 2 questions :
# - pourquoi il n'y a pas de méthode JD3_SARIMA_ESTIMATION à cette fonction ?
# - comment sont fixés les coefficients par défaut du ARIMA

# On remarque que pour une spec estimé, on a la même classe :
mod <- x13(rjd3toolkit::ABS[, 3])
# estimation ARIMA
mod$estimation_spec$regarima$arima

# results ARIMA
mod$result_spec$regarima$arima
mod$result$preprocessing$description$arima

class(mod$estimation_spec$regarima$arima)
class(mod$result_spec$regarima$arima)

# Alors à quoi sert la classe JD3_SARIMA_ESTIMATE ?


# Enfin si on veut différencier l'affichage, il faut pouvoir différencier
# - les coefficuients estimés (JD3_SARIMA_ESTIMATE ?),
# - des coefficients fixés par l'utilisateur,
# - des coefficients fixés par l'algo,
# - des coefficients laissé vides (ou absents de la liste) ?


