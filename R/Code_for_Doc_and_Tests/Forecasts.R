library(rjd3toolkit)
library(rjd3x13)
y <- rjd3toolkit::ABS$X0.2.09.10.M
end(y)
# forecast series has to be defined
userdefined_variables_x13(x = "RegArima")


regarima_y <- rjd3x13::regarima(y, spec = "RG3", userdefined = "y_f(-1)")

y_f <- regarima_y$user_defined
y_f

# # Tout est inclus dans sa_x13_v3

# # version 3
# ud <- userdefined_variables_x13()[15:17] # b series
# ud
# sa_x13_v3_UD <- rjd3x13::x13(y_raw, "RSA5c", userdefined = ud)
# sa_x13_v3_UD$user_defined # remainder of the names
# # retrieve the object
# sa_x13_v3_UD$user_defined$decomposition.b1

# You can try the following code using rjd3x13::regarima (or in a similar way rjd3tramoseats::tramo())

library(rjd3toolkit)
library(rjd3x13)
y <- rjd3toolkit::ABS$X0.2.09.10.M
end(y)
# forecast series has to be defined in the user defined output
# check the list of names
userdefined_variables_x13(x = "RegArima")
# you want y_f(): forecast raw series with horizon in the (), (1) = 1 month (positive integers), (-1) = one year (negative integers)

regarima_y <- rjd3x13::regarima(y, spec = "RG3", userdefined = "y_f(-1)")
# check the defaults specs in function's help page

# retrieve forecasts
y_f <- regarima_y$user_defined
y_f
