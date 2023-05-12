## Error ?

spec_x13_d <- rjd3x13::spec_x13("rsa3")
spec_x13_d <- set_tradingdays(spec_x13_d, stocktd = 28)
sa_x13_d <- rjd3x13::x13(y_raw, spec_x13_d)

