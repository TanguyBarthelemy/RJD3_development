
library("rjd3toolkit")
library("rjd3workspace")
library("rjd3tramoseats")

# Create a workspace
jws <- .jws_new()
jsap <- .jws_sap_new(jws, "jsap1")

# Add a series
ts <- Imports$Spain
rsafull <- tramoseats_spec("rsafull")
add_sa_item(jsap, "sa", x = ts, spec = rsafull)

.jws_compute(jws)

# Change specification
newspec <- tramoseats_spec("rsa0")

set_specification(jsap = jsap, idx = 1L, spec = newspec)    # ERROR!




jspec <- rjd3tramoseats::.r2jd_spec_tramoseats(newspec)
jspec <- .jcast(jspec, "jdplus/sa/base/api/SaSpecification")
.jcall(
    obj = jsap,
    returnSig = "V",
    method = "setSpecification",
    as.integer(0),
    jspec
)
