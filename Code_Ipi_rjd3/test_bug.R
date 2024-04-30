
library("rJava")
k <- 121
{
#get_model
x <- ws_n_1 |> get_object(1L) |> get_object(k)
workspace <- ws_n_1
jsa_result <- RJDemetra:::get_jmodel.sa_item(x, workspace)
if (is.null(jsa_result))
    return(NULL)
jspec <- jsa_result[["spec"]]
jresult <- jsa_result[["result"]]@internal
y_ts <- get_ts(x)
context_dictionary <- .jcall(workspace, "Lec/tstoolkit/algorithm/ProcessingContext;",
                             "getContext")


userdefined = NULL
context_dictionary = NULL

extra_info = FALSE
freq = NA

jrslt = jresult
spec = jspec
context_dictionary = context_dictionary
extra_info = TRUE
freq = frequency(y_ts)

#x13JavaResults

jrarima <- .jcall(jrslt, "Lec/tstoolkit/jdr/regarima/Processor$Results;",
                  "regarima")
jrobct_arima <- new(Class = "RegArima_java", internal = jrarima)
jrobct <- new(Class = "X13_java", internal = jrslt)
if (is.null(jrobct@internal)) {
    return(NULL)
}
res <- jrslt$getResults()$getProcessingInformation()
if (is.null(jrslt$getDiagnostics()) & !.jcall(res, "Z",
                                              "isEmpty")) {
    proc_info <- jrslt$getResults()$getProcessingInformation()
    error_msg <- .jcall(proc_info, "Ljava/lang/Object;",
                        "get", 0L)$getErrorMessages(proc_info)
    warning_msg <- .jcall(proc_info, "Ljava/lang/Object;",
                          "get", 0L)$getWarningMessages(proc_info)
    if (!.jcall(error_msg, "Z", "isEmpty"))
        stop(error_msg$toString())
    if (!.jcall(warning_msg, "Z", "isEmpty"))
        warning(warning_msg$toString())
}
reg <- RJDemetra:::regarima_defX13(jrobj = jrobct_arima, spec = spec,
                       context_dictionary = context_dictionary, extra_info = extra_info,
                       freq = freq)


#RJDemetra:::decomp_defX13
jrobj = jrobct
spec = spec
freq = freq

specification <- RJDemetra:::specX11_jd2r(spec = spec, freq = freq)
rownames(specification) <- ""

#RJDemetra:::decomp_rsltsX13
mode <- RJDemetra:::result(jrobj, "mode")
mstats_rownames <- c(sprintf("M(%s)", 1:11), "Q", "Q-M2")
mstats_names <- sprintf("mstats.%s", mstats_rownames)

RJDemetra:::result(jrobj, "mstats.M(1)")
#RJDemetra:::proc_data(jrobj@internal, "mstats.M(1)")

rslt <- jrobj@internal
name <- "mstats.M(1)"
}

if(is.null(RJDemetra:::rjdemetra_java$clobject)){
    RJDemetra:::rjdemetra_java$clobject <- .jcall("java/lang/Class", "Ljava/lang/Class;", "forName", "java.lang.Object")
}
s <- .jcall(rslt, "Ljava/lang/Object;", "getData", name, RJDemetra:::rjdemetra_java$clobject)


if (is.null(s))
    res_mstat <- NULL
if (.jinstanceof(s, "ec.tstoolkit.timeseries.simplets.TsData"))
    res_mstat <-ts_jd2r(.jcast(s,"ec.tstoolkit.timeseries.simplets.TsData"))
else if (.jinstanceof(s, "ec.tstoolkit.maths.matrices.Matrix"))
    res_mstat <-matrix_jd2r(.jcast(s,"ec.tstoolkit.maths.matrices.Matrix"))
else if (.jinstanceof(s, "ec.tstoolkit.information.StatisticalTest"))
    res_mstat <-test_jd2r(s)
else if (.jinstanceof(s, "ec.tstoolkit.Parameter")){
    val<-.jcall(s, "D", "getValue")
    e<-.jcall(s, "D", "getStde")
    res_mstat <-c(val, e)
} else if (.jinstanceof(s, "[Lec.tstoolkit.Parameter;")){
    p<-.jcastToArray(s)
    len<-length(p)
    if (len==0)
        res_mstat <-NULL
    all<-array(0, dim=c(len,2))
    for (i in 1:len){
        all[i, 1]<-.jcall(p[[i]], "D", "getValue")
        all[i, 2]<-.jcall(p[[i]], "D", "getStde")
    }
    res_mstat <-all
} else if (.jcall(.jcall(s, "Ljava/lang/Class;", "getClass"), "Z", "isArray"))
    res_mstat <-.jevalArray(s, silent=TRUE)
else if (.jinstanceof(s, "java/lang/Number"))
    res_mstat <-.jcall(s, "D", "doubleValue")
else if (.jinstanceof(s, "ec/tstoolkit/information.RegressionItem"))
    res_mstat <-reg_item_jd2r(s)
else
    res_mstat <-.jcall(s, "S", "toString")





mstats <- lapply(mstats_names, function(diag) {
    RJDemetra:::result(jrobj, diag)
})
mstats <- matrix(unlist(mstats), ncol = 1)




rownames(mstats) <- mstats_rownames
colnames(mstats) <- c("M stats")
d8 <- result(jrobj, "decomposition.d8")
d10 <- result(jrobj, "decomposition.d10")
si_ratio <- cbind(d8 = d8, d10 = d10)
s_filter <- result(jrobj, "decomposition.d9filter")
t_filter <- result(jrobj, "decomposition.d12filter")
z <- list(mode = mode, mstats = mstats, si_ratio = si_ratio,
          s_filter = s_filter, t_filter = t_filter)
return(z)





z <- list(specification = specification, mode = jd_results$mode,
          mstats = jd_results$mstats, si_ratio = jd_results$si_ratio,
          s_filter = jd_results$s_filter, t_filter = jd_results$t_filter)
class(z) <- c("decomposition_X11")
deco <- z
#FIN  RJDemetra:::decomp_defX13














fin <- RJDemetra:::final(jrobj = jrobct)
diagn <- RJDemetra:::diagnostics(jrobj = jrobct)
z <- list(regarima = reg, decomposition = deco, final = fin,
          diagnostics = diagn, user_defined = user_defined(userdefined,
                                                           jrobct))
class(z) <- c("SA", "X13")

#FIN  x13JavaResults