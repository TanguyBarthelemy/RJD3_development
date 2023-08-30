get_name <- function(x, ...){
    UseMethod("get_name", x)
}
#' @export
get_name.workspace <- function(x, pos){
    sap_i <- get_object(x, pos = pos)
    return(get_name(sap_i))
}
#' @export
get_name.multiprocessing <- function(x, pos){
    if (missing(pos)) {
        return(.jcall(x, "S", "getName"))
    } else {
        sa_item_i <- get_object(x, pos = pos)
        return(get_name(sa_item_i))
    }
}
#' @export
get_name.sa_item <- function(x){
    jt <- .jcall(x, "Ljd2/datatypes/sa/SaItemType;", "getSaDefinition")
    jts <- .jcall(jt, "Ljd2/datatypes/Ts;", "getTs")
    name <- .jcall(jts, "S", "getName")
    # Remove the name of the file link to the saitem
    name <- gsub("^.*\\n", "", name)
    return(name)
}

print(get_name(ws_in))
print(get_name(ws_in, 1))
print(get_name(mp1))
print(get_name(mp1, 1))
print(get_name(sa1_mp1))
print(get_name(sa1_mp1, 1))
