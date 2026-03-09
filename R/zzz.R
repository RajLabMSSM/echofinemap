#' @importFrom stats sd
#' @importFrom utils capture.output find read.csv tail write.csv
NULL

.onLoad <- function(libname, pkgname){
    .datatable.aware <- TRUE
}