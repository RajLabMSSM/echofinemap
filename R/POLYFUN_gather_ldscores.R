#' Find and import LD score files
#' @keywords internal
#' @family polyfun
#' @examples
#' \dontrun{
#' output_prefix <- file.path(system.file("tools/polyfun/gold","",package = "echofinemap"),"testrun.22")
#' ldscore <- POLYFUN_gather_ldscores(output_prefix=output_prefix)
#' }
POLYFUN_gather_ldscores <- function(output_prefix){
    ldscore.files <-  list.files(dirname(output_prefix),
                                 pattern = ".l2.ldscore.parquet", full.names = TRUE)
    if(length(ldscore.files)>1){messager("POLYFUN:: >1 ldscore file detected. Only using the first:",ldscore.files[1])}
    parquor <- read_parquet(ldscore.files[1])
    return(parquor)
}
