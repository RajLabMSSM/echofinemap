#' POLYFUN: check version
#' 
#' Check PolyFun version installed.
#' @inheritParams POLYFUN_help
#' @inheritParams echoconda::find_python_path
#' @keywords internal
POLYFUN_check_version <- function(polyfun=NULL,
                                  conda_env="echoR_mini",
                                  verbose=TRUE){ 
        h <- POLYFUN_help(polyfun = polyfun, 
                          conda_env = conda_env, 
                          verbose = verbose)
        l <- grep("Version ",h,value = TRUE)
        ver <- package_version(rev(strsplit(l," ")[[1]])[1])
        messager("POLYFUN version:",ver,v=verbose)
        return(ver)
}
