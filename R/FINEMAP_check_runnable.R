#' Check FINEMAP is runnable
#'
#' Test that the FINEMAP binary can actually execute on the current system.
#' On Apple Silicon Macs, the x86_64 FINEMAP binary requires Rosetta 2
#' and x86_64 versions of several dynamic libraries (zstd, gcc, libomp).
#'
#' @param FINEMAP_path Path to the FINEMAP executable.
#' @param verbose Print messages.
#' @returns \code{TRUE} if FINEMAP runs successfully, otherwise stops with
#' an informative error message.
#' @keywords internal
#' @importFrom echodata get_os
FINEMAP_check_runnable <- function(FINEMAP_path,
                                   verbose = TRUE){

    messager("Checking FINEMAP is runnable...", v = verbose)
    out <- tryCatch({
        suppressWarnings(
            system(paste(FINEMAP_path, "-h"),
                   intern = TRUE, ignore.stderr = TRUE)
        )
    }, error = function(e) e,
       warning = function(w) w)

    exit_code <- attr(out, "status")

    ## Success
    if(is.null(exit_code) || exit_code == 0){
        messager("FINEMAP is runnable.", v = verbose)
        return(TRUE)
    }

    ## Exit code 134 = dylib loading failure
    if(isTRUE(exit_code == 134)){
        ## Get detailed error
        err_msg <- tryCatch(
            system(paste(FINEMAP_path, "-h"), intern = TRUE,
                   ignore.stdout = TRUE),
            warning = function(w) conditionMessage(w),
            error = function(e) conditionMessage(e)
        )

        OS <- echodata::get_os()
        arch <- Sys.info()[["machine"]]

        if(OS == "osx" && arch == "arm64"){
            stp <- paste0(
                "FINEMAP binary (x86_64) cannot run on Apple Silicon ",
                "without the required x86_64 dynamic libraries.\n\n",
                "The FINEMAP binary requires x86_64 versions of: ",
                "libzstd, libgfortran, libstdc++, libomp, libgcc_s\n\n",
                "To fix this, install Intel Homebrew and the required ",
                "libraries:\n",
                "  1. Install Intel Homebrew (if not already installed):\n",
                "     arch -x86_64 /bin/bash -c ",
                "\"$(curl -fsSL https://raw.githubusercontent.com/",
                "Homebrew/install/HEAD/install.sh)\"\n",
                "  2. Install required libraries:\n",
                "     arch -x86_64 /usr/local/bin/brew install ",
                "gcc@10 zstd libomp\n\n",
                "Alternatively, you can supply a custom FINEMAP binary ",
                "compiled for ARM64 via the FINEMAP_path argument."
            )
        } else if(OS == "osx"){
            stp <- paste0(
                "FINEMAP binary failed to run (exit code 134). ",
                "This usually means required dynamic libraries are missing.\n",
                "Install them with: brew install gcc@10 zstd libomp"
            )
        } else {
            stp <- paste0(
                "FINEMAP binary failed to run (exit code ", exit_code, ").\n",
                "Please ensure the binary is compatible with your system.\n",
                "Error: ", paste(err_msg, collapse = "\n")
            )
        }
        stop(stp)
    }

    ## Other failures
    stp <- paste0(
        "FINEMAP binary failed to run (exit code ", exit_code, ").\n",
        "Path: ", FINEMAP_path, "\n",
        "Please check that the binary is compatible with your system."
    )
    stop(stp)
}
