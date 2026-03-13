#' Setup gcc
#'
#' Set up a symlink to override xcode's clang with a valid version of gcc.
#' On Apple Silicon Macs, this checks both \code{/opt/homebrew} and
#' \code{/usr/local} Homebrew prefixes.
#' @param version gcc version to use (e.g. 7,8,9,10,14).
#' If \code{"latest"}, the latest version of gcc
#' currently installed on your machine will be used.
#' @param verbose Print messages.
#' @keywords internal
#' @importFrom R.utils createLink
#' @returns A data.frame listing all gcc versions, their paths,
#' and which one is currently active.
setup_gcc <- function(version='latest',
                      verbose=TRUE){

    #### Use xcode implementation ####
    if(version=="clang"){
        out <- R.utils::createLink(link="/usr/local/bin/gcc",
                                   target="/usr/bin/clang",
                                   overwrite=TRUE)
        return(NULL)
    }
    #### Detect Homebrew prefix ####
    ## Apple Silicon uses /opt/homebrew, Intel uses /usr/local
    brew_prefixes <- c("/opt/homebrew", "/usr/local")

    #### Find gcc installations ####
    gcc_paths <- character(0)
    for(prefix in brew_prefixes){
        cellar <- file.path(prefix, "Cellar")
        if(!dir.exists(cellar)) next
        ## Check for gcc@VERSION format (older Homebrew)
        gcc_at <- list.files(cellar, pattern = "^gcc@", full.names = TRUE)
        ## Check for plain gcc (newer Homebrew)
        gcc_plain <- list.files(cellar, pattern = "^gcc$", full.names = TRUE)
        gcc_paths <- c(gcc_paths, gcc_at, gcc_plain)
    }

    if(length(gcc_paths)==0){
        stp <- paste(
            "No valid version of gcc installed.",
            "You can install it in the terminal using:",
            "`brew install gcc`"
        )
        stop(stp)
    }
    version <- tolower(as.character(version))

    ## Extract version numbers from paths
    ## Handles both "gcc@10" and "gcc" (with version subdirectory)
    extract_version <- function(p){
        bn <- basename(p)
        if(grepl("@", bn)){
            return(tail(strsplit(bn, "@")[[1]], 1))
        }
        ## For plain "gcc", get version from subdirectory name
        subdirs <- list.files(p)
        if(length(subdirs) > 0){
            ## Extract major version from e.g. "14.2.0_1"
            v <- gsub("[._].*$", "", subdirs[1])
            return(v)
        }
        return(NA_character_)
    }

    gcc_versions <- vapply(gcc_paths, extract_version, character(1))

    ## Find the actual gcc binary for each installation
    find_gcc_bin <- function(p){
        v <- extract_version(p)
        ## Check common locations for gcc-{version} binary
        for(prefix in brew_prefixes){
            bin_path <- file.path(prefix, "bin", paste0("gcc-", v))
            if(file.exists(bin_path)) return(bin_path)
        }
        ## Search within the Cellar installation
        bins <- list.files(p, pattern = paste0("^gcc-", v, "$"),
                          recursive = TRUE, full.names = TRUE)
        bins <- grep("/bin/", bins, value = TRUE)
        if(length(bins) > 0) return(bins[1])
        return(NA_character_)
    }

    gcc_bins <- vapply(gcc_paths, find_gcc_bin, character(1))

    gcc_df <- data.frame(
        dir = gcc_paths,
        version = gcc_versions,
        bin = gcc_bins,
        stringsAsFactors = FALSE
    ) |> dplyr::arrange(dplyr::desc(version))

    if(version=="latest"){
        i <- 1
    } else {
        if(version %in% gcc_df$version){
            i <- which(gcc_df$version==version)[1]
        } else {
            messager("Requested gcc version not installed.",
                     "You can install it in the terminal using:",
                     paste0("`brew install gcc@", version, "`"),
                     "\nFor now, using the latest version of gcc installed.",
                     v=verbose)
            i <- 1
        }
    }
    #### Select gcc version ####
    gcc_select <- gcc_df[i,]
    gcc_df$active <- ifelse(seq_len(nrow(gcc_df))==i,"*","")
    messager("Using gcc version:", gcc_select$version, v=verbose)
    return(gcc_df)
}
