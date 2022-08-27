#' Setup gcc
#' 
#' Set up a symlink to override xcode's clang with a valid version of gcc.
#' @param version gcc version to use (e.g. 7,8,9,10).
#' If \code{"latest"}, the latest version of gcc 
#' currently installed on your machine will be used.
#' @param verbose Print messages. 
#' @keywords internal
#' @importFrom R.utils createLink
#' @returns A data.frame listing all gcc versions, their paths, 
#' and which one is currently active.
setup_gcc <- function(version='latest',
                      verbose=TRUE){ 
    gcc_paths <- list.files("/usr/local/Cellar",
                               pattern = "gcc@", 
                               full.names = TRUE)
    if(length(gcc_paths)==0){
        stp <- paste(
            "No valid version of gcc installed.",
            "You can install it in the terminal using:",
            "`brew install gcc@10`"
        )
        stop(stp)
    }
    version <- tolower(as.character(version))
    
    gcc_df <- data.frame(
        dir=gcc_paths, 
        subdir=list.files(gcc_paths, full.names = TRUE),
        version=unlist(lapply(basename(gcc_paths),
                              function(x){tail(strsplit(x,"@")[[1]],1)}))
        ) |>
        dplyr::mutate(link=paste0("/usr/local/bin/gcc-",version)) |>
        dplyr::arrange(dplyr::desc(version))
    if(version=="latest"){
        i <- 1
    } else { 
        if(version %in% gcc_df$version){ 
            i <- which(gcc_df$version==version)
        } else {
            messager("Requested gcc version not installed.",
                     "You can install it in the terminal using:",
                     "`brew install gcc@<version>`",
                     "\nFor now, using the latest version of gcc installed.",
                     v=verbose
                     )
            i <- 1
        } 
    }
    #### Select gcc version ####
    gcc_select <- gcc_df[i,] 
    gcc_df$active <- ifelse(seq_len(nrow(gcc_df))==i,"*","")
    messager("Using gcc version:",gcc_select$version,v=verbose)
    #### Overwrite symlink with selected version ####
    out <- R.utils::createLink(link="/usr/local/bin/gcc", 
                               target=gcc_select$link, 
                               overwrite=TRUE)  
    # system("gcc -v") ## Check active version 
    return(gcc_df)
}
