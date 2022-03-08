cmd_print <- function(cmd,
                      raw=FALSE,
                      basepath=TRUE,
                      prefix="...",
                      wrap=TRUE,
                      verbose=TRUE){
    if(verbose){
        if(raw){
            cat(cmd)
        } else {
            if(basepath){
                split <- stringr::str_split(cmd," ")[[1]]
                cmd <- paste(
                    lapply(split, function(l){ 
                        if(grepl("[/]|[\\]",l)){
                            if(tolower(basename(l)) %in% c("python","r") |
                               endsWith(tolower(basename(l)),".py") ){
                                basename(l)
                            } else {
                                file.path(prefix,basename(l))
                            }
                            
                        } else {l}
                    }),
                    collapse = " "
                )
            }
            if(wrap){
                cmd <- gsub("--","\n   --",cmd) 
            }
            cat(cmd)
        } 
    }
   
}