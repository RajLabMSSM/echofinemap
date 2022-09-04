PAINTOR_find_folder <- function(paintor_path=NULL){
    if(is.null(paintor_path)){
        paintor_path <- system.file("tools/PAINTOR_V3.0",
                                    package = "echofinemap")
    }
    return(paintor_path)
}