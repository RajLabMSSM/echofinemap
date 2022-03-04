exists_not_empty <- function(files){
    sapply(files, function(f){
        if(file.exists(f)){
            l <- readLines(f)
            not_empty <- length(l)>0 
            return(not_empty) 
        } else {return(FALSE)} 
    }) 
}