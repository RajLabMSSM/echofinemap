#' Remove duplicate SNPs
#' 
#' Remove any duplicate SNPs and rewrite the file to disk.
#' @param path Path to file. 
#' @param remove_dup Remove duplicate SNPs from the file.
#' @param verbose Print messages.
#' @keywords internal
#' @importFrom data.table fread fwrite
remove_dup_snps<- function(path,
                           remove_dup=TRUE,
                           verbose=TRUE){
    if(isTRUE(remove_dup)){
        dat <- echodata::get_header(path = path, 
                                    nrows = NULL,
                                    verbose = verbose)
        dups <- duplicated(dat$SNP)
        if(sum(dups)>0){
            path <-  paste0(gsub("\\.gz|\\.txt|\\.tsv|\\.csv|\\.parquet","",
                                  path),
                            ".tsv")
            messager("PolyFun:: Removing",formatC(sum(dups),big.mark = ','),
                     "duplicate SNP(s)",
                     "from the full summary stats and rewriting to disk ==>",
                     path, v=verbose)
            dat <- dat[!dups,]
        } 
        data.table::fwrite(dat, 
                           file = path,
                           sep="\t")
        remove(dat)
    } 
    return(path)
}
