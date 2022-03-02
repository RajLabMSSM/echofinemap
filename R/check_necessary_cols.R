#' Check for necessary columns
#'
#' @examples
#' dat <- echodata::BST1;
#' finemap_methods <- c("ABF","FINEMAP","SUSIE","POLYFUN_SUSIE")
#' finemap_methods <- check_necessary_cols(dat=dat, 
#'                                         finemap_methods=finemap_methods)
check_necessary_cols <- function(dat,
                                 finemap_methods,
                                 sample_size=NULL,
                                 dataset_type="GWAS",
                                 verbose=TRUE){
    for_all <- c("SNP","CHR","POS","Effect","StdErr")
    required_dict <- list(ABF=c(for_all,
                                if(is.null(sample_size)) "N" else NULL,
                                if(dataset_type=="GWAS") 
                                    "proportion_cases" else NULL),
                          FINEMAP=c(for_all),
                          SUSIE=c(for_all),
                          POLYFUN_SUSIE=c(for_all,"P","A1","A2"),
                          COLOC=c(for_all),
                          PAINTOR=c(for_all),
                          COJO=c(for_all,"A1","A2"))
    suggested_dict <- list(ABF=c("MAF"),
                           FINEMAP=c("A1","A2","MAF","N"),
                           SUSIE=c("N"),
                           POLYFUN_SUSIE=c("MAF","N"),
                           PAINTOR=c("MAF"),
                           COJO=c("Freq","P","N")) # check these
    finemap_methods_suggests <-  finemap_methods
    for(m in finemap_methods){
        message("vvvvv-- ",m," --vvvvv")
        # Check required cols
        if(!all(required_dict[[m]] %in% colnames(dat))){
            finemap_methods <- finemap_methods[finemap_methods!=m]
            missing_cols <- required_dict[[m]][
                !required_dict[[m]] %in% colnames(dat)]
            message("⛔ Missing required columns for ",m,": ",
                    paste(missing_cols,collapse=", ")," (Skipping)");
            next()
        } else{message("✅ All required columns present.")}
        # Check suggested cols
        if(!m %in% names(suggested_dict)) next()
        if(is.null(suggested_dict[[m]])) next()
        if(!all(suggested_dict[[m]] %in% colnames(dat))){
            missing_cols <- suggested_dict[[m]][
                !suggested_dict[[m]] %in% colnames(dat)]
            message("⚠️  Missing optional columns for ",m,": ",
                    paste(missing_cols,collapse=", "))
        } else{message("✅ All suggested columns present.")}
    }
    return(finemap_methods)
}
