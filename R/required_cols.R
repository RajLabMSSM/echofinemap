#' Table of required columns
#' 
#' Return a table with the minimum columns required
#' to run each fine-mapping method, as well as suggested columns.
#' @param dataset_type Dataset type ("GWAS" or "QTL"). 
#' @param for_all Columns required for all methods.
#' @export
#' @importFrom data.table data.table setkey
#' @examples 
#' d <- echofinemap::required_cols()
required_cols <- function(dataset_type = "GWAS",
                          for_all = c("SNP","CHR","POS","Effect","StdErr")){
    
    required_dict <- list(ABF=c(for_all,
                                "N","MAF",
                                if(dataset_type=="GWAS") 
                                    "proportion_cases" else NULL),
                          FINEMAP=c(for_all),
                          SUSIE=c(for_all),
                          POLYFUN_SUSIE=c(for_all,"P","A1","A2"),
                          POLYFUN_FINEMAP=c(for_all,"P","A1","A2"),
                          COLOC=c(for_all),
                          PAINTOR=c(for_all,"ZSCORE"),
                          COJO_stepwise=c(for_all,"A1","A2"),
                          COJO_conditional=c(for_all,"A1","A2"),
                          COJO_joint=c(for_all,"A1","A2"))
    suggested_dict <- list(ABF=NULL,
                           FINEMAP=c("A1","A2","MAF","N"),
                           SUSIE=c("N"),
                           POLYFUN_SUSIE=c("MAF","N"),
                           POLYFUN_FINEMAP=c("MAF","N"),
                           PAINTOR=c("MAF"),
                           COJO_stepwise=c("Freq","P","N"),
                           COJO_conditional=c("Freq","P","N"),
                           COJO_joint=c("Freq","P","N")) # check these
    d <- data.table::data.table(method=names(required_dict),
                                required = required_dict)
    d$suggested <- suggested_dict[d$method]
    data.table::setkey(d, "method")
    return(d)
}
