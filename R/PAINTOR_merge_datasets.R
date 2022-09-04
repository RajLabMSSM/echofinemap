#' Merge datasets
#' 
#' Merge a named list of GWAS/QTL/TWAS datasets.
#' @keywords internal
#' @importFrom data.table .SD := fwrite
PAINTOR_merge_datasets <- function(dat_ls, 
                                   NA_method=c("drop","fill")){
    .SD <- NULL;
    
    NA_method <- tolower(NA_method)[1]
    #### Merge data #### 
    dat_merged <- Reduce(f = function(...){
        merge(..., 
              by = c("CHR","POS","SNP"),
              suffixes = paste0(".",names(dat_ls)))},
                         x =  dat_ls)
    zcols <- grep("^ZSCORE",names(dat_merged), value = TRUE)
    keep_cols <- c("CHR","POS","SNP",zcols)
    dat_merged <- dat_merged[,keep_cols, with=FALSE] 
    dat_merged[,CHR:=paste0("chr",gsub("chr","",CHR))]
    #### Fill NAs ####
    if(NA_method=="fill"){
        dat_merged <- dat_merged[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})] 
    } else if (NA_method=="drop"){
        dat_merged <- dat_merged[complete.cases(dat_merged),]
    }  
    #### Return ####
    return(dat_merged) 
}
