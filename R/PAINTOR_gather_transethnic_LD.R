#' Prepare transethnic PAINTOR results
#'
#' @keywords internal
#' @family PAINTOR
PAINTOR_gather_transethnic_LD <- function(merged_DT,
                                  locus,
                                  conditions=c("Nalls23andMe_2019",
                                               "MESA_AFA","MESA_CAU","MESA_HIS")){
    for(cond in conditions){
        messager("+ PAINTOR:: Retrieving LD with lead SNP for",cond)
        fullSS <- dirname(Directory_info(cond, variable = "fullSS.local"))
        ld.path <- file.path(fullSS, locus,"plink","LD_matrix.RData")
        LD_matrix <- readRDS(ld.path)
        lead.snp <- merged_DT[merged_DT[,paste0(cond,".leadSNP")]==TRUE,]$SNP
        dat <- data.table(SNP=names(LD_matrix[lead.snp,]),
                          r2=LD_matrix[lead.snp,]^2)
        merged_DT <- echodata::merge_robust(merged_DT, dat, by = "SNP")
        colnames(merged_DT)[colnames(merged_DT)=="r2"] <- paste0(cond,".r2")
    }
    merged_DT <- dplyr::mutate(merged_DT,
                               Mb=round(POS/1000000,3))
    
    return(data.table::data.table(merged_DT))
}
