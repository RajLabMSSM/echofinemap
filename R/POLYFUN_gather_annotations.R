#' Find and import PolyFun annotation files
#' @keywords internal
#' @family polyfun
#' @examples
#' \dontrun{
#' dat <- echodata::BST1  
#' annot_DT <- POLYFUN_gather_annotations(chromosomes=dat$CHR[1],
#'                                        subset_snps=subset_snps, 
#'                                        polyfun_annots="/pd-omics/tools/polyfun/annotations/baselineLF2.2.UKB")
#' }
POLYFUN_gather_annotations <- function(chromosomes=seq_len(22),
                                       subset_snps=NULL,
                                       polyfun_annots){
    annot_DT <- lapply(chromosomes, function(chrom){
        messager("+ POLYFUN:: Chromosome",chrom,"...")
        parquet.file <- list.files(path = polyfun_annots,
                                   # e.g. " /pd-omics/tools/polyfun/annotations/baselineLF2.2.UKB/baselineLF2.2.UKB.5.annot.parquet"
                                   pattern = paste0("*\\.",chrom,"\\.annot\\.parquet"),
                                   full.names = TRUE)
        annot_df <- echodata::read_parquet(path = parquet.file)
        annot_df$BP <- as.integer(annot_df$BP)
        if(!is.null(subset_snps)){
            annot_df <- subset(annot_df, SNP %in% subset_snps)
        }
        return(data.table::data.table(annot_df))
    }) |> data.table::rbindlist()
    return(annot_DT)
}



