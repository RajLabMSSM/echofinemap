#' @source
#' https://www.biorxiv.org/content/10.1101/807792v3
#' @keywords internal
#' @family polyfun
POLYFUN_gather_annot_proportions <- function(base_url="/sc/arion/projects/pd-omics/tools/polyfun/annotations/baselineLF2.2.UKB"){
    prop.file <- file.path(base_url,"annot_proportions.csv")
    if(!file.exists(prop.file)){
        all_annot <- list.files(base_url, pattern = "*.annot.parquet$", full.names = TRUE)
        annot_PROP <- parallel::mclapply(all_annot, function(x){
            print(x)
            annot <- read_parquet(parquet_path = x)
            annot_sums <- colSums(annot[,6:ncol(annot)])
            annot_prop <- annot_sums/nrow(annot)
            return(annot_prop)
        }, mc.cores = 4)
        
        annot_PROP_DF <- do.call("cbind",annot_PROP) %>% `colnames<-`(paste0("chr",1:length(annot_PROP)))
        write.csv(annot_PROP_DF, file = prop.file)
    } else {
        annot_PROP_DF <- read.csv(prop.file, row.names = 1)
    }
    return(annot_PROP_DF)
}

