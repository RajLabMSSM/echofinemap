#' Import QTL data for PAINTOR
#'
#'  @keywords internal
#' @importFrom tidyr spread
PAINTOR_import_qtl_dat <- function(QTL_datasets,
                                  locus_dir,
                                  trim_gene_limits = FALSE,
                                  force_new_subset = FALSE,
                                  metric = "t_stat",
                                  verbose = TRUE){
    QTL.dat <- lapply(QTL_datasets, 
                      function(qtl){
        messager("++ PAINTOR:: Importing",qtl,v=verbose) 
        qtl.dir <- basename(dirname(dirname(qtl)))
        qtl.subdir <- basename(dirname(qtl))
        subset_path <- file.path(dirname(fullSS_path),
                                 paste0(locus,"_",qtl,".txt"))
        
        # Save subset
        # force_new_subset=T
        if(file.exists(subset_path) & force_new_subset==FALSE){
            dat <- data.table::fread(subset_path, nThread = 1)
        } else {
            messager("PAINTOR:: Creating subset file for",locus)
            qtl.dat <- data.table::fread(fullSS_path, nThread = 1)
            ## Remove the "locus" column bc it confuses subsetting functions
            # qtl.dat <- dplyr::select(qtl.dat, select = -locus) |>
            #   subset(gene_name==locus)
            data.table::fwrite(qtl.dat, subset_path, sep="\t")
            dat <- preprocess_subset(locus = locus,
                                     subset_path = subset_path,
                                     chrom_col = "chr",
                                     position_col = "pos_snps",
                                     snp_col = "snps",
                                     effect_col = "beta",
                                     pval_col = "pvalue",
                                     tstat_col = "statistic",
                                     stderr_col = "calculate",
                                     A1_col = "ref",
                                     A2_col = "alt")  |>
                data.table::data.table()
        }
        dat <- cbind(Dataset=qtl, dat)
        return(dat)
    }) |> data.table::rbindlist()
    # Trim by coordinates
    if(trim_gene_limits){
        QTL.dat <- echodata:::gene_trimmer(dat = QTL.dat, gene = locus)
    }
    # Spread data
    lead.snps <- (QTL.dat |> dplyr::group_by(Dataset) |> top_n(n=1, wt=-P))
    QTL.dat$Dataset <- paste(QTL.dat$Dataset,metric,sep=".")
    QTL.spread <-  tidyr::spread(
        data = data.frame(QTL.dat)[,c("Dataset","CHR","POS","SNP",
                                      "leadSNP","A1","A2",metric)],
        key="Dataset", value = metric) |>
        data.table::data.table()
    
    # Find lead SNP for each QTL condition
    
    for (d in unique(lead.snps$Dataset)){
        lead <- subset(lead.snps, Dataset==d)$SNP
        QTL.spread[,paste0(d,".leadSNP")] <- ifelse(QTL.spread$SNP==lead, T, F)
    }
    return(QTL.spread)
}
