#' Run functional enrichment tests
#' @source
#' https://www.nature.com/articles/s41588-020-00735-5
#' @keywords internal
#' @family polyfun
POLYFUN_functional_enrichment <- function(dat,
                                          PP_thresh=.95,
                                          save_plot="./Data/GWAS/Nalls23andMe_2019/_genome_wide/PolyFun/annot_enrichment.png"){
    # "...functional enrichment of fine-mapped common SNPs in the PIP range,
    ## defined as the proportion of common SNPs in the PIP range lying in the annotation
    ## divided by the proportion of genome-wide common SNPs lying in the annotation"
    base_url <- "/pd-omics/tools/polyfun/annotations/baselineLF2.2.UKB"
    chrom <- dat$CHR[1]
    annot.file <- list.files(base_url, pattern = paste0(".UKB.",chrom,".annot.parquet"), full.names = TRUE)
      
    annot <- echodata::read_parquet(path = annot.file)
    annot_names <- annot %>% dplyr::select(-c(SNP,CHR,BP,A1,A2)) %>% colnames()
    annot_DT <- echodata::merge_robust(dat,
                                              data.table::data.table(annot) %>%
                                                  dplyr::rename(SNP_y = SNP, A1_y=A1, A2_y=A2),
                                              by.x = c("CHR","POS"),
                                              by.y = c("CHR","BP"),
                                              all.x = TRUE)
    ## SNP Groups
    # Nominal sig. GWAS
    nom.sig.GWAS <- annot_DT %>% dplyr::summarise_at(.vars =dplyr::vars(annot_names),
                                                     .funs = list(sum(P < 0.05 & .>0) / n())) %>% t() %>% `colnames<-`("nom.sig.GWAS")
    # Sig. GWAS
    sig.GWAS <- annot_DT %>% dplyr::summarise_at(.vars =dplyr::vars(annot_names),
                                                 .funs = list(sum(P < 5e-8 & .>0) / n())) %>% t() %>% `colnames<-`("sig.GWAS")
    # Lead GWAS
    lead.GWAS <- annot_DT %>% dplyr::summarise_at(.vars =dplyr::vars(annot_names),
                                                  .funs = list(sum(leadSNP==T & .>0) / n())) %>% t() %>% `colnames<-`("lead.GWAS")
    # Across all CS
    UCS <- annot_DT %>% dplyr::summarise_at(.vars =dplyr::vars(annot_names),
                                            .funs = list(sum(Support > 0 & .>0) / n())) %>% t() %>% `colnames<-`("UCS")
    # Tool-specific CS
    FM_methods <- gsub("*\\.PP$","",grep(pattern = "*\\.PP$",colnames(dat), value = TRUE))
    CS <- lapply(FM_methods, function(m){
        print(m)
        annot_mod <- annot_DT
        # col_ind <- grep(paste0("^",m,".PP$"),colnames(annot_mod))
        # colnames(annot_mod)[col_ind] <- "target.PP"
        annot_mod <- annot_mod %>% dplyr::select(paste0(m,".PP"),"Support",annot_names)
        colnames(annot_mod)[1] <- "PP"
        annot_mod %>% dplyr::summarise_at(.vars =dplyr::vars(annot_names),
                                          .funs = list( sum(PP > PP_thresh & .>0) / n() ))
    }) %>% data.table::rbindlist() %>% t() %>% `colnames<-`(FM_methods)
    
    # Gather annotation proportions genome-wide
    annot_SUMS_DF <- POLYFUN_gather_annot_proportions(base_url)
    SNP_groups <- cbind(nom.sig.GWAS, sig.GWAS, lead.GWAS, UCS, CS)
    enrich <- (SNP_groups / rowMeans(annot_SUMS_DF)) %>% melt() %>%
        `colnames<-`(c("Annot","SNP_group","Enrichment"))
    
    # Plot
    gp <- ggplot(enrich, aes(x=Annot, y=Enrichment, fill=SNP_group)) +
        geom_col(position="dodge", show.legend  = FALSE) + coord_flip() +
        facet_grid(facets = . ~ SNP_group) + #scales = "free_x"
        theme_bw()+ theme(axis.text.y = element_text(size = 7))
    print(gp)
    if(save_plot!=FALSE){
        ggsave(filename = save_plot, plot = gp, dpi = 400, height = 20, width = 12)
    }
    return(enrich)
}
