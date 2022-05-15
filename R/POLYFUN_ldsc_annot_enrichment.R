#' Run and plot heritability enrichment tests
#'
#' @source
#' https://www.nature.com/articles/s41588-020-00735-5
#' @keywords internal
#' @family polyfun
POLYFUN_ldsc_annot_enrichment <- function(.results = "Data/GWAS/Nalls23andMe_2019/_genome_wide/PolyFun/output/PD_GWAS_LDSC/PD_GWAS_LDSC.results",
                                          show_plot=TRUE,
                                          save_plot=FALSE,
                                          title = "LDSC Heritability Enrichment",
                                          subtitle = "PD GWAS"){
    res <- data.table::fread(.results)
    res$Category <- gsub("*_0$","", res$Category)
    res$Group <- gsub("^([^_]*_[^_]*)_.*$", "\\1", res$Category)
    # Get rid of absurd enrichment values
    res <- subset(res, Enrichment>-5000 & Enrichment<5000)
    res$Valence <- ifelse(res$Enrichment>=0,1,-1)
    res$p_adj <- p.adjust(res$Enrichment_p, method="fdr")
    
    
    POLYFUN_annot_enrichment_plot  <- function(res,
                                               title = "LDSC Heritability Enrichment",
                                               subtitle = "PD GWAS"){
        sig_res <- subset(res, p_adj<0.05)
        nudge_x <- ifelse(nrow(res)>150, -.5, -.03)
        gp <- ggplot(res, aes(x=Category, y=Enrichment, fill=Group)) +
            geom_col(show.legend  = FALSE) +
            geom_errorbar(aes(ymin=Enrichment-Enrichment_std_error, ymax=Enrichment+Enrichment_std_error),
                          width=.5, position=position_dodge(.9)) +
            geom_text(data = sig_res, aes(x=Category, y=(Enrichment+Enrichment_std_error*Valence)+5*Valence),
                      label="*", nudge_x=nudge_x, color="magenta", size=7) +
            coord_flip() +
            labs(title = title,
                 subtitle = subtitle) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5))
        if(nrow(res)>150){ gp <- gp + theme(axis.text.y=element_text(size=7)) }
        return(gp)
    }
    
    if(show_plot){
        hist(res$Enrichment, breaks = 50)
        hist(res$Enrichment_p, breaks = 50)
        plot_dir <- dirname(.results)
        # Alphabetical
        gp.all <- POLYFUN_annot_enrichment_plot(res, title, subtitle)
        if(show_plot){print(gp.all)}
        if(save_plot){ggsave(gp.all, filename = file.path(plot_dir,"ldsc_annot_enrich_all.png"),
                             dpi = 400, width=10, height=20)}
        # Top p-vals
        gp.sig <- POLYFUN_annot_enrichment_plot(subset(res, p_adj<0.05), title, subtitle)
        if(show_plot){print(gp.sig)}
        if(save_plot){ggsave(gp.sig, filename = file.path(plot_dir,"ldsc_annot_enrich_sig.png"),
                             dpi = 400, width=10,height=5)}
    }
    
    
    # POLYFUN_get_annot_refs <- function(res,
    #                                    supp_file="./echolocatoR/tools/polyfun/SuppTables.xlsx", sheet="S1"){
    #     supp <- openxlsx::read.xlsx(supp_file,sheet = sheet)
    #     supp_merge <- echodata::merge_robust(data.table::data.table(supp), res,
    #                                                 by.x = "Annotation", by.y = "Category")
    #     return(supp_merge)
    # }
    # supp_merge <- POLYFUN_get_annot_refs(sig_res)
    # echodata::createDT(supp_merge[,c("Annotation","Reference","Prop._SNPs","Prop._h2")])
    return(res)
}

