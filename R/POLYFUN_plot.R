#' Plot PolyFun and other fine-mapping results
#' @source
#' https://www.biorxiv.org/content/10.1101/807792v3
#' @keywords internal
#' @family polyfun
POLYFUN_plot <- function(dat,
                         LD_matrix,
                         locus=NULL,
                         subtitle="PolyFun Comparison",
                         conditions=c("SUSIE","POLYFUN_SUSIE","FINEMAP","PAINTOR","PAINTOR_Fairfax")){
    # Quickstart
    # locus="LRRK2"; subtitle="PolyFun Comparison"; conditions=c("SUSIE","POLYFUN_SUSIE","FINEMAP","PAINTOR","PAINTOR_Fairfax")
    # # Get r2
    
    if(plot_ld){
        lead.snp <- top_n(dat,1,-P)$SNP #subset(dat, leadSNP==TRUE)$SNP
        r2 <- data.table::data.table(SNP=names(LD_matrix[lead.snp,]),
                                     r2=LD_matrix[lead.snp,]^2)
        dat <- data.table:::merge.data.table(dat, r2, by="SNP")
    } else{dat <- dplyr::mutate(dat, r2=1)}
    dat <- dplyr::mutate(dat, Mb=round(POS/1000000,3))
    
    
    
    library(patchwork)
    # GWAS
    gg <- ggplot(dat, aes(x=Mb, y=-log10(P), color=r2)) +
        scale_color_gradient(low="blue",high="red", breaks=c(0,.5,1), limits=c(0,1)) +
        geom_point() +
        labs(y="GWAS -log10(P)") +
        ggrepel::geom_label_repel(data = top_n(dat,n=1,-P),
                                  aes(label=SNP),alpha=0.7) +
        geom_point(data=top_n(dat,n=1,-P), size=5, shape=1, color="red") +
        scale_y_continuous(limits = c(0,max(-log10(dat$P))*1.1)) +
        
        # PolyFun priors
        ggplot(dat, aes(x=Mb, y=POLYFUN_h2, color=POLYFUN_h2)) +
        scale_color_viridis_c(limits=c(0,1), breaks=c(0,.5,1)) +
        geom_point() +
        # ylim(0,1) +
        
        # PolyFun+SUSIE PP
        ggplot(dat, aes(x=Mb, y=POLYFUN_SUSIE.PP, color=POLYFUN_SUSIE.PP)) +
        geom_point() +
        ggrepel::geom_label_repel(data = subset(dat,POLYFUN_SUSIE.PP>=.5),
                                  aes(label=SNP),alpha=0.7, color='green') +
        geom_point(data=subset(dat, POLYFUN_SUSIE.CS>0),
                   #subset(dat, PolyFun_SUSIE.PP>=.95),
                   size=5, shape=1, color="green") +
        scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1.1)) +
        scale_color_continuous(breaks=c(0,.5,1), limits=c(0,1)) +
        
        # SUSIE PP
        ggplot(dat, aes(x=Mb, y=SUSIE.PP, color=SUSIE.PP)) +
        geom_point() +
        ggrepel::geom_label_repel(data = subset(dat,SUSIE.CS>0),
                                  aes(label=SNP),alpha=0.8, color='green') +
        geom_point(data=subset(dat,SUSIE.PP>=.95),
                   size=5, shape=1, color="green") +
        scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1.1)) +
        scale_color_continuous(breaks=c(0,.5,1), limits=c(0,1)) +
        # Overall layers
        patchwork::plot_layout(ncol = 1) +
        patchwork::plot_annotation(title = locus,
                                   subtitle = paste(nrow(dat),"SNPs"),#"PolyFun Comparison",
                                   theme =  theme(plot.title = element_text(hjust = 0.5),
                                                  plot.subtitle = element_text(hjust = 0.5)))
    print(gg)
    ggsave(file.path(locus_dir,'PolyFun',"PolyFun.plot.png"), plot = gg,
           dpi = 400, height = 10, width = 7)
}

