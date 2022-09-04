#' Plot transethnic PAINTOR results
#'
#' @keywords internal
transethnic_plot <- function(merged_DT,
                             save_path,
                             title=locus,
                             subtitle="Trans-ethnic Fine-mapping",
                             PAINTOR_label="PAINTOR\nTrans-ethnic",
                             conditions=c("MESA_AFA","MESA_CAU","MESA_HIS")){
    requireNamespace("ggplot2")
    requireNamespace("ggrepel")
    requireNamespace("patchwork")
    # cons.snp <-  "rs7294619"; subset(plot_DT, SNP==cons.snp);
    plot_DT <- gather.transethnic.LD(merged_DT, conditions=conditions)
    plot_DT$PAINTOR_label <- PAINTOR_label
    # Melt P
    P.vars <- paste0(conditions,".P")
    r2.vars <- paste0(conditions,".r2")
    leadSNP.vars <-  paste0(conditions,".leadSNP")
    id.vars <- grep(paste(c(P.vars, r2.vars), collapse="|"), colnames(plot_DT), value = TRUE, invert = TRUE)
    dat <- data.table::melt.data.table(plot_DT,
                                       id.vars = id.vars,
                                       measure.vars = list(P.vars, r2.vars, leadSNP.vars),
                                       variable.name = c("Condition_number"),
                                       value.name = c("P","r2","lead.snp"))
    cond.dict <- setNames(conditions, 1:length(conditions))
    dat$Condition <- cond.dict[dat$Condition_number]
    
    
    gg <- ggplot(data=dat, aes(x=Mb, y=-log10(P), color=r2)) +
        geom_point() +
        facet_grid(Condition~., scales="free_y") +
        theme_classic() +
        labs(y="-log10(P-value)", color=paste0("r2 with\n", subset(dat, lead.snp==TRUE)$SNP)) +
        # Lead SNP
        geom_point(dat=subset(dat, lead.snp==TRUE), shape=1, size=6, color="red") +
        ggrepel::geom_label_repel(dat=subset(dat, lead.snp==TRUE) , aes(label=SNP),
                                  alpha=0.8, point.padding = 1) +
        # Credible Set
        geom_point(dat=subset(dat, Support>0), shape=1, size=6, color="green") +
        #
        scale_colour_gradient(low = "blue", high = "red", limits=c(0,1), breaks=c(0,.5,1)) +
        theme(strip.background = element_rect(fill = "grey10"),
              strip.text.y = element_text(color = "white", angle = 0)) +
        
        # Trans-ethnic PP layer
        ggplot(data=plot_DT, aes(x=Mb, y=PAINTOR_MESA_transethnic.PP,
                                 color=PAINTOR_MESA_transethnic.PP)) +
        geom_point() +
        scale_color_viridis_c(limits=c(0,1), breaks=c(0,.5,1)) +
        # Credible Set
        geom_point(dat=subset(dat, Support>0), shape=1, size=6, color="green") +
        # Trans-ethnic fine-mapped CS SNPs
        geom_point(dat=subset(dat, Support>0),
                   shape=1, size=6, color="green") +
        ggrepel::geom_label_repel(dat=subset(dat, PAINTOR_MESA_transethnic.CS>0,
                                             select=c("SNP","CHR","POS","Mb","leadSNP","PAINTOR_MESA_transethnic.PP")) |> unique(), aes(label=SNP),
                                  alpha=0.8, point.padding = 1, color="green") +
        theme_classic() +
        facet_grid(PAINTOR_label~.) +
        labs(y="PP", color=paste0("PP")) +
        theme(strip.background = element_rect(fill = "grey10"),
              strip.text.y = element_text(color = "white", angle = 0)) +
        # Overall layers
        patchwork::plot_layout(ncol = 1, heights = c(1,.5)) +
        patchwork::plot_annotation(title = title,
                                   subtitle = subtitle,
                                   theme =  theme(plot.title = element_text(hjust = 0.5),
                                                  plot.subtitle = element_text(hjust = 0.5)))
    # Display
    print(gg)
    
    if(!is.null(save_path)){
        messager("PAINTOR:: Saving plot to =>",save_path)
        ggsave(save_path, plot = gg,
               dpi = 400, height = 9, width = 9)
    }
}
