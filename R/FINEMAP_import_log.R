FINEMAP_import_log <- function(locus_dir,
                               config_thresh=.95){
    log_path <- list.files(file.path(locus_dir,"FINEMAP"),
                           "^data.log_sss*",
                           full.names = TRUE) 
    l <- readLines(log_path)
    #### Max number of causal SNPs set by user #####
    n_causal <- NULL
    l0 <- grep("- n-causal-snps*",l, value = TRUE)
    if(length(l0)>0){
        n_causal <- as.integer(
            trimws(
                strsplit(l0,":")[[1]][2]
            )
        )
    } else {
        data.k_path <- file.path(locus_dir,"FINEMAP","data.k")
        if(file.exists(data.k_path)){
            data.k <- data.table::fread(data.k_path, nrows = 1)
            n_causal <- ncol(data.k)
        }  
    } 
    
    #### Get post-probabilities of each number of causal SNPs ####
    FINEMAP_parse_k <- function(pattern,
                                n_causal){
        i <- grep(pattern = pattern, l)
        pr <- data.frame(pr=trimws(gsub("[(]|[)]","",
                                        grep("^-",l[seq(i,i+n_causal+2)], 
                                             invert = TRUE, value = TRUE)))) %>%
            tidyr::separate(col = "pr",sep = "->", into = c("k","prob")) %>%
            dplyr::mutate_all(as.numeric)
        return(pr)
    }
    priorPr <- FINEMAP_parse_k(pattern = "- Prior-Pr(# of causal SNPs is k)*",
                               n_causal=n_causal)
    postPr <- FINEMAP_parse_k(pattern = "- Post-Pr(# of causal SNPs is k)*",
                              n_causal=n_causal)
    causal_k <- postPr[postPr$prob>=config_thresh,]$k
    priorPr_k <- priorPr[priorPr$prob==max(priorPr$prob,na.rm = TRUE),]$k
    postPr_k <- postPr[postPr$prob==max(postPr$prob,na.rm = TRUE),]$k
    return(list(
        priorPr=priorPr,
        postPr=postPr,
        causal_k=causal_k,
        priorPr_k=priorPr_k,
        postPr_k=postPr_k,
        n_causal=n_causal
    ))
}
