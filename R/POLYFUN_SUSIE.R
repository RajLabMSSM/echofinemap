#' Run PolyFun+SUSIE fine-mapping pipeline
#'
#' Uses echolocatoR wrapper for SUSIE instead of the \code{POLYFUN_finemapper}.
#' function which uses a python script provided with PolyFun.
#' @source \url{https://www.biorxiv.org/content/10.1101/807792v3}
#' @export
#' @family polyfun
#' @examples 
#' locus_dir <- file.path(tempdir(),echodata::locus_dir)
#' 
#' dat <- echodata::BST1
#' dat2 <- echofinemap::POLYFUN_SUSIE(locus_dir=)
POLYFUN_SUSIE <- function(locus_dir,
                          polyfun=NULL,
                          dat=NULL,
                          LD_matrix=NULL,
                          polyfun_approach="non-parametric",
                          dataset_type="GWAS",
                          max_causal=5,
                          sample_size=NULL, 
                          PP_threshold=.95,
                          conda_env="echoR"){
    # polyfun="./echolocatoR/tools/polyfun";  locus_dir="./Data/GWAS/Nalls23andMe_2019/_genome_wide"; dataset="Nalls23andMe_2019"; locus="LRRK2"; dat=NULL; polyfun_priors="parametric"; sample_size=1474097; min_INFO=0; min_MAF=0; server=T; dataset_type="GWAS"; n_causal=10; PP_threshold=.95
    polyfun <- POLYFUN_find_polyfun_folder(polyfun_path = polyfun)
    out.path <- file.path(dirname(locus_dir),"_genome_wide/PolyFun/output")
    chrom <- unique(dat$CHR) 
    # Import priors
    # ~~~~~~~~ Approach 1 ~~~~~~~~
    if (polyfun_approach=="precomputed"){
        priors <- POLYFUN_get_precomputed_priors(locus_dir=locus_dir,
                                                 dat=dat,
                                                 force_new_priors=FALSE,
                                                 conda_env=conda_env)
        # precomputed.priors <- priors
        # ~~~~~~~~ Approach 2 ~~~~~~~~
    } else if (polyfun_approach=="parametric"){
        ldsc.files <- list.files(out.path, pattern = "*.snpvar_ridge_constrained.gz", full.names = TRUE) %>%
            grep(pattern = paste0(".",chrom,"."), value = TRUE, fixed=TRUE)
        priors <- .rbind.file.list(ldsc.files)
        # ~~~~~~~~ Approach 3 ~~~~~~~~
    } else if (polyfun_approach=="non-parametric"){
        ldsc.files <- list.files(out.path, pattern = "*.snpvar_constrained.gz", full.names = TRUE) %>%  base::grep(pattern = paste0(".",chrom,"."), value = TRUE, fixed = TRUE)
        priors <- .rbind.file.list(ldsc.files)
    }
    
    # Ensure formatting is correct (sometimes SNP gets turned into logical?)
    dat$SNP <- as.character(dat$SNP)
    priors <- dplyr::select(priors, SNP, POLYFUN_h2=SNPVAR) %>%
        data.table::data.table() %>%
        dplyr::mutate(SNP=as.character(SNP))
    # Prepare data
    merged_dat <- data.table::merge.data.table(x = dat,
                                               y = priors,
                                               by="SNP")
    sub.out <- echoLD::subset_common_snps(LD_matrix = LD_matrix,
                                          dat = merged_dat,
                                          verbose = verbose)
    LD_matrix <- sub.out$LD
    new_DT <- sub.out$DT
    # Run SUSIE
    dat <- SUSIE(dat=new_DT,
                 LD_matrix=LD_matrix,
                 dataset_type=dataset_type,
                 max_causal=max_causal,
                 sample_size=sample_size,
                 PP_threshold=PP_threshold,
                 
                 prior_weights=new_DT$POLYFUN_h2,
                 rescale_priors = TRUE)
    # dat.precomputed <- dat
    # dat.computed <- dat
    # Check for differences between pre-computed and re-computed heritabilities
    # library(patchwork)
    # ## GWAS
    # ggplot() +
    #   geom_point(data=dat, aes(x=POS, y=-log10(P), fill="PD GWAS", color=-log10(P))) +
    #   #3 Priors
    #   ggplot() +
    #   geom_point(data=precomputed.priors, aes(x=BP, y=SNPVAR, color="Pre-computed")) +
    #   geom_point(data=subset(priors, BP>=min(precomputed.priors$BP) & BP<=max(precomputed.priors$BP)),
    #              aes(x=BP, y=SNPVAR, color="Computed")) +
    #   ## fine-mapping with computed priors
    #   ggplot() +
    #   geom_point(data=dat.computed, aes(x=POS, y=PP, color="POLYFUN+SUSIE \n Computed Priors")) +
    #   patchwork::plot_layout(ncol = 1) +
    #   ## fine-mapping with pre-computed priors
    #   ggplot() +
    #   geom_point(data=dat.precomputed, aes(x=POS, y=PP, fill="POLYFUN+SUSIE \n Pre-computed Priors"), color="turquoise")
    return(dat)
}
