#' Run PolyFun+SUSIE fine-mapping pipeline
#'
#' Uses echolocatoR wrapper for SUSIE instead of the \code{POLYFUN_finemapper}.
#' function which uses a python script provided with PolyFun.
#' 
#' @param mode PolyFun can run in several different modes corresponding 
#' to how SNP-wise prior probabilities are computed:
#' \itemize{
#' \item{"precomputed"}{Use precomputed priors from the original 
#' \href{https://www.nature.com/articles/s41588-020-00735-5}{
#' PolyFun publication}. These were computed using GWAS summary statistics from 
#' 49 UK Biobank traits performed in individuals of British ancestry 
#' (mean n=318,000).}
#' #' \item{"parametric"}{Gathered from the "*.snpvar_ridge_constrained.gz" 
#' output files from PolyFun}.
#' \item{"non-parametric"}{Gathered from the "*.snpvar_constrained.gz"
#' output files from PolyFun}.
#' }
#' @inheritParams multifinemap
#' @inheritParams SUSIE
#' 
#' @family polyfun
#' @source \href{https://www.nature.com/articles/s41588-020-00735-5}{
#' PolyFun publication}
#' @export
#' @examples 
#' locus_dir <- file.path(tempdir(),echodata::locus_dir)
#' dat <- echodata::BST1
#' LD_matrix <- echodata::BST1_LD_matrix
#' dat2 <- echofinemap::POLYFUN_SUSIE(locus_dir=locus_dir, 
#'                                    dat=dat, 
#'                                    LD_matrix = LD_matrix)
POLYFUN_SUSIE <- function(dat,
                          LD_matrix,
                          locus_dir,
                          polyfun=NULL,
                          mode=c("precomputed","parametric","non-parametric"),
                          dataset_type="GWAS",
                          max_causal=5,
                          sample_size=NULL, 
                          PP_threshold=.95,
                          rescale_priors=TRUE,
                          conda_env="echoR",
                          verbose=TRUE,
                          ...){
    # echoverseTemplate:::args2vars(POLYFUN_SUSIE)
    SNP <- SNPVAR <- NULL;
     
    polyfun <- POLYFUN_find_polyfun_folder(polyfun_path = polyfun)
    out.path <- file.path(dirname(locus_dir),
                          "_genome_wide/PolyFun/output") 
    ##### Ensure formatting is correct ####
    ## Sometimes SNP gets turned into logical?
    dat[,SNP:=as.character(SNP)] 
    #### Import priors ####
    priors <- POLYFUN_import_priors_handler(dat = dat, 
                                            mode = mode, 
                                            out.path = out.path, 
                                            locus_dir = locus_dir, 
                                            conda_env = conda_env)
    #### Prepare data ####
    merged_dat <- echodata::merge_robust(x = dat,
                                         y = priors,
                                         by="SNP") 
    sub.out <- echoLD::subset_common_snps(LD_matrix = LD_matrix,
                                          dat = merged_dat,
                                          verbose = verbose)
    LD_matrix <- sub.out$LD
    new_DT <- sub.out$DT
    #### Run SUSIE ####
    dat <- SUSIE(dat=new_DT,
                 LD_matrix=LD_matrix,
                 dataset_type=dataset_type,
                 max_causal=max_causal,
                 sample_size=sample_size,
                 PP_threshold=PP_threshold, 
                 prior_weights=new_DT$POLYFUN_h2,
                 rescale_priors = rescale_priors, 
                 verbose = verbose, 
                 ...) 
    return(dat)
}
