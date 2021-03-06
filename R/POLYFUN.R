#' Run PolyFun+SUSIE fine-mapping pipeline
#'
#' Uses echolocatoR wrapper for SUSIE instead of the \code{POLYFUN_finemapper}.
#' function which uses a python script provided with PolyFun.
#' @source \href{https://www.nature.com/articles/s41588-020-00735-5}{
#' PolyFun publication}
#' @source \href{https://github.com/omerwe/polyfun}{
#' PolyFun GitHub repo}
#' 
#' @param mode PolyFun can run in several different modes corresponding 
#' to how SNP-wise prior causal probabilities (i.e. priors) are computed:
#' \itemize{
#' \item{"precomputed" : }{
#' Using precomputed prior causal probabilities based on a meta-analysis of 
#' 15 UK Biobank traits. The meta-analysis was performed as part of the 
#' original \href{https://www.nature.com/articles/s41588-020-00735-5}{
#' PolyFun publication}.}
#' 
#' \item{"parametric" : }{
#' Computing prior causal probabilities via an L2-regularized extension of 
#' stratified LD-score regression (S-LDSC). This is a relatively simple 
#' approach, but the prior causal probabilities may not be robust to modeling
#'  misspecification.
#' Gathered from the "*.snpvar_ridge_constrained.gz" output files from PolyFun}.
#' 
#' \item{"non-parametric" : }{
#' Computing prior causal probabilities non-parametrically. 
#' This is the most robust approach, but it is computationally intensive and
#'  requires access to individual-level genotypic data from a large reference 
#'  panel (optimally >10,000 population-matched individuals).
#' Gathered from the "*.snpvar_constrained.gz" output files from PolyFun}.
#' }
#' @param ... Additional arguments passed to the chosen 
#' fine-mapping \code{method}.
#' @inheritParams multifinemap
#' @inheritParams SUSIE
#' @inheritParams FINEMAP
#' @inheritParams echodata::get_sample_size
#' 
#' @returns The same input SNP-wise
#'  \code{dat} but with the following additional columns:
#' \itemize{
#' \item{"CS" : }{Credible Set of putative causal SNPs.}
#' \item{"PP" : }{Posterior (Inclusion) Probability of each SNP being causal, 
#' or belonging to the causal Credible Set.}
#' \item{"POLYFUN.h2" : }{The normalized heritability (h^2) used as  
#' prior probabilities during fine-mapping.} 
#' }
#' @family polyfun 
#' @export
#' @importFrom data.table :=
#' @importFrom stats setNames
#' @examples 
#' locus_dir <- file.path(tempdir(),echodata::locus_dir)
#' dat <- echodata::BST1
#' LD_matrix <- echodata::BST1_LD_matrix
#' 
#' dat2 <- echofinemap::POLYFUN(locus_dir=locus_dir,
#'                              dat=dat,
#'                              LD_matrix = LD_matrix,
#'                              method="SUSIE")
POLYFUN <- function(dat,
                    LD_matrix,
                    locus_dir,
                    polyfun=NULL,
                    mode=c("precomputed",
                           "parametric",
                           "non-parametric"),
                    method=c("SUSIE","FINEMAP"),
                    dataset_type="GWAS",
                    max_causal=5,
                    compute_n="ldsc", 
                    credset_thresh=.95,
                    rescale_priors=TRUE,
                    conda_env="echoR_mini",
                    force_new=FALSE,
                    nThread=1,
                    verbose=TRUE,
                    ...){
    # echoverseTemplate:::source_all(packages = "dplyr")
    # echoverseTemplate:::args2vars(POLYFUN)
    
    SNP <- NULL;
    #### Install PolyFun ####
    POLYFUN_install()
    method <- POLYFUN_check_method(method=method,
                                   verbose=verbose)
    polyfun <- POLYFUN_find_polyfun_folder(polyfun_path = polyfun)
    out.path <- file.path(dirname(locus_dir),
                          "_genome_wide/PolyFun/output") 
    ##### Ensure formatting is correct ####
    ## Sometimes SNP gets turned into logical?
    dat[,SNP:=as.character(SNP)] 
    #### Import priors ####
    if((!"POLYFUN.h2" %in% names(dat)) | isTRUE(force_new)){
        dat <- POLYFUN_import_priors_handler(dat = dat, 
                                             mode = mode, 
                                             out.path = out.path, 
                                             locus_dir = locus_dir, 
                                             conda_env = conda_env)
    } 
    #### Run SUSIE ####
    if(method=="SUSIE"){
        dat <- SUSIE(dat=dat,
                     LD_matrix=LD_matrix,
                     dataset_type=dataset_type,
                     max_causal=max_causal,
                     compute_n=compute_n,
                     credset_thresh=credset_thresh, 
                     priors_col="POLYFUN.h2",
                     rescale_priors=rescale_priors, 
                     verbose=verbose,
                     ...)
    } 
    #### Run FINEMAP ####
    if(method=="FINEMAP"){
        dat <- FINEMAP(dat=dat,
                       ## Put results in their own subolder
                       ## to avoid using FINEMAP results
                       ## computed without PolyFun priors.
                       locus_dir=file.path(locus_dir,"POLYFUN"),
                       LD_matrix=LD_matrix,
                       compute_n=compute_n,
                       n_causal=max_causal,
                       credset_thresh=credset_thresh,
                       priors_col="POLYFUN.h2",
                       rescale_priors=rescale_priors, 
                       force_new=force_new,
                       nThread=nThread,
                       verbose=verbose,
                       ...)
    }
    return(dat)
}

