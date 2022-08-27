#' Fine-map locus with \code{FINEMAP}
#'
#' The stepwise conditional search starts with a causal configuration 
#' containing the SNP with the lowest P-value alone and then iteratively 
#' adds to the causal configuration the SNP given the highest 
#' posterior model probability until no further SNP yields
#' a higher posterior model probability.\cr\cr
#' \bold{Output coumns}\cr
#' Note that not not all versions of \code{FINEMAP} will necessarily 
#' have all of these columns, 
#' but "PP" and "CS" will always be present regardless of version. 
#' \itemize{
#' \item{\emph{PP} : \cr}{
#' Per-SNP conditional posterior probability (PP), 
#'  after conditioning on the other SNPs 
#'  within its respective Credible Set (CS).
#'  If a given SNP is present in more than one CS, 
#'  each row will be a list of PP (one per CS).
#'  These PP were extracted from \emph{.cred} file(s).
#'  SNPs that were not within the CS are designated \code{NA}.
#' }
#' \item{\emph{CS} : \cr}{
#' The Credible Set (CS) that a given SNP belongs to.
#'  If a given SNP is present in more than one CS, 
#'  each row will be a list of CS id numbers (one per CS).
#'  These CS were extracted from \emph{.cred} file(s).
#' }
#' \item{\emph{PP_snp} : \cr}{
#' Per-SNP marginal posterior inclusion probability (PIP), 
#'  which is the probability that a given SNP is in the Credible Set (CS).
#'  These PIP were extracted from \emph{.snp} file(s).
#'  Only SNPs that were excluded from the fine-mapping input. 
#' (e.g. due to not overlapping with the LD panel) are designated \code{NA}.
#' } 
#' \item{\emph{PP_config} : \cr}{
#' Per-CS posterior probability (PP) that a given Credible Set (CS) 
#'  (i.e. "configuration" of SNPs) is causal.
#'  These PP were extracted from \emph{.config} file(s).
#'  Only SNPs that were not within any CS are designated \code{NA}.
#' }   
#' \item{\emph{k} : \cr}{
#' Optimized number of causal SNPs, which can be less than or equal to 
#' the user-supplied \code{n_causal} argument.
#'  These values were extracted from \emph{.log} file(s).
#' } 
#' } 
#' @param model "cond" for stepwise conditional search, 
#' "sss" for stochastic shotgun search.
#' @param finemap_version Which FINEMAP version to use (specify as a string).
#' @param args_list A named list of additional arguments to pass to FINEMAP
#' (e.g.: args_list = list("--n-iterations"=5000,"--sss"="")).
#' Alternatively, can supply a string instead 
#' (e.g.: args_list = "--n-iterations 5000 --sss").
#' @param FINEMAP_path Path to a custom FINEMAP executable to use
#' instead of the ones included in \pkg{echolocatoR}.
#' Users can also simply supply "finemap" if this command is linked to
#'  the executable.
#' @param fillNA Fill CS/PP values without fine-mapping results 
#' (i.e. \code{NA}) with some default value (e.g. 0).
#' @param force_new If saved results already exist in the given
#' \code{locus_dir}, skip re-running FINEMAP and use them 
#' (default: \code{force_new}). 
#' Set \code{TRUE} to ignore these files and re-run FINEMAP.
#' @param nThread Number of threads to parallelise across.
#' Passed to \code{"--n-threads"} in FINEMAP.
#' @inheritParams multifinemap 
#' @inheritParams echodata::get_sample_size
#' 
#' @source \url{http://www.christianbenner.com}
#' @family FINEMAP
#' 
#' @export
#' @importFrom echodata get_sample_size
#' @importFrom data.table :=
#' @examples
#' locus_dir <- file.path(tempdir(),echodata::locus_dir)
#' dat <- echodata::BST1;
#' LD_matrix <- echofinemap::drop_finemap_cols(echodata::BST1_LD_matrix)
#' out <- echoLD::subset_common_snps(LD_matrix, dat)
#' LD_matrix <- out$LD
#' dat <- out$DT
#'
#' dat2 <- echofinemap::FINEMAP(dat=dat,
#'                              locus_dir=locus_dir,
#'                              LD_matrix=LD_matrix)
FINEMAP <- function(dat,
                    locus_dir=tempdir(),
                    LD_matrix,
                    FINEMAP_path=NULL,
                    compute_n="ldsc",
                    n_causal=5,# Max number of allowed causal SNPs
                    model="sss",
                    remove_tmps=FALSE,
                    force_new=FALSE,
                    credset_thresh=.95,
                    finemap_version=package_version("1.4.1"), 
                    priors_col=NULL,
                    rescale_priors=TRUE,
                    args_list=list(),
                    fillNA=0,
                    nThread=1,
                    verbose=TRUE){
  # echoverseTemplate:::source_all(packages = "dplyr")
  # echoverseTemplate:::args2vars(FINEMAP)
  
  CS <- PP <- NULL;
  
  if(!methods::is(finemap_version,"package_version")){
      finemap_version <- package_version(finemap_version)
  }
  #### Remove rows with NAs ####
  dat <- remove_na_rows(dat=dat, 
                        cols = c("Effect","StdErr","SNP","MAF",
                                 "CHR","POS","A1","A2"),
                        verbose=verbose)
  #### Add sample size #### 
  n_samples <- echodata::get_sample_size(dat = dat,
                                       compute_n = compute_n,
                                       force_new = FALSE,
                                       return_only = max,
                                       verbose = verbose) 
  dir.create(locus_dir, showWarnings = FALSE, recursive = TRUE) 
  #### Use pre=existing results #### 
  dat_prev <- FINEMAP_check_existing_results(dat = dat,
                                             locus_dir = locus_dir,
                                             credset_thresh = credset_thresh,
                                             finemap_version = finemap_version,
                                             force_new = force_new,
                                             verbose = verbose)
  if(!is.null(dat_prev)) return(dat_prev) 
  #### Subset data ####
  sub.out <- echoLD::subset_common_snps(LD_matrix = LD_matrix,
                                        dat = dat,
                                        verbose = verbose)
  LD_matrix <- sub.out$LD
  dat <- sub.out$DT 
  remove(sub.out)
  #### Prepare priors #### 
  prior_k <- prepare_priors(dat = dat, 
                            priors_col = priors_col, 
                            snp_col = "SNP",
                            rescale_priors = rescale_priors,
                            verbose = verbose)
  ### Setup files ####
  data.k_path <- FINEMAP_construct_datak(prior_k=prior_k, 
                                         locus_dir=locus_dir,
                                         n_causal=n_causal,
                                         verbose=verbose)
  master_path <- FINEMAP_construct_master(locus_dir = locus_dir,
                                          n_samples = n_samples,
                                          data.k_path = data.k_path,
                                          verbose = verbose)
  dat_out <- FINEMAP_construct_data(locus_dir = locus_dir,
                                      dat = dat,
                                      LD_matrix = LD_matrix,
                                      verbose = verbose)   
  #### Check FINEMAP exec ####
  if(is.null(FINEMAP_path)){
    FINEMAP_path <- FINEMAP_find_executable(version = finemap_version,
                                            verbose = verbose)
  }  
  messager("FINEMAP path:",FINEMAP_path, v=verbose)
  finemap_version <- FINEMAP_check_version(FINEMAP_path = FINEMAP_path,
                                           verbose = verbose) 
  dylib_msg <- paste(
      "\n*********\n",
      "System dependency error detected:",
      "If you are using Mac OS, please make sure you have the",
      "following software installed using brew:", 
      "e.g. `brew install zstd libomp gcc`",
      
      "If this error persists,\n",
      "please see the main FINEMAP website for additional support\n",
      "(http://www.christianbenner.com).",
      "\n*********\n\n"
  )
  if(length(finemap_version)==0){
      warning(dylib_msg)
      finemap_version <- package_version("1.3.1")
      FINEMAP_path <- FINEMAP_find_executable(version = "1.3.1",
                                              verbose  = verbose)
  }
  if(finemap_version<"1.4") {
      nThread <- 1
  }
  #### Run FINEMAP ####
  # NOTE: Must cd into the directory first,
  # or else FINEMAP won't be able to find the input files.
  msg <- FINEMAP_run(locus_dir=locus_dir,
                     FINEMAP_path=FINEMAP_path,
                     model=model,
                     master_path=master_path,
                     n_causal=n_causal, 
                     prior_k=prior_k,
                     args_list=args_list,
                     nThread=nThread,
                     verbose=verbose)
  ## Check if FINEMAP is giving an error due to `zstd` 
  ## not being installed.
  if(any(attr(msg,"status")==134)){ 
    warning(dylib_msg)
    #### Rerun if preferred version of FINEMAP fails ####
    FINEMAP_path <- FINEMAP_find_executable(version = "1.3.1",
                                            verbose  = FALSE)
    finemap_version <- package_version("1.3.1")
    messager("Rerunning with FINEMAP v1.3.1.",v=verbose)
    msg <- FINEMAP_run(locus_dir=locus_dir,
                       FINEMAP_path=FINEMAP_path,
                       model=model,
                       master_path=master_path,
                       n_causal=n_causal,
                       prior_k=prior_k,
                       ## May not have the args that the user
                       ## was expecting due to version differences.
                       args_list=args_list,
                       ### Must be 1 for older versions of FINEMAP
                       nThread=1,
                       verbose=verbose)
    ## Note!: concatenating this output in rmarkdown
    ## can accidentally print many many lines.
    if(verbose) try({cat(paste(msg, collapse = "\n"))})
  } else {
    if(verbose) try({cat(paste(msg, collapse = "\n"))})
  }
  #### Process results #### 
  dat <- FINEMAP_process_results(locus_dir = locus_dir,
                                 dat = dat,
                                 credset_thresh = credset_thresh,
                                 finemap_version = finemap_version,
                                 verbose = verbose)
  # Remove tmp files
  if(remove_tmps){
    messager("Removing temp files.",v=verbose)
    tmp_files <- file.path(locus_dir,"FINEMAP",
                           c("data.snp",
                             "data.config",

                             "data.ld",
                             "data.log_cond",
                             "data.log_sss",
                             "data.z",
                             "master")
    )
    tmp_bool <- suppressWarnings(file.remove(tmp_files))
    tmp_bool <- suppressWarnings(file.remove(file.path(locus_dir,"FINEMAP")))
  }
  #### Fill NA #### 
  if(!is.null(fillNA)){
      for(x in c("PP","CS","PP_snp","PP_config")){
          if(x %in% names(dat) && 
             sum(is.na(dat[[x]]))>0 ){
              dat[is.na(dat[[x]]),x] <- fillNA 
          }
      }
     
      dat[is.na(PP),]$PP <- fillNA
  } 
  return(dat)
}
