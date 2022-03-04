#' Fine-map locus with \code{FINEMAP}
#'
#' The stepwise conditional search starts with a causal configuration 
#' containing the SNP with the lowest P-value alone and then iteratively 
#' adds to the causal configuration the SNP given the highest 
#' posterior model probability until no further SNP yields
#' a higher posterior model probability.
#'
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
#' @source \url{http://www.christianbenner.com}
#' @family FINEMAP
#' 
#' @export
#' @importFrom echodata get_sample_size
#' @examples
#' locus_dir <- file.path(tempdir(),echodata::locus_dir)
#' dat <- echodata::BST1;
#' LD_matrix <- echodata::BST1_LD_matrix
#' 
#' dir.create(file.path(locus_dir,"FINEMAP"),
#'  showWarnings = FALSE, recursive = TRUE)
#' out <- echoLD::subset_common_snps(LD_matrix, dat)
#' LD_matrix <- out$LD
#' dat <- out$DT
#' 
#' dat <- echofinemap::FINEMAP(dat=dat,
#'                locus_dir=locus_dir,
#'                LD_matrix=LD_matrix,
#'                finemap_version="1.3.1")
FINEMAP <- function(dat,
                    locus_dir,
                    LD_matrix,
                    FINEMAP_path=NULL,
                    n_samples=NULL,
                    n_causal=5,# Max number of allowed causal SNPs
                    model="sss",
                    remove_tmps=FALSE,
                    force_new=FALSE,
                    credset_thresh=.95,
                    finemap_version="1.4", 
                    args_list=list(),
                    verbose=TRUE){
  # n_causal=5; model="cond"; credset_thresh=.95; verbose=T;
  # finemap_version="1.3.1"; n_samples=NULL; args_list=list()
  
  #### Add sample size ####
  if(is.null(n_samples)){
    ss_df <- echodata::get_sample_size(dat = dat,
                                       method = n_samples,
                                       force_new = FALSE,
                                       verbose = verbose)
    if(!is.null(ss_df$N)) n_samples <-  max(ss_df$N, na.rm = TRUE);
  }  
  dir.create(locus_dir, showWarnings = FALSE, recursive = TRUE)
  #### Setup files ####
  master_path <- FINEMAP_construct_master(locus_dir = locus_dir,
                                          n_samples = n_samples)
  dat_paths <- FINEMAP_construct_data(locus_dir = locus_dir,
                                      dat = dat,
                                      LD_matrix = LD_matrix)
  #### Use pre=existing results #### 
  dat_prev <- FINEMAP_check_existing_results(locus_dir = locus_dir,
                                             credset_thresh = credset_thresh,
                                             finemap_version = finemap_version,
                                             master_path = master_path,
                                             force_new = force_new,
                                             verbose = verbose)
  if(!is.null(dat_prev)) return(dat_prev)
  
  ####  Command line example  ####
  # cmd <- paste(FINEMAP_path," --sss --in-files",
  # file.path(dirname(FINEMAP_path),
  # "example","master"), "--dataset 1 --n-causal-snps 5")
  
  #### Check FINEMAP exec ####
  if(is.null(FINEMAP_path)){
    FINEMAP_path <- FINEMAP_find_executable(version = finemap_version,
                                            verbose = verbose)
  }else {
    messager("+ FINEMAP:: User-defined FINEMAP path:",FINEMAP_path, v=verbose)
    finemap_version <- FINEMAP_check_version(FINEMAP_path = FINEMAP_path,
                                             verbose = verbose)
  }
  #### Run FINEMAP ####
  # NOTE: Must cd into the directory first,
  # or else FINEMAP won't be able to find the input files.
  msg <- FINEMAP_run(locus_dir=locus_dir,
                     FINEMAP_path=FINEMAP_path,
                     model=model,
                     master_path=master_path,
                     n_causal=n_causal,
                     args_list=args_list,
                     verbose=FALSE)

  #### Check if FINEMAP is giving an error due to `zstd` not being installed ####
  if(any(attr(msg,"status")==134)){
    warning("\n*********\n
    'dyld: Library not loaded: /usr/local/lib/libzstd.1.dylib' error message detected.
         If you are using a Mac OSX, please install Zstandard (https://facebook.github.io/zstd/).
         e.g. via Brew: `brew install zstd`\n\n

         If Zstandard is already installed and this error persists,
         please see the main FINEMAP website for additional support (http://www.christianbenner.com).
            *********\n\n")
    #### Rerun if preferred version of FINEMAP fails ####
    FINEMAP_path <- FINEMAP_find_executable(version = "1.3.1",
                                            verbose  = FALSE)
    message("+ FINEMAP:: Rerunning with FINEMAP v1.3.1.")
    msg <- FINEMAP_run(locus_dir=locus_dir,
                       FINEMAP_path=FINEMAP_path,
                       model=model,
                       master_path=master_path,
                       n_causal=n_causal,
                       ## May not have the args that the user
                       ## was expecting due to version differences.
                       args_list=args_list,
                       verbose=FALSE)
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
                                         results_file = ".cred",
                                         finemap_version = finemap_version)
  # Remove tmp files
  if(remove_tmps){
    messager("+ FINEMAP:: Removing tmp files...")
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
  return(dat)
}
