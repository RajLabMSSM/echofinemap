#' Fine-map with SUSIE
#'
#' Sum of Single Effects (SuSiE): Iterative Bayesian Step-wise Selection.
#'
#' \strong{Notes on convergence:}
#' \pkg{susieR} will often give the warning: 
#' \code{IBSS algorithm did not converge in 100 iterations!}.
#' This means the results might not necessarily be reliable.
#' There's several things you can try to avoid this:
#' \itemize{
#' \item{Make sure \code{susieR} is up-to-date: 
#' \code{ devtools::install_github("stephenslab/susieR@@0.9.0")}}
#' \item{Increase \code{max_causal} (e.g. 5 => 10).}
#' \item{Increase \code{max_iter} (e.g. 100 => 1000), 
#' though this will take longer.}
#' \item{Decrease the locus window size, which will also speed up
#'  the algorithm but potentially miss causal variants far from the lead SNP.}
#' }
#' Changing \code{estimate_prior_method} does not seem to affect 
#' convergence warnings.
#'
#' \strong{Notes on variance:}
#' \href{https://github.com/stephenslab/susieR/issues/90}{GitHub Issue}
#' If \code{estimate_residual_variance=TRUE} \emph{without} 
#' providing \code{var_y} \emph{and} \code{L>1}, \pkg{susieR} will throw error:
#' \code{Estimating residual variance failed: the estimated value is negative}
#' Running \pkg{susieR} with \code{var_y = var(b)} provides \emph{exactly}
#'  the same results.
#'
#' @param max_causal The maximum number of non-zero effects 
#' (and thus causal variants).
#' @param plot_track_fit Record each iteration and make a GIF of the
#' fine-mapping algorithm learning the causal variants.
#' \strong{WARNING!:} Making this plot can take a long time if there's
#'  many iterations.
#' @param var_y [Optional] User-supplied phenotypic variance value(s). 
#' Can be one of the following:
#' \itemize{
#' \item{\code{NULL}: }{Variance will be inferred automatically by SUSIE.}
#' \item{Numeric vector: }{Variance will be computed directly from vector.}
#' \item{Character string: }{The name of a column in \code{dat}
#'  to extract a numeric vector from to compute variance.}
#' \item{"case_control"}{Variance will be inferred from the proportion 
#' of cases/controls in the study. 
#' Only works when both "N_cases" and "N_controls" are columns in \code{dat}.}
#' }
#' @param return_all_CS If >1 Credible Set is identified, 
#' return them all (\code{TRUE}), or just the first (\code{FALSE}).
#' @inheritParams multifinemap
#' @inheritParams prepare_priors
#' @inheritParams susieR::susie_suff_stat
#' @inheritParams susieR::susie_plot_iteration
#' @inheritParams echoLD::get_LD
#' @inheritParams echodata::get_sample_size
#' 
#' @source
#' \href{https://stephenslab.github.io/susieR/}{GitHub}
#' \href{https://rss.onlinelibrary.wiley.com/doi/full/10.1111/rssb.12388}{
#' Publication}
#' 
#' @importFrom echoLD subset_common_snps
#' @importFrom stats setNames
#' @importFrom rlang missing_arg
#' @importFrom Matrix forceSymmetric
#' @importFrom susieR susie_get_pip susie_get_cs susie_plot_iteration
#' 
#' @export
#' @examples
#' dat <- echodata::BST1
#' LD_matrix <- echodata::BST1_LD_matrix
#' dat2 <- echofinemap::SUSIE(dat=dat, LD_matrix=LD_matrix)
SUSIE <- function(dat,
                  LD_matrix,
                  case_control=TRUE,
                  # susieR default max_causal=L=10
                  max_causal=5,
                  # susieR default sample_size=n=<missing>
                  compute_n="ldsc",
                  # susieR default prior_weights=NULL
                  priors_col=NULL,
                  rescale_priors=TRUE,
                  # susieR default credset_thresh=coverage=.95
                  credset_thresh=.95,
                  # PolyFun default: scaled_prior_variance=0.0001 
                  # susieR default: scaled_prior_variance=0.2
                  scaled_prior_variance=0.001, 
                  # susieR default estimate_residual_variance=T
                  estimate_residual_variance=FALSE,
                  # susieR default estimate_prior_variance=T
                  estimate_prior_variance=TRUE,
                  # susieR default residual_variance=NULL
                  residual_variance=NULL,
                  # susieR default max_iter=100
                  max_iter=100,
                  # susieR default="optim"
                  estimate_prior_method="optim",
                  var_y=NULL,
                  
                  plot_track_fit=FALSE,
                  return_all_CS=TRUE,
                  file_prefix=file.path(tempdir(),"SUSIE"), 
                  verbose=TRUE){
  
    if(!requireNamespace("Rfast")){
        warning("Install Rfast to speed up susieR even further:\n",
                "   install.packages('Rfast')")
    } 
    #### Remove rows with NAs ####
    dat <- remove_na_rows(dat=dat, 
                          cols = c("Effect","StdErr","SNP","MAF"),
                          verbose=verbose)
    #### sample_size ####
    sample_size <- echodata::get_sample_size(dat = dat,
                                             compute_n = compute_n,
                                             force_new = FALSE,
                                             return_only = max,
                                             verbose = verbose) 
    if(is.null(sample_size)) {
        stop("sample_size=NULL: must be valid integer.") 
    }
    messager("+ SUSIE::",
             paste0("sample_size=",formatC(sample_size,big.mark = ",")),
             v=verbose)
  #### Get phenotype variance ####
  if(!is.null(var_y)){
    var_y <- get_pheno_variance(dat = dat,
                                case_control = case_control,
                                var_y = var_y,
                                verbose = verbose)
  } else {var_y <- rlang::missing_arg()}
  #### Filter SNPs to only those in LD ref ####
  sub.out <- echoLD::subset_common_snps(
      LD_matrix=LD_matrix,
      dat=dat,
      fillNA = 0,
      verbose = verbose)
  LD_matrix <- sub.out$LD
  dat <- sub.out$DT 
  #### Prepare priors #### 
  prior_weights <- prepare_priors(dat = dat, 
                                  priors_col = priors_col, 
                                  snp_col = "SNP",
                                  rescale_priors = rescale_priors,
                                  verbose = verbose)
  ## Ensure the matrix is of "symmetric" class (not just objectively symmetric)
  ## susieR will throw an error otherwise.
  LD_matrix <- Matrix::forceSymmetric(x = LD_matrix,
                                      uplo = "U")
  #### Run fine-mapping #####
  fitted_bhat <- SUSIE_run(
    dat=dat,
    LD_matrix=LD_matrix,
    sample_size=sample_size,
    max_causal=max_causal,
    scaled_prior_variance=scaled_prior_variance,
    estimate_prior_variance=estimate_prior_variance,
    residual_variance=residual_variance,
    max_iter=max_iter,
    estimate_prior_method=estimate_prior_method,
    estimate_residual_variance=estimate_residual_variance,
    prior_weights=prior_weights,
    credset_thresh=credset_thresh,
    plot_track_fit=plot_track_fit,
    verbose=verbose)
  #### Animated plot track ####
  SUSIE_plot_track(plot_track_fit=plot_track_fit,
                   fitted_bhat=fitted_bhat,
                   max_causal=max_causal,
                   file_prefix=file_prefix)
  dat <- SUSIE_extract_cs(dat=dat,
                          fitted_bhat=fitted_bhat,
                          credset_thresh=credset_thresh,
                          return_all_CS=return_all_CS,
                          verbose=verbose)
  return(dat)
}



# * Notes on L parameter
# + L is the expected number of causal variants
# + Increasing L increases computational time
# + L=1: Gives a good amount of variation in PIP.
# + L=2: Warns "IBSS algorithm did not converge in 100 iterations!", but gives good variation in PIP.
# + L=3: Warns "IBSS algorithm did not converge in 100 iterations!". All PIPs 1s and 0s.
# + These results seem to be at least partially dependent on whether the ethnic composition of the LD matrix.

# * Statistical Terms:
#   + posterior inclusion probability (PIP)
#   + coefficient estimate (Beta)
#   + Effect allele frequency (EAF)
#   + The I^2 statistic describes the percentage of variation across studies that seems not to be due to chance.
