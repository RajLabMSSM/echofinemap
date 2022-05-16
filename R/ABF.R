#' Fine-map with ABF
#'
#' Conduct statistical fine-mapping with Approximate Bayes Factor (ABF)
#' via \link[coloc]{finemap.abf}.
#'
#' @inheritParams echodata::get_sample_size
#' @inheritParams coloc::finemap.abf
#' 
#' @source \href{https://www.nature.com/articles/ng.2435}{
#' JB Maller et al., Bayesian refinement of association signals for
#'  14 loci in 3 common diseases. Nature Genetics. 44, 1294–1301 (2012).}
#' @source \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1950810/}{
#' J Wakefield, A bayesian measure of the probability of false discovery 
#' in genetic epidemiology studies. American Journal of Human Genetics. 
#' 81, 208–227 (2007).} 
#' @export
#' @importFrom dplyr %>% rename arrange desc
#' @importFrom echodata get_sample_size
#' @importFrom coloc finemap.abf
#' @importFrom data.table data.table
#' @examples 
#' dat <- echodata::LRRK2
#' dat2 <- echofinemap::ABF(dat=dat) 
ABF <- function(dat,
                PP_threshold=.95,
                compute_n="ldsc",
                sdY=NULL,
                case_control=TRUE,
                verbose=TRUE){
  SNP.PP <- PP <- snp <- NULL;

  #### Remove rows with NAs ####
  dat <- remove_na_rows(dat=dat, 
                        cols = c("Effect","P",
                                 "StdErr","SNP","MAF"),
                        verbose=verbose)
  #### initialize list ####
  dataset <- list(beta = dat$Effect,
                  pvalues=dat$P,
                  # MUST be squared
                  varbeta = dat$StdErr^2, 
                  snp = dat$SNP)
  if(case_control){ 
    dataset$s <- dat$proportion_cases 
    dataset$type <- "cc"
  } else{
    dataset$type <- "quant" 
    # Add sdY (standard deviation of quant trait)
    if(!is.null(sdY)) dataset$sdY <- sdY
  }
  #### Add MAF ####
  if("MAF" %in% colnames(dat)) dataset$MAF <- dat$MAF;
  #### Add sample size ####
  sample_size <- echodata::get_sample_size(dat = dat,
                                           compute_n = compute_n,
                                           force_new = FALSE,
                                           return_only = max,
                                           verbose = verbose)
  dataset$N <- sample_size
  #### Run ABF ####
  messager("Running ABF.",v=verbose)
  res <- coloc::finemap.abf(dataset = dataset)

  #### Post-process results ####
  res <- subset(res, snp!="null") %>%
    dplyr::rename(SNP=snp, PP=SNP.PP) %>%
    dplyr::arrange(dplyr::desc(PP)) %>% 
    # Any SNPs with a PP greater than the set threshold 
    # get included in the credible set
    dplyr::mutate(CS = ifelse(PP >= PP_threshold, 1, 0)) 
  #### Merge ABF results with input data ####
  res <- echodata::merge_robust(
    x=data.table::data.table(dat), 
    y=data.table::data.table(subset(res, select=c("SNP","PP","CS"))),
    on="SNP")
  res <- res %>% dplyr::arrange(dplyr::desc(PP))
  return(res)
}
