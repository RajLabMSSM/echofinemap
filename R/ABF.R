# ***************** #
#====    ABF     ===#
# ***************** #

#' Fine-map with ABF
#'
#' Conduct statistical (non-functional) fine-mapping with approximate Bayes factor (ABF).
#'
#' @inheritParams coloc::finemap.abf
#' @source \href{https://www.nature.com/articles/ng.2435}{
#' JB Maller et al., Bayesian refinement of association signals for
#'  14 loci in 3 common diseases. Nature Genetics. 44, 1294–1301 (2012).}
#' @source \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1950810/}{
#' J Wakefield, A bayesian measure of the probability of false discovery 
#' in genetic epidemiology studies. American Journal of Human Genetics. 
#' 81, 208–227 (2007).} 
#' @export
#' @importFrom dplyr %>%
#' @importFrom echodata get_sample_size
#' @examples
#' # GWAS
#' BST1 <- echodata::BST1;
#' finemap_DT <- ABF(dat=BST1)
#'
#' # QTL
#' locus_dir <- "/sc/arion/projects/pd-omics/brian/Fine_Mapping/Data/QTL/Microglia_all_regions/BIN1"
#' dat <- data.table::fread(file.path(locus_dir,"/Multi-finemap/BIN1.Microglia_all_regions.1KGphase3_LD.Multi-finemap.tsv.gz"))
#' finemap_DT <- ABF(dat=dat, case_control=FALSE, sample_size=90)
ABF <- function(dat,
                PP_threshold=.95,
                sample_size=NULL,
                sdY=NULL,
                case_control=TRUE,
                verbose=TRUE){
  #data.table::fread("Data/GWAS/Nalls23andMe_2019/LRRK2/LRRK2_Nalls23andMe_2019_subset.txt")
  if(case_control){
   dataset <- list(beta = dat$Effect,
                   varbeta = dat$StdErr^2, # MUST be squared
                   s = dat$proportion_cases,
                   snp = dat$SNP,
                   type = "cc")
  } else{
    dataset <- list(beta = dat$Effect,
                    varbeta = dat$StdErr^2, # MUST be squared
                    snp = dat$SNP,
                    type = "quant")
    # Add sdY (standard deviation of quant trait)
    if(!is.null(sdY)) dataset$sdY <- sdY
  }
  # Add MAF
  if("MAF" %in% colnames(dat)) dataset$MAF <- dat$MAF;
  # Add sample size
  if(is.null(sample_size)){
    ss_df <- echodata::get_sample_size(dat = dat,
                                       sample_size = sample_size,
                                       verbose = verbose)
    if(!is.null(ss_df)) dataset$N <-  max(ss_df$N, na.rm = TRUE);
  } else {dataset$N <- sample_size}

  #### Run ABF ####
  finemap_dat <- coloc::finemap.abf(dataset = dataset)

  finemap_dat <- subset(finemap_dat, snp!="null") %>%
    dplyr::rename(SNP=snp, PP=SNP.PP) %>%
    dplyr::arrange(dplyr::desc(PP))
  # Arbitarily assign SNPs with the top N probability as the top candidate causal SNPs
  # finemap_dat$CS <- c(rep(1,n_causal), rep(0,dim(finemap_dat)[1]-n_causal))
  # Any SNPs with a PP greater than the set threshold get included in the credible set
  finemap_dat$CS <- ifelse(finemap_dat$PP >= PP_threshold, 1, 0)
  finemap_dat <- data.table:::merge.data.table(x=data.table::data.table(dat),
                                               y=data.table::data.table(subset(finemap_dat, select=c("SNP","PP","CS")) ),
                                               on="SNP")
  finemap_dat <- finemap_dat %>% dplyr::arrange(desc(PP))
  return(finemap_dat)
}

