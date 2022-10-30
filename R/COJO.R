#' Run \emph{GCTA-COJO}
#'
#' Main function to run either the conditional stepwise procedure (genome-wide)
#' or the conditional analysis (locus-specific) from \emph{GCTA-COJO}.
#' 
#' \href{http://cnsgenomics.com/software/gcta/#COJO}{\strong{Documentation}}\cr
#' Columns are SNP, the effect allele, the other allele, 
#' frequency of the effect allele,
#' effect size, standard error, p-value and sample size.
#' The headers are not keywords and will be omitted by the program.
#' Important: "A1" needs to be the effect allele
#' with "A2" being the other allele and "freq" 
#' should be the frequency of "A1".'\cr
#' Note: 1) For a case-control study, the effect size should be log(odds ratio) 
#' with its corresponding standard error.
#' 2) Please always input the summary statistics of all SNPs even 
#' if your analysis only focuses on a subset of SNPs
#' because the program needs the summary data of all SNPs to calculate the 
#' phenotypic variance. 
#' You can use one of the \code{--extract} options (Data management) to limit 
#' the  COJO analysis in a certain genomic region.\cr\cr
#' \strong{General results columns}:\cr
#' \itemize{
#' \item{Chr : }{Chromosome.}
#' \item{SNP : }{SNP RSID.}
#' \item{bp : }{Physical position.}
#' \item{refA : }{Effect allele.}
#' \item{freq : }{Frequency of the effect allele in the original data.}
#' \item{b : }{Effect size.}
#' \item{se : }{Standard error.}
#' \item{p : }{p-value from the original GWAS or meta-analysis.}
#' \item{n : }{Estimated effective sample size.}
#' \item{freq_geno : }{Frequency of the effect allele in the reference sample.}
#' }
#' \strong{Stepwise analysis results columns}:\cr
#' \itemize{
#' \item{bJ : }{Effect size from the joint analysis of all the selected SNPs.}
#' \item{bJ_se : }{Standard error from the joint analysis of all 
#' the selected SNPs.}
#' \item{pJ : }{p-value from the joint analysis of all the selected SNPs.}
#' \item{LD_r : }{LD correlation between the SNP i and SNP i + 1 for
#'  the SNPs on the list.}
#' \item{LD_r2 : }{LD_r squared}
#' \item{CS : }{Whether the SNP is in the Credible Set, 
#' defined as any SNP with where \code{pJ<(1-credset_thresh)}.}
#' }
#' \strong{Conditional analysis results columns:}\cr
#' \itemize{
#' \item{bC : }{effect size from the conditional analysis}
#' \item{bC_se : }{standard error from the conditional analysis}
#' \item{pC : }{p-value from the conditional analysis}
#' \item{CS : }{Whether the SNP is in the Credible Set, 
#' defined as any SNP with where \code{pC<(1-credset_thresh)}}.
#' } 
#' @param run_stepwise \code{--cojo-slct}:
#'  Perform a stepwise model selection procedure
#'  to select independently associated SNPs.
#'  Results will be saved in a \emph{*.jma} file
#'  with additional file \emph{*.jma.ldr} showing
#'  the LD correlations between the SNPs.
#' @param run_conditional \code{--cojo-cond}: 
#' Perform association analysis of the included SNPs conditional on the given
#'  list of SNPs. Results will be saved in a \emph{*.cma}. 
#'  The conditional SNP effects (i.e. bC) will be labelled as "NA" if 
#'  the multivariate correlation between the SNP in question and all
#'  the covariate SNPs is > 0.9.
#' @param run_joint \code{--cojo-joint}: 
#' Fit all the included SNPs to estimate their joint 
#' effects without model selection. Results will be saved in a 
#' \emph{*.jma} file with additional file \emph{*.jma.ldr}.
#' @param gcta_path Path to the GCTA-COJO executable.
#' @param prefix Prefix to use for file names.
#' @param freq_cutoff Minimum variant frequency cutoff.
#' @param full_genome Whether to run GCTA-COJO across genome-wide (\code{TRUE}),
#' or within a specific locus (default: \code{FALSE})
#' @inheritParams COJO_args
#' @inheritParams multifinemap
#' @inheritParams echodata::find_consensus_snps
#' @inheritParams echodata::get_sample_size
#' @inheritParams echodata::import_topSNPs
#' @inheritDotParams COJO_args
#' @family COJO
#' @source 
#' \href{https://yanglab.westlake.edu.cn/software/gcta/#COJO}{
#' COJO documentation}
#' \href{https://doi.org/10.1016/j.ajhg.2010.11.011}{Publication 1}
#' \href{https://doi.org/10.1038/ng.2213}{Publication 2}
#' 
#' @export
#' @importFrom echodata construct_colmap
#' @importFrom echoconda find_executables_remote
#' @examples 
#' vcf <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
#'     package = "echodata")
#' dat <- echodata::BST1
#' locus_dir <- file.path(tempdir(), echodata::locus_dir)
#' fullSS_path <- echodata::example_fullSS()
#' bfile <- echoLD::vcf_to_plink(vcf = vcf)$prefix
#' cojo_DT <- COJO(dat = dat, 
#'                 locus_dir = locus_dir,
#'                 fullSS_path = fullSS_path,
#'                 bfile = bfile)
COJO <- function(dat,
                 locus_dir,
                 bfile = file.path(locus_dir,"LD/plink"),
                 fullSS_path = NULL,
                 conditioned_snps = NULL,
                 exclude = NULL, 
                 prefix = "cojo",
                 run_stepwise = TRUE,
                 run_conditional = FALSE,
                 run_joint = FALSE, 
                 credset_thresh = 0.95, 
                 freq_cutoff = 0.1,
                 compute_n = "ldsc",
                 colmap = echodata::construct_colmap(),
                 full_genome = FALSE,
                 gcta_path = 
                     echoconda::find_executables_remote(tool="gcta")[[1]],
                 verbose = TRUE,
                 ...
                 ){ 
    # echoverseTemplate:::args2vars(COJO)
    # echoverseTemplate:::source_all()
    
    N <- NULL;
    if(all(run_stepwise,run_joint)){
        wrn <- paste(
            "When both run_stepwise=TRUE and run_joint=TRUE,",
            "the latter will override the first",
            "(i.e. only joint analysis will be run)."
        )
        warning(wrn)
    }
    #### Find executable ####
    gcta_path <- COJO_find_executable(gcta_path = gcta_path, 
                                      verbose = verbose)
    #### Make path ####
    cojo_dir <- COJO_locus_subdir(locus_dir)  
    #### Create cojo .ma file ####
    ## NOTE: cojo-file.ma Must be a SPACE-separated file
    if(isTRUE(full_genome)){
        if(is.null(fullSS_path)) {
            stp <- "fullSS_path must be provided when full_genome=TRUE"
            stop(stp)
        }
        cojo.ma <- data.table::fread(fullSS_path)
        cojo.ma <- echodata::get_sample_size(dat = cojo.ma,
                                             compute_n = compute_n,
                                             verbose = verbose) |>
                   dplyr::select(SNP=colmap$SNP,
                                 A1=colmap$A1,
                                 A2=colmap$A2,
                                 freq=colmap$Freq,
                                 b=colmap$Effect,
                                 se=colmap$StdErr,
                                 p=colmap$P,
                                 N)
      # Create genome-wide dir
      cojo_dir <- file.path(dirname(locus_dir),"_genome_wide","COJO")
      dir.create(cojo_dir, showWarnings = FALSE, recursive = TRUE)
    } else {
      # Use subset of summary stats (not for the stepwise conditional procedure)
        cojo.ma <- echodata::get_sample_size(dat = dat,
                                         compute_n = compute_n) |>
                   dplyr::select(SNP=colmap$SNP,
                                 A1=colmap$A1,
                                 A2=colmap$A2,
                                 freq=colmap$Freq,
                                 b=colmap$Effect,
                                 se=colmap$StdErr,
                                 p=colmap$P,
                                 N)
    }
    #### Write master file ####
    cojo_file <- file.path(cojo_dir,paste0(prefix,".file.ma"))
    data.table::fwrite(cojo.ma, cojo_file, sep=" ")
    #### Create of SNPs to exclude from analysis ####
    if(!is.null(exclude)){
        exclude_path <- file.path(cojo_dir,paste0(prefix,".excluded_snps.txt"))
        data.table::fwrite(x = list(exclude),
                           file = exclude_path, 
                           quote  = FALSE)
    } else {exclude_path <- NULL}
    
    if(isTRUE(run_conditional) && length(conditioned_snps)>0){
        snp_list <- paste(conditioned_snps, collapse="\n")
        cojo_cond <- file.path(cojo_dir,paste0(prefix,".cond.txt"))
        data.table::fwrite(list(snp_list),
                           cojo_cond, 
                           quote = FALSE, sep=" ")
    } else {cojo_cond <- NULL}
    #### Run COJO #### 
    paths <- COJO_run(
        gcta_path = gcta_path,
        cojo_dir = cojo_dir,
        cojo_file = cojo_file,
        cojo_slct = run_stepwise,
        cojo_joint = run_joint,
        cojo_cond = cojo_cond,
        conditioned_snps = conditioned_snps,
        bfile = bfile,
        prefix = prefix, 
        exclude = exclude_path, 
        verbose = verbose,
        ...) 
    cojo_DT <- COJO_process_results(dat = dat,
                                    paths = paths,
                                    credset_thresh = credset_thresh,
                                    freq_cutoff = freq_cutoff,
                                    verbose = verbose)
    return(cojo_DT)
}


