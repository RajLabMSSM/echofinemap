#' COJO arguments
#' 
#' COJO arguments, with documentation from the 
#' \href{https://yanglab.westlake.edu.cn/software/gcta/#COJO}{GCTA website}.
#' @keywords internal
#' @param bfile Input PLINK binary PED files, e.g. test.fam, 
#' test.bim and test.bed (see PLINK user manual for details).
#' @param cojo_file Input the summary-level statistics 
#' from a meta-analysis GWAS (or a single GWAS). 
#' @param out Specify output root filename.
#' @param cojo_slct Perform a stepwise model selection procedure to select 
#' independently associated SNPs. Results will be saved in a 
#' *.jma file with additional file *.jma.ldr showing the LD 
#' correlations between the SNPs.
#' @param cojo_cond Perform association analysis of the included
#'  SNPs conditional on the given list of SNPs. 
#'  Results will be saved in a *.cma. The conditional SNP effects 
#'  (i.e. bC) will be labelled as "NA" if the
#'   multivariate correlation between the SNP in question and
#'    all the covariate SNPs is > 0.9.
#' @param cojo_joint Fit all the included SNPs to estimate their joint effects 
#' without model selection. Results will be saved in a
#'  *.jma file with additional file *.jma.ldr showing the 
#'  LD correlations between the SNPs.
#' @param maf Exclude SNPs with minor allele frequency (MAF) 
#' less than a specified value, e.g. 0.01.
#' @param max_maf Include SNPs with MAF less than a specified value, e.g. 0.1.
#' @param exclude Specify a list of SNPs to be excluded from the analysis.
#' @param cojo_top_SNPs Perform a stepwise model selection procedure 
#' to select a fixed number of independently associated SNPs without 
#' a p-value threshold. The output format is the same as that 
#' from \code{--cojo-slct}.
#' @param cojo_p Threshold p-value to declare a genome-wide significant hit.
#' The default value is 5e-8 if not specified. 
#' This option is only valid in conjunction with the option \code{--cojo-slct}.
#' Note: it will be extremely time-consuming if you set 
#' a very low significance level, e.g. 5e-3.
#' @param cojo_wind Specify a distance d (in Kb unit).
#' It is assumed that SNPs more than d Kb away from each other are in 
#' complete linkage equilibrium. The default value is 10000 Kb (i.e. 10 Mb) 
#' if not specified.
#' @param cojo_collinear During the model selection procedure, 
#' the program will check the collinearity between the SNPs that
#'  have already been selected and a SNP to be tested. 
#'  The testing SNP will not be selected if its multiple regression R2 
#'  on the selected SNPs is greater than the cutoff value. 
#'  By default, the cutoff value is 0.9 if not specified.
#' @param diff_freq To check the difference in allele frequency of 
#' each SNP between the GWAS summary datasets and the LD reference sample. 
#' SNPs with allele frequency differences greater than the specified 
#' threshold value will be excluded from the analysis. The default value is 0.2.
#' @param cojo_gc If this option is specified, p-values will be adjusted 
#' by the genomic control method. By default, the genomic 
#' inflation factor will be calculated from the summary-level statistics 
#' of all the SNPs unless you specify a value, e.g. \code{--cojo-gc 1.05}.
#' @param ... Additional arguments to be passed directly to GCTA-COJO 
#' (as character strings).
COJO_args <- function(cojo_file,
                      bfile,
                      out,
                      cojo_slct=NULL,
                      cojo_cond=NULL,
                      cojo_joint=NULL,
                      thread_num=NULL,
                      maf=NULL,
                      max_maf=NULL,
                      exclude=NULL,
                      cojo_top_SNPs=NULL,
                      cojo_p=NULL,
                      cojo_wind=NULL,
                      cojo_collinear=NULL,
                      diff_freq=NULL,
                      cojo_gc=NULL,
                      ...){
    
    check <- function(x){(!is.null(x) && (x!=FALSE))}
    paste(
        if(check(bfile)){
            paste("--bfile",bfile)
        }, 
        if(check(cojo_file)){
            paste("--cojo-file",cojo_file)
        }, 
        if(check(cojo_slct)){
            "--cojo-slct"
        },
        if(check(cojo_cond)){
            paste("--cojo-cond",cojo_cond)
        },
        if(check(cojo_joint)){
            "--cojo-joint"
        },
        if(check(out)){
            paste("--out",out)
        },
        if(check(thread_num)){
            paste("--thread-num",thread_num)
        },
        if(check(maf)){
            paste("--maf",maf)
        },
        if(check(max_maf)){
            paste("--max-maf",max_maf)
        },
        if(check(exclude)){
            paste("--exclude",exclude)
        },
        if(check(cojo_top_SNPs)){
            paste("--cojo-top-SNPs",cojo_top_SNPs)
        },
        if(check(cojo_p)){
            paste("--cojo-p",cojo_p)
        },
        if(check(cojo_p)){
            paste("--cojo-p",cojo_p)
        },
        if(check(cojo_wind)){
            paste("--cojo-wind",cojo_wind)
        },
        if(check(cojo_collinear)){
            paste("--cojo-collinear",cojo_collinear)
        },
        if(check(diff_freq)){ 
            paste("--diff-freq",diff_freq)
        },
        if(check(cojo_gc)){
            "--cojo-gc"
        },
        paste(...,collapse = " ")
    )
}
