#' Run a modified version of S-LDSC
#'
#' Modifications to S-LDSC include L2-regularization.
#' @source
#' https://www.nature.com/articles/s41588-020-00735-5
#' @keywords internal
#' @family polyfun
POLYFUN_run_ldsc <- function(polyfun=NULL,
                             output_dir=NULL,
                             munged.path,
                             min_INFO = 0.6,
                             min_MAF = 0.05,
                             annotations.path=file.path(polyfun,"example_data/annotations."),
                             weights.path=file.path(polyfun,"example_data/weights."),
                             prefix="LDSC",
                             chrom="all",
                             compute_ldscores=FALSE,
                             allow_missing_SNPs=TRUE,
                             munged_path="/sc/arion/projects/pd-omics/tools/polyfun/Nalls23andMe_2019.sumstats_munged.parquet",
                             ref.prefix="/sc/arion/projects/pd-omics/data/1000_Genomes/Phase1/1000G.mac5eur.",
                             freq.prefix="/sc/arion/projects/pd-omics/tools/polyfun/1000G_frq/1000G.mac5eur.",
                             conda_env="echoR_mini",
                             verbose=TRUE){
    polyfun <- POLYFUN_find_polyfun_folder(polyfun_path = polyfun)
    python <- echoconda::find_python_path(conda_env = conda_env) 
    
    # 0. Create paths
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    out.path <- file.path(output_dir,"output")
    output_prefix <- file.path(out.path, prefix, prefix)
    dir.create(out.path, showWarnings = FALSE, recursive = TRUE)
    # https://github.com/bulik/ldsc/wiki/Partitioned-Heritability
    cmd <- paste(python,
                 file.path(polyfun,"ldsc.py"),
                 "--h2",munged_path,
                 "--ref-ld-chr",annotations.path,
                 "--w-ld-chr",weights.path,
                 "--overlap-annot",
                 "--frqfile-chr",freq.prefix,
                 "--not-M-5-50", # Important! enrichment estimates will be provided with MAF>0.1% SNPs instead of MAF>5% SNPs.
                 "--out",output_prefix)
    # help_cmd <- paste("python",file.path(polyfun,"ldsc.py -h"))
    echoconda::cmd_print(cmd, verbose=verbose)
    system(cmd)
}
