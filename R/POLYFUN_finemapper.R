#' Run \emph{PolyFun+SUSIE} fine-mapping pipeline
#'
#' @source
#' https://www.nature.com/articles/s41588-020-00735-5
#' @keywords internal
#' @family polyfun
POLYFUN_finemapper <- function(polyfun=NULL,
                               dat=NULL,
                               npz_gz_LD=NULL,
                               locus=NULL,
                               sample_size=NULL,
                               locus_dir,#="Data/GWAS/Nalls23andMe_2019/LRRK2",
                               n_causal=5,
                               method="susie",
                               h2_path=NULL,
                               conda_env="echoR_mini",
                               verbose=TRUE){
    # base_url  <- "./echolocatoR/tools/polyfun/LD_temp"
    polyfun <- POLYFUN_find_polyfun_folder(polyfun_path = polyfun)
    python <- echoconda::find_python_path(conda_env = conda_env)
    
    chrom <- unique(dat$CHR)
    file.name <- paste0("chr",chrom,"_","40000001_43000001")
    ld_path <- file.path(locus_dir,"LD",file.name)
    out_path <- file.path(locus_dir,"PolyFun",
                          paste0("finemapper_res.",basename(locus_dir),".gz"))
    dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
    
    # if(is.null(h2_path)){
    #   url_prefix <- "/sc/arion/projects/pd-omics/brian/Fine_Mapping/Data/GWAS/Nalls23andMe_2019/_genome_wide/PolyFun/output/PD_GWAS."
    #   h2_path <- paste0(url_prefix,chrom,".snpvar_constrained.gz")
    # }
    
    if(is.null(sample_size) & (!"N" %in% colnames(dat))){
        dat <- get_sample_size(dat)
        sample_size <- max(dat$N)
    }
    # munged.path <- "~/Desktop/Nalls23andMe_2019.sumstats_munged.parquet"
    # locus="LRRK2"
    if(is.null(npz_gz_LD)){
        npz_path <- list.files(file.path(locus_dir, "LD"), ".npz$",full.names = TRUE)
        if(length(npz_path)>0){
            npz_path <- npz_path[1]
        } else {
            rds_path <- list.files(file.path(locus_dir, "LD"), ".RDS$",full.names = TRUE)[1]
            npz_path <- echoLD:::rds_to_npz(rds_path = rds_path)
        }
        npz_prefix <- gsub(".npz$","",npz_path)
    }
    
    cmd <- paste(python,
                 file.path(polyfun,"run_finemapper.py"),
                 "--ld",npz_prefix,
                 "--sumstats",h2_path,
                 "--n",sample_size,
                 "--chr",chrom,
                 "--start",min(dat$POS),
                 "--end",max(dat$POS),
                 "--method",method,
                 "--max-num-causal",n_causal,
                 # "--threads 2",# use max detected cores if not specified
                 "--out",out_path)
    echoconda::cmd_print(cmd, verbose=verbose)
    system(cmd)
    
    h2 <- rbind_filelist("Data/GWAS/Nalls23andMe_2019/_genome_wide/PolyFun/output/PD_GWAS.16.snpvar_constrained.gz")
}

