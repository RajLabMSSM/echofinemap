#' Extract pre-computed prior
#'
#' Extract SNP-wise prior probabilities pre-computed from many UK Biobank traits.
#'
#' @source
#' https://www.biorxiv.org/content/10.1101/807792v3
#' @keywords internal
#' @family polyfun
#' @examples
#' \dontrun{
#'
#' dat <- echodata::BST1; 
#' locus_dir <- echodata::locus_dir;
#' priors <- POLYFUN_get_precomputed_priors(locus_dir=locus_dir, dat=dat)
#' }
POLYFUN_get_precomputed_priors <- function(polyfun=NULL,
                                           locus_dir,
                                           dat=NULL,
                                           force_new_priors=TRUE,
                                           remove_tmps=FALSE,
                                           nThread=1,
                                           conda_env="echoR"){
    python <- echoconda::find_python_path(conda_env = conda_env)
    polyfun <- POLYFUN_find_polyfun_folder(polyfun_path = polyfun)
    dataset <- basename(dirname(locus_dir))
    locus <- basename(locus_dir)
    PF.output.path <- file.path(locus_dir, "PolyFun")
    snp_w_priors.file <- file.path(PF.output.path,"snps_with_priors.snpvar.tsv.gz")
    
    if((file.exists(snp_w_priors.file)) & force_new_priors==FALSE){
        print("++ Importing pre-existing priors.")
        priors <- data.table::fread(snp_w_priors.file, nThread = nThread) %>%
            dplyr::rename(SNP=SNP_x) %>% dplyr::select(-SNP_y)
        return(priors)
    } else {
        dat <- POLYFUN_initialize(dat=dat,
                                  locus_dir=locus_dir)
        chrom <- dat$CHR[1]
        # [1]. Prepare input
        snp.path <- POLYFUN_prepare_snp_input(PF.output.path=PF.output.path,
                                              locus_dir=locus_dir,
                                              dat=dat)
        # [2.] Retrieve priors
        # test <- data.table::fread("./Data/GWAS/Nalls23andMe_2019/LRRK2/PolyFun/snps_to_finemap.txt.gz")
        try({
            cmd <- paste(python,
                         file.path(polyfun,"extract_snpvar.py"),
                         "--snps",snp.path,
                         "--out",snp_w_priors.file)
            print(cmd)
            system(cmd)
            messager("++ Remove tmp file.")
        })
        
        miss.file <- paste0(snp_w_priors.file,".miss.gz")
        if(file.exists(miss.file)){
            messager("+ PolyFun:: Rerunning after removing missing SNPs.")
            miss.snps <- data.table::fread(miss.file)
            filt_DT <- subset(dat, !(SNP %in% miss.snps$SNP) )
            if(remove_tmps){file.remove(miss.file)}
            priors <- POLYFUN_get_precomputed_priors(locus_dir=locus_dir,
                                                     dat=filt_DT,
                                                     force_new_priors=force_new_priors,
                                                     conda_env = conda_env)
        }
        # Import results
        priors <- data.table::fread(snp_w_priors.file,
                                    nThread = nThread) %>%
            dplyr::rename(SNP=SNP_x) %>% dplyr::select(-SNP_y)
        return(priors)
    }
    if(remove_tmps){ file.remove(snp.path) }
}


