POLYFUN_import_priors_handler <- function(dat, 
                                          mode,
                                          out.path,
                                          locus_dir,
                                          conda_env,
                                          force_new_priors=FALSE){  
    SNPVAR <- SNP <- NULL;
    
    chrom <- unique(dat$CHR) 
    if(length(chrom)>1) {
        stp1 <- paste(
            "Only one unique chromosome can be",
            "present in the CHR column of dat."
        )
        stop(stp1)
    }
    #### ~~~~~~~~ Approach 1 ~~~~~~~~ 
    if (mode=="precomputed"){ 
        priors <- POLYFUN_import_priors(locus_dir=locus_dir,
                                        dat=dat,
                                        force_new_priors=force_new_priors,
                                        conda_env=conda_env) 
    #### ~~~~~~~~ Approach 2 ~~~~~~~~ #####
    } else if (mode=="parametric"){
        ldsc.files <- list.files(out.path, 
                                 pattern = "*.snpvar_ridge_constrained.gz",
                                 full.names = TRUE) |>
            grep(pattern = paste0(".",chrom,"."),
                 value = TRUE, fixed=TRUE)
        priors <- rbind_filelist(ldsc.files)
    #### ~~~~~~~~ Approach 3 ~~~~~~~~ ####
    } else if (mode=="non-parametric"){
        ldsc.files <- list.files(out.path, 
                                 pattern = "*.snpvar_constrained.gz",
                                 full.names = TRUE) |>  
            base::grep(pattern = paste0(".",chrom,"."), 
                       value = TRUE, fixed = TRUE)
        priors <- rbind_filelist(ldsc.files)
    }
    if(nrow(priors)==0){
        stp2 <- paste("PolyFun:: Could not identify files with priors",
                     "computed using the selected mode:",mode)
        stop(stp2)
    } 
    ##### Ensure formatting is correct ####
    priors <- dplyr::select(priors, SNP, POLYFUN.h2=SNPVAR) |>
        data.table::data.table() |>
        dplyr::mutate(SNP=as.character(SNP))
    merged_dat <- echodata::merge_robust(x = dat,
                                         y = priors,
                                         by="SNP")
    return(merged_dat)
}
