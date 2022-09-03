#' Multi-finemap handler: select fine-mapping method 
#' 
#' @family finemapping functions
#' @keywords internal
#' @inheritParams multifinemap
#' @inheritParams echodata::get_sample_size
#' @inheritParams echodata::find_consensus_snps
multifinemap_handler_method <- function(dat,
                                        locus_dir,
                                        fullSS_path=NULL,
                                        finemap_method,
                                        finemap_args=NULL,
                                        
                                        dataset_type="GWAS",
                                        force_new_finemap=FALSE,
                                        LD_matrix=NULL,
                                        n_causal=5,
                                        conditioned_snps,
                                        compute_n="ldsc", 
                                        PAINTOR_QTL_datasets=NULL,
                                        credset_thresh=.95,
                                        case_control=TRUE,
                                        priors_col=NULL,
                                        
                                        verbose=TRUE,
                                        nThread=1,
                                        conda_env="echoR_mini"){
    sub.out <- echoLD::subset_common_snps(LD_matrix=LD_matrix,
                                          dat=dat,
                                          verbose  = FALSE)
    LD_matrix <- sub.out$LD
    dat <- sub.out$DT
    #### Remove existing fine-mapping cols to avoid duplicates ####
    dat <- drop_finemap_cols(dat = dat,
                             finemap_methods = finemap_method, 
                             verbose = verbose)
    #### Check methods ####
    all_methods <- required_cols()$method
    # INITIATE FINE-MAPPING
    if(finemap_method=="SUSIE"){
        #### method: SUSIE ####
        dat <- SUSIE(dat = dat,
                     dataset_type = dataset_type,
                     LD_matrix = LD_matrix,
                     max_causal = n_causal,
                     compute_n = compute_n,
                     credset_thresh = credset_thresh,
                     verbose = verbose)
        
    } else if(finemap_method=="POLYFUN_SUSIE"){
        #### PolyFun+SUSIE ####
        dat <- POLYFUN(locus_dir = locus_dir,
                       dat = dat,
                       LD_matrix = LD_matrix,
                       dataset_type = dataset_type,
                       method = "SUSIE",
                       max_causal = n_causal,
                       compute_n = compute_n,
                       mode = "precomputed", #"non-parametric",
                       credset_thresh = credset_thresh,
                       force_new = force_new_finemap,
                       conda_env = conda_env,
                       verbose = verbose)
        
    } else if(finemap_method=="POLYFUN_FINEMAP"){
        #### PolyFun+SUSIE ####
        dat <- POLYFUN(locus_dir = locus_dir,
                       dat = dat,
                       LD_matrix = LD_matrix,
                       dataset_type = dataset_type,
                       method = "FINEMAP",
                       max_causal = n_causal,
                       compute_n = compute_n, 
                       mode = "precomputed", #"non-parametric",
                       credset_thresh = credset_thresh,
                       force_new = force_new_finemap,
                       conda_env = conda_env, 
                       verbose = verbose)
        
    } else if(finemap_method=="ABF"){
        #### ABF ####
        dat <- ABF(dat = dat,
                   credset_thresh = credset_thresh,
                   compute_n = compute_n,
                   case_control = case_control,
                   verbose = verbose)
        
        
    } else if(finemap_method=="FINEMAP"){
        #### FINEMAP ####
        dat <- FINEMAP(dat = dat,
                       locus_dir = locus_dir,
                       LD_matrix = LD_matrix,
                       compute_n = compute_n,  
                       n_causal = n_causal,
                       credset_thresh = credset_thresh,
                       args_list = if("FINEMAP" %in% names(finemap_args)) {
                           finemap_args[["FINEMAP"]]
                       } else {NULL}, 
                       force_new = force_new_finemap,
                       nThread=nThread,
                       verbose=verbose
                       )
        
        
    } else if("COJO_stepwise" %in% finemap_method){
        #### COJO_stepwise #### 
        dat <- COJO(dat = dat,
                    locus_dir = locus_dir,
                    fullSS_path = fullSS_path,
                    conditioned_snps = conditioned_snps,
                    run_stepwise = TRUE,
                    run_conditional = FALSE,
                    run_joint = FALSE,
                    full_genome = TRUE,
                    credset_thresh = credset_thresh,
                    verbose = verbose)
        
    } else if("COJO_conditional" %in% finemap_method){
        #### COJO_conditional #### 
        dat <- COJO(dat = dat,
                    locus_dir = locus_dir,
                    fullSS_path = fullSS_path,
                    conditioned_snps = conditioned_snps,
                    run_stepwise = FALSE,
                    run_conditional = TRUE,
                    run_joint = FALSE,
                    full_genome = FALSE,
                    credset_thresh = credset_thresh,
                    verbose = verbose)
        
    } else if("COJO_joint" %in% finemap_method){
        #### COJO_conditional #### 
        dat <- COJO(dat = dat,
                    locus_dir = locus_dir,
                    fullSS_path = fullSS_path,
                    conditioned_snps = conditioned_snps,
                    run_stepwise = FALSE,
                    run_conditional = FALSE,
                    run_joint = TRUE,
                    full_genome = TRUE,
                    credset_thresh = credset_thresh,
                    verbose = verbose)
        
    } else if("PAINTOR" %in% finemap_method) {
        #### PAINTOR ####
        dat <- PAINTOR(dat=dat,
                       GWAS_datasets=ifelse(dataset_type=="GWAS",
                                            basename(dirname(locus_dir)),NULL),
                       QTL_datasets=NULL,
                       locus=basename(locus_dir),
                       n_causal=n_causal,
                       use_annotations=FALSE,
                       credset_thresh=credset_thresh,
                       GWAS_populations="EUR",
                       LD_matrix=LD_matrix,
                       force_new_LD=FALSE)
    } else {
        stp <- paste(
            "finemap_methods must be one or more of:",
            paste("\n -",lfm(),collapse = "")
        )
        stop(stp)
    }
    return(dat)
}
