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
                                        conditioned_snps=NULL,
                                        compute_n="ldsc",  
                                        credset_thresh=.95,
                                        case_control=TRUE,
                                        priors_col=NULL,
                                        seed = 2022,
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
                             verbose = FALSE)
    fma <- check_args(finemap_args = finemap_args,
                      finemap_method = finemap_method, 
                      verbose = verbose)
    # INITIATE FINE-MAPPING
    if(finemap_method=="SUSIE"){
        #### method: SUSIE #### 
        dat <- SUSIE(
                    dat = dat, 
                    LD_matrix = LD_matrix,
                    max_causal = n_causal,
                    compute_n = compute_n,
                    case_control = case_control,
                    priors_col = fma$priors_col,
                    rescale_priors = fma$rescale_priors,
                    scaled_prior_variance = fma$scaled_prior_variance,
                    estimate_residual_variance = fma$estimate_residual_variance,
                    estimate_prior_variance = fma$estimate_prior_variance, 
                    residual_variance = fma$residual_variance, 
                    estimate_prior_method = fma$estimate_prior_method,
                    max_iter = fma$max_iter,
                    var_y = fma$var_y,
                    plot_track_fit = fma$plot_track_fit,
                    return_all_CS = fma$return_all_CS, 
                    file_prefix = fma$file_prefix,  
                    credset_thresh = credset_thresh,
                    verbose = verbose)
        
    } else if(finemap_method=="POLYFUN_SUSIE"){
        #### PolyFun+SUSIE ####
        dat <- POLYFUN(locus_dir = locus_dir,
                       dat = dat,
                       LD_matrix = LD_matrix, 
                       method = "SUSIE",
                       case_control = case_control,
                       max_causal = n_causal,
                       compute_n = compute_n,
                       polyfun = fma$polyfun,
                       mode = fma$mode,
                       credset_thresh = credset_thresh,
                       force_new = force_new_finemap,
                       nThread = nThread,
                       conda_env = conda_env,
                       verbose = verbose)
        
    } else if(finemap_method=="POLYFUN_FINEMAP"){
        #### PolyFun+SUSIE ####
        dat <- POLYFUN(locus_dir = locus_dir,
                       dat = dat,
                       LD_matrix = LD_matrix, 
                       method = "FINEMAP",
                       case_control = case_control,
                       max_causal = n_causal,
                       compute_n = compute_n, 
                       polyfun = fma$polyfun,
                       mode = fma$mode,
                       credset_thresh = credset_thresh,
                       force_new = force_new_finemap,
                       nThread = nThread,
                       conda_env = conda_env, 
                       verbose = verbose)
        
    } else if(finemap_method=="ABF"){
        #### ABF ####
        dat <- ABF(dat = dat,
                   credset_thresh = credset_thresh,
                   compute_n = compute_n,
                   case_control = case_control,
                   sdY = fma$sdY,
                   verbose = verbose) 
        
    } else if(finemap_method=="FINEMAP"){
        #### FINEMAP ####
        dat <- FINEMAP(dat = dat,
                       locus_dir = locus_dir,
                       LD_matrix = LD_matrix,
                       compute_n = compute_n,  
                       n_causal = n_causal,
                       credset_thresh = credset_thresh,
                       finemap_version = fma$finemap_version,
                       model = fma$model,
                       remove_tmps = fma$remove_tmps,
                       FINEMAP_path = fma$FINEMAP_path,
                       args_list = fma$args_list, 
                       priors_col = priors_col,
                       force_new = force_new_finemap,
                       nThread=nThread,
                       verbose=verbose)
        
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
                    compute_n = compute_n,
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
                    compute_n = compute_n,
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
                    compute_n = compute_n,
                    credset_thresh = credset_thresh,
                    verbose = verbose)
        
    } else if("PAINTOR" %in% finemap_method) {
        #### PAINTOR ####
        dat <- PAINTOR(dat = dat,
                       locus_dir = locus_dir,
                       LD_reference = fma$LD_reference,
                       LD_matrix = LD_matrix,
                       superpopulation = fma$superpopulation,
                       annot_roadmap = fma$annot_roadmap,
                       annot_xgr = fma$annot_xgr,
                       max_causal = n_causal,
                       credset_thresh = credset_thresh,  
                       annot = fma$annot,
                       use_annotations = fma$use_annotations,
                       method = fma$method,
                       conda_env = conda_env,
                       seed = seed,
                       nThread = nThread,
                       verbose = verbose)
    } else {
        stp <- paste(
            "finemap_methods must be one or more of:",
            paste("\n -",lfm(),collapse = "")
        )
        stop(stp)
    }
    return(dat)
}
