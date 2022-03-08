#' @family finemapping functions
#' @keywords internal
multifinemap_handler_method <- function(dat,
                                        locus_dir,
                                        fullSS_path,
                                        finemap_method="SUSIE",
                                        finemap_args=NULL,
                                        
                                        dataset_type="GWAS",
                                        force_new_finemap=TRUE,
                                        LD_matrix=NULL,
                                        n_causal=5,
                                        conditioned_snps,
                                        sample_size=NULL, 
                                        PAINTOR_QTL_datasets=NULL,
                                        PP_threshold=.95,
                                        case_control=TRUE,
                                        
                                        verbose=TRUE,
                                        nThread=1,
                                        conda_env="echoR"){
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
        #### SUSIE ####
        dat <- SUSIE(dat = dat,
                     dataset_type = dataset_type,
                     LD_matrix = LD_matrix,
                     max_causal = n_causal,
                     sample_size = sample_size,
                     PP_threshold = PP_threshold,
                     verbose = verbose)
        
    } else if(finemap_method=="POLYFUN_SUSIE"){
        #### PolyFun+SUSIE ####
        dat <- POLYFUN_SUSIE(locus_dir = locus_dir,
                                     dat = dat,
                                     LD_matrix = LD_matrix,
                                     dataset_type = dataset_type,
                                     max_causal = n_causal,
                                     sample_size = sample_size,
                                     mode = "precomputed",
                                     # mode = "non-parametric",
                                     PP_threshold = PP_threshold,
                                     conda_env = conda_env)
        
    }else if(finemap_method=="ABF"){
        #### ABF ####
        dat <- ABF(dat = dat,
                   PP_threshold = PP_threshold,
                   sample_size = sample_size,
                   case_control = case_control)
        
        
    } else if(finemap_method=="FINEMAP"){
        #### FINEMAP ####
        dat <- FINEMAP(dat = dat,
                       locus_dir = locus_dir,
                       LD_matrix = LD_matrix,
                       n_samples = sample_size,
                       force_new = force_new_finemap,
                       n_causal = n_causal,
                       credset_thresh = PP_threshold,
                       args_list = if("FINEMAP" %in% names(finemap_args)) {
                           finemap_args[["FINEMAP"]]
                       } else {NULL}
                       )
        
        
    } else if("COJO" %in% finemap_method){
        #### COJO ####
        conditioned_snps <- subset(dat, leadSNP==TRUE)$SNP
        dat <- COJO(dat = dat,
                    locus_dir = locus_dir,
                    fullSS_path = fullSS_path,
                    conditioned_snps = conditioned_snps,
                    conditional_analysis = TRUE,
                    stepwise_procedure = FALSE 
        )
        
    } else if("PAINTOR" %in% finemap_method) {
        #### PAINTOR ####
        dat <- PAINTOR(dat=dat,
                       GWAS_datasets=ifelse(dataset_type=="GWAS",
                                            basename(dirname(locus_dir)),NULL),
                       QTL_datasets=NULL,
                       locus=basename(locus_dir),
                       n_causal=n_causal,
                       use_annotations=FALSE,
                       PP_threshold=PP_threshold,
                       GWAS_populations="EUR",
                       LD_matrix=LD_matrix,
                       force_new_LD=FALSE)
    } else {
        stp <- paste(
            "finemap_methods must be one or more of:\n",
            paste()
        )
        stop()
    }
    return(dat)
}
