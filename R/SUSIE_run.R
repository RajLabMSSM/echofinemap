#' Run \pkg{susieR} fine-mapping
#' 
#' Finds the proper function to call from \pkg{susieR}
#'  (which can depend on which version is installed) and conducts 
#'  statistical or functional fine-mapping using GWAS/QTL summary stats.
#'  
#' @keywords internal
SUSIE_run <- function(dat,
                      LD_matrix,
                      sample_size,
                      max_causal,
                      scaled_prior_variance,
                      estimate_prior_variance,
                      residual_variance,
                      max_iter,
                      estimate_prior_method,
                      estimate_residual_variance,
                      prior_weights,
                      credset_thresh,
                      plot_track_fit,
                      verbose=TRUE){
    #### Get the proper susie function ####
    susie_func <- SUSIE_get_func(verbose = verbose)
    
    fitted_bhat <- susie_func( 
        bhat = dat$Effect,
        shat = dat$StdErr,
        maf = if("MAF" %in% colnames(dat)) dat$MAF else NULL,
        R = base::data.matrix(LD_matrix),
        n = sample_size, # Number of samples/individuals in the dataset
        L = max_causal, # maximum number of non-zero effects
        ## NOTE: setting L == 1 has a strong tendency to simply return the 
        # SNP with the largest effect size.
        # 0.1: Equates to "proportion of variance explained"
        scaled_prior_variance = scaled_prior_variance, 
        estimate_prior_variance = estimate_prior_variance,
        residual_variance = residual_variance,
        # Raising max_iter can help susie converge
        max_iter = max_iter,
        ### Correspondence with Omer Weissbrod (7/28/2020):
        ## The value of var_y also shouldn't make a big difference 
        ## if estimate_residual_variance=TRUE,
        ## because it just sets the initial value of the optimization algorithm. 
        ## However, if estimate_residual_variance=F it makes a big difference.
        ## I also found that I often get the error you mentioned if
        ## var_y is very small.
        ## It could be due to not supplying a good initial parameter value.
        ## I believe that you can change the optimization method to EM 
        ## and then you will get more robust convergence. 
        ## In any case, if the causal effects in your target locus is small, 
        ## var_y=1 to a first order approximation should give you pretty 
        ## robust results.
        estimate_prior_method = estimate_prior_method,
        # standardize = TRUE,
        estimate_residual_variance = estimate_residual_variance, # TRUE
        #### IMPORTANT!! susieR uses the missing() function,
        ## which means supplying var_y=NULL will give you errors!!!
        ## When var_y is missing, it will be calculated automatically.
        ### Correspondence with Omer Weissbrod (7/28/2020):
        ## The value of var_y also shouldn't make a big difference if
        ## estimate_residual_variance=TRUE,
        ## because it just sets the initial value of the optimization algorithm.
        ## However, if estimate_residual_variance=F it makes a big difference.
        ## I also found that I often get the error you mentioned 
        ## ("Estimating residual variance failed: the estimated value
        ##  is negative") 
        ## if var_y is very small.
        ## It could be due to not supplying a good initial parameter value.
        ## I believe that you can change the optimization method to EM and 
        ## then you will get more robust convergence.
        ## In any case, if the causal effects in your target locus is small, 
        ## var_y=1 to a first order approximation should give you pretty
        ## robust results.
        ## var_y = var_y, # Variance of the phenotype 
        ### (e.g. gene expression, or disease status)
        
        # A p vector of prior probability that each element is non-zero
        prior_weights = prior_weights,
        coverage = credset_thresh,
        track_fit = plot_track_fit,
        
        verbose  = FALSE)
    return(fitted_bhat)
}
