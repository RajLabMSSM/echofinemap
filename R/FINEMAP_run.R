FINEMAP_run <- function(locus_dir,
                        FINEMAP_path,
                        model=c("sss","cond"),
                        master_path,
                        n_causal=5,
                        prior_k=NULL,
                        args_list=list(),
                        nThread=1,
                        verbose=TRUE){
    
    messager("Running FINEMAP.",v=verbose)
    model <- tolower(model)[1]
    ## '--n-causal-snps' cannot be used 
    ## together with '--prior-k'!
    if(!is.null(prior_k)) n_causal <- NULL
    # FINEMAP_path <- echofinemap:::FINEMAP_find_executable(version = "1.3.1")
    # fm <- echoconda::import_cli(path = FINEMAP_path)
    cmd <- paste("cd",locus_dir,"&&",
                 FINEMAP_path,
                 paste0("--",model),
                 "--in-files", master_path,
                 "--log",
                 ### Argument only available in FINEMAP >=1.4
                 if(nThread>1) paste("--n-threads",nThread) else NULL,
                 # Option to set the maximum number of allowed causal SNPs
                 # (Default is 5)
                 if(!is.null(n_causal)) {
                     paste("--n-causal-snps",n_causal)
                 } else {NULL},
                 if(!is.null(prior_k)){
                     "--prior-k"
                 }else{NULL},
                 collapse_args(args_list)
    )
    echoconda::cmd_print(cmd, verbose=verbose)
    msg <- system(cmd, intern =  TRUE)
    return(msg)
}
