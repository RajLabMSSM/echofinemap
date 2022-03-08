FINEMAP_run <- function(locus_dir,
                        FINEMAP_path,
                        model=c("sss","cond"),
                        master_path,
                        n_causal=5,
                        args_list=list(),
                        verbose=TRUE){
    model <- tolower(model)[1]
    cmd <- paste("cd",locus_dir,"&&",
                 FINEMAP_path,
                 paste0("--",model),
                 "--in-files", master_path,
                 "--log",
                 # Option to set the maximum number of allowed causal SNPs
                 # (Default is 5)
                 "--n-causal-snps",n_causal,
                 collapse_args(args_list)
    )
    cmd_print(cmd, v=verbose)
    msg <- system(cmd, intern =  TRUE)
    return(msg)
}
