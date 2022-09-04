#' Run PAINTOR
#'
#'  @keywords internal
PAINTOR_run <- function(paintor_path=NULL,
                        PT_results_path,
                        ld_paths,
                        n_datasets,
                        method=c("mcmc","enumerate"),
                        n_causal = 5,
                        seed = 2019,
                        verbose = TRUE){
    
    messager("+ PAINTOR:: Running PAINTOR",v=verbose) 
    method <- tolower(method)[1] 
    paintor_path <- PAINTOR_find_folder(paintor_path=paintor_path)
    PT.start <- Sys.time() 
    .ld_suffixes <- names(ld_paths)
    ## RUN
    # https://github.com/gkichaev/PAINTOR_V3.0/wiki/3.-Running-Software-and-Suggested-Pipeline
    cmd <- paste(
        file.path(paintor_path,"PAINTOR"),
        
        #### REQUIRED ####
        # (required) Filename of the input file containing the
        ## list of the fine-mapping loci [default: N/A]
        "-input",file.path(PT_results_path,"input.files"),
        
        #  (required) The name(s) of the Zscore column
        ## in the header of the locus file (comma separated) [default: N/A]
        "-Zhead", paste(paste0("ZSCORE.",seq_len(n_datasets)), collapse=","),
        
        # (required) Suffix(es) for LD files. Must match the order of
        ## Z-scores in which the -Zhead flag is specified (comma separated) [Default:N/A]
        "-LDname", paste(.ld_suffixes, collapse=","),
        
        # specify this flag if you want to enumerate all possible configurations
        ## followed by the max number of causal SNPs (eg. -enumerate 3 considers
        ## up to 3 causals at each locus) [Default: not specified]
        if(method=="enumerate"){paste("-enumerate",n_causal)}else{"-mcmc"},
        #  should the algorithm be run with MCMC? [Default: not specified]
        # "-mcmc",
        
        #	specify the number of causals to pre-compute enrichments with [default: 2]
        "-max_causal",n_causal,
        
        #### OPTIONAL ####
        # The names of the annotations to include in model (comma separated)
        ## [default: N/A]
        # "-annotations",paste(basename(bed),collapse=","),
        
        # Input directory with all run files [default: ./ ]
        "-in", paste0(PT_results_path),
        
        #  Output directory where output will be written [default: ./ ]
        "-out",file.path(PT_results_path),
        
        # Output Filename for enrichment estimates [default: Enrichment.Estimate]
        "-Gname","Enrichment.Estimates.txt",
        
        # Suffix for ouput files of results [Default: results]
        "-RESname","results.txt",
        
        # Suffix for annotation files [Default: annotations]
        "-ANname","annotations.txt",
        
        "-set_seed",seed
    )
    echoconda::cmd_print(cmd)
    system(cmd)
    # }
    # EXAMPLE
    # cmd <- paste("cd",paintor_path,"&& ./PAINTOR -input SampleData/input.files -in SampleData/ -out SampleData/ -Zhead Zscore -LDname ld -enumerate 2 -annotations DHS")
    # system(cmd)
    PT.end <- Sys.time()
    messager("PAINTOR:: Completed fine-mapping in",round((PT.end-PT.start)/60, 2),"minutes.")
    
    # Check the time it took to see if it didn't actually run.
    ## Re-enter command until it does.
    if(PT.end-PT.start<1){
        PAINTOR_run(paintor_path,
                    PT_results_path,
                    .LD_file.paths,
                    n_datasets,
                    method,
                    n_causal)
    }
}
