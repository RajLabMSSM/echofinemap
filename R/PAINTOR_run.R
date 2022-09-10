#' Run PAINTOR
#'
#'  @keywords internal
PAINTOR_run <- function(paintor_path=NULL,
                        PT_results_path,
                        inputFile_path,
                        ld_paths,
                        zscore_cols,
                        max_causal,
                        Gname = "Enrichment.Estimates.txt",
                        RESname = "results.txt",
                        ANname = "annotations.txt",
                        method=c("mcmc","enumerate"),
                        set_seed = 2022,
                        auto_restart = FALSE,
                        verbose = TRUE){
    
    messager("+ PAINTOR:: Running PAINTOR",v=verbose) 
    method <- tolower(method)[1] 
    paintor_path <- PAINTOR_find_folder(paintor_path=paintor_path)  
    Zhead <- paste(zscore_cols, collapse=",")
    .ld_suffixes <- stringr::str_split(names(ld_paths),"\\.",
                                       simplify = TRUE)[,2]
    LDname <- paste(.ld_suffixes, collapse=",")
    PT.start <- Sys.time()  
    ## RUN
    # https://github.com/gkichaev/PAINTOR_V3.0/wiki/3.-Running-Software-and-Suggested-Pipeline
    cmd <- paste(
        file.path(paintor_path,"PAINTOR"), 
        #### REQUIRED ####
        # (required) Filename of the input file containing the
        ## list of the fine-mapping loci [default: N/A]
        "-input",inputFile_path, 
        #  (required) The name(s) of the Zscore column
        ## in the header of the locus file (comma separated) [default: N/A]
        "-Zhead",Zhead, 
        # (required) Suffix(es) for LD files. Must match the order of
        ## Z-scores in which the -Zhead flag is specified (comma separated) [Default:N/A]
        "-LDname",LDname, 
        # -enumerate:
        # specify this flag if you want to enumerate all possible configurations
        ## followed by the max number of causal SNPs (eg. -enumerate 3 considers
        ## up to 3 causals at each locus) [Default: not specified]
        # "-mcmc":
        #  should the algorithm be run with MCMC? [Default: not specified]
        if(method=="enumerate"){paste("-enumerate",max_causal)}else{"-mcmc"},
        
        #	specify the number of causals to pre-compute enrichments with [default: 2]
        "-max_causal",max_causal,
        
        #### OPTIONAL ####
        # The names of the annotations to include in model (comma separated)
        ## [default: N/A]
        # "-annotations",paste(basename(bed),collapse=","),
        
        # Input directory with all run files [default: ./ ]
        "-in",PT_results_path,
        
        #  Output directory where output will be written [default: ./ ]
        "-out",PT_results_path,
        
        # Output Filename for enrichment estimates [default: Enrichment.Estimate]
        "-Gname",basename(Gname),
        
        # Suffix for ouput files of results [Default: results]
        "-RESname",basename(RESname),
        
        # Suffix for annotation files [Default: annotations]
        "-ANname",basename(ANname),
        
        "-set_seed",set_seed
    )
    echoconda::cmd_print(cmd, basepath = FALSE)
    system(cmd)
    PT.end <- Sys.time()
    messager("+ PAINTOR:: Completed fine-mapping in",
             round((PT.end-PT.start)/60, 2),"minutes.")
    
    # Check the time it took to see if it didn't actually run.
    ## Re-enter command until it does.
    if(isTRUE(auto_restart) &&
       PT.end-PT.start<1){
        res_paths <- PAINTOR_run(paintor_path = paintor_path,
                                       PT_results_path = PT_results_path,
                                       inputFile_path = inputFile_path,
                                       ld_paths = ld_paths,
                                       zscore_cols = zscore_cols, 
                                       Gname = Gname,
                                       RESname = RESname,
                                       ANname = ANname,
                                       method = method,
                                       max_causal = max_causal,
                                       set_seed = set_seed,
                                       verbose = verbose)
        return(res_paths)
    }
    #### Return #####
    res_paths <- list(cmd=cmd, 
                     input=inputFile_path,
                     "in"=PT_results_path,
                     out=PT_results_path,
                     Zhead=Zhead,
                     LDname=LDname,
                     Gname = list.files(PT_results_path,
                                        basename(Gname), 
                                        full.names = TRUE),
                     RESname = grep("LogFile\\.",
                                    list.files(
                                        PT_results_path,
                                        basename(RESname), 
                                        full.names = TRUE),
                                    invert = TRUE, 
                                    value = TRUE
                     ),
                     ANname = list.files(PT_results_path,
                                         basename(ANname),
                                         full.names = TRUE), 
                     LogFile = grep("LogFile\\.",
                                    list.files(PT_results_path,
                                               basename(RESname), 
                                               full.names = TRUE),
                                    value = TRUE
                     ),
                     max_causal=max_causal,
                     method=method,
                     set_seed=set_seed
                     )
    names(res_paths$RESname) <- stringr::str_split(
        basename(res_paths$RESname),"\\.", simplify =  TRUE)[,1]
    return(res_paths)
}
