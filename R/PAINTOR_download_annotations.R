#' Download annotations for PAINTOR
#'
#' @keywords internal
PAINTOR_download_annotations <- function(query_dat, 
                                         locus_dir,
                                         PT_results_path,
                                         annot_sample = FALSE,
                                         annot_xgr = NULL,
                                         annot_roadmap = NULL,
                                         chrom_states = NULL,
                                         use_annotations = TRUE,
                                         conda_env = "echoR_mini",
                                         nThread = 1,
                                         verbose = TRUE){
    ## removed arg until i figure out 
    ## how get programmatically get PAINTOR annotations.
    annot_paintor = FALSE;
    messager("++ PAINTOR:: Creating Annotation Matrix File.",v=verbose)
    .annotations_file <- file.path(PT_results_path, 
                                   paste0(basename(locus_dir),
                                          ".annotations.txt"))
    paths <- list()
    if(isFALSE(use_annotations)){
        messager("+++ PAINTOR:: no_annotations=TRUE.",
                 "Prior Probability set to 1 for all SNPs.")
        data.table::fwrite(x = data.frame(Default_Priors = rep(1,nrow(query_dat))),
                           file = .annotations_file,
                           sep=" ", 
                           quote  = FALSE)
        return(.annotations_file)
        
    } else{  
        #### PAINTOR: full data ####
        if(!is.null(annot_paintor)){
            messager("+++ Preparing annotations from PAINTOR server.",
                     v=verbose) 
            readline(prompt = "WARNING: The PAINTOR") 
            slct <- menu(choices = c("y","n"), 
                 title = paste(
                     "WARNING: The full PAINTOR annotations are large (6.7Gb).",
                     "Are you sure you wish to proceed with downloading them?"
                 ))
            if(slct==1){
                URL <- "https://dl2.boxcloud.com/d/1/b1!PTo8C3G0iE4E4CJTBwiWB26q6Big97XCVtMchJNpsk6v3IpN-H8SIsKLdmkkDWEC0UDVatrp32e_kJNblEN-Xxer0eHlMebIgz5nHlB9sQDGNzPYHPKb5p-t2AXCEDv4TqPF5-s46LW4JllBevs0iXZG7SZzqZia0kcbsbD1ITNCb68K15fkADnCo6nK1zn_5Na8RjE7yhIa5EYDdjV-dDqsmgynWQEQcjcYANahsAAtaMJMOx_aV4hN5UJCt8urrj3Yc2MN7qTGxPgmvPOQIAYZ4HFyySqfOGvyo-sIALEAyCTcJDzyNn6JPw-dGQE4xPi6Slk9tUN_DSOqyr86H1FatmMD62WGcm4hfKKTrul5vmb-80NIVSfYvRVinUebExceyZYazUqiomdX2-C4gQnJ8gOQNEdG5Q8aIopDaw1g0Kcvw8zxuqPCC7DLrf8Upy1KESbdjvry86idh2CbvQtoagqbVSx5SxAJlVKy8DcgN3VnTUiyFYG-ma3iLF0Blh2cKjL7fJTLo-Skaf2vsde802FOigmuz_2ptL3Jfm5LkrsI_Xr_lVkPstgU9rgtQ5vnksGTZCCR_63K_AMtvn6jZ9u0upNMAzUC3ZfxubAd2jlF45RMMzXjH0EETw6TgU3aFjv1rM7B5U1Cxm-zpbyUULrHEvP2Vf05Je2QOZg8K62gnzbq-a5wwDLn_my3Ss6cCI5ikVKiqQlgrP3aXBp4ekyOjHgwtJ3HWBaoMivLho-EqG6DI5rBW5U-vXHpeX2vkO8I22TveTOTLLn5Wdiw3hL_ADl2I_SGCveJvlwI7yfxsgv6vNChzr6DUVswwKbpaX9sTKqJry_TlAt0USwtODO_A9N9fUxo9BqoFGB0QSp__qBQOrCIYAD6LB7GeG2nPMYhR0iCpmOdgG_2vET5FMIMftr9XDgwVt1M34CVJ0ZPg_1HQ0ggudph9bsDhB0TunBcie_Nbwasc8upn8Oe734wzjXyDgxv88CSzcqX4C2Ov0Hsi3DA0HugcqLpMsTH4wPHS0oF76yP3I-nflUcypneAXzLrewclv4wjcNSEZwg2GXTsmy8SFrq2FI4oIF5CPL-9qTz_EcpCzIxZXTop12Dwp-97lN3x8IRDTQo67ZQP60bL0RbLeP0ByuA7M9M3mBmhFDoe5HwAHWUfmwHIZsOALdPfMFOJiVvrLYmyIGqae-ckYI5dJU9JvFGrX8twu00Ts-RgwEPlCfVPpPjELE0en4Qb-u6m8OnlFbKD51gTqguPk765sszLB-B1yajcBdk-U8kxHaRkXw1xA../download"
                URL <- "https://bit.ly/3D5Erbk"
                tmp <- tempfile()
                # utils::download.file(URL, tmp)
                annot_file <- downloadR::downloader(
                    input_url = URL,
                    download_method = "wget",
                    output_path = file.path(tempdir(),
                                            "Functional_Annotations.tar.gz"),
                    nThread = 1
                ) 
            }  
        }
        #### XGR ####
        if(!is.null(annot_xgr)){
            messager("+++ Preparing annotations via XGR.",v=verbose) 
            xgr.gr <- echoannot::XGR_download_and_standardize(
                lib.selections = annot_xgr, 
                as_grangesList = TRUE, 
                dat = query_dat,
                nThread = nThread)
            paths[["XGR"]] <- echodata::granges_to_bed(
                grlist = xgr.gr,
                save_dir = PT_results_path, 
                gzip = TRUE) 
        }
        #### ROADMAP ####
        if(!is.null(annot_roadmap)){
            messager("+++ Preparing annotations via ROADMAP API.",v=verbose) 
            # Gather roadmap annotations
            if(annot_roadmap=="all"){annot_roadmap <- ""} 
            paths[["ROADMAP"]] <- echoannot::ROADMAP_query(
                query_dat = query_dat,
                keyword_query = annot_roadmap,
                chrom_states = chrom_states,
                conda_env = conda_env,
                return_paths = TRUE,
                nThread = nThread, 
                verbose = verbose)
        }
    }
    return(paths)
}
