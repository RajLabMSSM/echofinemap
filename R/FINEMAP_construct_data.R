#' Prepare input files for \code{FINEMAP}
#'
#' Creates and saves 1) the summary stats file, and 2) the LD matrix.
#' "Columns beta and se are required for fine-mapping.
#' Column maf is needed to output posterior effect size estimates on the
#' allelic scale. All other columns are not required for computations and
#' can be specified arbitrarily."
#' @family FINEMAP
#' @keywords internal
#' @source
#' \url{http://www.christianbenner.com}
#' @source
#' \code{
#' locus_dir <- echodata::locus_dir;
#' dat <- echodata::BST1; 
#' LD_matrix <- echodata::BST1_LD_matrix
#' dir.create(file.path(locus_dir,"FINEMAP"), 
#'            showWarnings = FALSE, recursive = TRUE)
#' out <- echoLD::subset_common_snps(LD_matrix=LD_matrix, dat=dat)
#' LD_matrix <- out$LD
#' dat <- out$DT
#' dat_paths <- echofinemap:::FINEMAP_construct_data(dat=dat, 
#'                                                   locus_dir=locus_dir, 
#'                                                   LD_matrix=LD_matrix)
#' } 
FINEMAP_construct_data <- function(dat,
                                   locus_dir, 
                                   LD_matrix,
                                   nThread=1,
                                   verbose=TRUE){
    SNP <- CHR <- POS <- A1 <- A2 <- MAF <- Effect <- StdErr <- NULL;
    ####### data.z #######
    if(!"A1" %in% colnames(dat)) {
        dat$A1 <- "A"; 
        messager("Optional A1 col missing.",
                 "Replacing with all 'A's.",v=verbose)
    }
    if(!"A2" %in% colnames(dat)) {
        dat$A2 <- "T";  
        messager("Optional A2 col missing.",
                 "Replacing with all 'T's.",v=verbose)
    }
    if(!"MAF" %in% colnames(dat)) {
        dat$MAF <- .1; 
        messager("Optional MAF col missing.",
                 "Replacing with all '.1's",v=verbose)
    };
    #### Construct files #####
    messager("Constructing data.z file.",v=verbose)
    data.z <- dat %>% dplyr::select(rsid=SNP,
                                    chromosome=CHR,
                                    position=POS,
                                    allele1=A1,
                                    allele2=A2,
                                    maf=MAF,
                                    beta=Effect, # *required
                                    se=StdErr # *required
    )
    data.z$flip <- 0 # [optional] - flip==1, don't flip==0
    
    #### !!! IMPORTANT !!! ####
    ## Trim whitespaces
    ## Extra whitespace causes problems when you try 
    ## to make space-delimited files
    # https://stackoverflow.com/questions/20760547/removing-whitespace-\
    ## from-a-whole-data-frame-in-r
    cols_to_be_rectified <- names(data.z)[
        vapply(data.z, is.character, logical(1))
    ]
    data.z <- data.z %>%
        dplyr::mutate_at(.vars = dplyr::all_of(cols_to_be_rectified),
                         .funs = trimws )
    
    ####### data.ld #######
    messager("Constructing data.ld file.",v=verbose)
    ## The order of the SNPs in the dataset.ld must correspond to
    ## the order of variants in dataset.z. 
    # Filter
    keep_i <- data.z$rsid %in% rownames(LD_matrix) 
    data.z <- data.z[keep_i,]
    ## This filters AND sorts LD_matrix by the order of rsids in data.z
    LD_filt <- LD_matrix[data.z$rsid, data.z$rsid]
    
    # Write files
    ## MUST be space-delimited
    # messager("Writing z and ld files...",v=verbose)
    if( dim(data.z)[1]==dim(LD_filt)[1] ){
        # data.z
        data.z_path <- file.path(locus_dir,"FINEMAP","data.z")
        data.table::fwrite(data.z, data.z_path, sep = " ",
                           nThread = nThread)
        # Sys.chmod(data.z_path, "777", use_umask = FALSE)
        # data.ld
        data.ld_path <- file.path(locus_dir,"FINEMAP","data.ld")
        data.table::fwrite(data.table:::as.data.table.matrix(LD_filt),
                           data.ld_path, sep=" ", 
                           quote = FALSE, col.names = FALSE,
                           nThread = nThread)
        # Sys.chmod(data.ld_path, "777", use_umask = FALSE)
    } else {
        messager(
            "WARNING: + FINEMAP:: Summary statistics file (data.z)",
            "and LD matrix (data.ld) must contain the same number of SNPs.",
            v=verbose)
    }
    #### Return ####
    return(list("data.z"=data.z,
                "data.z_path"=data.z_path,
                "data.ld"=LD_filt,
                "data.ld_path"=data.ld_path))
}
