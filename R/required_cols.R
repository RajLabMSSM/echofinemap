#' Table of required columns
#' 
#' Return a table with the minimum columns required
#' to run each fine-mapping method, as well as suggested columns.
#' @param dataset_type Dataset type ("GWAS" or "QTL"). 
#' @param for_all Columns required for all methods.
#' @param add_versions Add software versions for each method.
#' @param add_sources Add source code URLs for each method.
#' @param add_citations Add citations for each method.
#' @param add_executables Add path to executables for each method.
#' @param verbose Print messages.
#' @export
#' @importFrom data.table data.table setkey
#' @examples 
#' d <- required_cols(add_versions=TRUE, add_executables=TRUE)
required_cols <- function(dataset_type = "GWAS",
                          for_all = c("SNP","CHR","POS","Effect","StdErr"),
                          add_versions = FALSE,
                          add_sources = TRUE,
                          add_citations = TRUE,
                          add_executables = FALSE,
                          verbose = TRUE){
    
    #### Add required cols ####
    required_dict <- list(ABF=c(for_all,
                                "N","MAF",
                                if(dataset_type=="GWAS") 
                                    "proportion_cases" else NULL),
                          FINEMAP=c(for_all),
                          SUSIE=c(for_all),
                          POLYFUN_SUSIE=c(for_all,"P","A1","A2"),
                          POLYFUN_FINEMAP=c(for_all,"P","A1","A2"),
                          # COLOC=c(for_all),
                          PAINTOR=c(for_all,"ZSCORE"),
                          COJO_stepwise=c(for_all,"A1","A2"),
                          COJO_conditional=c(for_all,"A1","A2"),
                          COJO_joint=c(for_all,"A1","A2"))
    #### Add suggested cols ####
    suggested_dict <- list(ABF=NULL,
                           FINEMAP=c("A1","A2","MAF","N"),
                           SUSIE=c("N"),
                           POLYFUN_SUSIE=c("MAF","N"),
                           POLYFUN_FINEMAP=c("MAF","N"),
                           PAINTOR=c("MAF"),
                           COJO_stepwise=c("Freq","P","N"),
                           COJO_conditional=c("Freq","P","N"),
                           COJO_joint=c("Freq","P","N")) # check these 
   
    d <- data.table::data.table(method=names(required_dict),
                                required=required_dict)
    d$suggested <- suggested_dict[d$method]
    #### Extract software versions ####
    if(isTRUE(add_versions)){
        messager("Gathering method versions.",v=verbose)
        polyfun_version <- POLYFUN_check_version(verbose = FALSE)
        cojo_version <- COJO_check_version(verbose = FALSE)
        versions_dict <- list(ABF=packageVersion("coloc"),
                              FINEMAP=FINEMAP_check_version(verbose = FALSE),
                              SUSIE=packageVersion("susieR"),
                              POLYFUN_SUSIE=polyfun_version,
                              POLYFUN_FINEMAP=polyfun_version,
                              PAINTOR=PAINTOR_check_version(verbose = FALSE),
                              COJO_stepwise=cojo_version,
                              COJO_conditional=cojo_version,
                              COJO_joint=cojo_version) 
        d$version <- unlist(lapply(versions_dict[d$method],as.character))
    } 
    #### Add GitHub repos ####
    if(isTRUE(add_sources)){
        messager("Gathering method sources.",v=verbose)
        source_dict <- list(ABF="https://github.com/chr1swallace/coloc",
                            FINEMAP="http://www.christianbenner.com/",
                            SUSIE="https://github.com/stephenslab/susieR",
                            POLYFUN_SUSIE="https://github.com/omerwe/polyfun",
                            POLYFUN_FINEMAP="https://github.com/omerwe/polyfun",
                            PAINTOR="https://github.com/gkichaev/PAINTOR_V3.0",
                            COJO_stepwise="https://github.com/jianyangqt/gcta",
                            COJO_conditional="https://github.com/jianyangqt/gcta",
                            COJO_joint="https://github.com/jianyangqt/gcta")  
        d$source <- source_dict[d$method]
    }
    #### Extract executable paths ####
    if(isTRUE(add_executables)){
        messager("Gathering method executables.",v=verbose)
        finemap <- FINEMAP_find_executable(verbose = FALSE)
        polyfun <- POLYFUN_find_folder()
        paintor <- PAINTOR_find_executable()
        cojo <- COJO_find_executable(verbose = FALSE) 
        exec_dict <- list(ABF="`library(coloc)`",
                          FINEMAP=finemap,
                          SUSIE="`library(susieR)`",
                          POLYFUN_SUSIE=polyfun,
                          POLYFUN_FINEMAP=polyfun,
                          PAINTOR=paintor,
                          COJO_stepwise=cojo,
                          COJO_conditional=cojo,
                          COJO_joint=cojo) 
        d$executable <- unlist(lapply(exec_dict[d$method],as.character))
    } 
    #### Add citations ####
    if(isTRUE(add_citations)){
        messager("Gathering method citations.",v=verbose)
        citations_dict <- list(
            ABF="https://doi.org/10.1086%2F519024",
            FINEMAP="https://doi.org/10.1093%2Fbioinformatics%2Fbtw018",
            SUSIE="https://doi.org/10.1371/journal.pgen.1010299",
            POLYFUN_SUSIE="https://doi.org/10.1038/s41588-022-01036-9",
            POLYFUN_FINEMAP="https://doi.org/10.1038/s41588-022-01036-9",
            PAINTOR="https://doi.org/10.1093/bioinformatics/btw615",
            COJO_stepwise="https://doi.org/10.1038/ng.2213",
            COJO_conditional="https://doi.org/10.1038/ng.2213",
            COJO_joint="https://doi.org/10.1038/ng.2213") 
        d$citation <- as.character(citations_dict[d$method])
    }
    data.table::setkey(d, "method")
    return(d)
}
