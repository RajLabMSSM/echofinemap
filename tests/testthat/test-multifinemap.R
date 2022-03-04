test_that("multifinemap works", {
  
    dat <- echofinemap::drop_finemap_cols(echodata::BST1)
    LD_matrix <- echodata::BST1_LD_matrix
    locus_dir <- file.path("~/Desktop",echodata::locus_dir)
    fullSS_path <- echodata::example_fullSS(dataset = "Nalls2019")
    finemap_methods <- c("ABF","SUSIE","FINEMAP")
    
    run_tests <- function(dat,
                          dat2,
                          finemap_methods,
                          cs_sizes=NULL,
                          pp_sizes=NULL){
        testthat::expect_gte(nrow(dat), nrow(dat2))
        testthat::expect_true(all(c(
            paste(finemap_methods,"CS",sep="."),
            paste(finemap_methods,"PP",sep="."),
            c("Support","Consensus_SNP")
        ) %in% colnames(dat2))
        )
        #### Test whether Credible Set sizes are as expected ####
        if(!is.null(cs_sizes)){
            cs_dict <- stats::setNames(cs_sizes,finemap_methods)
            for(m in finemap_methods){
                message("Testing number of SNPs in each CS: ",m)
                cs <- dat2[,(paste0(m,".CS")), with=FALSE]
                n_cs <- sum(cs>0, na.rm = TRUE)
                testthat::expect_equal(n_cs, cs_dict[[m]])
            }
        }
        #### Test whether SNPs with >0.95 PP are as expected ####
        if(!is.null(pp_sizes)){
            pp_dict <- stats::setNames(pp_sizes,finemap_methods)
            for(m in finemap_methods){
                message("Testing number of SNPs with PP>0.95: ",m)
                pp <- dat2[,(paste0(m,".PP")), with=FALSE]
                n_pp <- sum(pp>0.95, na.rm = TRUE)
                testthat::expect_equal(n_pp, pp_dict[[m]])
            }
        }
    }
    
    #### Round 0: one causal SNP####
    dat0 <- echofinemap::multifinemap(dat = dat,
                                      locus_dir = locus_dir, 
                                      LD_matrix = LD_matrix,
                                      fullSS_path = fullSS_path,
                                      sample_size = 100000,
                                      force_new_finemap = TRUE,
                                      n_causal = 1,
                                      finemap_methods = finemap_methods)
    run_tests(dat = dat,
              dat2 = dat0,
              finemap_methods = finemap_methods, 
              cs_sizes = c(1,1,1),
              pp_sizes = c(1,1,1))
    top_snp <- subset(dat0, Support==max(Support))
    testthat::expect_true(top_snp$Support==3)
    testthat::expect_true(top_snp$SNP=="rs4698412")
    testthat::expect_true(all.equal(top_snp$mean.PP, 0.9999993))
     
    #### Round 1 ####
    dat2 <- echofinemap::multifinemap(dat = dat,
                                     locus_dir = locus_dir, 
                                     LD_matrix = LD_matrix,
                                     fullSS_path = fullSS_path,
                                     sample_size = 100000,
                                     force_new_finemap = TRUE,
                                     finemap_methods = finemap_methods)
    run_tests(dat = dat,
              dat2 = dat2,
              finemap_methods = finemap_methods, 
              cs_sizes = c(1,5,5),
              pp_sizes = c(1,5,5))
    
    #### Round 2 ####
    dat3 <- echofinemap::multifinemap(dat = dat2,
                                      locus_dir = locus_dir,
                                      LD_matrix = LD_matrix,
                                      fullSS_path = fullSS_path,
                                      sample_size = 100000,
                                      n_causal = 10,
                                      finemap_methods = "SUSIE")
    run_tests(dat = dat, 
              dat2 = dat3,
              finemap_methods = finemap_methods, 
              cs_sizes = c(1,6,5),
              pp_sizes = c(1,6,5))
    
    
    #### Test LRRK2 #####
    dat <- echofinemap:: drop_finemap_cols(echodata::LRRK2)[seq_len(100),]
    locus_dir <- file.path(tempdir(),"LRR2K2")
    LD_list <- echoLD::load_or_create(dat = dat,
                                        locus_dir = locus_dir, 
                                        LD_reference = "1KGphase3")
    testthat::expect_equal(
        sum(grepl(paste(finemap_methods,collapse = "|"),colnames(dat))),0
      )
    dat4 <- echofinemap::multifinemap(dat = dat,
                                      locus_dir = file.path(tempdir(),"LRRK2"), 
                                      LD_matrix = LD_list$LD,
                                      fullSS_path = fullSS_path,
                                      sample_size = 100000,
                                      # force_new_finemap = TRUE,
                                      finemap_methods = finemap_methods)
    run_tests(dat = dat,
              dat2 = dat4,
              finemap_methods = finemap_methods, 
              cs_sizes = c(1,2,5),
              pp_sizes = c(1,2,5))

    #### Using an LD Panel from the wrong locus ####
    testthat::expect_error(
        echofinemap::multifinemap(dat = dat,
                                  locus_dir = file.path(tempdir(),"LRR2K2"), 
                                  LD_matrix = echodata::BST1_LD_matrix,
                                  fullSS_path = fullSS_path,
                                  sample_size = 100000,
                                  force_new_finemap = TRUE,
                                  finemap_methods = finemap_methods)
    ) 
})
