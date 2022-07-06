test_that("POLYFUN_SUSIE works", {
    
    set.seed(1234)
    locus_dir <- file.path(tempdir(),echodata::locus_dir)
    dat <- echodata::BST1 
    LD_matrix <- echodata::BST1_LD_matrix
    
    run_tests <- function(datx,
                          daty,
                          n_causal,
                          cs_snps){
        testthat::expect_equal(nrow(LD_matrix), nrow(daty))
        testthat::expect_true(!all(c("POLYFUN.h2","CS","PP") %in% colnames(datx)))
        testthat::expect_true(all(c("POLYFUN.h2","CS","PP") %in% colnames(daty)))
        cs <- subset(daty, PP>.95)
        testthat::expect_equal(nrow(cs),n_causal) 
        testthat::expect_equal(cs[cs$CS==min(cs$CS),]$SNP,cs_snps)
    }
    
    #### POLYFUN: SUSIE #####
    dat2 <- echofinemap::POLYFUN(locus_dir=locus_dir,
                               dat=dat,
                               LD_matrix = LD_matrix, 
                               method = "SUSIE")
    run_tests(datx=dat,
              daty=dat2,
              n_causal=5,
              cs_snps="rs10003136") 
    
    #### POLYFUN: FINEMAP #####
    dat3 <- echofinemap::POLYFUN(locus_dir=locus_dir,
                                 dat=dat,
                                 LD_matrix = LD_matrix, 
                                 method = "FINEMAP", 
                                 force_new = TRUE)
    run_tests(datx=dat,
              daty=dat3,
              n_causal=5,
              cs_snps=c("rs10007824","rs10008644",
                        "rs4698412","rs6852450","rs10001565" ))
})
