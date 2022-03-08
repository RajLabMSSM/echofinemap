test_that("POLYFUN_SUSIE works", {
    
    locus_dir <- file.path(tempdir(),echodata::locus_dir)
    dat <- echodata::BST1 
    LD_matrix <- echodata::BST1_LD_matrix
    dat2 <- echofinemap::POLYFUN_SUSIE(locus_dir=locus_dir,
                                       dat=dat,
                                       LD_matrix = LD_matrix)
    testthat::expect_equal(nrow(LD_matrix), nrow(dat2))
    testthat::expect_true(!all(c("POLYFUN_h2","CS","PP") %in% colnames(dat)))
    testthat::expect_true(all(c("POLYFUN_h2","CS","PP") %in% colnames(dat2)))
    cs <- subset(dat2, PP>.95)
    testthat::expect_equal(nrow(cs),5)
    testthat::expect_equal(nrow(cs),5)
    testthat::expect_equal(cs[cs$CS==min(cs$CS),]$SNP,"rs10003136")
})
