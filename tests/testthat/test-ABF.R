test_that("ABF works", {
  
    dat <- echodata::LRRK2    
    dat2 <- echofinemap::ABF(dat=dat)
    testthat::expect_equal(nrow(dat2), nrow(dat))
    testthat::expect_true(!all(c("PP","CS") %in% colnames(dat)))
    testthat::expect_true(all(c("PP","CS") %in% colnames(dat2)))
    testthat::expect_true(sum(dat2$CS)==1)
})
