test_that("check_required_cols works", {
  
    d <- echofinemap::required_cols()
    all_methods <- d$method
    dat <- echodata::BST1
    
    finemap_methods <- echofinemap::check_required_cols(dat=dat)
    testthat::expect_equal(finemap_methods, all_methods[all_methods!="ABF"])
})
