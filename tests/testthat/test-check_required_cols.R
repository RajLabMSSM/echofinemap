test_that("check_required_cols works", {

    d <- echofinemap::required_cols()
    all_methods <- d$method
    dat <- echodata::BST1

    finemap_methods <- echofinemap::check_required_cols(dat=dat)
    ## Returned methods should be a non-empty subset of all methods
    testthat::expect_true(length(finemap_methods) > 0)
    testthat::expect_true(all(finemap_methods %in% all_methods))
    ## Each returned method should have all its required columns present
    for(m in finemap_methods){
        req <- d[m,]$required[[1]]
        testthat::expect_true(all(req %in% colnames(dat)),
                              info = paste("Method", m, "missing required cols"))
    }
    ## Methods NOT returned should be missing at least one required column
    excluded <- setdiff(all_methods, finemap_methods)
    for(m in excluded){
        req <- d[m,]$required[[1]]
        testthat::expect_false(all(req %in% colnames(dat)),
                               info = paste("Method", m,
                                            "has all cols but was excluded"))
    }
})
