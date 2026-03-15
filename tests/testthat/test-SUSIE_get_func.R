test_that("SUSIE_get_func returns a function", {

    testthat::skip_if_not_installed("susieR")

    res <- echofinemap:::SUSIE_get_func(verbose = FALSE)
    testthat::expect_true(is.function(res))
})

test_that("SUSIE_get_func returns susie_rss in modern susieR", {

    testthat::skip_if_not_installed("susieR")

    ## Modern susieR should not have susie_bhat
    if (length(find("susie_bhat")) == 0) {
        res <- echofinemap:::SUSIE_get_func(verbose = FALSE)
        ## The function should exist in susieR namespace
        expected <- get("susie_rss", asNamespace("susieR"))
        testthat::expect_identical(res, expected)
    }
})
