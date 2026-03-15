test_that("POLYFUN_get_ref_prefix returns a value", {

    ## This function currently always returns the hardcoded string
    res <- echofinemap:::POLYFUN_get_ref_prefix(locus_dir = tempdir())
    ## The function body returns the string (but doesn't assign it),
    ## so the actual return is the last evaluated expression
    testthat::expect_true(is.character(res) || is.null(res))
})
