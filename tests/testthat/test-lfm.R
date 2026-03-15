test_that("lfm returns all methods by default", {

    methods <- echofinemap::lfm(verbose = FALSE)
    testthat::expect_true(length(methods) >= 9)
    testthat::expect_true("ABF" %in% methods)
    testthat::expect_true("SUSIE" %in% methods)
    testthat::expect_true("FINEMAP" %in% methods)
    testthat::expect_true("PAINTOR" %in% methods)
    testthat::expect_true("POLYFUN_SUSIE" %in% methods)
})

test_that("lfm filters to requested methods", {

    methods <- echofinemap::lfm(
        finemap_methods = c("ABF", "SUSIE"),
        verbose = FALSE
    )
    testthat::expect_equal(methods, c("ABF", "SUSIE"))
})

test_that("lfm silently drops unrecognized methods", {

    methods <- echofinemap::lfm(
        finemap_methods = c("ABF", "FAKE_METHOD"),
        verbose = FALSE
    )
    testthat::expect_equal(methods, "ABF")
    testthat::expect_false("FAKE_METHOD" %in% methods)
})

test_that("lfm returns empty when all methods unrecognized", {

    methods <- echofinemap::lfm(
        finemap_methods = c("FAKE1", "FAKE2"),
        verbose = FALSE
    )
    testthat::expect_length(methods, 0)
})

test_that("list_finemap_methods is an alias for lfm", {

    m1 <- echofinemap::lfm(verbose = FALSE)
    m2 <- echofinemap:::list_finemap_methods(verbose = FALSE)
    testthat::expect_equal(m1, m2)
})
