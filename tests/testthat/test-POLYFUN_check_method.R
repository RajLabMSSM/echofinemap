test_that("POLYFUN_check_method accepts valid methods", {

    res_s <- echofinemap:::POLYFUN_check_method(
        method = "SUSIE", verbose = FALSE
    )
    testthat::expect_equal(res_s, "SUSIE")

    res_f <- echofinemap:::POLYFUN_check_method(
        method = "FINEMAP", verbose = FALSE
    )
    testthat::expect_equal(res_f, "FINEMAP")
})

test_that("POLYFUN_check_method handles lowercase input", {

    res <- echofinemap:::POLYFUN_check_method(
        method = "susie", verbose = FALSE
    )
    testthat::expect_equal(res, "SUSIE")
})

test_that("POLYFUN_check_method errors on invalid method", {

    testthat::expect_error(
        echofinemap:::POLYFUN_check_method(
            method = "INVALID", verbose = FALSE
        ),
        regexp = "must be one of"
    )
})

test_that("POLYFUN_check_method takes first valid when multiple given", {

    res <- echofinemap:::POLYFUN_check_method(
        method = c("SUSIE", "FINEMAP"), verbose = FALSE
    )
    testthat::expect_equal(res, "SUSIE")
})
