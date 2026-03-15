test_that("is_32bit returns logical", {

    res <- echofinemap:::is_32bit()
    testthat::expect_true(is.logical(res))
    testthat::expect_length(res, 1)
})

test_that("is_32bit returns FALSE on 64-bit systems", {

    ## On any modern system running these tests, expect FALSE
    ## (32-bit Windows R is essentially extinct)
    if(.Platform$r_arch != "i386" || .Platform$OS.type != "windows") {
        testthat::expect_false(echofinemap:::is_32bit())
    }
})
