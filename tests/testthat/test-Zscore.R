test_that("Zscore computes correct z-scores", {

    z.info <- list(sample.mean = 0, sample.stdv = 1)
    res <- echofinemap:::Zscore(x = 2, z.info = z.info)
    testthat::expect_equal(res, 2)

    z.info2 <- list(sample.mean = 5, sample.stdv = 2)
    res2 <- echofinemap:::Zscore(x = 9, z.info = z.info2)
    testthat::expect_equal(res2, 2)

    ## Vectorized input
    z.info3 <- list(sample.mean = 10, sample.stdv = 5)
    res3 <- echofinemap:::Zscore(x = c(10, 15, 20), z.info = z.info3)
    testthat::expect_equal(res3, c(0, 1, 2))
})
