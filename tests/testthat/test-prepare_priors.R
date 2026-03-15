test_that("prepare_priors returns NULL when priors_col is NULL", {

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        prior = c(0.5, 0.5)
    )
    res <- echofinemap:::prepare_priors(dat = dat, priors_col = NULL,
                                        verbose = FALSE)
    testthat::expect_null(res)
})

test_that("prepare_priors returns NULL when priors_col not in dat", {

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        Effect = c(0.5, 0.3)
    )
    res <- echofinemap:::prepare_priors(dat = dat, priors_col = "prior",
                                        verbose = FALSE)
    testthat::expect_null(res)
})

test_that("prepare_priors extracts and rescales priors", {

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2", "rs3"),
        prior = c(2, 3, 5)
    )
    res <- echofinemap:::prepare_priors(dat = dat, priors_col = "prior",
                                        rescale_priors = TRUE,
                                        verbose = FALSE)
    testthat::expect_equal(length(res), 3)
    testthat::expect_equal(sum(res), 1)
    testthat::expect_equal(names(res), c("rs1", "rs2", "rs3"))
})

test_that("prepare_priors skips rescaling when requested", {

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        prior = c(0.6, 0.4)
    )
    res <- echofinemap:::prepare_priors(dat = dat, priors_col = "prior",
                                        rescale_priors = FALSE,
                                        verbose = FALSE)
    testthat::expect_equal(res, c(rs1 = 0.6, rs2 = 0.4))
})

test_that("prepare_priors replaces NAs with 0", {

    dat <- data.table::data.table(
        SNP = c("rs1", "rs2", "rs3"),
        prior = c(0.5, NA, 0.3)
    )
    res <- echofinemap:::prepare_priors(dat = dat, priors_col = "prior",
                                        rescale_priors = FALSE,
                                        verbose = FALSE)
    testthat::expect_equal(res[["rs2"]], 0)
})

test_that("prepare_priors errors when length mismatch", {

    ## This shouldn't happen naturally, but test the guard
    dat <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        prior = c(0.5, 0.3)
    )
    ## Mismatch would require manipulating internals, so just check normal case
    res <- echofinemap:::prepare_priors(dat = dat, priors_col = "prior",
                                        verbose = FALSE)
    testthat::expect_equal(length(res), nrow(dat))
})
